"""Negative tests for the release-gate security boundaries."""

from __future__ import annotations

import ast
import importlib
import inspect
from datetime import UTC, datetime, timedelta
from pathlib import Path
from unittest.mock import AsyncMock, MagicMock

import jwt
import pytest
from fastapi import HTTPException, Response

import backend.app.routes.debug as debug_routes
import backend.app.routes.ws as websocket_routes
import backend.auth.jwt_utils as jwt_utils
import backend.config as legacy_config
import scripts.check_release_security as release_security
from backend.app.services.websocket import (
    ConnectionManagerService,
    LocalTokenBucket,
    TimelineWebSocketService,
    WebSocketMessageError,
    parse_client_message,
)
from backend.auth.dependencies import TokenVerifier
from backend.auth.jwt_utils import JWTManager
from backend.core.settings import (
    DEFAULT_SECRET,
    IntegrationSettings,
    JWTSettings,
    Settings,
)
from backend.memory_timeline import MemoryTimeline
from backend.websocket.connection_manager import ConnectionManager
from backend.websocket.redis_connection_manager import RedisConnectionManager

core_settings = importlib.import_module("backend.core.settings")

STRONG_SECRET = "release-gate-test-secret-0123456789abcdef"


def make_manager(secret: str = STRONG_SECRET) -> JWTManager:
    return JWTManager(Settings(jwt=JWTSettings(secret_key=secret)))


def test_production_rejects_default_jwt_secret(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setenv("ENV", "production")
    with pytest.raises(RuntimeError, match="JWT_SECRET_KEY"):
        make_manager(DEFAULT_SECRET)


def test_flat_environment_names_load_into_central_settings(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    monkeypatch.setenv("JWT_SECRET_KEY", STRONG_SECRET)
    monkeypatch.setenv("NEO4J_PASSWORD", "non-default-password")
    monkeypatch.setenv("USE_REDIS", "true")
    core_settings.get_settings.cache_clear()

    loaded = core_settings.get_settings()

    assert loaded.jwt.secret_key == STRONG_SECRET
    assert loaded.integrations.neo4j_password == "non-default-password"
    assert loaded.integrations.use_redis is True

    core_settings.get_settings.cache_clear()


def test_compat_config_uses_same_authoritative_jwt_secret(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    central = Settings(
        jwt=JWTSettings(secret_key=STRONG_SECRET),
        integrations=IntegrationSettings(neo4j_password="non-default-password"),
    )
    monkeypatch.setattr(legacy_config, "get_core_settings", lambda: central)
    monkeypatch.setenv("ENV", "production")
    legacy_config.get_settings.cache_clear()

    assert legacy_config.get_settings().jwt_secret_key == central.jwt.secret_key

    legacy_config.get_settings.cache_clear()


def test_production_debug_routes_are_disabled_by_default(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    settings = type("Settings", (), {"environment": "production"})()
    monkeypatch.setattr(debug_routes, "get_settings", lambda: settings)
    monkeypatch.delenv("ENABLE_DEBUG_ROUTES", raising=False)

    with pytest.raises(HTTPException) as exc_info:
        debug_routes.require_debug_routes_enabled()

    assert exc_info.value.status_code == 404


def test_debug_availability_gate_runs_before_authentication() -> None:
    dependencies = [
        dependency.dependency for dependency in debug_routes.router.dependencies
    ]

    assert dependencies == [
        debug_routes.require_debug_routes_enabled,
        debug_routes.require_debug_access,
    ]


def test_access_token_has_explicit_purpose() -> None:
    manager = make_manager()
    token = manager.create_access_token({"sub": "user-1"})

    payload = manager.verify_token(token, expected_type="access")

    assert payload is not None
    assert payload["token_type"] == "access"


def test_refresh_token_is_rejected_by_access_dependency() -> None:
    manager = make_manager()
    refresh = manager.create_access_token(
        {"sub": "user-1", "token_type": "refresh"},
        expires_delta=timedelta(days=7),
    )
    verifier = TokenVerifier(manager, expected_type="access")

    with pytest.raises(HTTPException) as exc_info:
        verifier._validate(refresh)

    assert exc_info.value.status_code == 401


def test_access_token_is_rejected_as_refresh_token() -> None:
    manager = make_manager()
    access = manager.create_access_token({"sub": "user-1"})

    assert manager.verify_token(access, expected_type="refresh") is None


def test_token_without_purpose_is_rejected() -> None:
    manager = make_manager()
    token = jwt.encode(
        {
            "sub": "user-1",
            "exp": datetime.now(UTC) + timedelta(minutes=5),
        },
        manager.secret_key,
        algorithm=manager.algorithm,
    )

    assert manager.verify_token(token, expected_type="access") is None


def test_legacy_sha256_password_hash_is_rejected() -> None:
    manager = make_manager()

    assert manager.verify_password("password", "sha256$deadbeef") is False


def test_malformed_password_hash_is_rejected() -> None:
    assert make_manager().verify_password("password", "not-a-password-hash") is False


def test_password_verifier_operational_failure_is_not_hidden(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    class BrokenPasswordContext:
        def verify(self, plain_password: str, hashed_password: str) -> bool:
            raise RuntimeError("password verifier unavailable")

    monkeypatch.setattr(jwt_utils, "pwd_context", BrokenPasswordContext())

    with pytest.raises(RuntimeError, match="password verifier unavailable"):
        make_manager().verify_password("password", "valid-looking-hash")


def test_auth_logs_do_not_include_user_identifiers(
    caplog: pytest.LogCaptureFixture,
) -> None:
    user_identifier = "sensitive-user@example.test"
    caplog.set_level("DEBUG", logger="auth.jwt_utils")

    make_manager().create_access_token({"sub": user_identifier})
    jwt_utils.authenticate_user(user_identifier, "incorrect-password")

    assert user_identifier not in caplog.text


def test_password_hashing_failure_does_not_downgrade(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    class BrokenPasswordContext:
        def hash(self, password: str) -> str:
            raise ValueError("bcrypt failure")

    monkeypatch.setattr(jwt_utils, "pwd_context", BrokenPasswordContext())

    with pytest.raises(ValueError, match="bcrypt failure"):
        make_manager().get_password_hash("secret")


def test_websocket_rejects_oversized_message() -> None:
    oversized = '{"type":"broadcast","channel":"x","content":"' + ("a" * 20000) + '"}'

    with pytest.raises(WebSocketMessageError) as exc_info:
        parse_client_message(oversized)

    assert exc_info.value.code == "message_too_large"


@pytest.mark.parametrize(
    "message, expected_code",
    [
        ('{"type":"unknown"}', "unknown_type"),
        ('{"type":"subscribe","channel":"bad channel"}', "invalid_channel"),
        (
            '{"type":"subscribe","channel":"timeline","extra":true}',
            "invalid_schema",
        ),
        ('{"type":"broadcast","channel":"timeline","content":""}', "invalid_content"),
    ],
)
def test_websocket_schema_rejects_invalid_messages(
    message: str,
    expected_code: str,
) -> None:
    with pytest.raises(WebSocketMessageError) as exc_info:
        parse_client_message(message)

    assert exc_info.value.code == expected_code


def test_auth_phase_rejects_non_auth_message() -> None:
    with pytest.raises(WebSocketMessageError) as exc_info:
        parse_client_message(
            '{"type":"subscribe","channel":"timeline"}',
            expected_types={"auth"},
        )

    assert exc_info.value.code == "unexpected_type"


def test_local_rate_limit_protects_when_redis_is_unavailable() -> None:
    limiter = LocalTokenBucket(rate=1, burst=2)

    assert limiter.is_limited("connection") is False
    assert limiter.is_limited("connection") is False
    assert limiter.is_limited("connection") is True


def test_websocket_authentication_has_no_query_token_parameter() -> None:
    assert (
        "token" not in inspect.signature(websocket_routes.websocket_timeline).parameters
    )
    assert (
        "token"
        not in inspect.signature(TimelineWebSocketService.handle_connection).parameters
    )


def test_legacy_api_entrypoint_reexports_hardened_app() -> None:
    legacy_entrypoint = importlib.import_module("api")
    hardened_main = importlib.import_module("backend.app.main")

    assert legacy_entrypoint.app is hardened_main.app
    assert str(legacy_entrypoint.app.url_path_for("websocket_timeline")) == (
        "/ws/timeline"
    )


def test_legacy_backend_route_delegates_without_query_token() -> None:
    source = (Path(__file__).resolve().parents[2] / "backend" / "api.py").read_text(
        encoding="utf-8"
    )
    module = ast.parse(source)
    endpoint = next(
        node
        for node in module.body
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
        and node.name == "websocket_timeline"
    )

    assert "token" not in {argument.arg for argument in endpoint.args.args}
    assert any(
        isinstance(node, ast.Attribute) and node.attr == "handle_connection"
        for node in ast.walk(endpoint)
    )


@pytest.mark.asyncio
async def test_readiness_returns_503_when_required_redis_is_disconnected() -> None:
    hardened_main = importlib.import_module("backend.app.main")
    response = Response()
    manager = type("DisconnectedRedisManager", (), {"_is_connected": False})()

    payload = await hardened_main.readiness_check(response, manager)

    assert response.status_code == 503
    assert payload["ready"] is False
    assert payload["checks"]["redis_connected"] is False


def test_core_entrypoint_imports_the_gated_personality_router() -> None:
    source = (
        Path(__file__).resolve().parents[2] / "backend" / "app" / "main.py"
    ).read_text(encoding="utf-8")
    module = ast.parse(source)
    personality_imports = [
        node
        for node in ast.walk(module)
        if isinstance(node, ast.ImportFrom)
        and node.module is not None
        and node.module.startswith("backend.personality")
    ]

    assert any(node.module == "backend.personality" for node in personality_imports)
    assert all(
        node.module != "backend.personality.router" for node in personality_imports
    )


def test_redis_factory_wires_shared_rate_limit_configuration(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    monkeypatch.setenv("USE_REDIS", "true")
    monkeypatch.setenv("WS_RATE_LIMIT_PER_SECOND", "7")
    monkeypatch.setenv("WS_RATE_LIMIT_BURST", "11")

    manager = ConnectionManagerService()._create_manager()

    assert isinstance(manager, RedisConnectionManager)
    assert manager.redis_client is manager.redis
    assert manager.rate_limit_messages_per_second == 7
    assert manager.rate_limit_burst == 11


@pytest.mark.asyncio
async def test_redis_manager_loads_distributed_rate_limit_script() -> None:
    manager = RedisConnectionManager()
    pubsub = MagicMock()
    pubsub.subscribe = AsyncMock()
    manager.redis.redis = MagicMock()
    manager.redis.redis.pubsub.return_value = pubsub
    manager.redis.connect = AsyncMock(return_value=True)
    manager.redis.subscribe = AsyncMock()
    manager._load_rate_limit_script = AsyncMock()

    assert await manager.initialize() is True

    manager._load_rate_limit_script.assert_awaited_once_with()


@pytest.mark.asyncio
async def test_redis_manager_preserves_base_subscription_contract() -> None:
    assert inspect.signature(RedisConnectionManager.subscribe) == inspect.signature(
        ConnectionManager.subscribe
    )
    assert inspect.signature(RedisConnectionManager.unsubscribe) == inspect.signature(
        ConnectionManager.unsubscribe
    )
    assert inspect.signature(RedisConnectionManager.broadcast) == inspect.signature(
        ConnectionManager.broadcast
    )
    assert inspect.signature(
        RedisConnectionManager.send_personal_message
    ) == inspect.signature(ConnectionManager.send_personal_message)
    assert inspect.signature(
        RedisConnectionManager.get_connection_stats
    ) == inspect.signature(ConnectionManager.get_connection_stats)

    manager = RedisConnectionManager()
    sender = AsyncMock()
    recipient = AsyncMock()
    await manager.subscribe("sender", "timeline", sender)
    await manager.subscribe("recipient", "timeline", recipient)

    sent_count = await manager.broadcast(
        "timeline", {"type": "message"}, sender_id="sender"
    )

    assert sent_count == 1
    sender.send_json.assert_not_awaited()
    recipient.send_json.assert_awaited_once_with({"type": "message"})

    await manager.send_personal_message({"type": "personal"}, "recipient")
    recipient.send_json.assert_awaited_with({"type": "personal"})

    await manager.unsubscribe("recipient", "timeline")
    assert manager.is_user_subscribed("recipient", "timeline") is False
    assert manager.get_connection_stats()["is_distributed"] is False


@pytest.mark.asyncio
async def test_timeline_does_not_reauthenticate_an_authenticated_socket() -> None:
    timeline = MemoryTimeline()
    websocket = AsyncMock()
    websocket.user_id = "authenticated-user"
    websocket.headers = {}

    await timeline.subscribe(websocket)

    websocket.close.assert_not_awaited()
    websocket.send_json.assert_awaited_once()
    assert websocket in timeline.subscribers


def test_release_guard_rejects_research_dependencies() -> None:
    dockerfile = """\
ARG PYTHON_IMAGE
FROM ${PYTHON_IMAGE}
RUN pip install -r requirements-core.txt -r requirements-research.txt
"""

    assert release_security.production_dockerfile_errors(dockerfile) == [
        "production Dockerfile must not install test/dev/research dependencies"
    ]


def test_artillery_websocket_smoke_uses_in_band_authentication() -> None:
    load_test = (
        Path(__file__).resolve().parents[2] / "tests/load/ws-burst.yml"
    ).read_text(encoding="utf-8")
    websocket_scenario = load_test.split("scenarios:", 1)[1]

    assert "?token=" not in load_test
    assert "engine: ws" in websocket_scenario
    assert websocket_scenario.index("- connect:") < websocket_scenario.index("- send:")
    assert '"type":"auth","token":"{{ token }}"' in websocket_scenario
