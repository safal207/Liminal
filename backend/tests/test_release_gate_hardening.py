"""Negative tests for the release-gate security boundaries."""

from __future__ import annotations

from datetime import datetime, timedelta

import pytest
from fastapi import HTTPException
from jose import jwt

import backend.app.routes.debug as debug_routes
import backend.auth.jwt_utils as jwt_utils
import backend.config as legacy_config
import backend.core.settings as core_settings
from backend.app.services.websocket import (
    LocalTokenBucket,
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
        debug_routes.require_debug_access({"sub": "user-1"})

    assert exc_info.value.status_code == 404


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
            "exp": datetime.utcnow() + timedelta(minutes=5),
        },
        manager.secret_key,
        algorithm=manager.algorithm,
    )

    assert manager.verify_token(token, expected_type="access") is None


def test_legacy_sha256_password_hash_is_rejected() -> None:
    manager = make_manager()

    assert manager.verify_password("password", "sha256$deadbeef") is False


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
