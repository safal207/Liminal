"""Unit tests for optional GraphQL dependency handling in personality router."""

from importlib import import_module
from types import ModuleType
import sys


def test_create_graphql_router_handles_missing_strawberry(monkeypatch):
    """_create_graphql_router should gracefully handle missing strawberry."""

    config_stub = ModuleType("config")
    config_stub.get_database_settings = lambda: {}  # type: ignore[attr-defined]
    monkeypatch.setitem(sys.modules, "config", config_stub)

    datomic_stub = ModuleType("datomic_client")

    class _StubDatomicClient:  # pragma: no cover - simple test double
        def __init__(self, *args, **kwargs):
            pass

    class _StubConnectionError(Exception):
        pass

    datomic_stub.DatomicClient = _StubDatomicClient  # type: ignore[attr-defined]
    datomic_stub.DatomicConnectionError = _StubConnectionError  # type: ignore[attr-defined]
    monkeypatch.setitem(sys.modules, "datomic_client", datomic_stub)

    jwt_utils_stub = ModuleType("backend.auth.jwt_utils")

    class _StubUser:  # pragma: no cover - simple value object
        def __init__(self, user_id: str = "test-user"):
            self.id = user_id

    async def _stub_get_current_user():  # pragma: no cover - FastAPI dependency stub
        return _StubUser()

    jwt_utils_stub.User = _StubUser  # type: ignore[attr-defined]
    jwt_utils_stub.get_current_user = _stub_get_current_user  # type: ignore[attr-defined]
    monkeypatch.setitem(sys.modules, "backend.auth.jwt_utils", jwt_utils_stub)

    personality_router = import_module("backend.personality.router")

    def fake_import(name: str) -> ModuleType:
        if name.startswith("strawberry"):
            raise ModuleNotFoundError("strawberry")
        return import_module(name)

    monkeypatch.setattr(personality_router, "import_module", fake_import)

    graphql_router = personality_router._create_graphql_router()

    assert graphql_router is None
