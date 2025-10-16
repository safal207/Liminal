"""Test configuration for the refactored backend FastAPI app."""
from __future__ import annotations

import os
from pathlib import Path
from typing import Iterator

import importlib.util

import pytest
from fastapi.testclient import TestClient

from backend.app.dependencies import (
    get_memory_service,
    get_neo4j_service,
)
from backend.app.main import app
from backend.app.services.neo4j import Neo4jService
from backend.infrastructure.neo4j.mock import MockNeo4jGateway


_OPTIONAL_MODULE_TESTS = {
    "colorama": {
        "tests/test_metrics_integration.py",
        "tests/test_metrics_with_auth.py",
    },
    "selenium": {
        "tests/test_websocket_chat.py",
        "tests/test_websocket_selenium.py",
    },
    "webdriver_manager": {
        "tests/test_websocket_chat.py",
        "tests/test_websocket_selenium.py",
    },
}


_OPTIONAL_ENV_TESTS = {
    "tests/test_metrics_standalone.py": "RUN_STANDALONE_METRICS_TESTS",
    "tests/test_websocket_chat.py": "RUN_SELENIUM_TESTS",
    "tests/test_websocket_selenium.py": "RUN_SELENIUM_TESTS",
}


def _module_missing(name: str) -> bool:
    return importlib.util.find_spec(name) is None


@pytest.hookimpl(tryfirst=True)
def pytest_ignore_collect(collection_path, path=None, config=None):  # type: ignore[override]
    """Skip optional integration tests when their dependencies are absent."""

    if config is None:
        # Legacy signature: pytest<8 passes (path, config)
        config = path
        file_path = Path(str(collection_path))
    else:
        file_path = Path(str(getattr(collection_path, "path", collection_path)))

    if file_path.suffix != ".py" or config is None:
        return False

    root = Path(config.rootpath)
    try:
        relative = file_path.resolve().relative_to(root)
    except ValueError:
        relative = file_path.name

    normalized = str(relative).replace("\\", "/")
    if normalized in _OPTIONAL_ENV_TESTS:
        env_var = _OPTIONAL_ENV_TESTS[normalized]
        if not os.getenv(env_var):
            return True

    for module, targets in _OPTIONAL_MODULE_TESTS.items():
        if normalized in targets and _module_missing(module):
            return True
    return False


@pytest.fixture(autouse=True)
def override_neo4j_service() -> Iterator[MockNeo4jGateway]:
    """Inject the in-memory Neo4j gateway for every test."""

    gateway = MockNeo4jGateway()
    service = Neo4jService(writer=gateway)
    app.dependency_overrides[get_neo4j_service] = lambda: service
    try:
        yield gateway
    finally:
        app.dependency_overrides.pop(get_neo4j_service, None)


@pytest.fixture
def client(override_neo4j_service: MockNeo4jGateway) -> Iterator[TestClient]:
    """Create a TestClient and reset timeline state between tests."""

    memory_service = get_memory_service()
    timeline = memory_service.get_timeline()
    timeline.timeline.clear()
    if hasattr(timeline, "_subscribers"):
        timeline._subscribers.clear()  # type: ignore[attr-defined]

    original_subscribe = getattr(timeline, "subscribe", None)
    original_unsubscribe = getattr(timeline, "unsubscribe", None)

    async def _test_subscribe(websocket):  # type: ignore[no-untyped-def]
        if hasattr(timeline, "_subscribers"):
            timeline._subscribers.append(websocket)  # type: ignore[attr-defined]
        await websocket.send_json({"event": "initial_state", "data": []})

    async def _test_unsubscribe(websocket):  # type: ignore[no-untyped-def]
        if hasattr(timeline, "_subscribers"):
            try:
                timeline._subscribers.remove(websocket)  # type: ignore[attr-defined]
            except ValueError:
                pass

    if original_subscribe is not None:
        timeline.subscribe = _test_subscribe  # type: ignore[attr-defined]
    if original_unsubscribe is not None:
        timeline.unsubscribe = _test_unsubscribe  # type: ignore[attr-defined]

    with TestClient(app) as test_client:
        yield test_client

    if original_subscribe is not None:
        timeline.subscribe = original_subscribe  # type: ignore[attr-defined]
    if original_unsubscribe is not None:
        timeline.unsubscribe = original_unsubscribe  # type: ignore[attr-defined]


