"""Конфигурация тестов для работы с FastAPI приложением."""

import pytest
from fastapi.testclient import TestClient

from backend.api import app, get_neo4j_gateway, memory_timeline
from backend.infrastructure.neo4j.mock import MockNeo4jGateway


@pytest.fixture(autouse=True)
def override_neo4j_gateway():
    """Подмешивает моковую реализацию Neo4j для всех тестов."""

    gateway = MockNeo4jGateway()
    app.dependency_overrides[get_neo4j_gateway] = lambda: gateway
    try:
        yield gateway
    finally:
        app.dependency_overrides.pop(get_neo4j_gateway, None)


@pytest.fixture
def client(override_neo4j_gateway):
    """Создает тестовый клиент для FastAPI приложения."""

    memory_timeline.timeline = []
    if hasattr(memory_timeline, "_subscribers"):
        memory_timeline._subscribers.clear()

    original_subscribe = getattr(memory_timeline, "subscribe", None)
    original_unsubscribe = getattr(memory_timeline, "unsubscribe", None)

    async def _test_subscribe(websocket):
        if hasattr(memory_timeline, "_subscribers"):
            memory_timeline._subscribers.append(websocket)
        await websocket.send_json({"event": "initial_state", "data": []})

    async def _test_unsubscribe(websocket):
        if hasattr(memory_timeline, "_subscribers"):
            try:
                memory_timeline._subscribers.remove(websocket)
            except ValueError:
                pass

    if original_subscribe is not None:
        memory_timeline.subscribe = _test_subscribe  # type: ignore[attr-defined]
    if original_unsubscribe is not None:
        memory_timeline.unsubscribe = _test_unsubscribe  # type: ignore[attr-defined]

    with TestClient(app) as test_client:
        yield test_client

    if original_subscribe is not None:
        memory_timeline.subscribe = original_subscribe  # type: ignore[attr-defined]
    if original_unsubscribe is not None:
        memory_timeline.unsubscribe = original_unsubscribe  # type: ignore[attr-defined]
