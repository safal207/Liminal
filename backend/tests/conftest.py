"""Test configuration for the refactored backend FastAPI app."""
from __future__ import annotations

from typing import Iterator

import pytest
from fastapi.testclient import TestClient

from backend.app.dependencies import (
    get_memory_service,
    get_neo4j_service,
)
from backend.app.main import app
from backend.app.services.neo4j import Neo4jService
from backend.infrastructure.neo4j.mock import MockNeo4jGateway


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


