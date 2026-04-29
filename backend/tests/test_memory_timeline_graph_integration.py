"""Integration tests for the memory timeline to Neo4j gateway bridge."""

from __future__ import annotations

from typing import Dict, Iterator

import pytest
from fastapi.testclient import TestClient

from backend.app.main import app
from backend.infrastructure.neo4j.mock import MockNeo4jGateway


@pytest.fixture()
def client_with_mock_gateway(
    override_neo4j_service: MockNeo4jGateway,
) -> Iterator[TestClient]:
    """Use the autouse mock gateway so routes and the timeline bridge share one instance."""

    gateway = override_neo4j_service
    with TestClient(app) as client:
        client.app.state.mock_gateway = gateway  # type: ignore[attr-defined]
        yield client


def _gateway(client: TestClient) -> MockNeo4jGateway:
    gateway = getattr(client.app.state, "mock_gateway", None)
    assert isinstance(gateway, MockNeo4jGateway)
    return gateway


def test_timeline_event_creates_fragment(client_with_mock_gateway: TestClient) -> None:
    client = client_with_mock_gateway
    payload: Dict[str, object] = {
        "content": "Discover hidden dune patterns",
        "memory_type": "observation",
        "metadata": {"growth_stage": "seedling", "tags": ["dune", "insight"]},
    }

    response = client.post("/timeline/memories/", json=payload)
    assert response.status_code == 201
    data = response.json()
    assert data["content"] == payload["content"]

    gateway = _gateway(client)
    assert len(gateway.memory_fragments) == 1
    stored = gateway.memory_fragments[0]
    assert stored["growth_stage"] == "seedling"
    assert stored["metadata"]["tags"] == ["dune", "insight"]

    fragments_response = client.get("/fragments/")
    assert fragments_response.status_code == 200
    fragments = fragments_response.json()
    assert len(fragments) == 1
    assert fragments[0]["content"] == payload["content"]


def test_direct_fragment_creation_uses_gateway(
    client_with_mock_gateway: TestClient,
) -> None:
    client = client_with_mock_gateway
    fragment_payload = {
        "content": "Map ancient mentorship links",
        "type": "mentor",
        "growth_stage": "sprout",
        "metadata": {"notes": "origin story"},
    }

    response = client.post("/fragments/", json=fragment_payload)
    assert response.status_code == 200

    gateway = _gateway(client)
    assert any(
        fragment["content"] == fragment_payload["content"]
        for fragment in gateway.memory_fragments
    )
