"""Integration tests for the simplified Emotime API that power the BurnoutGuard chat UI."""

from __future__ import annotations

import importlib
from pathlib import Path
from typing import Tuple

import pytest
from fastapi.testclient import TestClient


@pytest.fixture()
def api_client(tmp_path: Path, monkeypatch) -> Tuple[TestClient, Path]:
    """Create a TestClient instance backed by a temporary SQLite database."""
    db_path = tmp_path / "emotime_chat.db"
    monkeypatch.setenv("EMOTIME_DB_PATH", str(db_path))

    # Reload the module so that it picks up the overridden environment variable.
    import backend.emotime.api_simple as api_simple_module

    api_simple = importlib.reload(api_simple_module)

    client = TestClient(api_simple.app)
    # Trigger startup actions (init_db). TestClient.as_context ensures shutdown too.
    client.__enter__()

    try:
        yield client, db_path
    finally:
        client.__exit__(None, None, None)


def test_health_endpoint(api_client):
    client, db_path = api_client

    response = client.get("/health")
    payload = response.json()

    assert response.status_code == 200
    assert payload["status"] == "healthy"
    assert "timestamp" in payload
    assert db_path.exists(), "Database should be created during startup"


def test_analyze_requires_authentication(api_client):
    client, _ = api_client

    response = client.post("/analyze", json={"text": "Hello", "language": "en"})
    assert response.status_code == 403


def test_chat_flow_creates_usage_record(api_client):
    client, db_path = api_client

    user_response = client.post(
        "/users",
        json={"email": "chat-user@example.com", "plan": "starter"},
    )
    assert user_response.status_code == 200
    user_payload = user_response.json()
    api_key = user_payload["api_key"]

    headers = {"Authorization": f"Bearer {api_key}"}
    analysis_response = client.post(
        "/analyze",
        json={"text": "I am incredibly happy today!", "language": "en"},
        headers=headers,
    )
    assert analysis_response.status_code == 200
    analysis_payload = analysis_response.json()

    joy_score = analysis_payload["emotions"].get("joy", 0)
    assert joy_score > 0, "Joy should be detected in a positive text sample"
    assert analysis_payload["confidence"] > 0

    usage_response = client.get("/usage", headers=headers)
    assert usage_response.status_code == 200
    usage_payload = usage_response.json()

    assert usage_payload["today_usage"] == 1
    assert usage_payload["plan"] == "starter"
    assert db_path.exists()
