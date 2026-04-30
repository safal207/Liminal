"""Тест эндпоинта /ready (актуальная форма ответа modular app)."""

from unittest.mock import MagicMock

from fastapi.testclient import TestClient

from backend.app.dependencies import get_connection_manager
from backend.main import app


def test_ready_endpoint_reports_checks():
    client = TestClient(app)
    response = client.get("/ready")
    assert response.status_code == 200
    data = response.json()
    assert "ready" in data
    assert "checks" in data
    assert data["checks"]["app_loaded"] is True


def test_ready_when_redis_down_via_connection_manager():
    mock_manager = MagicMock()
    mock_manager._is_connected = True

    client = TestClient(app)
    try:
        app.dependency_overrides[get_connection_manager] = lambda: mock_manager
        ok = client.get("/ready").json()
        assert ok["ready"] is True

        mock_manager._is_connected = False
        degraded = client.get("/ready").json()
        assert degraded["ready"] is False
        assert degraded["checks"]["redis_connected"] is False
    finally:
        app.dependency_overrides.pop(get_connection_manager, None)
