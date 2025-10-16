# Integration Tests for API functionality
# Тесты для проверки интеграции компонентов API

from fastapi.testclient import TestClient

from backend.app.main import app


class TestAPIIntegration:
    """Integration tests for API components"""

    def test_root_endpoint(self):
        client = TestClient(app)
        response = client.get("/")
        assert response.status_code == 200
        assert response.json()["message"] == "Welcome to LIMINAL API"

    def test_readiness_endpoint(self):
        client = TestClient(app)
        response = client.get("/ready")
        assert response.status_code == 200
        payload = response.json()
        assert "ready" in payload
        assert "checks" in payload
