# Integration Tests for API functionality
# Тесты для проверки интеграции компонентов API

from types import SimpleNamespace
from unittest.mock import AsyncMock

import pytest
from fastapi.testclient import TestClient

from backend.api import app, connection_manager


class TestAPIIntegration:
    """Integration tests for API components"""

    @pytest.fixture
    def client(self):
        """Provide a FastAPI test client instance."""
        with TestClient(app) as client:
            yield client

    def test_health_endpoint_integration(self, client):
        """Health endpoint should respond with application status."""
        response = client.get("/health")
        assert response.status_code == 200
        payload = response.json()
        assert payload["status"].lower() == "ok"
        assert "ml_enabled" in payload

    def test_ready_endpoint_integration(self, client):
        """Readiness endpoint should expose readiness checks."""
        response = client.get("/ready")
        assert response.status_code == 200
        payload = response.json()
        assert "checks" in payload
        assert isinstance(payload["checks"], dict)

    def test_websocket_auth_flow_integration(self, client):
        """WebSocket connection without token should request authentication."""
        with client.websocket_connect("/ws/timeline") as websocket:
            message = websocket.receive_json()
            assert message["type"] == "auth_required"
            assert "token" not in message

    @pytest.mark.asyncio
    async def test_connection_manager_pending_acceptance(self):
        """Connection manager should accept pending connections for auth."""
        class DummyWebSocket:
            def __init__(self):
                self.client = SimpleNamespace(host="127.0.0.1")
                self.accept = AsyncMock()
                self.close = AsyncMock()

        websocket = DummyWebSocket()
        result = await connection_manager.accept_pending_connection(websocket)
        assert result is True
        assert websocket in connection_manager.pending_connections
        await connection_manager.reject_connection(websocket, "test cleanup")

    def test_database_api_integration(self):
        """Test database and API integration"""
        # This would test actual database operations
        # For now, just verify the import works
        try:
            from backend.database_api import DatabaseAPI

            api = DatabaseAPI()
            assert api is not None
        except ImportError:
            pytest.skip("Database dependencies not available")

    def test_ml_adapter_integration(self):
        """Test ML adapter integration"""
        try:
            from backend.personality.ml_adapter import MLAdapter

            adapter = MLAdapter()
            assert adapter is not None
        except ImportError:
            pytest.skip("ML dependencies not available")
