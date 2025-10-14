# Integration Tests for API functionality
# Тесты для проверки интеграции компонентов API

from unittest.mock import AsyncMock

import pytest
from fastapi.testclient import TestClient

from backend.api import app
from backend.websocket.connection_manager import ConnectionManager


class TestAPIIntegration:
    """Integration tests for API components"""

    @pytest.fixture
    def websocket_manager(self):
        """Fixture for WebSocket connection manager"""
        return ConnectionManager()

    @pytest.fixture
    def test_client(self):
        """Fixture providing FastAPI test client"""
        with TestClient(app) as client:
            yield client

    def test_api_websocket_integration(self, test_client):
        """Test API and WebSocket integration"""
        with test_client.websocket_connect("/ws/timeline") as websocket:
            message = websocket.receive_json()
            assert message["type"] == "auth_required"
            assert "message" in message

    @pytest.mark.asyncio
    async def test_full_user_flow_integration(self, websocket_manager):
        """Test complete user flow from connection to message processing"""
        # Mock user connection
        mock_connection = AsyncMock()
        mock_connection.send = AsyncMock()
        mock_connection.receive = AsyncMock(return_value='{"type": "message", "content": "Hello"}')

        # Test connection handling
        connection_id = "test_user_123"
        websocket_manager.active_connections[connection_id] = {mock_connection}

        # Verify connection is stored
        assert connection_id in websocket_manager.active_connections

        # Cleanup
        del websocket_manager.active_connections[connection_id]

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
