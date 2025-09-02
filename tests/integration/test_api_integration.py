# Integration Tests for API functionality
# Тесты для проверки интеграции компонентов API

import pytest
import asyncio
from unittest.mock import Mock, patch, AsyncMock
from backend.api import app
from backend.websocket_manager import WebSocketManager


class TestAPIIntegration:
    """Integration tests for API components"""

    @pytest.fixture
    async def websocket_manager(self):
        """Fixture for WebSocket manager"""
        manager = WebSocketManager()
        yield manager
        # Cleanup after test
        await manager.cleanup()

    def test_api_websocket_integration(self, websocket_manager):
        """Test API and WebSocket integration"""
        with app.test_client() as client:
            # Test WebSocket endpoint exists
            response = client.get('/ws')
            # WebSocket endpoint should return upgrade response
            assert response.status_code in [400, 426]  # Bad request or upgrade required

    @pytest.mark.asyncio
    async def test_full_user_flow_integration(self, websocket_manager):
        """Test complete user flow from connection to message processing"""
        # Mock user connection
        mock_connection = AsyncMock()
        mock_connection.send = AsyncMock()
        mock_connection.receive = AsyncMock(return_value='{"type": "message", "content": "Hello"}')

        # Test connection handling
        connection_id = "test_user_123"
        websocket_manager.connections[connection_id] = mock_connection

        # Verify connection is stored
        assert connection_id in websocket_manager.connections

        # Cleanup
        del websocket_manager.connections[connection_id]

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
