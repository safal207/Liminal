"""
Тест для мягкого эндпоинта /ready в degraded режиме.
"""

import pytest
from fastapi.testclient import TestClient
from unittest.mock import Mock, patch

from backend.main import app


def test_ready_endpoint_degraded_redis():
    """Тест /ready когда Redis недоступен (degraded статус)."""
    client = TestClient(app)
    
    # Мокаем app.state.redis_client с DummyRedis
    mock_redis_client = Mock()
    mock_redis_client.client = Mock()
    mock_redis_client.client.ping.side_effect = ConnectionError("Redis connection failed")
    
    with patch.object(app.state, 'redis_client', mock_redis_client):
        response = client.get("/ready")
    
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "degraded"
    assert "components" in data
    assert data["components"]["redis"]["status"] == "error"


def test_ready_endpoint_degraded_neo4j():
    """Тест /ready когда Neo4j недоступен (degraded статус)."""
    client = TestClient(app)
    
    # Мокаем успешный Redis
    mock_redis_client = Mock()
    mock_redis_client.client = Mock()
    mock_redis_client.client.ping.return_value = True
    
    # Мокаем Neo4j с ошибкой
    with patch.object(app.state, 'redis_client', mock_redis_client), \
         patch('neo4j.GraphDatabase.driver') as mock_driver:
        mock_driver.side_effect = ConnectionError("Neo4j connection failed")
        
        response = client.get("/ready")
    
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "degraded"
    assert "components" in data
    assert data["components"]["redis"]["status"] == "ok"
    assert data["components"]["neo4j"]["status"] == "error"


def test_ready_endpoint_ready():
    """Тест /ready когда все компоненты работают (ready статус)."""
    client = TestClient(app)
    
    # Мокаем успешный Redis
    mock_redis_client = Mock()
    mock_redis_client.client = Mock()
    mock_redis_client.client.ping.return_value = True
    
    # Мокаем успешный Neo4j
    mock_driver = Mock()
    mock_session = Mock()
    mock_result = Mock()
    mock_result.single.return_value = {"test": 1}
    mock_session.run.return_value = mock_result
    mock_driver.session.return_value.__enter__.return_value = mock_session
    
    with patch.object(app.state, 'redis_client', mock_redis_client), \
         patch('neo4j.GraphDatabase.driver', return_value=mock_driver):
        
        response = client.get("/ready")
    
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "ready"
    assert "components" in data
    assert data["components"]["redis"]["status"] == "ok"
    assert data["components"]["neo4j"]["status"] == "ok"
