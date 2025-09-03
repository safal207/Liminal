# Test fixtures for Resonance-Liminal
# Фикстуры для тестирования компонентов системы

import asyncio
from typing import Any, Dict
from unittest.mock import AsyncMock, Mock

import pytest


@pytest.fixture
def sample_user_data() -> Dict[str, Any]:
    """Sample user data for testing"""
    return {
        "user_id": "test_user_123",
        "session_id": "session_456",
        "enhancement_level": 0.5,
        "active_systems": ["emotion_analysis", "consciousness_tracking"],
        "preferences": {"language": "ru", "theme": "dark", "notifications": True},
    }


@pytest.fixture
def sample_emotion_data() -> Dict[str, Any]:
    """Sample emotion analysis data"""
    return {
        "text": "Сегодня прекрасный день, я чувствую себя счастливым",
        "emotions": {
            "joy": 0.85,
            "sadness": 0.05,
            "anger": 0.03,
            "fear": 0.02,
            "surprise": 0.05,
        },
        "dominant_emotion": "joy",
        "confidence": 0.92,
    }


@pytest.fixture
def sample_websocket_message() -> Dict[str, Any]:
    """Sample WebSocket message for testing"""
    return {
        "type": "emotion_update",
        "user_id": "test_user_123",
        "data": {
            "current_emotion": "joy",
            "intensity": 0.8,
            "timestamp": "2024-01-15T10:30:00Z",
        },
        "session_id": "session_456",
    }


@pytest.fixture
async def mock_websocket_connection():
    """Mock WebSocket connection for testing"""
    mock_ws = AsyncMock()
    mock_ws.send = AsyncMock()
    mock_ws.receive = AsyncMock(return_value='{"type": "ping"}')
    mock_ws.close = AsyncMock()
    return mock_ws


@pytest.fixture
def mock_database_response() -> Dict[str, Any]:
    """Mock database response"""
    return {
        "success": True,
        "data": {
            "user_id": "test_user_123",
            "consciousness_score": 0.75,
            "last_activity": "2024-01-15T10:30:00Z",
            "insights_count": 15,
        },
        "timestamp": "2024-01-15T10:30:00Z",
    }


@pytest.fixture
def mock_ml_model_response() -> Dict[str, Any]:
    """Mock ML model prediction response"""
    return {
        "predictions": {
            "emotion": "joy",
            "confidence": 0.89,
            "secondary_emotions": ["happiness", "contentment"],
        },
        "processing_time": 0.15,
        "model_version": "v2.1.0",
    }
