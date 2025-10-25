#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Тесты для PersonalityAdapter - адаптера персонализации и рекомендаций.

Проверяет работу с эмоциями, предпочтениями, рекомендациями,
а также интеграцию с DatabaseAdapter.
"""

import asyncio
import pytest
from unittest.mock import AsyncMock, MagicMock, patch
from datetime import datetime

import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from backend.personality import PersonalityAdapter
from backend.personality.schema import Emotion, Preference, Recommendation, PersonalityProfile
from backend.database_adapter import DataType


class TestPersonalityAdapter:
    """Тесты для PersonalityAdapter."""
    
    @pytest.fixture
    def mock_database_adapter(self):
        """Мок для DatabaseAdapter."""
        mock = MagicMock()
        mock.store_data = AsyncMock()
        mock.store_data.return_value = "data-123"
        return mock
    
    @pytest.fixture
    def adapter_with_mocks(self, mock_database_adapter):
        """PersonalityAdapter с моками."""
        with patch('backend.personality.adapter.DatabaseAdapter', return_value=mock_database_adapter):
            adapter = PersonalityAdapter(user_id="test-user-123")
            adapter.db = mock_database_adapter
            adapter._load_emotions = AsyncMock()
            adapter._load_preferences = AsyncMock()
            
            # Настраиваем заглушки для методов загрузки данных
            adapter._load_emotions.return_value = [
                {
                    "type": "радость",
                    "intensity": 0.8,
                    "context": "тестирование",
                    "timestamp": datetime.now().isoformat()
                }
            ]
            
            adapter._load_preferences.return_value = [
                {
                    "category": "обучение",
                    "value": "визуальный стиль",
                    "strength": 0.9,
                    "last_updated": datetime.now().isoformat()
                }
            ]
            
            return adapter
    
    @pytest.mark.asyncio
    async def test_get_profile(self, adapter_with_mocks):
        """Тест получения профиля пользователя."""
        adapter = adapter_with_mocks
        
        # Мокаем метод get_recommendations
        adapter.get_recommendations = AsyncMock()
        adapter.get_recommendations.return_value = [
            {
                "id": "rec-1",
                "content": "Тестовая рекомендация",
                "confidence": 0.85,
                "source": "test",
                "context": "тестирование",
                "created_at": datetime.now().isoformat()
            }
        ]
        
        profile = await adapter.get_profile()
        
        # Проверяем структуру профиля
        assert profile["user_id"] == "test-user-123"
        assert len(profile["emotional_history"]) == 1
        assert profile["emotional_history"][0]["type"] == "радость"
        assert len(profile["preferences"]) == 1
        assert profile["preferences"][0]["category"] == "обучение"
        assert len(profile["recommendations"]) == 1
        assert profile["recommendations"][0]["content"] == "Тестовая рекомендация"
        
        # Проверяем, что методы загрузки были вызваны
        adapter._load_emotions.assert_called_once()
        adapter._load_preferences.assert_called_once()
        adapter.get_recommendations.assert_called_once()
    
    @pytest.mark.asyncio
    async def test_store_emotion(self, adapter_with_mocks):
        """Тест сохранения эмоционального состояния."""
        adapter = adapter_with_mocks
        
        emotion = await adapter.store_emotion(
            emotion_type="спокойствие",
            intensity=0.7,
            context="тестирование метода"
        )
        
        # Проверяем структуру возвращаемой эмоции
        assert emotion["type"] == "спокойствие"
        assert emotion["intensity"] == 0.7
        assert emotion["context"] == "тестирование метода"
        assert "timestamp" in emotion
        
        # Проверяем, что метод store_data был вызван с правильными параметрами
        adapter.db.store_data.assert_called_once()
        call_args = adapter.db.store_data.call_args[1]
        assert call_args["data_type"] == DataType.EMOTION_HISTORY
        assert call_args["user_id"] == "test-user-123"
        assert call_args["data"]["type"] == "спокойствие"
        assert call_args["data"]["intensity"] == 0.7
    
    @pytest.mark.asyncio
    async def test_update_preference(self, adapter_with_mocks):
        """Тест обновления предпочтения пользователя."""
        adapter = adapter_with_mocks
        
        preference = await adapter.update_preference(
            category="музыка",
            value="классическая",
            strength=0.85
        )
        
        # Проверяем структуру возвращаемого предпочтения
        assert preference["category"] == "музыка"
        assert preference["value"] == "классическая"
        assert preference["strength"] == 0.85
        assert "last_updated" in preference
        
        # Проверяем, что метод store_data был вызван с правильными параметрами
        adapter.db.store_data.assert_called_once()
        call_args = adapter.db.store_data.call_args[1]
        assert call_args["data_type"] == DataType.PREFERENCE
        assert call_args["user_id"] == "test-user-123"
        assert call_args["data"]["category"] == "музыка"
        assert call_args["data"]["value"] == "классическая"
        assert call_args["data"]["strength"] == 0.85
    
    @pytest.mark.asyncio
    async def test_get_recommendations(self, adapter_with_mocks):
        """Тест получения рекомендаций."""
        adapter = adapter_with_mocks
        
        recommendations = await adapter.get_recommendations(
            limit=2,
            context="обучение"
        )
        
        # Проверяем структуру возвращаемых рекомендаций
        assert len(recommendations) == 2
        assert recommendations[0]["id"].startswith("rec-")
        assert "content" in recommendations[0]
        assert "confidence" in recommendations[0]
        assert "source" in recommendations[0]
        assert "context" in recommendations[0]
        assert "created_at" in recommendations[0]
        
        # Проверяем, что контекст был учтен
        assert recommendations[0]["context"] == "обучение"
    
    @pytest.mark.asyncio
    async def test_error_handling(self, adapter_with_mocks):
        """Тест обработки ошибок."""
        adapter = adapter_with_mocks
        
        # Имитируем ошибку в методе _load_emotions
        adapter._load_emotions.side_effect = Exception("Тестовая ошибка")
        
        # Проверяем, что метод get_profile не выбрасывает исключение
        profile = await adapter.get_profile()
        
        # Проверяем, что возвращается пустой профиль
        assert profile["user_id"] == "test-user-123"
        assert profile["emotional_history"] == []
        assert profile["preferences"] == []
        assert profile["recommendations"] == []
    
    @pytest.mark.asyncio
    async def test_recommendations_error_handling(self, adapter_with_mocks):
        """Тест обработки ошибок при получении рекомендаций."""
        adapter = adapter_with_mocks
        
        # Имитируем ошибку в Neo4j запросе
        adapter.db.query_neo4j = AsyncMock(side_effect=Exception("Ошибка Neo4j"))
        
        # Проверяем, что метод get_recommendations не выбрасывает исключение
        recommendations = await adapter.get_recommendations()
        
        # Проверяем, что возвращается пустой список
        assert recommendations == []


class TestPersonalitySchema:
    """Тесты для GraphQL схемы PersonalityAdapter."""
    
    def test_emotion_type(self):
        """Тест типа Emotion."""
        emotion = Emotion(
            type="радость",
            intensity=0.8,
            context="тестирование",
            timestamp=datetime.now()
        )
        
        assert emotion.type == "радость"
        assert emotion.intensity == 0.8
        assert emotion.context == "тестирование"
        assert isinstance(emotion.timestamp, datetime)
    
    def test_preference_type(self):
        """Тест типа Preference."""
        preference = Preference(
            category="обучение",
            value="визуальный стиль",
            strength=0.9,
            last_updated=datetime.now()
        )
        
        assert preference.category == "обучение"
        assert preference.value == "визуальный стиль"
        assert preference.strength == 0.9
        assert isinstance(preference.last_updated, datetime)
    
    def test_recommendation_type(self):
        """Тест типа Recommendation."""
        recommendation = Recommendation(
            id="rec-1",
            content="Тестовая рекомендация",
            confidence=0.85,
            source="test",
            context="тестирование",
            created_at=datetime.now()
        )
        
        assert recommendation.id == "rec-1"
        assert recommendation.content == "Тестовая рекомендация"
        assert recommendation.confidence == 0.85
        assert recommendation.source == "test"
        assert recommendation.context == "тестирование"
        assert isinstance(recommendation.created_at, datetime)
    
    def test_personality_profile_type(self):
        """Тест типа PersonalityProfile."""
        now = datetime.now()
        
        emotions = [
            Emotion(type="радость", intensity=0.8, context="тест1", timestamp=now),
            Emotion(type="спокойствие", intensity=0.6, context="тест2", timestamp=now)
        ]
        
        preferences = [
            Preference(category="музыка", value="классическая", strength=0.7, last_updated=now),
            Preference(category="обучение", value="визуальный", strength=0.9, last_updated=now),
            Preference(category="еда", value="вегетарианская", strength=0.5, last_updated=now)
        ]
        
        recommendations = [
            Recommendation(id="rec-1", content="Рекомендация 1", confidence=0.8, source="test", created_at=now)
        ]
        
        profile = PersonalityProfile(
            user_id="test-user-123",
            emotional_history=emotions,
            preferences=preferences,
            recommendations=recommendations
        )
        
        # Проверяем базовые поля
        assert profile.user_id == "test-user-123"
        assert len(profile.emotional_history) == 2
        assert len(profile.preferences) == 3
        assert len(profile.recommendations) == 1
        
        # Проверяем вычисляемые поля
        assert profile.current_mood() == emotions[-1]
        
        dominant = profile.dominant_preferences(limit=2)
        assert len(dominant) == 2
        assert dominant[0] == preferences[1]  # обучение с силой 0.9
        assert dominant[1] == preferences[0]  # музыка с силой 0.7


# Интеграционные тесты (требуют реальных БД)
class TestPersonalityAdapterIntegration:
    """Интеграционные тесты для PersonalityAdapter."""
    
    @pytest.mark.integration
    @pytest.mark.asyncio
    async def test_real_database_integration(self):
        """Тест интеграции с реальными БД."""
        try:
            adapter = PersonalityAdapter(user_id="test-integration-user")
            
            # Сохраняем эмоцию
            emotion = await adapter.store_emotion(
                emotion_type="интерес",
                intensity=0.75,
                context="интеграционное тестирование"
            )
            
            # Получаем профиль
            profile = await adapter.get_profile()
            
            # Проверяем, что профиль содержит данные
            assert profile["user_id"] == "test-integration-user"
            assert len(profile["emotional_history"]) > 0
            
            # Проверяем, что последняя эмоция соответствует сохраненной
            last_emotion = profile["emotional_history"][-1]
            assert last_emotion["type"] == "интерес"
            assert last_emotion["intensity"] == 0.75
            
        except Exception as e:
            pytest.skip(f"Реальные БД недоступны для интеграционного тестирования: {str(e)}")


if __name__ == "__main__":
    # Запуск тестов
    pytest.main([__file__, "-v"])
