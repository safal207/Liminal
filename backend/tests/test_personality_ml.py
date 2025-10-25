#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Тесты для ML-интеграции с PersonalityAdapter.

Проверяет работу ML-адаптера для анализа эмоций и его интеграцию с PersonalityAdapter.
"""

import pytest
from unittest.mock import AsyncMock, MagicMock, patch
import json
from datetime import datetime

import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from backend.personality.ml_adapter import EmotionMLAdapter, emotion_ml_adapter
from backend.personality.adapter import PersonalityAdapter
from backend.emotime.ml_accuracy_optimizer import EmotionalDimension


class TestEmotionMLAdapter:
    """Тесты для EmotionMLAdapter."""
    
    @pytest.fixture
    def mock_classifier(self):
        """Мок для EnhancedEmotionalClassifier."""
        mock = MagicMock()
        mock.predict_emotional_dimensions = MagicMock()
        
        # Настраиваем возвращаемые значения
        mock.predict_emotional_dimensions.return_value = MagicMock(
            dimensions={
                "valence": 0.8,
                "arousal": 0.6,
                "dominance": 0.7
            },
            confidence=0.85,
            features_extracted={
                "lexical": {"word_count": 10},
                "semantic": {"total_emotional_intensity": 0.7}
            },
            accuracy_score=0.8,
            model_used="test_model"
        )
        
        return mock
    
    @pytest.fixture
    def adapter(self, mock_classifier):
        """Создает экземпляр EmotionMLAdapter с моком классификатора."""
        adapter = EmotionMLAdapter()
        adapter.classifier = mock_classifier
        return adapter
    
    @pytest.mark.asyncio
    async def test_analyze_text(self, adapter):
        """Тест метода analyze_text."""
        # Анализируем текст
        result = await adapter.analyze_text("Я очень рад, что все работает!")
        
        # Проверяем результат
        assert "emotion_type" in result
        assert "intensity" in result
        assert "dimensions" in result
        assert "confidence" in result
        assert "features" in result
        
        # Проверяем значения
        assert result["emotion_type"] in ["радость", "восторг"]  # Один из позитивных типов
        assert 0.0 <= result["intensity"] <= 1.0
        assert result["dimensions"]["valence"] == 0.8
        assert result["confidence"] == 0.85
    
    @pytest.mark.asyncio
    async def test_analyze_empty_text(self, adapter):
        """Тест анализа пустого текста."""
        # Анализируем пустой текст
        result = await adapter.analyze_text("")
        
        # Проверяем результат
        assert result["emotion_type"] == "нейтральность"
        assert result["intensity"] == 0.0
        assert result["confidence"] == 0.0
    
    @pytest.mark.asyncio
    async def test_map_dimensions_to_emotion(self, adapter):
        """Тест метода _map_dimensions_to_emotion."""
        # Тестируем различные комбинации измерений
        
        # Позитивные эмоции (высокая валентность)
        emotion, intensity = adapter._map_dimensions_to_emotion({
            "valence": 0.8,
            "arousal": 0.6,
            "dominance": 0.7
        })
        assert emotion in ["радость", "восторг", "удовлетворение"]
        assert 0.5 < intensity <= 1.0
        
        # Негативные эмоции (низкая валентность)
        emotion, intensity = adapter._map_dimensions_to_emotion({
            "valence": 0.2,
            "arousal": 0.7,
            "dominance": 0.3
        })
        assert emotion in ["страх", "тревога", "раздражение"]
        assert 0.5 < intensity <= 1.0
        
        # Нейтральные эмоции (средняя валентность)
        emotion, intensity = adapter._map_dimensions_to_emotion({
            "valence": 0.5,
            "arousal": 0.5,
            "dominance": 0.5
        })
        assert emotion in ["нейтральность", "удивление", "задумчивость"]
        assert intensity <= 0.5  # Близко к нейтральной точке
    
    @pytest.mark.asyncio
    async def test_get_emotion_suggestions(self, adapter):
        """Тест метода get_emotion_suggestions."""
        # Получаем предложения
        suggestions = await adapter.get_emotion_suggestions("test-user", "тестирование")
        
        # Проверяем результат
        assert isinstance(suggestions, list)
        assert len(suggestions) > 0
        
        # Проверяем структуру предложений
        for suggestion in suggestions:
            assert "emotion_type" in suggestion
            assert "description" in suggestion


class TestPersonalityAdapterML:
    """Тесты для ML-интеграции в PersonalityAdapter."""
    
    @pytest.fixture
    def mock_ml_adapter(self):
        """Мок для EmotionMLAdapter."""
        mock = MagicMock()
        mock.analyze_text = AsyncMock()
        mock.get_emotion_suggestions = AsyncMock()
        
        # Настраиваем возвращаемые значения
        mock.analyze_text.return_value = {
            "emotion_type": "радость",
            "intensity": 0.8,
            "dimensions": {
                "valence": 0.8,
                "arousal": 0.6,
                "dominance": 0.7
            },
            "confidence": 0.85,
            "features": {
                "lexical": {"word_count": 10},
                "semantic": {"total_emotional_intensity": 0.7}
            }
        }
        
        mock.get_emotion_suggestions.return_value = [
            {"emotion_type": "радость", "description": "Позитивное чувство"},
            {"emotion_type": "интерес", "description": "Любопытство"}
        ]
        
        return mock
    
    @pytest.fixture
    def mock_db_adapter(self):
        """Мок для DatabaseAdapter."""
        mock = MagicMock()
        mock.store_data = AsyncMock()
        return mock
    
    @pytest.fixture
    def adapter(self, mock_ml_adapter, mock_db_adapter):
        """Создает экземпляр PersonalityAdapter с моками."""
        adapter = PersonalityAdapter("test-user")
        adapter.ml_adapter = mock_ml_adapter
        adapter.db = mock_db_adapter
        return adapter
    
    @pytest.mark.asyncio
    async def test_analyze_text_emotion(self, adapter):
        """Тест метода analyze_text_emotion."""
        # Анализируем текст
        result = await adapter.analyze_text_emotion(
            "Я очень доволен результатами тестирования!",
            "unit-test"
        )
        
        # Проверяем результат
        assert "type" in result
        assert "intensity" in result
        assert "context" in result
        assert "timestamp" in result
        assert "ml_analysis" in result
        
        # Проверяем значения
        assert result["type"] == "радость"
        assert result["intensity"] == 0.8
        assert result["context"] == "unit-test"
        assert "dimensions" in result["ml_analysis"]
        assert "confidence" in result["ml_analysis"]
        assert "features" in result["ml_analysis"]
        
        # Проверяем, что методы были вызваны
        adapter.ml_adapter.analyze_text.assert_called_once_with(
            "Я очень доволен результатами тестирования!"
        )
        adapter.db.store_data.assert_called_once()
    
    @pytest.mark.asyncio
    async def test_analyze_text_emotion_with_error(self, adapter):
        """Тест обработки ошибок в analyze_text_emotion."""
        # Настраиваем мок для вызова исключения
        adapter.ml_adapter.analyze_text.side_effect = Exception("Тестовая ошибка")
        
        # Анализируем текст
        result = await adapter.analyze_text_emotion(
            "Проблемный текст",
            "error-test"
        )
        
        # Проверяем результат (должен вернуть нейтральную эмоцию)
        assert result["type"] == "нейтральность"
        assert result["intensity"] == 0.5
        assert "Ошибка анализа" in result["context"]
        
        # Проверяем, что методы были вызваны
        adapter.ml_adapter.analyze_text.assert_called_once()
        adapter.db.store_data.assert_called_once()
    
    @pytest.mark.asyncio
    async def test_get_emotion_suggestions(self, adapter):
        """Тест метода get_emotion_suggestions."""
        # Получаем предложения
        suggestions = await adapter.get_emotion_suggestions("test-context")
        
        # Проверяем результат
        assert isinstance(suggestions, list)
        assert len(suggestions) == 2
        assert suggestions[0]["emotion_type"] == "радость"
        
        # Проверяем, что метод был вызван
        adapter.ml_adapter.get_emotion_suggestions.assert_called_once_with(
            "test-user", "test-context"
        )
    
    @pytest.mark.asyncio
    async def test_get_emotion_suggestions_with_error(self, adapter):
        """Тест обработки ошибок в get_emotion_suggestions."""
        # Настраиваем мок для вызова исключения
        adapter.ml_adapter.get_emotion_suggestions.side_effect = Exception("Тестовая ошибка")
        
        # Получаем предложения
        suggestions = await adapter.get_emotion_suggestions()
        
        # Проверяем результат (должен вернуть базовый набор)
        assert isinstance(suggestions, list)
        assert len(suggestions) == 3
        assert suggestions[0]["emotion_type"] == "радость"
        
        # Проверяем, что метод был вызван
        adapter.ml_adapter.get_emotion_suggestions.assert_called_once()


class TestMLIntegration:
    """Интеграционные тесты для ML-компонентов."""
    
    @pytest.mark.integration
    @pytest.mark.asyncio
    async def test_full_ml_pipeline(self):
        """Тест полного пайплайна ML-анализа."""
        # Создаем реальные экземпляры (без моков)
        personality_adapter = PersonalityAdapter("test-integration-user")
        
        # Патчим метод store_data в DatabaseAdapter, чтобы не обращаться к реальной БД
        with patch("backend.database_adapter.DatabaseAdapter.store_data", new_callable=AsyncMock) as mock_store:
            # Анализируем текст
            result = await personality_adapter.analyze_text_emotion(
                "Я очень рад, что интеграционные тесты работают!",
                "integration-test"
            )
            
            # Проверяем результат
            assert "type" in result
            assert "intensity" in result
            assert "context" in result
            assert "timestamp" in result
            assert "ml_analysis" in result
            
            # Проверяем, что метод store_data был вызван
            mock_store.assert_called_once()
            
            # Получаем предложения по эмоциям
            suggestions = await personality_adapter.get_emotion_suggestions("integration-test")
            
            # Проверяем результат
            assert isinstance(suggestions, list)
            assert len(suggestions) > 0


if __name__ == "__main__":
    # Запуск тестов
    pytest.main([__file__, "-v"])
