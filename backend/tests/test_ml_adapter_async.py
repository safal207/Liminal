#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Тесты для асинхронной обработки в ML-адаптере.

Проверяет работу асинхронной обработки больших объемов текста
и пакетной обработки нескольких текстов.
"""

import asyncio
import pytest
from unittest.mock import AsyncMock, MagicMock, patch
import time

import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from backend.personality.ml_adapter import (
    EmotionMLAdapter, 
    emotion_ml_adapter,
    analyze_large_text,
    _aggregate_emotion_results
)


class TestMLAdapterAsync:
    """Тесты для асинхронной обработки в ML-адаптере."""
    
    @pytest.fixture
    def adapter(self):
        """Создает экземпляр адаптера для тестов."""
        adapter = EmotionMLAdapter()
        # Очищаем кэш перед каждым тестом
        adapter._analyze_text_cached.cache_clear()
        return adapter
    
    @pytest.mark.asyncio
    async def test_analyze_text_batch(self, adapter):
        """Тест пакетного анализа текстов."""
        texts = [
            "Я очень рад, что все работает!",
            "Мне грустно из-за этой ошибки.",
            "Я злюсь на эту проблему."
        ]
        
        results = await adapter.analyze_text_batch(texts)
        
        # Проверяем, что получили правильное количество результатов
        assert len(results) == len(texts)
        
        # Проверяем, что все результаты имеют правильную структуру
        for result in results:
            assert "emotion_type" in result
            assert "intensity" in result
            assert "confidence" in result
    
    @pytest.mark.asyncio
    async def test_analyze_text_stream_list(self, adapter):
        """Тест потоковой обработки списка текстов."""
        texts = [
            "Текст 1 для потоковой обработки",
            "Текст 2 для потоковой обработки",
            "Текст 3 для потоковой обработки",
            "Текст 4 для потоковой обработки",
            "Текст 5 для потоковой обработки",
            "Текст 6 для потоковой обработки",
        ]
        
        results = await adapter.analyze_text_stream(texts)
        
        # Проверяем, что получили правильное количество результатов
        assert len(results) == len(texts)
    
    @pytest.mark.asyncio
    async def test_analyze_text_stream_queue(self, adapter):
        """Тест потоковой обработки очереди текстов."""
        queue = asyncio.Queue()
        
        # Добавляем тексты в очередь
        await queue.put("Текст 1 из очереди")
        await queue.put("Текст 2 из очереди")
        await queue.put("Текст 3 из очереди")
        await queue.put(None)  # Сигнал завершения
        
        # Запускаем обработку в отдельной задаче
        task = asyncio.create_task(adapter.analyze_text_stream(queue))
        
        # Ждем завершения обработки
        results = await task
        
        # Проверяем результаты
        assert len(results) == 3
    
    @pytest.mark.asyncio
    async def test_analyze_large_text(self):
        """Тест анализа большого текста."""
        # Создаем большой текст
        large_text = "Это тестовый текст для проверки анализа больших объемов данных. " * 50
        
        # Анализируем большой текст
        result = await analyze_large_text(large_text, chunk_size=200, overlap=50)
        
        # Проверяем структуру результата
        assert "emotion_type" in result
        assert "intensity" in result
        assert "confidence" in result
        assert "aggregated" in result
        assert result["aggregated"] is True
        assert "chunks_analyzed" in result
        assert result["chunks_analyzed"] > 1
    
    @pytest.mark.asyncio
    async def test_analyze_large_text_small_input(self):
        """Тест анализа маленького текста через функцию для больших текстов."""
        small_text = "Это короткий текст."
        
        # Анализируем маленький текст через функцию для больших текстов
        result = await analyze_large_text(small_text, chunk_size=1000)
        
        # Для маленького текста не должно быть разбиения на части
        assert "aggregated" not in result or not result["aggregated"]
    
    @pytest.mark.asyncio
    async def test_aggregate_emotion_results(self):
        """Тест агрегации результатов анализа."""
        # Создаем тестовые результаты
        results = [
            {
                "emotion_type": "радость",
                "intensity": 0.8,
                "dimensions": {"valence": 0.9, "arousal": 0.7, "dominance": 0.6},
                "confidence": 0.85,
                "features": {"positive_words": 5}
            },
            {
                "emotion_type": "радость",
                "intensity": 0.7,
                "dimensions": {"valence": 0.8, "arousal": 0.6, "dominance": 0.5},
                "confidence": 0.75,
                "features": {"positive_words": 3}
            },
            {
                "emotion_type": "грусть",
                "intensity": 0.3,
                "dimensions": {"valence": 0.2, "arousal": 0.3, "dominance": 0.2},
                "confidence": 0.65,
                "features": {"negative_words": 2}
            }
        ]
        
        # Агрегируем результаты
        aggregated = await _aggregate_emotion_results(results)
        
        # Проверяем результат агрегации
        assert aggregated["emotion_type"] == "радость"  # Доминирующая эмоция
        assert "intensity" in aggregated
        assert "dimensions" in aggregated
        assert "confidence" in aggregated
        assert "aggregated" in aggregated
        assert aggregated["aggregated"] is True
        assert "chunks_analyzed" in aggregated
        assert aggregated["chunks_analyzed"] == 3
        
        # Проверяем агрегированные признаки
        assert "features" in aggregated
        assert "positive_words" in aggregated["features"]
        assert aggregated["features"]["positive_words"] == 8  # 5 + 3
        assert "negative_words" in aggregated["features"]
        assert aggregated["features"]["negative_words"] == 2


if __name__ == "__main__":
    # Запуск тестов
    pytest.main([__file__, "-v"])
