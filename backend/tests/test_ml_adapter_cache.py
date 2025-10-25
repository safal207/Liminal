#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Тесты для кэширования в ML-адаптере.

Проверяет работу кэширования результатов анализа эмоций
и асинхронную обработку больших объемов текста.
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
    clear_emotion_cache,
    get_emotion_cache_stats
)


class TestMLAdapterCache:
    """Тесты для кэширования в ML-адаптере."""
    
    @pytest.fixture
    def adapter(self):
        """Создает экземпляр адаптера для тестов."""
        adapter = EmotionMLAdapter()
        # Очищаем кэш перед каждым тестом
        adapter._analyze_text_cached.cache_clear()
        return adapter
    
    @pytest.mark.asyncio
    async def test_cache_hit(self, adapter):
        """Тест попадания в кэш при повторном запросе."""
        # Первый запрос (кэш-промах)
        text = "Я очень рад, что все работает!"
        result1 = await adapter.analyze_text(text)
        
        # Второй запрос с тем же текстом (должен быть кэш-хит)
        result2 = await adapter.analyze_text(text)
        
        # Проверяем, что результаты идентичны
        assert result1["emotion_type"] == result2["emotion_type"]
        assert result1["intensity"] == result2["intensity"]
        
        # Проверяем флаг кэширования
        assert result2["cached"] is True
        
        # Проверяем статистику кэша
        stats = await get_emotion_cache_stats()
        assert stats["hits"] > 0
        assert stats["currsize"] > 0
    
    @pytest.mark.asyncio
    async def test_cache_different_texts(self, adapter):
        """Тест работы с разными текстами."""
        # Анализируем разные тексты
        text1 = "Я очень рад, что все работает!"
        text2 = "Мне грустно из-за этой ошибки."
        
        result1 = await adapter.analyze_text(text1)
        result2 = await adapter.analyze_text(text2)
        
        # Проверяем, что результаты различаются
        assert result1["emotion_type"] != result2["emotion_type"]
        
        # Проверяем статистику кэша
        stats = await get_emotion_cache_stats()
        assert stats["currsize"] >= 2  # Должно быть минимум 2 записи в кэше
    
    @pytest.mark.asyncio
    async def test_cache_normalization(self, adapter):
        """Тест нормализации текста для кэширования."""
        # Тексты с разным форматированием, но одинаковым содержанием
        text1 = "Я очень рад, что все работает!"
        text2 = "  Я очень рад, что все работает!  "  # С пробелами
        
        result1 = await adapter.analyze_text(text1)
        result2 = await adapter.analyze_text(text2)
        
        # Проверяем, что результаты идентичны и второй запрос использовал кэш
        assert result1["emotion_type"] == result2["emotion_type"]
        assert result2["cached"] is True
        
        # Проверяем статистику кэша
        stats = await get_emotion_cache_stats()
        assert stats["hits"] > 0
        assert stats["currsize"] == 1  # Должна быть только одна запись в кэше
    
    @pytest.mark.asyncio
    async def test_cache_clear(self, adapter):
        """Тест очистки кэша."""
        # Заполняем кэш
        await adapter.analyze_text("Текст 1")
        await adapter.analyze_text("Текст 2")
        await adapter.analyze_text("Текст 3")
        
        # Проверяем, что кэш не пустой
        stats_before = await get_emotion_cache_stats()
        assert stats_before["currsize"] > 0
        
        # Очищаем кэш
        clear_result = await clear_emotion_cache()
        assert clear_result["cleared_entries"] > 0
        
        # Проверяем, что кэш пуст
        stats_after = await get_emotion_cache_stats()
        assert stats_after["currsize"] == 0
    
    @pytest.mark.asyncio
    async def test_cache_performance(self, adapter):
        """Тест производительности кэширования."""
        text = "Это длинный текст для проверки производительности кэширования. " * 10
        
        # Первый запрос (без кэша)
        start_time = time.time()
        await adapter.analyze_text(text)
        first_duration = time.time() - start_time
        
        # Второй запрос (с кэшем)
        start_time = time.time()
        await adapter.analyze_text(text)
        second_duration = time.time() - start_time
        
        # Кэшированный запрос должен быть быстрее
        assert second_duration < first_duration
    
    @pytest.mark.asyncio
    async def test_parallel_requests(self, adapter):
        """Тест параллельной обработки запросов."""
        # Создаем несколько разных текстов
        texts = [
            f"Текст для параллельного анализа {i}" for i in range(5)
        ]
        
        # Запускаем анализ параллельно
        tasks = [adapter.analyze_text(text) for text in texts]
        results = await asyncio.gather(*tasks)
        
        # Проверяем, что все запросы выполнены успешно
        assert len(results) == len(texts)
        assert all(isinstance(result, dict) for result in results)
        
        # Проверяем, что все результаты разные (т.к. тексты разные)
        emotion_types = [result["emotion_type"] for result in results]
        assert len(set(emotion_types)) > 1  # Должно быть несколько разных эмоций


if __name__ == "__main__":
    # Запуск тестов
    pytest.main([__file__, "-v"])
