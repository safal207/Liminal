#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
ML-адаптер для PersonalityAdapter.

Модуль предоставляет интерфейс для анализа эмоций в тексте пользователя
с использованием EnhancedEmotionalClassifier из emotime.

Включает кэширование результатов анализа и асинхронную обработку.
"""

import logging
import asyncio
import functools
import re
from typing import Dict, Any, Optional, List, Tuple, Union
from datetime import datetime
from functools import lru_cache
from concurrent.futures import ThreadPoolExecutor

# Импорт модуля мультиязычной поддержки
from backend.personality.multilingual_support import (
    detect_language,
    translate_emotion,
    get_emotion_in_language,
    analyze_multilingual_text,
    get_supported_languages
)

from backend.emotime.ml_accuracy_optimizer import (
    enhanced_classifier,
    EmotionalDimension,
    MLPrediction
)

# Инициализация логгера
logger = logging.getLogger(__name__)

# Карта соответствия эмоциональных измерений конкретным эмоциям
# Основана на модели VAD (Valence-Arousal-Dominance)
EMOTION_MAPPING = {
    # Базовые эмоции и их характеристики
    "BASE_EMOTIONS": {
        # Основные базовые эмоции
        "радость": {"valence": 0.8, "arousal": 0.6, "dominance": 0.7, "category": "positive"},
        "грусть": {"valence": 0.2, "arousal": 0.3, "dominance": 0.3, "category": "negative"},
        "злость": {"valence": 0.3, "arousal": 0.8, "dominance": 0.7, "category": "negative"},
        "страх": {"valence": 0.2, "arousal": 0.7, "dominance": 0.2, "category": "negative"},
        "удивление": {"valence": 0.6, "arousal": 0.7, "dominance": 0.5, "category": "neutral"},
        "отвращение": {"valence": 0.2, "arousal": 0.6, "dominance": 0.4, "category": "negative"},
        "нейтральность": {"valence": 0.5, "arousal": 0.3, "dominance": 0.5, "category": "neutral"},
        
        # Расширенный набор эмоций
        "восторг": {"valence": 0.9, "arousal": 0.8, "dominance": 0.8, "category": "positive"},
        "умиротворение": {"valence": 0.7, "arousal": 0.2, "dominance": 0.6, "category": "positive"},
        "интерес": {"valence": 0.7, "arousal": 0.5, "dominance": 0.6, "category": "positive"},
        "гордость": {"valence": 0.8, "arousal": 0.5, "dominance": 0.8, "category": "positive"},
        "благодарность": {"valence": 0.8, "arousal": 0.4, "dominance": 0.6, "category": "positive"},
        "любовь": {"valence": 0.9, "arousal": 0.7, "dominance": 0.7, "category": "positive"},
        "надежда": {"valence": 0.7, "arousal": 0.5, "dominance": 0.6, "category": "positive"},
        
        "тревога": {"valence": 0.3, "arousal": 0.7, "dominance": 0.3, "category": "negative"},
        "стыд": {"valence": 0.2, "arousal": 0.5, "dominance": 0.2, "category": "negative"},
        "вина": {"valence": 0.2, "arousal": 0.4, "dominance": 0.3, "category": "negative"},
        "зависть": {"valence": 0.3, "arousal": 0.6, "dominance": 0.4, "category": "negative"},
        "разочарование": {"valence": 0.3, "arousal": 0.4, "dominance": 0.3, "category": "negative"},
        "смущение": {"valence": 0.4, "arousal": 0.5, "dominance": 0.3, "category": "negative"},
        "отчаяние": {"valence": 0.1, "arousal": 0.6, "dominance": 0.1, "category": "negative"},
        
        "скука": {"valence": 0.4, "arousal": 0.2, "dominance": 0.4, "category": "neutral"},
        "ностальгия": {"valence": 0.6, "arousal": 0.3, "dominance": 0.5, "category": "neutral"},
        "замешательство": {"valence": 0.4, "arousal": 0.5, "dominance": 0.4, "category": "neutral"},
        "задумчивость": {"valence": 0.5, "arousal": 0.3, "dominance": 0.5, "category": "neutral"}
    },
    
    # Расширенный набор эмоций
    "восторг": {"valence": 0.9, "arousal": 0.8, "dominance": 0.8, "category": "positive"},
    "умиротворение": {"valence": 0.7, "arousal": 0.2, "dominance": 0.6, "category": "positive"},
    "интерес": {"valence": 0.7, "arousal": 0.5, "dominance": 0.6, "category": "positive"},
    "гордость": {"valence": 0.8, "arousal": 0.5, "dominance": 0.8, "category": "positive"},
    "благодарность": {"valence": 0.8, "arousal": 0.4, "dominance": 0.6, "category": "positive"},
    "любовь": {"valence": 0.9, "arousal": 0.7, "dominance": 0.7, "category": "positive"},
    "надежда": {"valence": 0.7, "arousal": 0.5, "dominance": 0.6, "category": "positive"},
    
    "тревога": {"valence": 0.3, "arousal": 0.7, "dominance": 0.3, "category": "negative"},
    "стыд": {"valence": 0.2, "arousal": 0.5, "dominance": 0.2, "category": "negative"},
    "вина": {"valence": 0.2, "arousal": 0.4, "dominance": 0.3, "category": "negative"},
    "зависть": {"valence": 0.3, "arousal": 0.6, "dominance": 0.4, "category": "negative"},
    "разочарование": {"valence": 0.3, "arousal": 0.4, "dominance": 0.3, "category": "negative"},
    "смущение": {"valence": 0.4, "arousal": 0.5, "dominance": 0.3, "category": "negative"},
    "отчаяние": {"valence": 0.1, "arousal": 0.6, "dominance": 0.1, "category": "negative"},
    
    "скука": {"valence": 0.4, "arousal": 0.2, "dominance": 0.4, "category": "neutral"},
    "ностальгия": {"valence": 0.6, "arousal": 0.3, "dominance": 0.5, "category": "neutral"},
    "замешательство": {"valence": 0.4, "arousal": 0.5, "dominance": 0.4, "category": "neutral"},
    "задумчивость": {"valence": 0.5, "arousal": 0.3, "dominance": 0.5, "category": "neutral"},
}




class EmotionMLAdapter:
    """
    Адаптер для работы с ML-моделями анализа эмоций.
    
    ML-адаптер для анализа эмоций в тексте пользователя.
    
    Использует EnhancedEmotionalClassifier из emotime для анализа текста
    и преобразует результаты в формат, удобный для PersonalityAdapter.
    
    Особенности:
    - Кэширование результатов анализа для повышения производительности
    - Асинхронная обработка для больших объемов текста
    - Мультиязычная поддержка с автоматическим определением языка
    """
    
    def __init__(self):
        """Инициализация адаптера."""
        self.classifier = enhanced_classifier
        self.cache_hits = 0
        self.cache_misses = 0
        self.cache_stats_last_reset = datetime.now()
        self.supported_languages = get_supported_languages()
        logger.info("EmotionMLAdapter initialized with caching, async and multilingual support")
    
    async def analyze_text(self, text: str) -> Dict[str, Any]:
        """
        Анализирует текст и возвращает информацию об эмоциях.
        
        Использует кэширование для повышения производительности при повторных запросах.
        
        Args:
            text: Текст для анализа
            
        Returns:
            Словарь с информацией об эмоциях:
            {
                "emotion_type": str,  # Тип эмоции (радость, грусть и т.д.)
                "intensity": float,   # Интенсивность эмоции (0-1)
                "dimensions": Dict,   # Эмоциональные измерения (VAD)
                "confidence": float,  # Уверенность в предсказании (0-1)
                "features": Dict,     # Извлеченные признаки
                "cached": bool        # Был ли результат получен из кэша
            }
        """
        if not text or len(text.strip()) == 0:
            logger.warning("Empty text provided for emotion analysis")
            return {
                "emotion_type": "нейтральность",
                "intensity": 0.0,
                "dimensions": {d.value: 0.5 for d in EmotionalDimension},
                "confidence": 0.0,
                "features": {},
                "cached": False
            }
        
        # Нормализуем текст для кэширования (удаляем лишние пробелы)
        normalized_text = text.strip()
        
        try:
            # Используем кэшированный анализ
            cached_result = await self._cached_analyze(normalized_text)
            
            # Добавляем информацию о кэшировании
            cached_result["cached"] = True
            
            # Обновляем статистику кэша
            self.cache_hits += 1
            
            # Логируем использование кэша каждые 100 запросов
            if (self.cache_hits + self.cache_misses) % 100 == 0:
                self._log_cache_stats()
                
            return cached_result
            
        except Exception as e:
            logger.error(f"Error analyzing text: {e}")
            # Возвращаем нейтральную эмоцию в случае ошибки
            return {
                "emotion_type": "нейтральность",
                "intensity": 0.0,
                "dimensions": {d.value: 0.5 for d in EmotionalDimension},
                "confidence": 0.0,
                "features": {},
                "cached": False
            }
    async def analyze_multilingual(self, text: str, target_lang: str = "ru", expected_lang: Optional[str] = None) -> Dict[str, Any]:
        """
        Анализирует текст на любом языке и возвращает результат на целевом языке.
        
        Использует автоматическое определение языка и перевод эмоций.
        
        Args:
            text: Текст для анализа на любом языке
            target_lang: Язык для вывода результата (ru, en, de, fr, es, it)
            expected_lang: Необязательная подсказка ожидаемого языка (для tie-break)
            
        Returns:
            Словарь с информацией об эмоциях на целевом языке
        """
        if target_lang not in self.supported_languages:
            logger.warning(f"Неподдерживаемый язык: {target_lang}, используется русский по умолчанию")
            target_lang = "ru"
            
        # Используем функцию из модуля мультиязычной поддержки
        result = await analyze_multilingual_text(text, self.analyze_text, target_lang, expected_lang=expected_lang)
        
        # Добавляем информацию о мультиязычном анализе
        result["multilingual"] = True
        result["target_language"] = target_lang
        
        return result
    
    @lru_cache(maxsize=1000)
    def _analyze_text_cached(self, text: str) -> Dict[str, Any]:
        """
        Кэшируемая версия анализа текста.
        
        Args:
            text: Текст для анализа
            
        Returns:
            Результат анализа в сериализуемом формате
        """
        # Анализируем текст с помощью классификатора
        prediction = self.classifier.predict_emotional_dimensions(text)
        
        # Преобразуем эмоциональные измерения в конкретную эмоцию
        emotion_type, intensity = self._map_dimensions_to_emotion(prediction.dimensions)
        
        # Возвращаем результат в сериализуемом формате
        return {
            "emotion_type": emotion_type,
            "intensity": intensity,
            "dimensions": prediction.dimensions,
            "confidence": prediction.confidence,
            "features": prediction.features_extracted
        }
    
    async def _cached_analyze(self, text: str) -> Dict[str, Any]:
        """
        Асинхронная обертка для кэшированного анализа.
        
        Args:
            text: Текст для анализа
            
        Returns:
            Результат анализа
        """
        # Используем run_in_executor для выполнения синхронной функции в отдельном потоке
        loop = asyncio.get_event_loop()
        
        # Проверяем, есть ли результат в кэше
        if text in self._analyze_text_cached.__wrapped__.__cache__:
            self.cache_hits += 1
        else:
            self.cache_misses += 1
            
        # Выполняем кэшированную функцию в отдельном потоке с использованием пула потоков
        result = await loop.run_in_executor(
            THREAD_POOL, 
            functools.partial(self._analyze_text_cached, text)
        )
        
        return result
        
    async def analyze_text_batch(self, texts: List[str]) -> List[Dict[str, Any]]:
        """
        Анализирует пакет текстов параллельно.
        
        Args:
            texts: Список текстов для анализа
            
        Returns:
            Список результатов анализа
        """
        if not texts:
            return []
        
        # Создаем задачи для каждого текста
        tasks = [self.analyze_text(text) for text in texts]
        
        # Выполняем все задачи параллельно
        results = await asyncio.gather(*tasks)
        
        return results
        
    async def analyze_text_stream(self, text_stream: Union[List[str], asyncio.Queue]) -> List[Dict[str, Any]]:
        """
        Анализирует поток текстов по мере их поступления.
        
        Args:
            text_stream: Список текстов или очередь asyncio.Queue
            
        Returns:
            Список результатов анализа
        """
        results = []
        
        if isinstance(text_stream, list):
            # Если передан список, обрабатываем его пакетами
            batch_size = 5  # Размер пакета для параллельной обработки
            for i in range(0, len(text_stream), batch_size):
                batch = text_stream[i:i + batch_size]
                batch_results = await self.analyze_text_batch(batch)
                results.extend(batch_results)
                
        else:  # Предполагаем, что это asyncio.Queue
            # Обрабатываем очередь, пока не получим None (сигнал завершения)
            while True:
                text = await text_stream.get()
                if text is None:  # Сигнал завершения
                    text_stream.task_done()
                    break
                    
                result = await self.analyze_text(text)
                results.append(result)
                text_stream.task_done()
        
        return results
        
    def _log_cache_stats(self):
        """
        Логирует статистику использования кэша.
        """
        total = self.cache_hits + self.cache_misses
        if total > 0:
            hit_rate = (self.cache_hits / total) * 100
            now = datetime.now()
            duration = (now - self.cache_stats_last_reset).total_seconds()
            
            logger.info(f"Cache stats: {self.cache_hits} hits, {self.cache_misses} misses, "
                       f"{hit_rate:.1f}% hit rate over {duration:.1f} seconds")
            
            # Сбрасываем статистику
            self.cache_hits = 0
            self.cache_misses = 0
            self.cache_stats_last_reset = now
    
    def _map_dimensions_to_emotion(self, dimensions: Dict[str, float]) -> Tuple[str, float]:
        """
        Преобразует эмоциональные измерения в конкретную эмоцию.
        
        Args:
            dimensions: Словарь эмоциональных измерений (VAD)
            
        Returns:
            Кортеж (тип_эмоции, интенсивность)
        """
        # Находим ближайшую эмоцию по евклидову расстоянию в пространстве VAD
        min_distance = float('inf')
        closest_emotion = "нейтральность"
        
        for emotion, emotion_dims in EMOTION_MAPPING.items():
            if emotion == "BASE_EMOTIONS":
                continue
            # Вычисляем расстояние между измерениями
            distance = sum(
                (dimensions.get(dim, 0.5) - val) ** 2 
                for dim, val in emotion_dims.items()
            )
            distance = distance ** 0.5  # Корень из суммы квадратов (евклидово расстояние)
            
            if distance < min_distance:
                min_distance = distance
                closest_emotion = emotion
        
        # Вычисляем интенсивность на основе отклонения от нейтральной точки (0.5)
        neutral_point = {dim: 0.5 for dim in dimensions}
        intensity = sum(
            (val - 0.5) ** 2 for val in dimensions.values()
        ) ** 0.5
        
        # Нормализуем интенсивность до диапазона 0-1
        # Максимальное расстояние от нейтральной точки в 3D пространстве = sqrt(3 * 0.5^2) ≈ 0.866
        intensity = min(intensity / 0.866, 1.0)
        
        return closest_emotion, intensity
    
    async def get_emotion_suggestions(self, user_id: str, context: str = None) -> List[Dict[str, Any]]:
        """
        Получает предложения по эмоциям на основе контекста пользователя.
        
        Args:
            user_id: ID пользователя
            context: Контекст для анализа (опционально)
            
        Returns:
            Список предложений по эмоциям
        """
        # Расширенный набор эмоций с дополнительными характеристиками
        base_emotions = [
            {
                "emotion_type": "радость", 
                "description": "Позитивное чувство удовлетворения",
                "valence": 0.8,
                "arousal": 0.6,
                "dominance": 0.7,
                "category": "позитивная"
            },
            {
                "emotion_type": "интерес", 
                "description": "Любопытство и вовлеченность",
                "valence": 0.6,
                "arousal": 0.5,
                "dominance": 0.6,
                "category": "нейтральная"
            },
            {
                "emotion_type": "спокойствие", 
                "description": "Состояние умиротворения",
                "valence": 0.7,
                "arousal": 0.2,
                "dominance": 0.6,
                "category": "позитивная"
            },
            {
                "emotion_type": "удивление", 
                "description": "Неожиданное открытие",
                "valence": 0.5,
                "arousal": 0.8,
                "dominance": 0.4,
                "category": "нейтральная"
            },
            {
                "emotion_type": "задумчивость", 
                "description": "Размышление над чем-то",
                "valence": 0.5,
                "arousal": 0.3,
                "dominance": 0.5,
                "category": "нейтральная"
            },
            {
                "emotion_type": "восторг", 
                "description": "Сильное чувство радости и восхищения",
                "valence": 0.9,
                "arousal": 0.9,
                "dominance": 0.8,
                "category": "позитивная"
            },
            {
                "emotion_type": "грусть", 
                "description": "Чувство печали и уныния",
                "valence": 0.3,
                "arousal": 0.3,
                "dominance": 0.3,
                "category": "негативная"
            },
            {
                "emotion_type": "тревога", 
                "description": "Беспокойство и напряжение",
                "valence": 0.3,
                "arousal": 0.7,
                "dominance": 0.2,
                "category": "негативная"
            }
        ]
        
        # Если есть контекст, можно использовать его для фильтрации или персонализации
        if context:
            # Анализируем контекст и возвращаем релевантные эмоции
            try:
                analysis = await self.analyze_text(context)
                
                # Сортируем эмоции по близости к проанализированному контексту
                # используя евклидово расстояние в пространстве VAD
                for emotion in base_emotions:
                    distance = sum(
                        (analysis["dimensions"].get(dim, 0.5) - emotion.get(dim, 0.5)) ** 2 
                        for dim in ["valence", "arousal", "dominance"]
                    ) ** 0.5
                    emotion["relevance"] = 1.0 - min(distance, 1.0)  # Чем меньше расстояние, тем выше релевантность
                
                # Сортируем по релевантности
                sorted_emotions = sorted(base_emotions, key=lambda e: e.get("relevance", 0.0), reverse=True)
                return sorted_emotions[:5]  # Возвращаем топ-5 наиболее релевантных
                
            except Exception as e:
                logger.error(f"Error analyzing context for suggestions: {e}")
        
        # Если нет контекста или произошла ошибка, возвращаем базовый набор
        return base_emotions[:5]


# Пул потоков для асинхронной обработки больших объемов текста
THREAD_POOL = ThreadPoolExecutor(max_workers=4)

# Глобальный экземпляр адаптера для использования в приложении
emotion_ml_adapter = EmotionMLAdapter()

# Экспортируем функцию для очистки кэша
async def clear_emotion_cache():
    """Очищает кэш анализа эмоций."""
    before_size = len(emotion_ml_adapter._analyze_text_cached.__wrapped__.__cache__)
    emotion_ml_adapter._analyze_text_cached.cache_clear()
    logger.info(f"Emotion analysis cache cleared. Removed {before_size} entries.")
    return {"cleared_entries": before_size}

# Экспортируем функцию для получения статистики кэша
async def get_emotion_cache_stats():
    """Возвращает статистику использования кэша."""
    cache_info = emotion_ml_adapter._analyze_text_cached.cache_info()
    return {
        "hits": cache_info.hits,
        "misses": cache_info.misses,
        "maxsize": cache_info.maxsize,
        "currsize": cache_info.currsize,
        "hit_rate": cache_info.hits / (cache_info.hits + cache_info.misses) if (cache_info.hits + cache_info.misses) > 0 else 0
    }

# Функция для асинхронной обработки больших текстов
async def analyze_large_text(text: str, chunk_size: int = 1000, overlap: int = 100) -> Dict[str, Any]:
    """
    Асинхронно анализирует большой текст, разбивая его на части.
    
    Args:
        text: Большой текст для анализа
        chunk_size: Размер каждого фрагмента текста
        overlap: Размер перекрытия между фрагментами
        
    Returns:
        Агрегированный результат анализа
    """
    if not text or len(text) < chunk_size:
        # Если текст небольшой, используем обычный анализ
        return await emotion_ml_adapter.analyze_text(text)
    
    # Разбиваем текст на фрагменты с перекрытием
    chunks = []
    for i in range(0, len(text), chunk_size - overlap):
        chunk = text[i:i + chunk_size]
        if len(chunk) > 50:  # Игнорируем слишком маленькие фрагменты
            chunks.append(chunk)
    
    logger.info(f"Analyzing large text: {len(text)} chars split into {len(chunks)} chunks")
    
    # Анализируем каждый фрагмент параллельно
    tasks = [emotion_ml_adapter.analyze_text(chunk) for chunk in chunks]
    results = await asyncio.gather(*tasks)
    
    # Агрегируем результаты
    aggregated = await _aggregate_emotion_results(results)
    
    return aggregated

async def _aggregate_emotion_results(results: List[Dict[str, Any]]) -> Dict[str, Any]:
    """
    Агрегирует результаты анализа нескольких фрагментов текста.
    
    Args:
        results: Список результатов анализа
        
    Returns:
        Агрегированный результат
    """
    if not results:
        return {
            "emotion_type": "нейтральность",
            "intensity": 0.0,
            "dimensions": {},
            "confidence": 0.0,
            "features": {},
            "cached": False,
            "aggregated": True
        }
    
    # Собираем все эмоции и их интенсивности
    emotions = {}
    dimensions = {"valence": 0.0, "arousal": 0.0, "dominance": 0.0}
    total_confidence = 0.0
    features = {}
    
    for result in results:
        emotion_type = result.get("emotion_type", "нейтральность")
        intensity = result.get("intensity", 0.0)
        
        # Учитываем эмоцию с её интенсивностью
        if emotion_type in emotions:
            emotions[emotion_type] += intensity
        else:
            emotions[emotion_type] = intensity
        
        # Суммируем измерения
        for dim, value in result.get("dimensions", {}).items():
            if dim in dimensions:
                dimensions[dim] += value
            else:
                dimensions[dim] = value
        
        # Учитываем уверенность
        total_confidence += result.get("confidence", 0.0)
        
        # Собираем признаки
        for feat_key, feat_value in result.get("features", {}).items():
            if isinstance(feat_value, (int, float)):
                features[feat_key] = features.get(feat_key, 0) + feat_value
    
    # Нормализуем измерения
    for dim in dimensions:
        dimensions[dim] /= len(results)
    
    # Находим доминирующую эмоцию
    dominant_emotion = max(emotions.items(), key=lambda x: x[1]) if emotions else ("нейтральность", 0.0)
    
    # Нормализуем интенсивность
    normalized_intensity = min(dominant_emotion[1] / len(results), 1.0)
    
    # Нормализуем уверенность
    normalized_confidence = total_confidence / len(results)
    
    return {
        "emotion_type": dominant_emotion[0],
        "intensity": normalized_intensity,
        "dimensions": dimensions,
        "confidence": normalized_confidence,
        "features": features,
        "cached": False,
        "aggregated": True,
        "chunks_analyzed": len(results)
    }
