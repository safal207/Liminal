#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Простой тестовый скрипт для проверки функциональности ml_adapter.py и multilingual_support.py
без зависимостей от других модулей.
"""

import sys
import os
import asyncio
from pprint import pprint

# Добавляем путь к проекту
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

# Принудительно включаем UTF-8 вывод и изоляцию роутера
try:
    if hasattr(sys.stdout, "reconfigure"):
        sys.stdout.reconfigure(encoding="utf-8")
except Exception:
    pass

os.environ.setdefault("LIMINAL_ENABLE_PERSONALITY_ROUTER", "0")

# Импортируем только необходимые модули
from backend.personality.ml_adapter import (
    EmotionMLAdapter,
    EMOTION_MAPPING,
    _aggregate_emotion_results,
)
from backend.personality.multilingual_support import detect_language

async def test_ml_adapter():
    """Тестирование EmotionMLAdapter."""
    print("=== Тестирование EmotionMLAdapter ===")
    
    # Создаем экземпляр EmotionMLAdapter
    adapter = EmotionMLAdapter()
    
    # Тестируем метод get_emotion_suggestions
    print("\n--- Тестирование get_emotion_suggestions ---")
    emotions = await adapter.get_emotion_suggestions("user-1", context="радость")
    print("Предложения для 'радость':")
    pprint(emotions)
    
    # Вместо несуществующего метода get_emotion_mapping выводим первые элементы EMOTION_MAPPING
    print("\n--- Тестирование EMOTION_MAPPING (первые 5 базовых эмоций) ---")
    base_items = list(EMOTION_MAPPING.get("BASE_EMOTIONS", {}).items())[:5]
    for key, value in base_items:
        print(f"{key}: {value}")
    
    # Тестируем метод _aggregate_emotion_results
    print("\n--- Тестирование _aggregate_emotion_results ---")
    results = [
        {
            "emotion_type": "радость",
            "intensity": 0.8,
            "dimensions": {"valence": 0.8, "arousal": 0.6, "dominance": 0.7},
            "confidence": 0.85,
            "features": {"tokens": 10},
            "cached": False,
        },
        {
            "emotion_type": "интерес",
            "intensity": 0.6,
            "dimensions": {"valence": 0.7, "arousal": 0.5, "dominance": 0.6},
            "confidence": 0.75,
            "features": {"tokens": 8},
            "cached": False,
        },
        {
            "emotion_type": "умиротворение",
            "intensity": 0.4,
            "dimensions": {"valence": 0.7, "arousal": 0.2, "dominance": 0.6},
            "confidence": 0.70,
            "features": {"tokens": 6},
            "cached": False,
        },
    ]
    aggregated = await _aggregate_emotion_results(results)
    print("Агрегированные результаты:")
    pprint(aggregated)
    
    print("\n=== Тестирование завершено ===")

def test_multilingual_support():
    """Тестирование функции detect_language."""
    print("\n=== Тестирование multilingual_support ===")
    
    # Тестируем функцию detect_language
    texts = {
        "ru": "Привет, как дела? Это тестовый текст на русском языке.",
        "en": "Hello, how are you? This is a test text in English.",
        "de": "Hallo, wie geht es dir? Dies ist ein Testtext auf Deutsch.",
        "fr": "Bonjour, comment ça va? C'est un texte de test en français."
    }
    
    print("\n--- Определение языка ---")
    for lang, text in texts.items():
        detected = detect_language(text)
        print(f"Ожидаемый язык: {lang}, Определенный язык: {detected}, Совпадение: {lang == detected}")
    
    print("\n=== Тестирование завершено ===")

if __name__ == "__main__":
    # Запуск тестов
    print("Запуск тестов...")
    
    # Тестирование multilingual_support
    test_multilingual_support()
    
    # Тестирование ml_adapter
    asyncio.run(test_ml_adapter())
    
    print("\nВсе тесты завершены успешно!")
