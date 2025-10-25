#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Модуль мультиязычной поддержки для анализа эмоций.

Предоставляет функции для определения языка текста и адаптации
анализа эмоций для различных языков.
"""

import re
import logging
from typing import Dict, Any, Optional, List, Tuple, Union
import asyncio
from functools import lru_cache

# Настройка логгера
logger = logging.getLogger(__name__)

# Поддерживаемые языки
SUPPORTED_LANGUAGES = {
    "ru": "russian",
    "en": "english",
    "de": "german",
    "fr": "french",
    "es": "spanish",
    "it": "italian",
    "zh": "chinese",
    "ja": "japanese"
}

# Словари эмоций для разных языков
EMOTION_DICTIONARIES = {
    "ru": {
        "joy": "радость",
        "sadness": "грусть",
        "anger": "злость",
        "fear": "страх",
        "surprise": "удивление",
        "disgust": "отвращение",
        "neutral": "нейтральность",
        "excitement": "восторг",
        "serenity": "умиротворение",
        "interest": "интерес",
        "pride": "гордость",
        "gratitude": "благодарность",
        "love": "любовь",
        "hope": "надежда",
        "anxiety": "тревога",
        "shame": "стыд",
        "guilt": "вина",
        "envy": "зависть",
        "disappointment": "разочарование",
        "embarrassment": "смущение",
        "despair": "отчаяние",
        "boredom": "скука",
        "nostalgia": "ностальгия",
        "confusion": "замешательство",
        "pensiveness": "задумчивость"
    },
    "en": {
        "joy": "joy",
        "sadness": "sadness",
        "anger": "anger",
        "fear": "fear",
        "surprise": "surprise",
        "disgust": "disgust",
        "neutral": "neutral",
        "excitement": "excitement",
        "serenity": "serenity",
        "interest": "interest",
        "pride": "pride",
        "gratitude": "gratitude",
        "love": "love",
        "hope": "hope",
        "anxiety": "anxiety",
        "shame": "shame",
        "guilt": "guilt",
        "envy": "envy",
        "disappointment": "disappointment",
        "embarrassment": "embarrassment",
        "despair": "despair",
        "boredom": "boredom",
        "nostalgia": "nostalgia",
        "confusion": "confusion",
        "pensiveness": "pensiveness"
    },
    "de": {
        "joy": "Freude",
        "sadness": "Traurigkeit",
        "anger": "Wut",
        "fear": "Angst",
        "surprise": "Überraschung",
        "disgust": "Ekel",
        "neutral": "Neutralität",
        "excitement": "Begeisterung",
        "serenity": "Gelassenheit",
        "interest": "Interesse",
        "pride": "Stolz",
        "gratitude": "Dankbarkeit",
        "love": "Liebe",
        "hope": "Hoffnung",
        "anxiety": "Besorgnis",
        "shame": "Scham",
        "guilt": "Schuld",
        "envy": "Neid",
        "disappointment": "Enttäuschung",
        "embarrassment": "Verlegenheit",
        "despair": "Verzweiflung",
        "boredom": "Langeweile",
        "nostalgia": "Nostalgie",
        "confusion": "Verwirrung",
        "pensiveness": "Nachdenklichkeit"
    },
    "fr": {
        "joy": "joie",
        "sadness": "tristesse",
        "anger": "colère",
        "fear": "peur",
        "surprise": "surprise",
        "disgust": "dégoût",
        "neutral": "neutralité",
        "excitement": "enthousiasme",
        "serenity": "sérénité",
        "interest": "intérêt",
        "pride": "fierté",
        "gratitude": "gratitude",
        "love": "amour",
        "hope": "espoir",
        "anxiety": "anxiété",
        "shame": "honte",
        "guilt": "culpabilité",
        "envy": "envie",
        "disappointment": "déception",
        "embarrassment": "embarras",
        "despair": "désespoir",
        "boredom": "ennui",
        "nostalgia": "nostalgie",
        "confusion": "confusion",
        "pensiveness": "pensivité"
    }
}

# Языковые маркеры для определения языка
LANGUAGE_MARKERS = {
    "ru": set("абвгдеёжзийклмнопрстуфхцчшщъыьэюя"),
    "en": set("abcdefghijklmnopqrstuvwxyz"),
    "de": set("abcdefghijklmnopqrstuvwxyzäöüß"),
    "fr": set("abcdefghijklmnopqrstuvwxyzàâæçéèêëîïôœùûüÿ"),
    "es": set("abcdefghijklmnopqrstuvwxyzáéíóúüñ"),
    "it": set("abcdefghijklmnopqrstuvwxyzàèéìíîòóùú"),
}


@lru_cache(maxsize=1000)
def detect_language(text: str) -> str:
    """
    Определяет язык текста на основе частоты символов.
    
    Args:
        text: Текст для анализа
        
    Returns:
        Код языка (ru, en, de, fr, es, it) или 'en' по умолчанию
    """
    if not text:
        return "en"
    
    # Приводим текст к нижнему регистру
    text = text.lower()
    
    # Считаем символы для каждого языка
    lang_counts = {}
    for lang, charset in LANGUAGE_MARKERS.items():
        count = sum(1 for char in text if char in charset)
        if count > 0:
            lang_counts[lang] = count / len(text)
    
    # Если не удалось определить язык, возвращаем английский
    if not lang_counts:
        return "en"
    
    # Возвращаем язык с наибольшим количеством совпадений
    return max(lang_counts.items(), key=lambda x: x[1])[0]


def translate_emotion(emotion: str, source_lang: str, target_lang: str) -> str:
    """
    Переводит название эмоции с одного языка на другой.
    
    Args:
        emotion: Название эмоции
        source_lang: Исходный язык (ru, en, de, fr)
        target_lang: Целевой язык (ru, en, de, fr)
        
    Returns:
        Переведенное название эмоции
    """
    if source_lang not in EMOTION_DICTIONARIES or target_lang not in EMOTION_DICTIONARIES:
        return emotion
    
    # Находим ключ эмоции в исходном языке
    emotion_key = None
    for key, value in EMOTION_DICTIONARIES[source_lang].items():
        if value.lower() == emotion.lower():
            emotion_key = key
            break
    
    # Если не нашли ключ, возвращаем исходную эмоцию
    if not emotion_key:
        return emotion
    
    # Возвращаем эмоцию на целевом языке
    return EMOTION_DICTIONARIES[target_lang].get(emotion_key, emotion)


def get_emotion_in_language(emotion: str, target_lang: str) -> str:
    """
    Возвращает название эмоции на указанном языке.
    
    Args:
        emotion: Название эмоции (на любом языке)
        target_lang: Целевой язык (ru, en, de, fr)
        
    Returns:
        Название эмоции на целевом языке
    """
    # Определяем исходный язык эмоции
    source_lang = "en"  # По умолчанию считаем, что эмоция на английском
    for lang, emotions in EMOTION_DICTIONARIES.items():
        if emotion.lower() in [e.lower() for e in emotions.values()]:
            source_lang = lang
            break
    
    # Переводим эмоцию
    return translate_emotion(emotion, source_lang, target_lang)


async def analyze_multilingual_text(text: str, analyzer_func, target_lang: str = "ru", expected_lang: Optional[str] = None) -> Dict[str, Any]:
    """
    Анализирует текст на любом языке и возвращает результат на целевом языке.
    
    Args:
        text: Текст для анализа
        analyzer_func: Функция анализа эмоций
        target_lang: Язык для вывода результата
        expected_lang: Необязательная подсказка ожидаемого языка; используется как
            tie-break в случае неоднозначности (например, только ASCII-символы).
            Если счёт expected_lang по LANGUAGE_MARKERS не хуже обнаруженного,
            выбирается expected_lang.
        
    Returns:
        Результат анализа с эмоциями на целевом языке
    """
    # Определяем язык текста
    detected_lang = detect_language(text)
    
    # Tie-break с подсказкой expected_lang при равенстве/неоднозначности
    if expected_lang and expected_lang in SUPPORTED_LANGUAGES:
        try:
            lt = text.lower()
            charset_expected = LANGUAGE_MARKERS.get(expected_lang, set())
            charset_detected = LANGUAGE_MARKERS.get(detected_lang, set())
            count_expected = sum(1 for ch in lt if ch in charset_expected)
            count_detected = sum(1 for ch in lt if ch in charset_detected)
            # Если подсказка не хуже по счёту, предпочитаем её
            if count_expected >= count_detected:
                detected_lang = expected_lang
        except Exception:
            # Безопасно игнорируем ошибки повышения надёжности
            pass
    
    # Анализируем текст
    result = await analyzer_func(text)
    
    # Если язык результата отличается от целевого, переводим эмоцию
    if detected_lang != target_lang:
        result["original_emotion"] = result["emotion_type"]
        result["original_language"] = detected_lang
        result["emotion_type"] = get_emotion_in_language(result["emotion_type"], target_lang)
        result["target_language"] = target_lang
    
    return result


# Функция для получения списка поддерживаемых языков
def get_supported_languages() -> Dict[str, str]:
    """
    Возвращает список поддерживаемых языков.
    
    Returns:
        Словарь с кодами и названиями языков
    """
    return SUPPORTED_LANGUAGES


# Функция для получения словаря эмоций для указанного языка
def get_emotion_dictionary(lang: str) -> Dict[str, str]:
    """
    Возвращает словарь эмоций для указанного языка.
    
    Args:
        lang: Код языка (ru, en, de, fr)
        
    Returns:
        Словарь эмоций для указанного языка
    """
    return EMOTION_DICTIONARIES.get(lang, EMOTION_DICTIONARIES["en"])
