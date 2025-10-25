#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Тесты для модуля мультиязычной поддержки.

Проверяет функции определения языка, перевода эмоций
и мультиязычного анализа текста.
"""

import pytest
import asyncio
from unittest.mock import AsyncMock, MagicMock, patch

import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from backend.personality.multilingual_support import (
    detect_language,
    translate_emotion,
    get_emotion_in_language,
    analyze_multilingual_text,
    get_supported_languages,
    get_emotion_dictionary
)


class TestMultilingualSupport:
    """Тесты для модуля мультиязычной поддержки."""
    
    def test_detect_language_russian(self):
        """Тест определения русского языка."""
        text = "Привет, как дела? Это тестовый текст на русском языке."
        lang = detect_language(text)
        assert lang == "ru"
    
    def test_detect_language_english(self):
        """Тест определения английского языка."""
        text = "Hello, how are you? This is a test text in English."
        lang = detect_language(text)
        assert lang == "en"
    
    def test_detect_language_german(self):
        """Тест определения немецкого языка."""
        text = "Hallo, wie geht es dir? Dies ist ein Testtext auf Deutsch."
        lang = detect_language(text)
        assert lang == "de"
    
    def test_detect_language_french(self):
        """Тест определения французского языка."""
        text = "Bonjour, comment ça va? C'est un texte de test en français."
        lang = detect_language(text)
        assert lang == "fr"
    
    def test_detect_language_mixed(self):
        """Тест определения языка в смешанном тексте."""
        text = "Hello привет, this is a mixed text с русскими словами."
        lang = detect_language(text)
        # Должен определить преобладающий язык
        assert lang in ["en", "ru"]
    
    def test_detect_language_empty(self):
        """Тест определения языка в пустом тексте."""
        text = ""
        lang = detect_language(text)
        assert lang == "en"  # По умолчанию английский
    
    def test_translate_emotion_ru_to_en(self):
        """Тест перевода эмоции с русского на английский."""
        emotion = "радость"
        translated = translate_emotion(emotion, "ru", "en")
        assert translated == "joy"
    
    def test_translate_emotion_en_to_ru(self):
        """Тест перевода эмоции с английского на русский."""
        emotion = "joy"
        translated = translate_emotion(emotion, "en", "ru")
        assert translated == "радость"
    
    def test_translate_emotion_unknown(self):
        """Тест перевода неизвестной эмоции."""
        emotion = "неизвестная_эмоция"
        translated = translate_emotion(emotion, "ru", "en")
        assert translated == "неизвестная_эмоция"  # Должна остаться без изменений
    
    def test_get_emotion_in_language(self):
        """Тест получения эмоции на указанном языке."""
        emotion = "радость"
        result = get_emotion_in_language(emotion, "de")
        assert result == "Freude"
    
    @pytest.mark.asyncio
    async def test_analyze_multilingual_text_same_language(self):
        """Тест анализа текста на том же языке, что и целевой."""
        # Создаем мок-функцию для анализа
        mock_analyzer = AsyncMock(return_value={
            "emotion_type": "радость",
            "intensity": 0.8,
            "dimensions": {"valence": 0.8, "arousal": 0.6, "dominance": 0.7},
            "confidence": 0.9,
            "features": {},
            "cached": False
        })
        
        text = "Привет, как дела? Я очень рад тебя видеть!"
        result = await analyze_multilingual_text(text, mock_analyzer, "ru")
        
        # Проверяем, что функция анализа была вызвана
        mock_analyzer.assert_called_once_with(text)
        
        # Проверяем результат
        assert result["emotion_type"] == "радость"
        assert "original_emotion" not in result  # Не должно быть перевода
    
    @pytest.mark.asyncio
    async def test_analyze_multilingual_text_different_language(self):
        """Тест анализа текста на языке, отличном от целевого."""
        # Создаем мок-функцию для анализа
        mock_analyzer = AsyncMock(return_value={
            "emotion_type": "joy",
            "intensity": 0.8,
            "dimensions": {"valence": 0.8, "arousal": 0.6, "dominance": 0.7},
            "confidence": 0.9,
            "features": {},
            "cached": False
        })
        
        text = "Hello, how are you? I'm very happy to see you!"
        result = await analyze_multilingual_text(text, mock_analyzer, "ru")
        
        # Проверяем, что функция анализа была вызвана
        mock_analyzer.assert_called_once_with(text)
        
        # Проверяем результат
        assert result["emotion_type"] == "радость"
        assert result["original_emotion"] == "joy"
        assert result["original_language"] == "en"
        assert result["target_language"] == "ru"
    
    def test_get_supported_languages(self):
        """Тест получения списка поддерживаемых языков."""
        languages = get_supported_languages()
        assert isinstance(languages, dict)
        assert "ru" in languages
        assert "en" in languages
        assert languages["ru"] == "russian"
    
    def test_get_emotion_dictionary(self):
        """Тест получения словаря эмоций для указанного языка."""
        emotions = get_emotion_dictionary("ru")
        assert isinstance(emotions, dict)
        assert "joy" in emotions
        assert emotions["joy"] == "радость"
    
    def test_get_emotion_dictionary_fallback(self):
        """Тест получения словаря эмоций для неподдерживаемого языка."""
        emotions = get_emotion_dictionary("xx")  # Несуществующий язык
        assert isinstance(emotions, dict)
        assert "joy" in emotions
        assert emotions["joy"] == "joy"  # Должен вернуть английский вариант


if __name__ == "__main__":
    # Запуск тестов
    pytest.main([__file__, "-v"])
