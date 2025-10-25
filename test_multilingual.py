#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Тестовый скрипт для проверки функциональности мультиязычной поддержки
и EmotionMLAdapter.
"""

import asyncio
import sys
import os
from pprint import pprint

# Добавляем путь к проекту
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

# Отключаем импорт модулей, которые могут вызывать проблемы
sys.modules['backend.personality.router'] = None

from backend.personality.multilingual_support import (
    detect_language,
    translate_emotion,
    analyze_multilingual_text
)
from backend.personality.ml_adapter import EmotionMLAdapter

async def test_multilingual_support():
    """Тестирование мультиязычной поддержки."""
    print("=== Тестирование мультиязычной поддержки ===")
    
    # Тест определения языка
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
    
    # Тест перевода эмоций
    print("\n--- Перевод эмоций ---")
    emotions = {
        "радость": {"ru": "радость", "en": "joy", "de": "Freude", "fr": "joie"},
        "грусть": {"ru": "грусть", "en": "sadness", "de": "Traurigkeit", "fr": "tristesse"},
        "злость": {"ru": "злость", "en": "anger", "de": "Wut", "fr": "colère"}
    }
    
    for emotion, translations in emotions.items():
        print(f"\nЭмоция: {emotion}")
        for source_lang, source_emotion in translations.items():
            for target_lang, target_emotion in translations.items():
                if source_lang != target_lang:
                    translated = translate_emotion(source_emotion, source_lang, target_lang)
                    print(f"  {source_lang} -> {target_lang}: {source_emotion} -> {translated}, Ожидаемо: {target_emotion}, Совпадение: {translated == target_emotion}")

    # Тест EmotionMLAdapter
    print("\n--- Тестирование EmotionMLAdapter ---")
    adapter = EmotionMLAdapter()
    
    # Тест анализа текста на русском
    print("\nАнализ русского текста:")
    ru_text = "Я очень счастлив сегодня, у меня отличное настроение!"
    ru_result = await adapter.analyze_text(ru_text)
    print(f"Результат анализа русского текста:")
    pprint(ru_result)
    
    # Тест анализа текста на английском
    print("\nАнализ английского текста:")
    en_text = "I am very happy today, I'm in a great mood!"
    en_result = await adapter.analyze_text(en_text)
    print(f"Результат анализа английского текста:")
    pprint(en_result)
    
    # Тест мультиязычного анализа
    print("\nМультиязычный анализ (английский текст, русский результат):")
    multi_result = await adapter.analyze_multilingual(en_text, "ru", expected_lang="en")
    print(f"Результат мультиязычного анализа:")
    pprint(multi_result)

    # Кейс tie-break: немецкий ASCII-текст без умляутов; expected_lang помогает выбрать de
    print("\nМультиязычный анализ (немецкий ASCII, tie-break expected_lang=de):")
    de_ascii_text = "Das ist ein guter Test und der Nutzer ist glücklich"
    de_tie_result = await analyze_multilingual_text(
        de_ascii_text,
        adapter.analyze_text,
        target_lang="de",
        expected_lang="de",
    )
    pprint(de_tie_result)
    # Явная проверка корректности tie-break
    detected_before = detect_language(de_ascii_text)
    print(f"detect_language(de_ascii_text) -> {detected_before}")
    assert "original_language" not in de_tie_result, "Не должно быть original_language, когда expected_lang совпадает с выбранным языком"
    assert de_tie_result.get("target_language") == "de", "target_language должен быть 'de' при expected_lang='de'"

    # Кейс tie-break: французский ASCII без диакритики
    print("\nМультиязычный анализ (французский ASCII, tie-break expected_lang=fr):")
    fr_ascii_text = "C est un bon test et le client est content"
    fr_tie_result = await analyze_multilingual_text(
        fr_ascii_text,
        adapter.analyze_text,
        target_lang="fr",
        expected_lang="fr",
    )
    pprint(fr_tie_result)
    detected_before_fr = detect_language(fr_ascii_text)
    print(f"detect_language(fr_ascii_text) -> {detected_before_fr}")
    assert "original_language" not in fr_tie_result, "Не должно быть original_language для fr при expected_lang"
    assert fr_tie_result.get("target_language") == "fr", "target_language должен быть 'fr' при expected_lang='fr'"

    # Кейс tie-break: испанский ASCII без диакритики
    print("\nМультиязычный анализ (испанский ASCII, tie-break expected_lang=es):")
    es_ascii_text = "Este es un buen test y el usuario esta feliz"
    es_tie_result = await analyze_multilingual_text(
        es_ascii_text,
        adapter.analyze_text,
        target_lang="es",
        expected_lang="es",
    )
    pprint(es_tie_result)
    detected_before_es = detect_language(es_ascii_text)
    print(f"detect_language(es_ascii_text) -> {detected_before_es}")
    assert "original_language" not in es_tie_result, "Не должно быть original_language для es при expected_lang"
    assert es_tie_result.get("target_language") == "es", "target_language должен быть 'es' при expected_lang='es'"
    
    print("\n=== Тестирование завершено ===")

if __name__ == "__main__":
    # Запуск тестов
    asyncio.run(test_multilingual_support())
