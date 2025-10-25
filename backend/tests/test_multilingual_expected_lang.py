#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import asyncio

from backend.personality.ml_adapter import EmotionMLAdapter
from backend.personality.multilingual_support import analyze_multilingual_text


def run(coro):
    try:
        return asyncio.run(coro)
    except RuntimeError:
        # Fallback для окружений без default event loop
        loop = asyncio.new_event_loop()
        try:
            return loop.run_until_complete(coro)
        finally:
            loop.close()


def test_tie_break_de_ascii_expected_lang_de():
    adapter = EmotionMLAdapter()
    de_ascii_text = "Das ist ein guter Test und der Nutzer ist gluecklich"
    res = run(
        analyze_multilingual_text(
            de_ascii_text, adapter.analyze_text, target_lang="de", expected_lang="de"
        )
    )
    assert "original_language" not in res
    assert res.get("target_language") == "de"


def test_tie_break_fr_ascii_expected_lang_fr():
    adapter = EmotionMLAdapter()
    fr_ascii_text = "C est un bon test et le client est content"
    res = run(
        analyze_multilingual_text(
            fr_ascii_text, adapter.analyze_text, target_lang="fr", expected_lang="fr"
        )
    )
    assert "original_language" not in res
    assert res.get("target_language") == "fr"


def test_tie_break_es_ascii_expected_lang_es():
    adapter = EmotionMLAdapter()
    es_ascii_text = "Este es un buen test y el usuario esta feliz"
    res = run(
        analyze_multilingual_text(
            es_ascii_text, adapter.analyze_text, target_lang="es", expected_lang="es"
        )
    )
    assert "original_language" not in res
    assert res.get("target_language") == "es"


def test_translation_en_to_ru_with_expected_lang_en():
    adapter = EmotionMLAdapter()
    en_text = "This is a test and the user is happy"
    res = run(
        analyze_multilingual_text(
            en_text, adapter.analyze_text, target_lang="ru", expected_lang="en"
        )
    )
    # При различии языков должен появиться original_language и измениться emotion_type при переводе
    assert res.get("target_language") == "ru"
    assert res.get("original_language") in {"en", "ru", "de", "fr", "es", "it"}
    # По крайней мере ключи присутствуют и корректно заполнены
    assert "emotion_type" in res and isinstance(res["emotion_type"], str)
