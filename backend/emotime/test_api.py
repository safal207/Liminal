#!/usr/bin/env python3
"""
Тестирование Emotime API
"""

import json
import socket

import pytest
import requests

BASE_URL = "http://localhost:8000"


def _server_available(host: str = "localhost", port: int = 8000) -> bool:
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        sock.settimeout(0.5)
        return sock.connect_ex((host, port)) == 0


if not _server_available():
    pytest.skip(
        "Emotime API tests require a running server on localhost:8000",
        allow_module_level=True,
    )


def test_create_user():
    """Тест создания пользователя"""
    print("✅ Тестируем создание пользователя...")

    user_data = {"email": "test@example.com", "plan": "starter"}

    response = requests.post(f"{BASE_URL}/users", json=user_data)

    if response.status_code == 200:
        user = response.json()
        print(f"✅ Пользователь создан: {user['email']}")
        print(f"🔑 API ключ: {user['api_key']}")
        return user["api_key"]
    else:
        print(f"❌ Ошибка: {response.status_code} - {response.text}")
        return None


def test_analyze_emotions(api_key):
    """Тест анализа эмоций"""
    print("\n🧪 Тестируем анализ эмоций...")

    headers = {"Authorization": f"Bearer {api_key}"}

    test_texts = [
        "I am so happy today! This is amazing!",
        "I feel sad and depressed about the situation",
        "I'm really angry about what happened",
        "I'm afraid of the future",
        "What a surprise! I can't believe it!",
    ]

    for text in test_texts:
        data = {"text": text, "language": "en"}
        response = requests.post(f"{BASE_URL}/analyze", json=data, headers=headers)

        if response.status_code == 200:
            result = response.json()
            print(f"✅ Текст: '{text[:30]}...'")
            print(f"   Эмоции: {result['emotions']}")
            print(f"   Уверенность: {result['confidence']}")
        else:
            print(f"❌ Ошибка: {response.status_code} - {response.text}")


def test_usage_stats(api_key):
    """Тест получения статистики"""
    print("\n�� Тестируем статистику использования...")

    headers = {"Authorization": f"Bearer {api_key}"}
    response = requests.get(f"{BASE_URL}/usage", headers=headers)

    if response.status_code == 200:
        stats = response.json()
        print(f"✅ Статистика: {json.dumps(stats, indent=2)}")
    else:
        print(f"❌ Ошибка: {response.status_code} - {response.text}")


def main():
    """Основная функция тестирования"""
    print("🚀 Начинаем тестирование Emotime API")
    print("=" * 50)

    # Тест 1: Создание пользователя
    api_key = test_create_user()
    if not api_key:
        print("❌ Не удалось создать пользователя. Завершаем тестирование.")
        return

    # Тест 2: Анализ эмоций
    test_analyze_emotions(api_key)

    # Тест 3: Статистика использования
    test_usage_stats(api_key)

    print("\n" + "=" * 50)
    print("�� Тестирование завершено!")


if __name__ == "__main__":
    main()
