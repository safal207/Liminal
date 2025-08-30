#!/usr/bin/env python
"""
Упрощенный тест универсального адаптера для OpenAI API

Проверяет работу нового универсального адаптера и его совместимость
с API ключами OpenAI, Anthropic и XAI без необходимости установки
официальных библиотек.
"""

import asyncio
import json
import os
import sys
from pathlib import Path

# Добавляем путь к бэкенду для импорта модулей
sys.path.append(str(Path(__file__).parent.parent))

# Загрузка переменных окружения
try:
    from dotenv import load_dotenv

    load_dotenv(Path(__file__).parent.parent / ".env")
    print("Переменные окружения загружены из .env файла")
except ImportError:
    print("dotenv не установлен, используем текущие переменные окружения")

# Цветной вывод
GREEN = "\033[92m"
RED = "\033[91m"
YELLOW = "\033[93m"
BLUE = "\033[94m"
RESET = "\033[0m"


def print_success(message):
    print(f"{GREEN}✓ {message}{RESET}")


def print_error(message):
    print(f"{RED}✗ {message}{RESET}")


def print_info(message):
    print(f"{BLUE}ℹ {message}{RESET}")


def print_warning(message):
    print(f"{YELLOW}⚠ {message}{RESET}")


def mask_key(api_key):
    """Маскирует API ключ для безопасного отображения"""
    if not api_key or len(api_key) < 8:
        return "не найден или некорректен"
    return f"{api_key[:5]}...{api_key[-4:]}"


async def main():
    print("\n===== Тестирование универсального OpenAI адаптера =====\n")

    # 1. Проверка API ключей
    print("--- Проверка API ключей ---")

    openai_key = os.getenv("OPENAI_API_KEY", "")
    anthropic_key = os.getenv("ANTHROPIC_API_KEY", "")
    xai_key = os.getenv("XAI_API_KEY", "")

    print_info(f"OpenAI API ключ: {mask_key(openai_key)}")
    print_info(f"Anthropic API ключ: {mask_key(anthropic_key)}")
    print_info(f"XAI API ключ: {mask_key(xai_key)}")

    # 2. Импортируем и тестируем универсальный адаптер
    print("\n--- Тестирование openai_wrapper ---")

    try:
        from backend.ml.openai_wrapper import (
            LLMRequest,
            initialize_llm_client,
            llm_client,
        )

        print_success("Модуль openai_wrapper успешно импортирован")

        # Попытка инициализации
        print("\nИнициализация клиента...")
        success = await initialize_llm_client()

        if success:
            print_success("Клиент инициализирован с реальным API")
        else:
            print_warning("Клиент инициализирован в мок-режиме")

        # Тестовый запрос
        print("\nОтправка тестового запроса...")

        request = LLMRequest(
            model="gpt-4",
            messages=[
                {
                    "role": "system",
                    "content": "Ты - аналитический AI для обработки данных.",
                },
                {
                    "role": "user",
                    "content": "Напиши краткое объяснение для аномального поведения: много подключений с одного IP.",
                },
            ],
            max_tokens=300,
            temperature=0.2,
        )

        response = await llm_client.call(request)

        print_success("Запрос выполнен успешно!")
        print_info(f"Модель: {response.model}")
        print_info(f"Режим мока: {response.is_mock}")
        print_info(f"Кэширован: {response.cached}")

        try:
            content = json.loads(response.content)
            print_success("Получен структурированный JSON ответ:")
            print(json.dumps(content, indent=2, ensure_ascii=False))
        except json.JSONDecodeError:
            print_warning("Ответ не является валидным JSON:")
            print(
                response.content[:200] + "..." if len(response.content) > 200 else response.content
            )

        # Проверка кэширования
        print("\n--- Проверка кэширования ---")
        cache_response = await llm_client.call(request)

        if cache_response.cached:
            print_success("Кэширование работает корректно!")
        else:
            print_warning("Кэширование не сработало")

    except ImportError as e:
        print_error(f"Ошибка импорта модуля: {e}")
    except Exception as e:
        print_error(f"Неожиданная ошибка: {e}")
        import traceback

        print(traceback.format_exc())


if __name__ == "__main__":
    asyncio.run(main())
