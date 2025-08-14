#!/usr/bin/env python
"""
Тестирование универсального OpenAI API адаптера с сохранением логов

Этот скрипт тестирует работу универсального адаптера,
поддерживающего работу с OpenAI, Anthropic и XAI API
как в режиме реального API, так и в режиме мока,
с подробным логированием в файл для диагностики.
"""
import asyncio
import json
import os
import sys
import time
import traceback
from datetime import datetime
from pathlib import Path

# Добавляем путь к бэкенду для импорта модулей
sys.path.append(str(Path(__file__).parent.parent))

# Создаем папку для логов, если она не существует
log_dir = Path(__file__).parent.parent / "logs"
log_dir.mkdir(exist_ok=True)

# Настройка логирования в файл
log_file = log_dir / f"api_test_{datetime.now().strftime('%Y%m%d_%H%M%S')}.log"
print(f"Логи будут сохранены в файл: {log_file}")


def log(message, console=True):
    """Логирует сообщение в файл и опционально в консоль"""
    timestamp = datetime.now().strftime("%H:%M:%S")
    log_message = f"[{timestamp}] {message}"

    with open(log_file, "a", encoding="utf-8") as f:
        f.write(log_message + "\n")

    if console:
        print(message)


# Загрузка переменных окружения
try:
    from dotenv import load_dotenv

    env_file = Path(__file__).parent.parent / ".env"
    load_dotenv(env_file)
    log(f"Переменные окружения загружены из {env_file}")
except ImportError:
    log("dotenv не установлен, используем текущие переменные окружения")


def mask_key(api_key):
    """Маскирует API ключ для безопасного отображения"""
    if not api_key or len(api_key) < 8:
        return "не найден или некорректен"
    return f"{api_key[:5]}...{api_key[-4:]}"


async def test_api_adapter():
    """Тестирует универсальный адаптер OpenAI API"""
    log("\n===== Тестирование универсального адаптера OpenAI/Anthropic/XAI =====\n")

    # 1. Проверка конфигурации окружения
    log("--- Проверка конфигурации окружения ---")

    mock_only = os.getenv("OPENAI_MOCK_ONLY", "false").lower() == "true"
    fallback = os.getenv("OPENAI_FALLBACK_TO_LOCAL", "true").lower() == "true"
    cache_ttl = os.getenv("OPENAI_CACHE_TTL", "3600")
    mock_dir = os.getenv("OPENAI_MOCK_DIR", "")

    log(f"OPENAI_MOCK_ONLY: {mock_only}")
    log(f"OPENAI_FALLBACK_TO_LOCAL: {fallback}")
    log(f"OPENAI_CACHE_TTL: {cache_ttl} секунд")
    log(f"OPENAI_MOCK_DIR: {mock_dir or 'не задан, используется путь по умолчанию'}")

    # 2. Проверка API ключей
    log("\n--- Проверка API ключей ---")

    openai_key = os.getenv("OPENAI_API_KEY", "")
    anthropic_key = os.getenv("ANTHROPIC_API_KEY", "")
    xai_key = os.getenv("XAI_API_KEY", "")

    log(f"OpenAI API ключ: {mask_key(openai_key)}")
    log(f"Anthropic API ключ: {mask_key(anthropic_key)}")
    log(f"XAI API ключ: {mask_key(xai_key)}")

    # 3. Проверка установки OpenAI библиотеки
    log("\n--- Проверка OpenAI библиотеки ---")

    try:
        import openai

        log(f"✓ OpenAI библиотека установлена (версия: {openai.__version__})")
        openai_installed = True
    except ImportError:
        log("✗ OpenAI библиотека не установлена")
        openai_installed = False

    # 4. Импортируем универсальный адаптер
    log("\n--- Импорт универсального адаптера ---")

    try:
        from backend.ml.openai_wrapper import (
            FALLBACK_TO_LOCAL,
            MOCK_ONLY,
            LLMRequest,
            initialize_llm_client,
            llm_client,
        )

        log("✓ Универсальный адаптер успешно импортирован")
        log(
            f"Параметры адаптера: MOCK_ONLY={MOCK_ONLY}, FALLBACK_TO_LOCAL={FALLBACK_TO_LOCAL}"
        )

        # 5. Инициализация клиента
        log("\n--- Инициализация клиента ---")

        start_time = time.time()
        try:
            success = await initialize_llm_client()
            elapsed = time.time() - start_time

            if success:
                log(f"✓ Клиент инициализирован с реальным API за {elapsed:.2f} сек")
                log(f"API ключ OpenAI: {mask_key(llm_client.api_key)}")
            else:
                log(f"⚠ Клиент инициализирован в мок-режиме за {elapsed:.2f} сек")
                log("Причина: библиотека OpenAI не установлена или MOCK_ONLY=true")
        except Exception as e:
            log(f"✗ Ошибка инициализации клиента: {e}")
            log(traceback.format_exc())
            return

        # 6. Тестовый запрос для аномалии
        log("\n--- Тестовый запрос (обнаружение аномалий) ---")

        request = LLMRequest(
            model="gpt-4",
            messages=[
                {
                    "role": "system",
                    "content": "Ты - аналитический AI для обнаружения аномалий в системах.",
                },
                {
                    "role": "user",
                    "content": "Объясни аномалию: большое количество запросов (120 в минуту) с одного IP адреса.",
                },
            ],
            max_tokens=500,
            temperature=0.2,
        )

        log(f"Модель: {request.model}")
        log(f"Температура: {request.temperature}")
        log(f"Макс. токенов: {request.max_tokens}")

        start_time = time.time()
        try:
            response = await llm_client.call(request)
            elapsed = time.time() - start_time

            log(f"✓ Запрос выполнен за {elapsed:.2f} сек")
            log(f"Модель: {response.model}")
            log(f"Мок-режим: {response.is_mock}")
            log(f"Кэширован: {response.cached}")

            if response.usage:
                log(f"Использовано токенов: {response.usage}")

            # Парсим ответ JSON
            try:
                content = json.loads(response.content)
                log("✓ Получен структурированный JSON ответ:")
                log(json.dumps(content, indent=2, ensure_ascii=False))

                # Проверяем наличие ключевых полей
                if all(
                    k in content
                    for k in ["analysis", "recommendations", "severity", "confidence"]
                ):
                    log("✓ Ответ содержит все необходимые поля")
                else:
                    log("⚠ Ответ не содержит все необходимые поля")

            except json.JSONDecodeError:
                log("⚠ Ответ не является валидным JSON:")
                log(
                    response.content[:300] + "..."
                    if len(response.content) > 300
                    else response.content
                )

        except Exception as e:
            log(f"✗ Ошибка при выполнении запроса: {e}")
            log(traceback.format_exc())

        # 7. Проверка кэширования
        log("\n--- Проверка кэширования ---")

        start_time = time.time()
        try:
            cache_response = await llm_client.call(request)
            elapsed = time.time() - start_time

            if cache_response.cached:
                log(
                    f"✓ Кэширование работает! Повторный запрос выполнен за {elapsed:.2f} сек"
                )
            else:
                log(
                    f"⚠ Кэширование не сработало. Повторный запрос выполнен за {elapsed:.2f} сек"
                )

            # Проверяем совпадение ответов
            if cache_response.content == response.content:
                log("✓ Ответы идентичны")
            else:
                log("⚠ Ответы отличаются")

        except Exception as e:
            log(f"✗ Ошибка при проверке кэширования: {e}")
            log(traceback.format_exc())

        # 8. Тестовый запрос для анализа производительности
        log("\n--- Тестовый запрос (анализ производительности) ---")

        perf_request = LLMRequest(
            model="gpt-4",
            messages=[
                {
                    "role": "system",
                    "content": "Ты - аналитический AI для оптимизации производительности систем.",
                },
                {
                    "role": "user",
                    "content": "Проанализируй проблему производительности: высокая латентность в WebSocket соединениях.",
                },
            ],
            max_tokens=400,
        )

        start_time = time.time()
        try:
            perf_response = await llm_client.call(perf_request)
            elapsed = time.time() - start_time

            log(f"✓ Запрос выполнен за {elapsed:.2f} сек")
            log(f"Мок-режим: {perf_response.is_mock}")
            log(f"Кэширован: {perf_response.cached}")

            try:
                content = json.loads(perf_response.content)
                log("✓ Получен структурированный JSON ответ:")

                # Выводим только рекомендации
                if "recommendations" in content:
                    log("\nРекомендации по производительности:")
                    for i, rec in enumerate(content["recommendations"], 1):
                        log(f"{i}. {rec}")

            except json.JSONDecodeError:
                log("⚠ Ответ не является валидным JSON")
                log(perf_response.content[:200])

        except Exception as e:
            log(f"✗ Ошибка при выполнении запроса: {e}")

    except ImportError as e:
        log(f"✗ Ошибка импорта модуля: {e}")
        log(traceback.format_exc())
    except Exception as e:
        log(f"✗ Неожиданная ошибка: {e}")
        log(traceback.format_exc())

    log("\n===== Тестирование завершено =====")
    log(f"Полные логи сохранены в файл: {log_file}")


if __name__ == "__main__":
    asyncio.run(test_api_adapter())
