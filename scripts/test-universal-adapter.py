#!/usr/bin/env python
"""
Универсальный тест адаптера для OpenAI, Anthropic и XAI интеграции

Этот скрипт проверяет работоспособность нового универсального адаптера
для API интеграций с OpenAI, Anthropic и XAI, поддерживая как реальную
работу с API, так и мок-режим для локальной разработки.
"""
import asyncio
import json
import os
import sys
import time
import traceback
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, Optional

# Добавляем путь к бэкенду для импорта модулей
sys.path.append(str(Path(__file__).parent.parent))

# Логирование
try:
    from loguru import logger

    has_loguru = True
except ImportError:
    has_loguru = False
    import logging

    logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")
    logger = logging.getLogger(__name__)

# Загрузка переменных окружения
try:
    from dotenv import load_dotenv

    load_dotenv(Path(__file__).parent.parent / ".env")
    logger.info("Переменные окружения загружены из .env файла")
except ImportError:
    logger.warning("dotenv не установлен, используем текущие переменные окружения")

# Цветной вывод
GREEN = "\033[92m"
RED = "\033[91m"
YELLOW = "\033[93m"
BLUE = "\033[94m"
MAGENTA = "\033[95m"
CYAN = "\033[96m"
RESET = "\033[0m"

# Конфигурация
MOCK_ONLY = os.getenv("OPENAI_MOCK_ONLY", "false").lower() == "true"
VERBOSE = os.getenv("TEST_VERBOSE", "true").lower() == "true"
TEST_TIMEOUT = int(os.getenv("TEST_TIMEOUT", "30"))  # Таймаут тестов в секундах


# Функции для форматированного вывода
def print_header(title: str) -> None:
    """Печатает заголовок секции"""
    print(f"\n{MAGENTA}{'=' * 50}{RESET}")
    print(f"{MAGENTA}  {title}{RESET}")
    print(f"{MAGENTA}{'=' * 50}{RESET}\n")


def print_section(title: str) -> None:
    """Печатает подзаголовок секции"""
    print(f"\n{CYAN}--- {title} ---{RESET}\n")


def print_success(message: str) -> None:
    """Печатает сообщение об успехе"""
    print(f"{GREEN}✓ {message}{RESET}")


def print_error(message: str) -> None:
    """Печатает сообщение об ошибке"""
    print(f"{RED}✗ {message}{RESET}")


def print_warning(message: str) -> None:
    """Печатает предупреждение"""
    print(f"{YELLOW}⚠ {message}{RESET}")


def print_info(message: str) -> None:
    """Печатает информационное сообщение"""
    print(f"{BLUE}ℹ {message}{RESET}")


def print_json(data: Dict[str, Any]) -> None:
    """Печатает данные в формате JSON с красивым форматированием"""
    print(f"{CYAN}{json.dumps(data, indent=2, ensure_ascii=False)}{RESET}")


def mask_key(api_key: str) -> str:
    """Маскирует API ключ для безопасного отображения"""
    if not api_key:
        return "не найден"
    return f"{api_key[:5]}...{api_key[-5:] if len(api_key) > 10 else ''}"


async def run_openai_wrapper_test():
    """Проверяет работу универсального адаптера OpenAI"""
    print_header("Тестирование универсального OpenAI адаптера")

    try:
        # Импортируем универсальный адаптер
        from backend.ml.openai_wrapper import (LLMRequest,
                                               initialize_llm_client,
                                               llm_client)

        print_success("Модуль openai_wrapper успешно импортирован")

        # Проверяем конфигурацию
        print_section("Конфигурация адаптера")
        print_info(f"MOCK_ONLY режим: {MOCK_ONLY}")

        # Инициализируем клиент
        print_section("Инициализация клиента")

        start_time = time.time()
        success = await initialize_llm_client()
        elapsed = time.time() - start_time

        if success:
            print_success(
                f"Клиент инициализирован в режиме реального API за {elapsed:.2f} сек"
            )
        else:
            print_warning(f"Клиент инициализирован в режиме мока за {elapsed:.2f} сек")

        # Проверяем наличие API ключей
        print_section("Проверка API ключей")

        openai_key = os.getenv("OPENAI_API_KEY", "")
        anthropic_key = os.getenv("ANTHROPIC_API_KEY", "")
        xai_key = os.getenv("XAI_API_KEY", "")

        print_info(f"OpenAI API ключ: {mask_key(openai_key)}")
        print_info(f"Anthropic API ключ: {mask_key(anthropic_key)}")
        print_info(f"XAI API ключ: {mask_key(xai_key)}")

        # Выполняем тестовый запрос для аномалии
        print_section("Тест запроса - обнаружение аномалий")
        request = LLMRequest(
            model="gpt-4",
            messages=[
                {
                    "role": "system",
                    "content": "Ты - аналитический AI для обнаружения аномалий в ML системах.",
                },
                {
                    "role": "user",
                    "content": "Проанализируй следующую аномалию: необычно высокое количество подключений с одного IP адреса (120 в минуту).",
                },
            ],
            max_tokens=1000,
            temperature=0.2,
        )

        try:
            start_time = time.time()
            response = await llm_client.call(request)
            elapsed = time.time() - start_time

            print_success(f"Запрос выполнен за {elapsed:.2f} сек")
            print_info(f"Модель: {response.model}")
            print_info(f"Мок-режим: {response.is_mock}")
            print_info(f"Кэширован: {response.cached}")

            if response.usage:
                print_info(f"Токены: {response.usage.get('total_tokens', 0)}")

            # Анализируем ответ
            try:
                content_json = json.loads(response.content)
                print_success("Получен структурированный JSON-ответ")

                # Выводим структуру ответа
                print_section("Структура ответа")
                print_json(
                    {
                        k: v
                        for k, v in content_json.items()
                        if k not in ["technical_details"]
                    }
                )

                if "technical_details" in content_json and VERBOSE:
                    print_section("Технические детали")
                    print_json(content_json["technical_details"])

            except json.JSONDecodeError:
                print_error("Ответ не является корректным JSON")
                print_info(f"Первые 100 символов ответа: {response.content[:100]}...")

        except Exception as e:
            print_error(f"Ошибка при выполнении запроса: {e}")
            if VERBOSE:
                print_error(traceback.format_exc())

        # Проверяем кэширование
        print_section("Проверка кэширования")

        try:
            start_time = time.time()
            cached_response = await llm_client.call(request)
            elapsed = time.time() - start_time

            if cached_response.cached:
                print_success(
                    f"Кэширование работает! Второй запрос выполнен за {elapsed:.2f} сек"
                )
            else:
                print_warning(
                    f"Кэширование не сработало. Второй запрос выполнен за {elapsed:.2f} сек"
                )

        except Exception as e:
            print_error(f"Ошибка при проверке кэширования: {e}")

        # Выполняем другой тип запроса
        print_section("Тест запроса - анализ производительности")

        performance_request = LLMRequest(
            model="gpt-4",
            messages=[
                {
                    "role": "system",
                    "content": "Ты - аналитический AI для оптимизации производительности систем ML.",
                },
                {
                    "role": "user",
                    "content": "Проанализируй проблему производительности: высокая латентность (500ms) в обработке WebSocket сообщений, рост потребления памяти.",
                },
            ],
        )

        try:
            start_time = time.time()
            perf_response = await llm_client.call(performance_request)
            elapsed = time.time() - start_time

            print_success(f"Запрос выполнен за {elapsed:.2f} сек")
            print_info(f"Модель: {perf_response.model}")
            print_info(f"Мок-режим: {perf_response.is_mock}")

            # Анализируем ответ
            try:
                content_json = json.loads(perf_response.content)
                print_success("Получен структурированный JSON-ответ")

                # Выводим рекомендации из ответа
                if "recommendations" in content_json:
                    print_section("Рекомендации по производительности")
                    for i, rec in enumerate(content_json["recommendations"], 1):
                        print_info(f"{i}. {rec}")

            except json.JSONDecodeError:
                print_error("Ответ не является корректным JSON")

        except Exception as e:
            print_error(f"Ошибка при выполнении запроса: {e}")

    except ImportError as e:
        print_error(f"Не удалось импортировать модуль openai_wrapper: {e}")
        if VERBOSE:
            print_error(traceback.format_exc())
    except Exception as e:
        print_error(f"Неожиданная ошибка: {e}")
        if VERBOSE:
            print_error(traceback.format_exc())


async def test_openai_service_integration():
    """Проверяет интеграцию с OpenAI Service"""
    print_header("Тестирование интеграции с OpenAI Service")

    try:
        # Импортируем OpenAI сервис
        from backend.ml.openai_service import (AnalysisRequest,
                                               AnalysisResponse,
                                               openai_service)

        print_success("Модуль openai_service успешно импортирован")

        # Инициализируем клиент
        print_section("Инициализация OpenAI сервиса")

        start_time = time.time()
        await openai_service.initialize()
        elapsed = time.time() - start_time

        if openai_service.client:
            print_success(f"Сервис инициализирован за {elapsed:.2f} сек")
        else:
            print_error("Сервис не инициализирован")
            return

        # Создаем тестовый запрос на анализ
        print_section("Тест analyze_anomaly")

        anomaly_data = {
            "timestamp": datetime.now().isoformat(),
            "user_id": "test123",
            "metric": "connections_per_minute",
            "value": 120,
            "threshold": 50,
            "deviation": 2.4,
            "source_ip": "192.168.1.100",
        }

        ml_explanation = {
            "shap_values": {"connections_per_minute": 0.8, "error_rate": 0.15},
            "feature_importance": [
                ("connections_per_minute", 0.8),
                ("error_rate", 0.15),
            ],
            "prediction_confidence": 0.92,
        }

        try:
            start_time = time.time()
            result = await openai_service.analyze_anomaly(
                anomaly_data=anomaly_data, ml_explanation=ml_explanation
            )
            elapsed = time.time() - start_time

            print_success(f"Анализ выполнен за {elapsed:.2f} сек")
            print_info(f"Степень серьезности: {result.severity}")
            print_info(f"Уверенность: {result.confidence}")

            print_section("Краткое описание")
            print_info(result.summary)

            print_section("Рекомендации")
            for i, rec in enumerate(result.recommendations, 1):
                print_info(f"{i}. {rec}")

            if VERBOSE:
                print_section("Полный анализ")
                print_info(
                    result.analysis[:500] + "..."
                    if len(result.analysis) > 500
                    else result.analysis
                )

        except Exception as e:
            print_error(f"Ошибка при анализе аномалии: {e}")
            if VERBOSE:
                print_error(traceback.format_exc())

        # Проверка работоспособности
        print_section("Проверка работоспособности")

        try:
            health = await openai_service.health_check()
            print_info(f"Статус: {health.get('status', 'unknown')}")

            if health.get("status") == "healthy":
                print_success("Сервис работает корректно")
                if VERBOSE:
                    print_json(health)
            else:
                print_warning(
                    f"Сервис не полностью работоспособен: {health.get('reason', 'неизвестная причина')}"
                )

        except Exception as e:
            print_error(f"Ошибка при проверке работоспособности: {e}")

    except ImportError as e:
        print_error(f"Не удалось импортировать модуль openai_service: {e}")
    except Exception as e:
        print_error(f"Неожиданная ошибка: {e}")
        if VERBOSE:
            print_error(traceback.format_exc())


async def run_all_tests():
    """Запускает все тесты с таймаутом"""
    print_header(f"Запуск всех тестов (таймаут: {TEST_TIMEOUT} сек)")

    start_time = time.time()

    try:
        # Запускаем тесты с таймаутом
        await asyncio.wait_for(run_openai_wrapper_test(), TEST_TIMEOUT)
        await asyncio.wait_for(test_openai_service_integration(), TEST_TIMEOUT)

        total_time = time.time() - start_time
        print_header(f"Все тесты завершены за {total_time:.2f} сек")

    except asyncio.TimeoutError:
        print_error(f"Тесты превысили таймаут {TEST_TIMEOUT} сек")
    except Exception as e:
        print_error(f"Неожиданная ошибка в тестах: {e}")
        if VERBOSE:
            print_error(traceback.format_exc())


if __name__ == "__main__":
    print_header(
        f"Universal OpenAI/Anthropic/XAI Adapter Test - {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}"
    )

    # Проверяем наличие аргументов командной строки
    if len(sys.argv) > 1:
        if "--help" in sys.argv or "-h" in sys.argv:
            print("Использование: python test-universal-adapter.py [опции]")
            print("Опции:")
            print("  --verbose, -v   Подробный вывод")
            print("  --mock-only, -m Использовать только мок-режим")
            print("  --timeout=N, -t N  Установить таймаут тестов (сек)")
            sys.exit(0)

        if "--verbose" in sys.argv or "-v" in sys.argv:
            VERBOSE = True
            print_info("Включен подробный вывод")

        if "--mock-only" in sys.argv or "-m" in sys.argv:
            os.environ["OPENAI_MOCK_ONLY"] = "true"
            MOCK_ONLY = True
            print_info("Включен режим MOCK_ONLY")

        for arg in sys.argv:
            if arg.startswith("--timeout="):
                try:
                    TEST_TIMEOUT = int(arg.split("=")[1])
                    print_info(f"Установлен таймаут: {TEST_TIMEOUT} сек")
                except (ValueError, IndexError):
                    pass
            elif arg == "-t" and sys.argv.index(arg) < len(sys.argv) - 1:
                try:
                    TEST_TIMEOUT = int(sys.argv[sys.argv.index(arg) + 1])
                    print_info(f"Установлен таймаут: {TEST_TIMEOUT} сек")
                except ValueError:
                    pass

    # Запускаем все тесты
    asyncio.run(run_all_tests())
