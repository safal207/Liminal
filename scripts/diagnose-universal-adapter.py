#!/usr/bin/env python
"""
Диагностический скрипт для проверки универсального OpenAI адаптера
Проверяет работу с реальным API и в режиме мока
"""
import asyncio
import json
import os
import sys
import traceback
from datetime import datetime
from pathlib import Path

# Добавляем путь проекта
sys.path.append(str(Path(__file__).parent.parent))

print("\n====================================")
print("  UNIVERSAL OPENAI ADAPTER DIAGNOSTICS")
print("====================================\n")

# Настраиваем формат вывода для лучшей читабельности
os.environ["PYTHONIOENCODING"] = "utf-8"


# Функции для форматирования вывода
def success(msg):
    print(f"[SUCCESS] {msg}")


def info(msg):
    print(f"[INFO] {msg}")


def warning(msg):
    print(f"[WARNING] {msg}")


def error(msg):
    print(f"[ERROR] {msg}")


# Загружаем .env если есть
try:
    from dotenv import load_dotenv

    env_path = Path(__file__).parent.parent / ".env"
    if env_path.exists():
        load_dotenv(env_path)
        success(f"Loaded .env file from {env_path}")
    else:
        warning(f".env file not found at {env_path}")
except ImportError:
    warning("dotenv module not found, skipping .env loading")


# Проверка переменных окружения
def mask_key(key, show_chars=4):
    """Маскирует API ключ для безопасного отображения"""
    if not key or len(key) < (show_chars * 2):
        return "[not found]"
    return f"{key[:show_chars]}...{key[-show_chars:]}"


# Проверяем API ключи
openai_key = os.environ.get("OPENAI_API_KEY", "")
anthropic_key = os.environ.get("ANTHROPIC_API_KEY", "")
xai_key = os.environ.get("XAI_API_KEY", "")

info("Checking API keys:")
info(f"- OpenAI API Key: {mask_key(openai_key)}")
info(f"- Anthropic API Key: {mask_key(anthropic_key)}")
info(f"- XAI API Key: {mask_key(xai_key)}")

# Проверка настроек мока и кэша
mock_only = os.environ.get("OPENAI_MOCK_ONLY", "").lower() in ["true", "1", "yes"]
cache_ttl = os.environ.get("OPENAI_CACHE_TTL", "3600")
mock_dir = os.environ.get("OPENAI_MOCK_DIR", "")

info("\nAdapter configuration:")
info(f"- OPENAI_MOCK_ONLY: {mock_only}")
info(f"- OPENAI_CACHE_TTL: {cache_ttl}")
info(f"- OPENAI_MOCK_DIR: {mock_dir or '[not set]'}")

# Начинаем тестирование адаптера
print("\n------------------------------------")
print("  TESTING UNIVERSAL ADAPTER")
print("------------------------------------")

# Пробуем импортировать модуль
try:
    info("Importing OpenAI wrapper module...")
    from backend.ml.openai_wrapper import (LLMRequest, initialize_llm_client,
                                           llm_client)

    success("OpenAI wrapper module imported successfully")

    # Проверяем состояние клиента до инициализации
    info("\nChecking client state before initialization:")
    if hasattr(llm_client, "api_key"):
        info(
            f"- API key: {mask_key(llm_client.api_key) if llm_client.api_key else '[not set]'}"
        )
    else:
        warning("- api_key attribute not found")

    # Атрибуты могут отсутствовать до инициализации, поэтому используем hasattr
    if hasattr(llm_client, "mock_only"):
        info(f"- Mock only: {llm_client.mock_only}")
    else:
        warning("- mock_only attribute not found")

    if hasattr(llm_client, "cache_enabled"):
        info(f"- Cache enabled: {llm_client.cache_enabled}")
    else:
        warning("- cache_enabled attribute not found")

    # Инициализируем клиент
    print("\n------------------------------------")
    print("  INITIALIZING CLIENT")
    print("------------------------------------")

    async def test_adapter():
        # Инициализация
        try:
            info("Initializing LLM client...")
            start_time = datetime.now()
            success = await initialize_llm_client()
            elapsed = (datetime.now() - start_time).total_seconds()

            if success:
                success(f"Client initialized with real API in {elapsed:.2f}s")
            else:
                warning(f"Client initialized in mock mode in {elapsed:.2f}s")

            # Проверяем состояние после инициализации
            info("\nClient state after initialization:")

            # Безопасно проверяем атрибуты
            if hasattr(llm_client, "api_key"):
                info(
                    f"- API key: {mask_key(llm_client.api_key) if llm_client.api_key else '[not set]'}"
                )
            else:
                warning("- api_key attribute not found")

            if hasattr(llm_client, "mock_only"):
                info(f"- Mock only: {llm_client.mock_only}")
            else:
                warning("- mock_only attribute not found")

            if hasattr(llm_client, "cache_enabled"):
                info(f"- Cache enabled: {llm_client.cache_enabled}")
            else:
                warning("- cache_enabled attribute not found")

            # Создаем тестовый запрос
            print("\n------------------------------------")
            print("  TESTING API CALL")
            print("------------------------------------")

            info("Creating test request...")
            request = LLMRequest(
                model="gpt-3.5-turbo",
                messages=[
                    {"role": "system", "content": "You are a helpful AI assistant."},
                    {
                        "role": "user",
                        "content": "Return a brief JSON with these keys: status, message, current_time",
                    },
                ],
                max_tokens=100,
                temperature=0.2,
            )

            # Выполняем запрос
            info("Sending request to LLM...")
            start_time = datetime.now()
            response = await llm_client.call(request)
            elapsed = (datetime.now() - start_time).total_seconds()

            # Анализируем ответ
            success(f"Request completed in {elapsed:.2f}s")
            info(f"- Model: {response.model}")
            info(f"- Mock: {response.is_mock}")
            info(f"- Cached: {response.cached}")

            if response.usage:
                info(f"- Token usage: {response.usage}")

            # Пытаемся распарсить JSON ответ
            try:
                content = json.loads(response.content)
                success("Response is valid JSON")

                # Отображаем содержимое
                print("\nResponse content:")
                print(json.dumps(content, indent=2))

            except json.JSONDecodeError:
                warning("Response is not valid JSON")
                print("\nRaw response content:")
                print(
                    response.content[:300] + "..."
                    if len(response.content) > 300
                    else response.content
                )

            # Тестируем кэширование
            print("\n------------------------------------")
            print("  TESTING CACHING")
            print("------------------------------------")

            info("Sending identical request (should use cache if enabled)...")
            start_time = datetime.now()
            cache_response = await llm_client.call(request)
            elapsed = (datetime.now() - start_time).total_seconds()

            if cache_response.cached:
                success(f"Cache working correctly! Response time: {elapsed:.2f}s")
            else:
                warning(f"Cache not used. Response time: {elapsed:.2f}s")

            # Тестируем integration с OpenAIService
            print("\n------------------------------------")
            print("  TESTING OPENAISERVICE INTEGRATION")
            print("------------------------------------")

            try:
                from backend.ml.openai_service import OpenAIService

                info("Creating OpenAIService instance...")
                service = OpenAIService()

                info("Initializing OpenAIService...")
                await service.initialize()
                success("OpenAIService initialized successfully")

                # Проверяем health check
                info("Performing health check...")
                health = await service.health_check()
                success(f"Health check result: {health}")

                # Тестируем analyze_anomaly
                info("Testing analyze_anomaly method...")
                anomaly_data = {
                    "event_type": "connection_spike",
                    "timestamp": datetime.now().isoformat(),
                    "details": {
                        "ip_address": "192.168.1.42",
                        "connection_count": 150,
                        "timeframe_minutes": 5,
                        "normal_baseline": 30,
                    },
                }

                analysis = await service.analyze_anomaly(anomaly_data)
                success("analyze_anomaly method completed successfully")

                print("\nAnalysis result:")
                if isinstance(analysis, dict):
                    print(json.dumps(analysis, indent=2))
                else:
                    print(analysis)

            except ImportError as e:
                error(f"Could not import OpenAIService: {e}")
            except Exception as e:
                error(f"Error testing OpenAIService: {e}")
                traceback.print_exc()

            # Итоговая диагностика
            print("\n====================================")
            print("          FINAL DIAGNOSIS")
            print("====================================")

            if not success and mock_only:
                success("Universal adapter is working in MOCK MODE as configured")
                info("- This is expected behavior with OPENAI_MOCK_ONLY=true")
            elif success:
                success("Universal adapter is working with REAL API")
                info("- API calls are being made to the actual OpenAI API")
                info("- Caching is working correctly")
            else:
                warning("Universal adapter is in MOCK MODE, but real API was requested")
                info("- Check your API keys and network connection")
                info("- Set OPENAI_MOCK_ONLY=true if mock mode is intended")

            if response.is_mock:
                info("\nMOCK RESPONSE DETAILS:")
                info(f"- Response generated from mock data")
                if hasattr(llm_client, "mock_dir") and llm_client.mock_dir:
                    info(f"- Mock directory: {llm_client.mock_dir}")

            success("\nDiagnostic completed successfully!")

        except Exception as e:
            error(f"Error during adapter testing: {e}")
            traceback.print_exc()

    # Запуск асинхронного тестирования
    print("\nStarting adapter tests...")
    asyncio.run(test_adapter())

except ImportError as e:
    error(f"Could not import openai_wrapper module: {e}")
    info(
        "Check that the backend/ml/openai_wrapper.py file exists and is correctly implemented"
    )

except Exception as e:
    error(f"Unexpected error: {e}")
    traceback.print_exc()

print("\nDiagnostic script finished")
