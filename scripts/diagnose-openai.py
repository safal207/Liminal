#!/usr/bin/env python
"""
Диагностический скрипт для проверки OpenAI API ключа и доступа к API
"""

import json
import os
import sys
import traceback
from pathlib import Path

print("\n====================================")
print("   OPENAI API KEY DIAGNOSTICS")
print("====================================\n")

# Добавляем путь проекта
sys.path.append(str(Path(__file__).parent.parent))
print(f"Added {Path(__file__).parent.parent} to sys.path")

# Настраиваем формат вывода для лучшей читабельности
os.environ["PYTHONIOENCODING"] = "utf-8"

# Проверяем .env файл
try:
    from dotenv import load_dotenv

    env_path = Path(__file__).parent.parent / ".env"
    if env_path.exists():
        load_dotenv(env_path)
        print(f"✅ Loaded .env file from {env_path}")
    else:
        print(f"⚠️ .env file not found at {env_path}")
except ImportError:
    print("⚠️ dotenv module not found, skipping .env loading")

# Проверяем API ключ из переменных окружения
api_key = os.getenv("OPENAI_API_KEY", "")
if api_key:
    key_length = len(api_key)
    # Показываем первые и последние 4 символа
    masked_key = (
        f"{api_key[:4]}...{api_key[-4:]}" if key_length > 8 else "[слишком короткий]"
    )
    print(f"\nAPI Key details:")
    print(f"✓ Length: {key_length} символов")
    print(f"✓ Format: {masked_key}")

    # Проверяем наличие пробелов или переносов строк
    if api_key.strip() != api_key:
        print("⚠️ Warning: API key contains whitespace at beginning/end")

    if "\n" in api_key or "\r" in api_key:
        print("⚠️ Warning: API key contains line breaks")

    # Проверяем префикс ключа
    valid_prefixes = ["sk-", "org-"]
    has_valid_prefix = any(api_key.startswith(prefix) for prefix in valid_prefixes)
    if not has_valid_prefix:
        print(
            f"⚠️ Warning: API key does not start with a valid prefix (should be one of: {', '.join(valid_prefixes)})"
        )
        print(f"  Key starts with: {api_key[:3]}...")
else:
    print("❌ Error: OPENAI_API_KEY environment variable is not set")
    print("  Please set it in your .env file or environment variables")

print("\nTrying to import OpenAI module...")
try:
    import openai
    from openai import AsyncOpenAI

    print("✅ OpenAI module imported successfully")
    print(f"  Version: {openai.__version__}")

    # Проверяем создание клиента
    try:
        client = AsyncOpenAI(api_key=api_key.strip() if api_key else "invalid-key")
        print("✅ OpenAI client created (not validated)")

        # Создаем простой асинхронный код для тестирования подключения
        print("\nCreating test async function...")
        import asyncio

        async def test_api():
            try:
                print("Testing API connection...")
                response = await client.models.list()
                print("✅ API connection successful!")
                print(f"✓ Available models: {len(response.data)} models found")
                # Показываем первые 3 модели
                for i, model in enumerate(response.data[:3]):
                    print(f"  - {model.id}")
                if len(response.data) > 3:
                    print(f"  - ... and {len(response.data) - 3} more")
                return True
            except Exception as e:
                print(f"❌ API connection failed: {e}")
                traceback.print_exc()
                return False

        # Запускаем тест подключения
        print("\nAttempting to connect to OpenAI API...")
        loop = asyncio.get_event_loop()
        success = loop.run_until_complete(test_api())

        # Вывод результатов
        print("\n====================================")
        print("            DIAGNOSIS")
        print("====================================")
        if success:
            print("✅ OpenAI API connection is WORKING!")
            print("  You can use the API for natural language explanations")
        else:
            print("❌ OpenAI API connection FAILED!")
            print("  Please check the error message above and fix your API key")
            print("  Common issues:")
            print("  1. Invalid or expired API key")
            print("  2. Network connection problems")
            print("  3. Incorrect key format (should start with 'sk-')")

    except Exception as e:
        print(f"❌ Error creating OpenAI client: {e}")
        traceback.print_exc()

except ImportError as e:
    print(f"❌ Error importing OpenAI module: {e}")
    print("  Please install it using: pip install openai")

print("\n🏁 Diagnostics completed")
