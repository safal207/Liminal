#!/usr/bin/env python
"""
Простой тест API ключей OpenAI, Anthropic и XAI
"""

import asyncio
import json
import os
import sys
import traceback
from pathlib import Path

# Настройка путей для импорта
BASE_DIR = Path(__file__).parent.parent
sys.path.append(str(BASE_DIR))

# Импорт dotenv для загрузки .env файла
try:
    from dotenv import load_dotenv

    load_dotenv(BASE_DIR / ".env")
    print("🟢 .env файл успешно загружен")
except ImportError:
    print(
        "🟠 WARNING: dotenv не установлен, используются только системные переменные окружения"
    )

# Проверка API ключей
api_keys = {
    "OPENAI_API_KEY": os.getenv("OPENAI_API_KEY"),
    "ANTHROPIC_API_KEY": os.getenv("ANTHROPIC_API_KEY"),
    "XAI_API_KEY": os.getenv("XAI_API_KEY"),
}

print("\n=== ПРОВЕРКА API КЛЮЧЕЙ ===\n")
for key_name, key_value in api_keys.items():
    if key_value:
        masked_key = (
            f"{key_value[:5]}...{key_value[-5:]}" if len(key_value) > 10 else "[скрыт]"
        )
        print(f"🟢 {key_name} найден: {masked_key}")
    else:
        print(f"🔴 {key_name} не найден")


# Функция для тестирования OpenAI API
async def test_openai_api():
    print("\n=== ТЕСТИРОВАНИЕ OPENAI API ===\n")

    try:
        api_key = os.getenv("OPENAI_API_KEY")

        if not api_key:
            print("🔴 OpenAI API ключ не найден")
            return False

        try:
            print("🟠 Импорт библиотеки OpenAI...")
            import openai
            from openai import AsyncOpenAI

            print(
                f"🟢 Библиотека OpenAI успешно импортирована (версия: {openai.__version__})"
            )
        except ImportError as e:
            print(f"🔴 Ошибка импорта библиотеки OpenAI: {e}")
            print("🟠 Попытка установки библиотеки OpenAI...")

            try:
                import subprocess

                result = subprocess.run(
                    [sys.executable, "-m", "pip", "install", "openai"],
                    capture_output=True,
                    text=True,
                    check=True,
                )
                print(f"🟢 Установка OpenAI выполнена успешно")

                # Повторная попытка импорта
                import openai
                from openai import AsyncOpenAI

                print(
                    f"🟢 Библиотека OpenAI успешно импортирована после установки (версия: {openai.__version__})"
                )
            except Exception as install_error:
                print(f"🔴 Ошибка установки OpenAI: {install_error}")
                print(f"🟠 Будет использована мок-реализация")
                return False

        # Инициализация клиента OpenAI
        print(f"🟠 Инициализация клиента OpenAI с ключом {api_key[:5]}...")
        client = AsyncOpenAI(api_key=api_key)

        # Тестирование API с простым запросом
        print("🟠 Отправка тестового запроса к OpenAI API...")
        response = await client.chat.completions.create(
            model="gpt-4-turbo-preview",
            messages=[
                {
                    "role": "user",
                    "content": "Say hello to Resonance Liminal ML backend!",
                }
            ],
            max_tokens=10,
        )

        response_text = response.choices[0].message.content.strip()
        print(f'🟢 Успешный ответ от OpenAI API: "{response_text}"')
        return True

    except Exception as e:
        print(f"🔴 Ошибка при тестировании OpenAI API: {e}")
        traceback.print_exc()
        return False


# Основная функция
async def main():
    print("\n=== ТЕСТИРОВАНИЕ ИНТЕГРАЦИИ API КЛЮЧЕЙ ===\n")
    print("Время запуска: " + Path(__file__).name)

    # Тестируем OpenAI API
    openai_success = await test_openai_api()

    # Вывод результатов
    print("\n=== РЕЗУЛЬТАТЫ ТЕСТИРОВАНИЯ ===\n")
    print(f"OpenAI API: {'🟢 УСПЕХ' if openai_success else '🔴 ОШИБКА'}")

    if openai_success:
        print("\n🟢 OpenAI API ключ работает корректно и готов к использованию")
        print("🟢 Вы можете использовать реальные API вызовы вместо мок-реализации")
    else:
        print("\n🔴 Проблемы с OpenAI API")
        print("🟠 Рекомендуемые действия:")
        print("  1. Проверьте правильность API ключа")
        print("  2. Убедитесь, что библиотека OpenAI установлена: pip install openai")
        print("  3. Проверьте подключение к сети и настройки прокси")
        print("  4. Временно используйте мок-реализацию для локальной разработки")


# Запуск теста
if __name__ == "__main__":
    asyncio.run(main())
