#!/usr/bin/env python3
"""
Простой тест WebSocket без внешних зависимостей
Использует только стандартную библиотеку Python
"""

import asyncio
import json
import time
from concurrent.futures import ThreadPoolExecutor

import websockets


async def test_websocket_connection():
    """Тест базового подключения"""
    print("🔄 Тестируем подключение к WebSocket...")

    try:
        async with websockets.connect("ws://localhost:8000/ws/test_user") as websocket:
            print("✅ Подключение установлено")

            # Подписываемся на канал
            subscribe_msg = {
                "type": "subscribe",
                "user_id": "test_user",
                "channel": "test_channel",
            }
            await websocket.send(json.dumps(subscribe_msg))
            print("📡 Отправлена подписка на канал")

            # Ждем ответ
            response = await asyncio.wait_for(websocket.recv(), timeout=5)
            print(f"📩 Получен ответ: {response}")

            # Отправляем сообщение
            chat_msg = {
                "type": "message",
                "user_id": "test_user",
                "channel": "test_channel",
                "message": {"text": "Тестовое сообщение", "timestamp": time.time()},
            }
            await websocket.send(json.dumps(chat_msg))
            print("💬 Отправлено сообщение в чат")

            # Ждем ответ
            response = await asyncio.wait_for(websocket.recv(), timeout=5)
            print(f"📩 Получен ответ: {response}")

    except Exception as e:
        print(f"❌ Ошибка: {e}")


async def test_multiple_users():
    """Тест с несколькими пользователями"""
    print("\n🔄 Тестируем чат между пользователями...")

    async def user_session(user_id, message_to_send=None):
        try:
            async with websockets.connect(
                f"ws://localhost:8000/ws/{user_id}"
            ) as websocket:
                # Подписываемся на канал
                subscribe_msg = {
                    "type": "subscribe",
                    "user_id": user_id,
                    "channel": "general",
                }
                await websocket.send(json.dumps(subscribe_msg))

                # Ждем подтверждение подписки
                await asyncio.wait_for(websocket.recv(), timeout=5)
                print(f"✅ {user_id} подписался на канал")

                if message_to_send:
                    # Отправляем сообщение
                    chat_msg = {
                        "type": "message",
                        "user_id": user_id,
                        "channel": "general",
                        "message": {"text": message_to_send, "timestamp": time.time()},
                    }
                    await websocket.send(json.dumps(chat_msg))
                    print(f"💬 {user_id} отправил: {message_to_send}")

                # Слушаем сообщения 5 секунд
                try:
                    for _ in range(3):
                        response = await asyncio.wait_for(websocket.recv(), timeout=2)
                        data = json.loads(response)
                        if data.get("type") == "message":
                            print(f"📩 {user_id} получил: {data}")
                except asyncio.TimeoutError:
                    pass

        except Exception as e:
            print(f"❌ Ошибка для {user_id}: {e}")

    # Запускаем пользователей параллельно
    tasks = [
        user_session("alice", "Привет от Alice!"),
        user_session("bob", "Привет от Bob!"),
        user_session("charlie"),  # Только слушает
    ]

    await asyncio.gather(*tasks)


async def test_metrics():
    """Проверяем метрики"""
    print("\n🔄 Проверяем метрики...")

    import urllib.request

    try:
        with urllib.request.urlopen("http://localhost:8001/metrics") as response:
            metrics = response.read().decode()

        # Ищем WebSocket метрики
        lines = metrics.split("\n")
        ws_connections = [
            line
            for line in lines
            if "websocket_connections_total" in line and not line.startswith("#")
        ]
        ws_messages = [
            line
            for line in lines
            if "websocket_messages_total" in line and not line.startswith("#")
        ]

        print("📊 WebSocket соединения:")
        for line in ws_connections:
            print(f"  {line}")

        print("📊 WebSocket сообщения:")
        for line in ws_messages:
            print(f"  {line}")

    except Exception as e:
        print(f"❌ Ошибка получения метрик: {e}")


async def main():
    """Главная функция"""
    print("🚀 Запуск тестов WebSocket сервера")
    print("=" * 50)

    # Тест 1: Базовое подключение
    await test_websocket_connection()

    # Тест 2: Несколько пользователей
    await test_multiple_users()

    # Тест 3: Метрики
    await test_metrics()

    print("\n✅ Все тесты завершены!")


if __name__ == "__main__":
    asyncio.run(main())
