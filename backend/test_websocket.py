import asyncio
import json
import time

import websockets


async def test_websocket():
    print("🔌 Подключаемся к WebSocket серверу...")
    try:
        async with websockets.connect(
            "ws://localhost:8000/ws/test_user_1", ping_interval=None
        ) as websocket:
            print("✅ Успешно подключились к WebSocket серверу")

            # Тест подписки
            subscribe_msg = {
                "type": "subscribe",
                "user_id": "test_user_1",
                "channel": "test_channel",
            }
            await websocket.send(json.dumps(subscribe_msg))
            print(f"📨 Отправили запрос: {subscribe_msg}")

            # Получаем ответ
            try:
                response = await asyncio.wait_for(websocket.recv(), timeout=5.0)
                print(f"📩 Получен ответ: {response}")

                # Отправляем тестовое сообщение
                test_msg = {
                    "type": "message",
                    "user_id": "test_user_1",
                    "channel": "test_channel",
                    "message": "Тест",
                    "timestamp": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
                }
                await websocket.send(json.dumps(test_msg))
                print("📨 Отправили тестовое сообщение")

                # Ждем ответ
                response = await asyncio.wait_for(websocket.recv(), timeout=5.0)
                print(f"📩 Получен ответ: {response}")

            except TimeoutError:
                print("⏱️ Таймаут ожидания ответа")
            except Exception as e:
                print(f"❌ Ошибка при получении: {str(e)}")

            input("Нажмите Enter для выхода...")

    except Exception as e:
        print(f"❌ Ошибка подключения: {str(e)}")


if __name__ == "__main__":
    asyncio.get_event_loop().run_until_complete(test_websocket())
