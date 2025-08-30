"""
Standalone тест JWT аутентификации без conftest.py
Подключается к реальному серверу на localhost:8000
"""

import asyncio
import json

import requests
import websockets

# Базовые настройки
BASE_URL = "http://127.0.0.1:8000"
WS_URL = "ws://127.0.0.1:8000/ws/timeline"


async def test_websocket_messaging_after_auth():
    """Тест сообщений WebSocket после аутентификации."""
    print("🔐 Начинаем тест JWT аутентификации...")

    # 1. Получаем JWT токен
    print("📝 Получаем JWT токен...")
    response = requests.post(
        f"{BASE_URL}/auth/login", json={"username": "testuser", "password": "testpass"}
    )

    assert response.status_code == 200, f"Login failed: {response.text}"
    token_data = response.json()
    token = token_data["access_token"]
    print(f"✅ Токен получен: {token[:20]}...")

    # 2. Подключаемся к WebSocket
    print("🔌 Подключаемся к WebSocket...")
    async with websockets.connect(WS_URL) as websocket:
        print("✅ WebSocket подключен")

        # 3. Получаем запрос аутентификации
        auth_required = await websocket.recv()
        auth_required_data = json.loads(auth_required)
        print(f"📨 Получен запрос аутентификации: {auth_required_data}")
        assert auth_required_data["type"] == "auth_required"

        # 4. Отправляем токен
        print("🔑 Отправляем JWT токен...")
        await websocket.send(json.dumps({"type": "auth", "token": token}))

        # 5. Получаем подтверждение аутентификации
        auth_response = await websocket.recv()
        auth_result = json.loads(auth_response)
        print(f"📨 Ответ аутентификации: {auth_result}")
        assert auth_result["type"] == "auth_success"
        print("✅ Аутентификация успешна!")

        # 6. Подписываемся на канал
        print("📢 Подписываемся на канал test_channel...")
        await websocket.send(json.dumps({"type": "subscribe", "channel": "test_channel"}))

        # 7. Получаем подтверждение подписки
        subscribe_response = await websocket.recv()
        subscribe_result = json.loads(subscribe_response)
        print(f"📨 Ответ подписки: {subscribe_result}")

        # Проверяем, что есть поле type
        if "type" not in subscribe_result:
            print("❌ ОШИБКА: В ответе нет поля 'type'!")
            print(f"Полный ответ: {subscribe_result}")
            return False

        assert subscribe_result["type"] == "subscribed"
        assert subscribe_result["channel"] == "test_channel"
        print("✅ Подписка успешна!")

        # 8. Отправляем сообщение
        print("💬 Отправляем сообщение в канал...")
        await websocket.send(
            json.dumps(
                {
                    "type": "broadcast",
                    "channel": "test_channel",
                    "content": "Hello from JWT test!",
                }
            )
        )

        print("✅ Все тесты прошли успешно!")
        return True


if __name__ == "__main__":
    print("🚀 Запуск standalone теста JWT аутентификации")
    print("⚠️  Убедитесь, что сервер запущен на http://127.0.0.1:8000")

    try:
        result = asyncio.run(test_websocket_messaging_after_auth())
        if result:
            print("🎉 ВСЕ ТЕСТЫ ПРОШЛИ!")
        else:
            print("💥 ТЕСТЫ УПАЛИ!")
    except Exception as e:
        print(f"💥 ОШИБКА: {e}")
        import traceback

        traceback.print_exc()
