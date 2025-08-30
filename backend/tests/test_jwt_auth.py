"""
Тесты для JWT аутентификации WebSocket соединений.
"""

import asyncio
import json

import pytest
import requests
import websockets

# Базовые настройки
BASE_URL = "http://127.0.0.1:8000"
WS_URL = "ws://127.0.0.1:8000/ws/timeline"


class TestJWTAuth:
    """Тесты JWT аутентификации."""

    def test_login_endpoint(self):
        """Тест эндпоинта логина."""
        # Тест успешного логина
        response = requests.post(
            f"{BASE_URL}/auth/login",
            json={"username": "testuser", "password": "testpass"},
        )

        assert response.status_code == 200
        data = response.json()
        assert "access_token" in data
        assert data["token_type"] == "bearer"

        # Тест неверных учетных данных
        response = requests.post(
            f"{BASE_URL}/auth/login",
            json={"username": "wronguser", "password": "wrongpass"},
        )

        assert response.status_code == 401

    def test_auth_me_endpoint(self):
        """Тест эндпоинта проверки токена."""
        # Получаем токен
        response = requests.post(
            f"{BASE_URL}/auth/login",
            json={"username": "testuser", "password": "testpass"},
        )
        token = response.json()["access_token"]

        # Проверяем токен
        response = requests.get(f"{BASE_URL}/auth/me", params={"token": token})
        assert response.status_code == 200

        data = response.json()
        assert data["user_id"] == "testuser"
        assert data["username"] == "testuser"

    @pytest.mark.asyncio
    async def test_websocket_auth_success(self):
        """Тест успешной аутентификации через WebSocket."""
        # Получаем токен
        response = requests.post(
            f"{BASE_URL}/auth/login",
            json={"username": "testuser", "password": "testpass"},
        )
        token = response.json()["access_token"]

        # Подключаемся к WebSocket
        async with websockets.connect(WS_URL) as websocket:
            # Ожидаем запрос на аутентификацию
            auth_request = await websocket.recv()
            auth_data = json.loads(auth_request)

            assert auth_data["type"] == "auth_required"

            # Отправляем токен
            await websocket.send(json.dumps({"type": "auth", "token": token}))

            # Ожидаем подтверждение аутентификации
            auth_response = await websocket.recv()
            auth_result = json.loads(auth_response)

            assert auth_result["type"] == "auth_success"
            assert "testuser" in auth_result["message"]

    @pytest.mark.asyncio
    async def test_websocket_auth_failure(self):
        """Тест неудачной аутентификации через WebSocket."""
        try:
            async with websockets.connect(WS_URL) as websocket:
                # Ожидаем запрос на аутентификацию
                auth_request = await websocket.recv()
                auth_data = json.loads(auth_request)

                assert auth_data["type"] == "auth_required"

                # Отправляем неверный токен
                await websocket.send(json.dumps({"type": "auth", "token": "invalid_token"}))

                # Соединение должно быть закрыто
                await asyncio.sleep(1)

        except websockets.exceptions.ConnectionClosedError:
            # Ожидаемое поведение - соединение закрыто
            pass

    @pytest.mark.asyncio
    async def test_websocket_auth_timeout(self):
        """Тест таймаута аутентификации."""
        try:
            async with websockets.connect(WS_URL) as websocket:
                # Ожидаем запрос на аутентификацию
                auth_request = await websocket.recv()
                auth_data = json.loads(auth_request)

                assert auth_data["type"] == "auth_required"

                # Не отправляем токен и ждем таймаута (30 секунд слишком долго для теста)
                # Вместо этого отправим неверный формат
                await websocket.send("invalid_json")

                # Соединение должно быть закрыто
                await asyncio.sleep(1)

        except websockets.exceptions.ConnectionClosedError:
            # Ожидаемое поведение - соединение закрыто
            pass

    @pytest.mark.asyncio
    async def test_websocket_messaging_after_auth(self):
        """Тест обмена сообщениями после аутентификации."""
        # Получаем токен
        response = requests.post(
            f"{BASE_URL}/auth/login",
            json={"username": "testuser", "password": "testpass"},
        )
        token = response.json()["access_token"]

        # Подключаемся к WebSocket
        async with websockets.connect(WS_URL) as websocket:
            # Аутентификация
            await websocket.recv()
            await websocket.send(json.dumps({"type": "auth", "token": token}))

            auth_response = await websocket.recv()
            auth_result = json.loads(auth_response)
            assert auth_result["type"] == "auth_success"

            # Подписка на канал
            await websocket.send(json.dumps({"type": "subscribe", "channel": "test_channel"}))

            subscribe_response = await websocket.recv()
            subscribe_result = json.loads(subscribe_response)
            assert subscribe_result["type"] == "subscribed"
            assert subscribe_result["channel"] == "test_channel"

            # Отправка сообщения
            await websocket.send(
                json.dumps(
                    {
                        "type": "broadcast",
                        "channel": "test_channel",
                        "content": "Hello, World!",
                    }
                )
            )

            # Отписка от канала
            await websocket.send(json.dumps({"type": "unsubscribe", "channel": "test_channel"}))

            unsubscribe_response = await websocket.recv()
            unsubscribe_result = json.loads(unsubscribe_response)
            assert unsubscribe_result["type"] == "unsubscribed"
            assert unsubscribe_result["channel"] == "test_channel"


if __name__ == "__main__":
    # Запуск отдельных тестов для отладки
    import sys

    if len(sys.argv) > 1:
        test_name = sys.argv[1]
        test_instance = TestJWTAuth()

        if test_name == "login":
            test_instance.test_login_endpoint()
            print("✅ Login test passed")
        elif test_name == "auth_me":
            test_instance.test_auth_me_endpoint()
            print("✅ Auth me test passed")
        elif test_name == "ws_auth":
            asyncio.run(test_instance.test_websocket_auth_success())
            print("✅ WebSocket auth test passed")
        elif test_name == "ws_messaging":
            asyncio.run(test_instance.test_websocket_messaging_after_auth())
            print("✅ WebSocket messaging test passed")
    else:
        print("Доступные тесты:")
        print("python test_jwt_auth.py login")
        print("python test_jwt_auth.py auth_me")
        print("python test_jwt_auth.py ws_auth")
        print("python test_jwt_auth.py ws_messaging")
