"""
Тесты для граничных случаев JWT аутентификации WebSocket соединений.
"""

import asyncio
import json
from datetime import datetime, timedelta

import pytest
import requests
import websockets
import websockets.exceptions
from jose import jwt

# Базовые настройки
BASE_URL = "http://127.0.0.1:8000"
WS_URL = "ws://127.0.0.1:8000/ws/timeline"

# Секретный ключ должен совпадать с тем, что используется в приложении
SECRET_KEY = "testsecret123"  # Для тестов используем фиксированный ключ


class TestJWTEdgeCases:
    """Тесты JWT аутентификации для граничных случаев."""

    def test_expired_token(self):
        """Тест обработки истекшего JWT токена."""
        # Создаем токен с истекшим сроком действия (истек 1 час назад)
        payload = {
            "sub": "testuser",
            "username": "testuser",
            "exp": datetime.utcnow() - timedelta(hours=1),
        }

        # Создаем токен вручную
        expired_token = jwt.encode(payload, SECRET_KEY, algorithm="HS256")

        # Проверяем через эндпоинт /auth/me
        response = requests.get(f"{BASE_URL}/auth/me", params={"token": expired_token})
        assert response.status_code == 401
        # Сервер возвращает "Недействительный токен" для истекших токенов
        assert "Недействительный токен" in response.json()["detail"]

        # Проверка через WebSocket
        @pytest.mark.asyncio
        async def test_ws():
            async with websockets.connect(WS_URL) as websocket:
                # Получаем запрос аутентификации
                auth_required = await websocket.recv()

                # Отправляем истекший токен
                await websocket.send(
                    json.dumps({"type": "auth", "token": expired_token})
                )

                # Должны получить ошибку или соединение должно закрыться
                try:
                    response = await asyncio.wait_for(websocket.recv(), timeout=2)
                    response_data = json.loads(response)
                    assert (
                        "error" in response_data
                        or "auth_error" in response_data.get("type", "")
                    )
                except asyncio.TimeoutError:
                    pytest.fail(
                        "WebSocket не вернул ошибку при отправке истекшего токена"
                    )
                except websockets.exceptions.ConnectionClosedError:
                    # Соединение закрылось - это тоже валидное поведение для неверного токена
                    pass

        asyncio.run(test_ws())

    def test_invalid_token_format(self):
        """Тест обработки токена с неверным форматом."""
        invalid_tokens = [
            "not.a.jwt.token",  # Неправильное количество сегментов
            "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9",  # Только header
            "totally-invalid-token",  # Полностью неправильный формат
            "",  # Пустой токен
        ]

        for invalid_token in invalid_tokens:
            # Проверка через REST API
            response = requests.get(
                f"{BASE_URL}/auth/me", params={"token": invalid_token}
            )
            assert (
                response.status_code == 401
            ), f"Ожидалась ошибка 401 для токена: {invalid_token}"

            # Проверка через WebSocket
            @pytest.mark.asyncio
            async def test_ws():
                async with websockets.connect(WS_URL) as websocket:
                    # Получаем запрос аутентификации
                    auth_required = await websocket.recv()

                    # Отправляем неверный токен
                    await websocket.send(
                        json.dumps({"type": "auth", "token": invalid_token})
                    )

                    # Должны получить ошибку или соединение должно закрыться
                    try:
                        response = await asyncio.wait_for(websocket.recv(), timeout=2)
                        response_data = json.loads(response)
                        assert (
                            "error" in response_data
                            or "auth_error" in response_data.get("type", "")
                        )
                    except asyncio.TimeoutError:
                        pytest.fail(
                            f"WebSocket не вернул ошибку для неверного токена: {invalid_token}"
                        )
                    except websockets.exceptions.ConnectionClosedError:
                        # Соединение закрылось - это валидное поведение для неверного токена
                        pass

            asyncio.run(test_ws())

    def test_channel_unsubscribe(self):
        """Тест отписки от канала timeline."""
        # Получаем валидный токен
        response = requests.post(
            f"{BASE_URL}/auth/login",
            json={"username": "testuser", "password": "testpass"},
        )

        assert response.status_code == 200
        token = response.json()["access_token"]

        @pytest.mark.asyncio
        async def test_unsubscribe():
            async with websockets.connect(WS_URL) as websocket:
                # 1. Аутентификация
                auth_required = await websocket.recv()
                await websocket.send(json.dumps({"type": "auth", "token": token}))

                auth_response = await websocket.recv()
                auth_data = json.loads(auth_response)
                assert auth_data["type"] == "auth_success"

                # 2. Подписываемся на канал timeline
                await websocket.send(
                    json.dumps({"type": "subscribe", "channel": "timeline"})
                )

                subscribe_response = await websocket.recv()
                subscribe_data = json.loads(subscribe_response)
                # Проверяем, что получили ответ о подписке
                if "type" in subscribe_data:
                    assert subscribe_data["type"] == "subscribed"
                    assert subscribe_data["channel"] == "timeline"
                else:
                    # Возможно, получили данные от memory_timeline, пропускаем
                    pass

                # Могут прийти данные от memory_timeline, пропускаем их
                try:
                    initial_data = await asyncio.wait_for(websocket.recv(), timeout=1)
                except asyncio.TimeoutError:
                    # Если данных нет, это нормально
                    pass

                # 3. Отписываемся от канала
                await websocket.send(
                    json.dumps({"type": "unsubscribe", "channel": "timeline"})
                )

                unsubscribe_response = await websocket.recv()
                unsubscribe_data = json.loads(unsubscribe_response)
                assert unsubscribe_data["type"] == "unsubscribed"
                assert unsubscribe_data["channel"] == "timeline"

                # 4. Проверяем, что мы действительно отписаны
                # (в данном тесте это сложно проверить, но мы можем хотя бы убедиться,
                # что нет ошибок при отписке)

        asyncio.run(test_unsubscribe())

    def test_reconnect_with_new_token(self):
        """Тест повторного подключения с новым токеном после отключения."""
        # Первый раунд аутентификации
        response1 = requests.post(
            f"{BASE_URL}/auth/login",
            json={"username": "testuser", "password": "testpass"},
        )
        assert response1.status_code == 200
        token1 = response1.json()["access_token"]

        # Первое подключение и отключение
        @pytest.mark.asyncio
        async def test_connection1():
            async with websockets.connect(WS_URL) as websocket:
                # Аутентификация
                await websocket.recv()  # auth_required
                await websocket.send(json.dumps({"type": "auth", "token": token1}))

                auth_response = await websocket.recv()
                auth_data = json.loads(auth_response)
                assert auth_data["type"] == "auth_success"

        asyncio.run(test_connection1())

        # Получаем новый токен
        response2 = requests.post(
            f"{BASE_URL}/auth/login",
            json={"username": "testuser", "password": "testpass"},
        )
        assert response2.status_code == 200
        token2 = response2.json()["access_token"]
        # Токены могут быть одинаковыми, если созданы в одну секунду
        # Это нормальное поведение для JWT с одинаковым временем жизни
        # assert token1 != token2, "Новый токен должен отличаться от старого"

        # Повторное подключение с новым токеном
        @pytest.mark.asyncio
        async def test_connection2():
            async with websockets.connect(WS_URL) as websocket:
                # Аутентификация
                await websocket.recv()  # auth_required
                await websocket.send(json.dumps({"type": "auth", "token": token2}))

                auth_response = await websocket.recv()
                auth_data = json.loads(auth_response)
                assert auth_data["type"] == "auth_success"

        asyncio.run(test_connection2())
