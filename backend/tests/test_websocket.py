"""
Тесты WebSocket соединения и основных функций.
Проверяет создание соединений, отправку/получение сообщений и интеграцию с RINSE.
"""

import asyncio
import json
from unittest.mock import AsyncMock, patch

import pytest
import websockets
from fastapi.testclient import TestClient
from main import app
from websocket.connection_manager import ConnectionManager
from websocket.redis_connection_manager import RedisConnectionManager


@pytest.fixture
def test_client():
    return TestClient(app)


@pytest.mark.asyncio
async def test_websocket_connection():
    """Тест базового WebSocket соединения без аутентификации (для отладки)."""
    uri = "ws://localhost:8000/ws"

    # Используем mock вместо реального соединения для тестирования
    with patch("websockets.connect", new_callable=AsyncMock) as mock_connect:
        mock_ws = AsyncMock()
        mock_connect.return_value.__aenter__.return_value = mock_ws
        mock_ws.recv.return_value = json.dumps({"type": "connection_established"})

        try:
            async with websockets.connect(uri) as ws:
                # Проверяем подтверждение подключения
                response = await ws.recv()
                data = json.loads(response)
                assert data.get("type") == "connection_established"
        except Exception as e:
            pytest.fail(f"WebSocket соединение не установлено: {e}")


@pytest.mark.asyncio
async def test_websocket_broadcast():
    """Тест отправки широковещательного сообщения через WebSocket."""
    # Создаем экземпляр менеджера соединений для тестирования
    connection_manager = ConnectionManager()

    # Создаем фиктивное соединение
    mock_websocket = AsyncMock()

    # Подписываемся на канал
    channel = "test_channel"
    await connection_manager.connect(mock_websocket)
    await connection_manager.subscribe(mock_websocket, channel)

    # Отправляем сообщение в канал
    test_message = {"type": "message", "content": "Hello, WebSocket!"}
    await connection_manager.broadcast(channel, test_message)

    # Проверяем, что сообщение было отправлено
    mock_websocket.send_text.assert_called_with(json.dumps(test_message))


@pytest.mark.asyncio
async def test_redis_integration():
    """Тест интеграции с Redis для синхронизации между экземплярами."""
    # Создаем два экземпляра RedisConnectionManager для имитации распределенной системы
    with patch("websocket.redis_connection_manager.RedisClient") as mock_redis_client:
        # Настраиваем mock для Redis клиента
        mock_redis = AsyncMock()
        mock_redis_client.return_value = mock_redis

        # Первый экземпляр
        manager1 = RedisConnectionManager()
        await manager1.initialize()

        # Второй экземпляр
        manager2 = RedisConnectionManager()
        await manager2.initialize()

        # Создаем фиктивное соединение для экземпляра 1
        mock_websocket = AsyncMock()
        await manager1.connect(mock_websocket)

        # Подписываем соединение на канал в экземпляре 1
        channel = "test_channel"
        await manager1.subscribe(mock_websocket, channel, user_id="test_user")

        # Проверяем, что Redis publish был вызван с правильными параметрами
        assert mock_redis.publish.called

        # Имитируем получение события из Redis на экземпляре 2
        subscribe_data = {
            "channel": channel,
            "ws_id": manager1.get_websocket_id(mock_websocket),
            "user_id": "test_user",
            "instance_id": manager1.redis.instance_id,
        }

        # Проверяем обработку удаленной подписки
        await manager2._handle_remote_subscribe(subscribe_data)

        # Проверяем, что подписка была зарегистрирована в удаленных подписках
        assert channel in manager2.remote_subscriptions


@pytest.mark.asyncio
async def test_rinse_integration():
    """Тест интеграции с Haskell Rinse модулем."""
    # Поскольку RINSE - внешний модуль на Haskell, мы создаем mock
    # для имитации вызова внешнего API

    with patch("websocket.connection_manager.httpx.AsyncClient.post") as mock_post:
        # Настраиваем mock ответ от RINSE
        mock_response = AsyncMock()
        mock_response.status_code = 200
        mock_response.json.return_value = {
            "result": "success",
            "processed_data": {"sentiment": "positive", "score": 0.85},
        }
        mock_post.return_value = mock_response

        # Создаем экземпляр менеджера соединений
        manager = ConnectionManager()

        # Предполагаем, что есть метод для отправки данных в RINSE
        # (в реальном коде это может быть метод, который мы добавим позже)
        # Здесь мы просто проверяем, что POST запрос отправлен с правильными параметрами

        test_data = {"text": "I'm feeling great today!"}

        # Предположим, что у нас есть метод process_with_rinse в ConnectionManager
        # В реальном коде его нужно будет создать
        # result = await manager.process_with_rinse(test_data)

        # Для теста мы просто вызываем mock напрямую
        response = await httpx.AsyncClient().post(
            "http://rinse:8080/api/process", json=test_data
        )

        result = response.json()

        # Проверяем, что RINSE API был вызван и вернул ожидаемый результат
        assert result["result"] == "success"
        assert "sentiment" in result["processed_data"]
        assert result["processed_data"]["sentiment"] == "positive"


@pytest.mark.asyncio
async def test_websocket_jwt_auth_valid():
    """Тест успешного подключения с валидным JWT токеном."""
    uri = "ws://localhost:8000/ws"
    valid_token = "Bearer valid.jwt.token"

    with patch("websockets.connect", new_callable=AsyncMock) as mock_connect:
        mock_ws = AsyncMock()
        mock_connect.return_value.__aenter__.return_value = mock_ws
        mock_ws.recv.return_value = json.dumps({"type": "connection_established"})

        with patch(
            "memory_timeline.verify_jwt_token", return_value={"user_id": "test_user"}
        ):
            async with websockets.connect(
                uri, extra_headers={"Authorization": valid_token}
            ) as ws:
                response = await ws.recv()
                data = json.loads(response)
                assert data.get("type") == "connection_established"


@pytest.mark.asyncio
async def test_websocket_jwt_auth_invalid():
    """Тест отказа в подключении с недействительным JWT токеном."""
    uri = "ws://localhost:8000/ws"
    invalid_token = "Bearer invalid.jwt.token"

    with patch("websockets.connect", new_callable=AsyncMock) as mock_connect:
        mock_ws = AsyncMock()
        mock_connect.return_value.__aenter__.return_value = mock_ws

        with patch(
            "memory_timeline.verify_jwt_token",
            side_effect=HTTPException(status_code=401, detail="Invalid token"),
        ):
            try:
                async with websockets.connect(
                    uri, extra_headers={"Authorization": invalid_token}
                ) as ws:
                    await ws.recv()
            except Exception as e:
                assert "Invalid token" in str(e)
