import asyncio
import json
import time
from concurrent.futures import ThreadPoolExecutor

import pytest
import websockets

BASE_URL = "ws://localhost:8000"


class WebSocketClient:
    def __init__(self, user_id):
        self.user_id = user_id
        self.websocket = None
        self.messages = []

    async def connect(self):
        """Подключение к WebSocket серверу"""
        self.websocket = await websockets.connect(f"{BASE_URL}/ws/{self.user_id}")

    async def send_message(self, message_type, **kwargs):
        """Отправка сообщения"""
        message = {"type": message_type, "user_id": self.user_id, **kwargs}
        await self.websocket.send(json.dumps(message))

    async def receive_message(self):
        """Получение сообщения"""
        try:
            message = await asyncio.wait_for(self.websocket.recv(), timeout=5.0)
            parsed = json.loads(message)
            self.messages.append(parsed)
            return parsed
        except asyncio.TimeoutError:
            return None

    async def subscribe_to_channel(self, channel):
        """Подписка на канал"""
        await self.send_message("subscribe", channel=channel)

    async def send_chat_message(self, channel, text):
        """Отправка сообщения в чат"""
        await self.send_message("message", channel=channel, message={"text": text})

    async def close(self):
        """Закрытие соединения"""
        if self.websocket:
            await self.websocket.close()


@pytest.mark.asyncio
async def test_single_user_connection():
    """Тест подключения одного пользователя"""
    client = WebSocketClient("test_user_1")

    try:
        await client.connect()
        await client.subscribe_to_channel("test_channel")

        # Ждем ответ на подписку
        response = await client.receive_message()
        assert response is not None
        assert response.get("status") == "success"

    finally:
        await client.close()


@pytest.mark.asyncio
async def test_multiple_users_chat():
    """Тест чата между несколькими пользователями"""
    users = ["user1", "user2", "user3"]
    clients = []

    try:
        # Создаем клиентов
        for user_id in users:
            client = WebSocketClient(user_id)
            await client.connect()
            await client.subscribe_to_channel("test_channel")
            clients.append(client)

        # Даем время на подписку
        await asyncio.sleep(1)

        # Отправляем сообщение от первого пользователя
        test_message = "Привет от user1!"
        await clients[0].send_chat_message("test_channel", test_message)

        # Даем время на доставку
        await asyncio.sleep(1)

        # Проверяем, что сообщение получили другие пользователи
        for i, client in enumerate(clients[1:], 1):
            response = await client.receive_message()
            assert response is not None
            assert test_message in str(response)
            print(f"User {users[i]} получил сообщение: {response}")

    finally:
        # Закрываем все соединения
        for client in clients:
            await client.close()


@pytest.mark.asyncio
async def test_channel_subscription():
    """Тест подписки на разные каналы"""
    client1 = WebSocketClient("user1")
    client2 = WebSocketClient("user2")

    try:
        await client1.connect()
        await client2.connect()

        # Подписываемся на разные каналы
        await client1.subscribe_to_channel("channel1")
        await client2.subscribe_to_channel("channel2")

        await asyncio.sleep(1)

        # Отправляем сообщение в channel1
        await client1.send_chat_message("channel1", "Сообщение в channel1")

        await asyncio.sleep(1)

        # client2 не должен получить сообщение (он подписан на channel2)
        response = await client2.receive_message()
        assert response is None or "channel1" not in str(response)

    finally:
        await client1.close()
        await client2.close()


@pytest.mark.asyncio
async def test_unsubscribe():
    """Тест отписки от канала"""
    client = WebSocketClient("test_user")

    try:
        await client.connect()
        await client.subscribe_to_channel("test_channel")

        # Отписываемся
        await client.send_message("unsubscribe", channel="test_channel")

        response = await client.receive_message()
        assert response is not None
        assert response.get("status") == "success"

    finally:
        await client.close()


if __name__ == "__main__":
    # Запуск тестов напрямую
    asyncio.run(test_single_user_connection())
    print("✅ Тест подключения пройден")

    asyncio.run(test_multiple_users_chat())
    print("✅ Тест чата пройден")
