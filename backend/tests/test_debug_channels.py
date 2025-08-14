import asyncio
import json
import logging
import sys
from pathlib import Path

import pytest
from websockets.exceptions import ConnectionClosed, ConnectionClosedError

# Настройка логирования
logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger(__name__)

# Добавляем корень проекта в PYTHONPATH
project_root = str(Path(__file__).parent.parent)
if project_root not in sys.path:
    sys.path.insert(0, project_root)

# Импортируем WebSocketClient из существующего теста
from tests.test_websocket_simple import WebSocketClient


@pytest.mark.asyncio
async def test_unsubscribe_debug():
    """Упрощенный тест отписки для отладки"""

    logger.info("=== Starting unsubscribe debug test ===")

    # Создаем одного пользователя
    user_id = "test_user"
    client = WebSocketClient(user_id)

    try:
        # Подключаемся
        logger.info(f"Connecting user {user_id}")
        await client.connect()

        # Подписываемся на два канала
        channels = ["channel1", "channel2"]
        for channel in channels:
            logger.info(f"Subscribing to {channel}")
            await client.subscribe_to_channel(channel)

            # Ждем подтверждение подписки
            response = await asyncio.wait_for(client.receive_message(), timeout=5.0)
            logger.info(f"Subscribe response for {channel}: {response}")
            assert response.get("status") == "success"
            assert response.get("channel") == channel

        # Отписываемся от channel1
        logger.info("Unsubscribing from channel1")
        await client.send_message("unsubscribe", channel="channel1")

        # Ждем подтверждение отписки
        response = await asyncio.wait_for(client.receive_message(), timeout=5.0)
        logger.info(f"Unsubscribe response: {response}")
        assert response.get("status") == "success"
        assert response.get("channel") == "channel1"

        # Отправляем сообщение в channel1 (не должно дойти)
        logger.info("Sending message to channel1 (should not receive)")
        await client.send_chat_message("channel1", "Message to unsubscribed channel")

        # Отправляем сообщение в channel2 (должно дойти)
        logger.info("Sending message to channel2 (should receive)")
        test_message = "Message to subscribed channel"
        await client.send_chat_message("channel2", test_message)

        # Даем время на доставку
        await asyncio.sleep(1)

        # Собираем все сообщения
        messages = []
        try:
            while True:
                response = await asyncio.wait_for(client.receive_message(), timeout=2.0)
                if response:
                    logger.info(f"Received message: {response}")
                    messages.append(response)
        except asyncio.TimeoutError:
            logger.info("No more messages")

        # Проверяем результаты
        logger.info(f"Total messages received: {len(messages)}")

        # Не должно быть сообщений из channel1
        channel1_messages = [
            msg for msg in messages if msg.get("channel") == "channel1"
        ]
        logger.info(f"Messages from channel1: {len(channel1_messages)}")
        assert (
            len(channel1_messages) == 0
        ), f"Received unexpected messages from channel1: {channel1_messages}"

        # Должно быть сообщение из channel2
        channel2_messages = [
            msg for msg in messages if msg.get("channel") == "channel2"
        ]
        logger.info(f"Messages from channel2: {len(channel2_messages)}")

        # Проверяем, что есть наше тестовое сообщение
        found_test_message = False
        for msg in channel2_messages:
            if "message" in msg:
                text = msg["message"].get("text", "")
                logger.info(f"Channel2 message text: '{text}'")
                if test_message in text:
                    found_test_message = True
                    break

        assert (
            found_test_message
        ), f"Test message not found in channel2. Messages: {channel2_messages}"

        logger.info("=== Test completed successfully ===")

    except Exception as e:
        logger.error(f"Test failed with error: {str(e)}")
        raise
    finally:
        await client.close()


if __name__ == "__main__":
    asyncio.run(test_unsubscribe_debug())
