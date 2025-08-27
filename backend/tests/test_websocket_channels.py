import asyncio
import logging
import sys
from pathlib import Path

import pytest
from websockets.exceptions import ConnectionClosedError

# Настройка логирования
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
    handlers=[logging.StreamHandler(), logging.FileHandler("test_websocket.log")],
)
logger = logging.getLogger(__name__)

# Добавляем корень проекта в PYTHONPATH
project_root = str(Path(__file__).parent.parent)
if project_root not in sys.path:
    sys.path.insert(0, project_root)

# Импортируем WebSocketClient из существующего теста
from tests.test_websocket_simple import WebSocketClient

# Базовый URL WebSocket сервера
BASE_URL = "ws://localhost:8000"

# Таймауты в секундах
CONNECT_TIMEOUT = 5.0
MESSAGE_TIMEOUT = 3.0
TEST_TIMEOUT = 30.0


@pytest.mark.asyncio
@pytest.mark.timeout(TEST_TIMEOUT)
async def test_multiple_channels():
    """
    Тестируем подписку пользователей на разные каналы и проверяем,
    что сообщения приходят только подписанным пользователям
    """
    logger.info("Starting test_multiple_channels")
    # Создаем пользователей с разными подписками
    users = [
        {"id": "user1", "channels": ["channel1", "channel2"], "messages": []},
        {"id": "user2", "channels": ["channel2", "channel3"], "messages": []},
        {"id": "user3", "channels": ["channel1", "channel3"], "messages": []},
    ]

    clients = {}

    try:
        # Подключаем пользователей и подписываем на каналы
        for user in users:
            logger.info(f"Connecting user {user['id']}")
            client = WebSocketClient(user["id"])
            try:
                await asyncio.wait_for(client.connect(), timeout=CONNECT_TIMEOUT)

                # Подписываем на все каналы пользователя
                for channel in user["channels"]:
                    logger.info(f"User {user['id']} subscribing to {channel}")
                    await client.subscribe_to_channel(channel)
                    try:
                        response = await asyncio.wait_for(
                            client.receive_message(), timeout=MESSAGE_TIMEOUT
                        )
                        assert (
                            response.get("status") == "success"
                        ), f"Subscribe failed: {response}"
                        assert (
                            response.get("channel") == channel
                        ), f"Wrong channel in response: {response}"
                        logger.info(f"User {user['id']} subscribed to {channel}")
                    except asyncio.TimeoutError:
                        logger.error(
                            f"Timeout while subscribing {user['id']} to {channel}"
                        )
                        raise
            except Exception as e:
                logger.error(f"Error setting up user {user['id']}: {str(e)}")
                raise

            clients[user["id"]] = client

        # Даем время на подписку
        logger.info(
            "All users connected and subscribed, waiting for subscriptions to complete..."
        )
        await asyncio.sleep(1)

        # Отправляем сообщения в разные каналы
        test_messages = [
            ("user1", "channel1", "Привет из channel1"),
            ("user2", "channel2", "Сообщение в channel2"),
            ("user3", "channel3", "Тест из channel3"),
        ]

        for sender_id, channel, message in test_messages:
            logger.info(f"Sending message from {sender_id} to {channel}: {message}")
            try:
                await asyncio.wait_for(
                    clients[sender_id].send_chat_message(channel, message),
                    timeout=MESSAGE_TIMEOUT,
                )
                await asyncio.sleep(0.5)  # Даем время на доставку
            except Exception as e:
                logger.error(f"Error sending message from {sender_id}: {str(e)}")
                raise

        # Проверяем, кто какие сообщения получил
        logger.info("Checking received messages...")
        for user in users:
            client = clients[user["id"]]
            logger.info(f"Checking messages for user {user['id']}...")

            # Получаем все входящие сообщения
            while True:
                try:
                    response = await asyncio.wait_for(
                        client.receive_message(),
                        timeout=1.0,  # Короткий таймаут для проверки оставшихся сообщений
                    )
                    if response:
                        logger.info(f"User {user['id']} received: {response}")
                        # Сохраняем только сообщения от других пользователей (type: "message")
                        # Игнорируем подтверждения отправки (type: "message_sent")
                        if response.get("type") == "message":
                            user["messages"].append(response)
                        elif response.get("type") == "message_sent":
                            logger.debug(
                                f"Ignoring message_sent confirmation from {user['id']}: {response}"
                            )
                        else:
                            logger.warning(
                                f"Unexpected message format from {user['id']}: {response}"
                            )
                except (asyncio.TimeoutError, ConnectionClosedError):
                    logger.info(f"No more messages for user {user['id']}")
                    break
                except Exception as e:
                    logger.error(f"Error receiving message for {user['id']}: {str(e)}")
                    break

            # Проверяем, что пользователь получил только сообщения из своих каналов
            user_channels = set(user["channels"])
            logger.info(
                f"Verifying messages for user {user['id']} (channels: {user_channels})"
            )

            for msg in user["messages"]:
                msg_channel = msg.get("channel")
                logger.debug(
                    f"Checking message in {msg_channel} for {user['id']}: {msg}"
                )
                assert (
                    msg_channel in user_channels
                ), f"Пользователь {user['id']} получил сообщение из неподписанного канала {msg_channel}"

            # Проверяем, что пользователь получил все ожидаемые сообщения
            expected_messages = [
                (ch, msg)
                for s_id, ch, msg in test_messages
                if ch in user_channels
                and s_id != user["id"]  # не считаем свои же сообщения
            ]

            received_messages = [
                (msg.get("channel"), msg.get("message", {}).get("text", ""))
                for msg in user["messages"]
                if "message" in msg
            ]

            logger.info(f"User {user['id']} received messages: {received_messages}")
            logger.info(f"Expected messages for {user['id']}: {expected_messages}")

            # Проверяем каждое ожидаемое сообщение
            for channel, expected_msg in expected_messages:
                found = any(
                    expected_msg in msg_text and msg_ch == channel
                    for msg_ch, msg_text in received_messages
                )
                assert found, (
                    f"Пользователь {user['id']} не получил сообщение "
                    f"в канале {channel}: {expected_msg}"
                )

            logger.info(
                f"Message verification for user {user['id']} completed successfully"
            )

        # Тестируем отписку от канала
        user_to_unsubscribe = users[0]  # user1
        channel_to_unsubscribe = user_to_unsubscribe["channels"][0]  # channel1

        logger.info(
            f"Testing unsubscribe for user {user_to_unsubscribe['id']} from {channel_to_unsubscribe}"
        )

        try:
            # Отписываем user1 от channel1
            logger.info(f"Sending unsubscribe message for {channel_to_unsubscribe}")
            await asyncio.wait_for(
                clients[user_to_unsubscribe["id"]].send_message(
                    "unsubscribe", channel=channel_to_unsubscribe
                ),
                timeout=MESSAGE_TIMEOUT,
            )

            # Ждем подтверждения отписки
            response = await asyncio.wait_for(
                clients[user_to_unsubscribe["id"]].receive_message(),
                timeout=MESSAGE_TIMEOUT,
            )

            logger.info(f"Unsubscribe response: {response}")
            assert response.get("status") == "success", "Unsubscribe failed"
            assert (
                response.get("channel") == channel_to_unsubscribe
            ), "Wrong channel in unsubscribe response"

            # Обновляем список каналов пользователя
            user_to_unsubscribe["channels"].remove(channel_to_unsubscribe)
            logger.info(
                f"User {user_to_unsubscribe['id']} unsubscribed from {channel_to_unsubscribe}"
            )

            # Отправляем сообщение в отписанный канал
            test_message_unsubscribed = (
                "Это сообщение не должно дойти до отписавшегося пользователя"
            )
            logger.info(
                f"Sending message to unsubscribed channel {channel_to_unsubscribe}"
            )
            await asyncio.wait_for(
                clients[user_to_unsubscribe["id"]].send_chat_message(
                    channel_to_unsubscribe, test_message_unsubscribed
                ),
                timeout=MESSAGE_TIMEOUT,
            )

            # Отправляем сообщение в другой канал, на который пользователь остался подписан
            remaining_channel = user_to_unsubscribe["channels"][0]
            test_message_subscribed = "Это сообщение должно дойти"
            logger.info(f"Sending message to remaining channel {remaining_channel}")
            await asyncio.wait_for(
                clients[user_to_unsubscribe["id"]].send_chat_message(
                    remaining_channel, test_message_subscribed
                ),
                timeout=MESSAGE_TIMEOUT,
            )

            # Даем время на доставку сообщений
            await asyncio.sleep(2)

            # Проверяем, что пользователь получил только сообщение из оставшегося канала
            user_to_unsubscribe["messages"] = []
            debug_received = []
            try:
                while True:
                    msg = await asyncio.wait_for(
                        clients[user_to_unsubscribe["id"]].receive(), timeout=3
                    )
                    print("DEBUG: user1 получил сообщение:", msg)
                    logger.info(f"DEBUG: user1 получил сообщение: {msg}")
                    debug_received.append(msg)
            except asyncio.TimeoutError:
                print("DEBUG: user1 — больше сообщений не пришло (таймаут)")
                logger.info("DEBUG: user1 — больше сообщений не пришло (таймаут)")
            except Exception as e:
                print("DEBUG: user1 — ошибка при чтении:", e)
                logger.info(f"DEBUG: user1 — ошибка при чтении: {e}")

            print("DEBUG: debug_received перед записью:", debug_received)
            ws = clients[user_to_unsubscribe["id"]].websocket
            with open("debug_ws_info.txt", "w", encoding="utf-8") as f:
                f.write(f"user1 websocket type: {type(ws)}\n")
                f.write(f"user1 websocket repr: {repr(ws)}\n")
                f.write(f"user1 websocket dir: {dir(ws)}\n")
                f.write(f"user1 websocket state: {ws.state}\n")
                f.write(f"user1 received after unsubscribe: {debug_received}\n")

            # Проверяем, что нет сообщений из отписанного канала
            for msg in user_to_unsubscribe["messages"]:
                assert msg.get("channel") != channel_to_unsubscribe, (
                    f"Пользователь {user_to_unsubscribe['id']} получил сообщение "
                    f"из отписанного канала {channel_to_unsubscribe}"
                )

            # Проверяем, что сообщение из оставшегося канала пришло
            received_messages = [
                msg.get("message", {}).get("text", "")
                for msg in user_to_unsubscribe["messages"]
                if msg.get("channel") == remaining_channel
            ]

            assert any(test_message_subscribed in msg for msg in received_messages), (
                f"Пользователь {user_to_unsubscribe['id']} не получил сообщение "
                f"из оставшегося канала {remaining_channel}"
            )

            logger.info("Unsubscribe test completed successfully")

        except Exception as e:
            logger.error(f"Error in unsubscribe test: {str(e)}")
            raise

    finally:
        # Закрываем все соединения
        for client in clients.values():
            await client.close()


if __name__ == "__main__":
    asyncio.run(test_multiple_channels())
