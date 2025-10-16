"""
Скрипт для тестирования отписки от канала timeline и memory_timeline.

Запуск скрипта:
python test_timeline_unsubscribe.py
"""

import asyncio
import json
import logging

import requests
import websockets

# Конфигурация логирования
logging.basicConfig(
    level=logging.INFO, format="%(asctime)s - %(name)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger("test_timeline_unsubscribe")

# Конфигурация
AUTH_URL = "http://127.0.0.1:8000/auth/login"
WS_URL = "ws://127.0.0.1:8000/ws/timeline"

# Тестовые пользовательские данные
USER_CREDENTIALS = {"username": "testuser", "password": "testpass"}


async def run_test():
    """
    Запускает основной тест для проверки отписки от канала timeline.
    """
    # 1. Получаем токен через REST API
    try:
        logger.info("Получение JWT токена через REST API...")
        response = requests.post(AUTH_URL, json=USER_CREDENTIALS)
        response.raise_for_status()
        token = response.json()["access_token"]
        logger.info(f"Токен получен: {token[:10]}...")
    except Exception as e:
        logger.error(f"Ошибка при получении токена: {str(e)}")
        return

    # 2. Подключаемся к WebSocket
    try:
        async with websockets.connect(
            WS_URL, extra_headers={"Authorization": f"Bearer {token}"}
        ) as websocket:
            logger.info("Подключено к WebSocket серверу")

            # Получаем сообщение о необходимости аутентификации
            auth_required = await websocket.recv()
            logger.info(f"Получено сообщение: {auth_required}")

            # Отправляем токен для аутентификации
            logger.info("Отправка токена для аутентификации...")
            auth_message = {"type": "auth", "token": token}
            await websocket.send(json.dumps(auth_message))

            # Получаем ответ об успешной аутентификации
            auth_response = await websocket.recv()
            logger.info(f"Получен ответ аутентификации: {auth_response}")

            # 3. Подписываемся на канал timeline
            logger.info("Подписываемся на канал timeline...")
            subscribe_message = {"type": "subscribe", "channel": "timeline"}
            await websocket.send(json.dumps(subscribe_message))

            # Получаем подтверждение подписки
            subscribe_response = await websocket.recv()
            logger.info(f"Получен ответ на подписку: {subscribe_response}")

            # Может быть получено начальное состояние от memory_timeline
            # Пропускаем до 3 сообщений или ждем максимум 2 секунды
            for _ in range(3):
                try:
                    initial_data = await asyncio.wait_for(websocket.recv(), timeout=2)
                    logger.info(f"Получено начальное сообщение: {initial_data}")
                except asyncio.TimeoutError:
                    logger.info("Нет больше начальных сообщений")
                    break

            # 4. Отправляем тестовое сообщение в канал timeline
            logger.info("Отправляем тестовое сообщение...")
            broadcast_message = {
                "type": "broadcast",
                "channel": "timeline",
                "content": "Тестовое сообщение перед отпиской",
            }
            await websocket.send(json.dumps(broadcast_message))

            # Получаем подтверждение отправки (если есть)
            try:
                broadcast_response = await asyncio.wait_for(websocket.recv(), timeout=2)
                logger.info(f"Получен ответ на сообщение: {broadcast_response}")
            except asyncio.TimeoutError:
                logger.info("Не получено подтверждения отправки сообщения")

            # 5. Отписываемся от канала timeline
            logger.info("Отписываемся от канала timeline...")
            unsubscribe_message = {"type": "unsubscribe", "channel": "timeline"}
            await websocket.send(json.dumps(unsubscribe_message))

            # Получаем подтверждение отписки
            unsubscribe_response = await websocket.recv()
            logger.info(f"Получен ответ на отписку: {unsubscribe_response}")

            # 6. Пробуем отправить еще одно сообщение в канал timeline
            logger.info("Отправляем сообщение после отписки...")
            post_unsub_message = {
                "type": "broadcast",
                "channel": "timeline",
                "content": "Тестовое сообщение после отписки",
            }
            await websocket.send(json.dumps(post_unsub_message))

            # Проверяем, получим ли мы сообщение (не должны)
            try:
                post_unsub_response = await asyncio.wait_for(
                    websocket.recv(), timeout=2
                )
                logger.info(f"Получено сообщение после отписки: {post_unsub_response}")
                # Проверяем, не ошибка ли это или сообщение от другого источника
                message_data = json.loads(post_unsub_response)
                if "error" in message_data or message_data.get("type") == "error":
                    logger.info(
                        "Получено сообщение об ошибке после отписки, это ожидаемое поведение"
                    )
                else:
                    logger.warning(
                        "Получено сообщение после отписки, хотя не должны были его получить"
                    )
            except asyncio.TimeoutError:
                logger.info(
                    "Не получено сообщений после отписки, это правильное поведение"
                )

            # 7. Заново подписываемся, чтобы проверить, что подписка работает после отписки
            logger.info("Подписываемся снова на канал timeline...")
            resubscribe_message = {"type": "subscribe", "channel": "timeline"}
            await websocket.send(json.dumps(resubscribe_message))

            # Получаем подтверждение подписки
            resubscribe_response = await websocket.recv()
            logger.info(f"Получен ответ на повторную подписку: {resubscribe_response}")

            # Снова могут прийти начальные сообщения от memory_timeline
            try:
                resubscribe_data = await asyncio.wait_for(websocket.recv(), timeout=2)
                logger.info(
                    f"Получено начальное сообщение при повторной подписке: {resubscribe_data}"
                )
            except asyncio.TimeoutError:
                logger.info("Нет начальных сообщений при повторной подписке")

            logger.info("Тест успешно завершен!")

    except Exception as e:
        logger.error(f"Ошибка во время выполнения теста: {str(e)}")


if __name__ == "__main__":
    asyncio.run(run_test())
