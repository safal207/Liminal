"""
Скрипт для проверки Redis интеграции и сигнатур callback функций.
Создан для диагностики и проверки исправления проблем.
"""

import asyncio
import sys
import uuid

from logging_config import get_logger
from websocket.redis_client import RedisClient
from websocket.redis_connection_manager import RedisConnectionManager

logger = get_logger(__name__)


async def test_redis_client_pubsub():
    """Проверка работы Pub/Sub в Redis клиенте с одноаргументным callback."""
    logger.info("Запуск теста Redis Client Pub/Sub...")

    client = RedisClient(prefix="test", test_mode=True)
    connected = await client.connect()

    if not connected:
        logger.error("Redis не запущен или недоступен")
        return False

    try:
        received_messages = []

        # Callback с одним аргументом (данные)
        async def message_handler(data):
            logger.info(f"Получено сообщение: {data}")
            received_messages.append(data)

        test_channel = f"test_channel_{uuid.uuid4().hex[:8]}"
        test_message = {"event": "test", "data": {"value": 42}}

        logger.info(f"Подписка на канал {test_channel}...")
        await client.subscribe(test_channel, message_handler)

        logger.info(f"Публикация сообщения в канал {test_channel}: {test_message}")
        await client.publish(test_channel, test_message)

        # Ждем получения сообщения
        logger.info("Ожидание получения сообщения...")
        await asyncio.sleep(1.0)

        logger.info(f"Полученные сообщения: {received_messages}")
        if len(received_messages) == 1 and received_messages[0] == test_message:
            logger.info("✅ Тест успешно пройден!")
            return True
        else:
            logger.error(
                "❌ Тест провалился: сообщение не получено или не соответствует ожидаемому"
            )
            return False

    finally:
        await client.close()


async def test_redis_connection_manager():
    """Проверка работы RedisConnectionManager с одноаргументными обработчиками."""
    logger.info("Запуск теста RedisConnectionManager...")

    manager1 = RedisConnectionManager(redis_prefix="test_cm1")
    manager2 = RedisConnectionManager(redis_prefix="test_cm2")

    manager1.redis.test_mode = True
    manager2.redis.test_mode = True

    try:
        manager1.redis.instance_id = "test_instance_1"
        manager2.redis.instance_id = "test_instance_2"

        await manager1.initialize()
        await manager2.initialize()

        logger.info("Инициализация менеджеров успешна")
        return True

    except Exception as e:
        logger.error(f"Ошибка при тестировании RedisConnectionManager: {e}")
        return False

    finally:
        await manager1.shutdown()
        await manager2.shutdown()


async def main():
    """Запускает все тесты и выводит результаты."""
    logger.info("Запуск диагностики Redis интеграции...")

    test_results = {
        "Redis Client Pub/Sub": await test_redis_client_pubsub(),
        "Redis Connection Manager": await test_redis_connection_manager(),
    }

    logger.info("\n== РЕЗУЛЬТАТЫ ТЕСТОВ ==")
    for test_name, passed in test_results.items():
        status = "✅ ПРОЙДЕН" if passed else "❌ ПРОВАЛЕН"
        logger.info(f"{test_name}: {status}")

    all_passed = all(test_results.values())
    logger.info(
        f"\nОбщий результат: {'✅ Все тесты пройдены' if all_passed else '❌ Есть проваленные тесты'}"
    )


if __name__ == "__main__":
    asyncio.run(main())
