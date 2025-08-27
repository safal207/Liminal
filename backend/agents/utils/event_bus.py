"""
Модуль event_bus - реализация шины событий для обмена сообщениями между агентами.

Использует Redis в качестве брокера сообщений для обеспечения масштабируемости.
"""

import asyncio
import json
import logging
import uuid
from datetime import datetime
from typing import Any, Awaitable, Callable, Dict, List, Optional

import redis.asyncio as redis

logger = logging.getLogger(__name__)


class EventBus:
    """
    Шина событий для асинхронного обмена сообщениями между агентами.

    Использует Redis Pub/Sub для обеспечения масштабируемости и отказоустойчивости.
    """

    def __init__(self, redis_url: str = "redis://localhost:6379"):
        """
        Инициализация шины событий.

        Args:
            redis_url: URL для подключения к Redis.
        """
        self.redis_url = redis_url
        self.redis: Optional[redis.Redis] = None
        self.pubsub: Optional[redis.client.PubSub] = None
        self.subscriptions: Dict[str, List[Callable]] = {}
        self._running = False
        self._listener_task: Optional[asyncio.Task] = None

    async def connect(self):
        """Установка соединения с Redis и инициализация подписчика."""
        try:
            self.redis = redis.from_url(self.redis_url)
            self.pubsub = self.redis.pubsub()
            await self.redis.ping()
            logger.info("Успешное подключение к Redis")
            self._running = True

            # Запускаем слушателя сообщений в фоновом режиме
            self._listener_task = asyncio.create_task(self._listen_messages())

        except Exception as e:
            logger.error(f"Ошибка подключения к Redis: {e}")
            self._running = False
            raise

    async def disconnect(self):
        """Закрытие соединения с Redis и очистка ресурсов."""
        self._running = False

        if self._listener_task:
            self._listener_task.cancel()
            try:
                await self._listener_task
            except asyncio.CancelledError:
                pass

        if self.pubsub:
            await self.pubsub.close()

        if self.redis:
            await self.redis.close()

        logger.info("Отключение от Redis завершено")

    async def publish(self, topic: str, message: Dict[str, Any]):
        """
        Публикация сообщения в указанную тему.

        Args:
            topic: Название темы, в которую публикуется сообщение.
            message: Словарь с данными сообщения.

        Raises:
            RuntimeError: Если соединение с Redis не установлено.
        """
        if not self.redis:
            raise RuntimeError("Не установлено соединение с Redis")

        try:
            message_id = str(uuid.uuid4())
            message_data = {
                "id": message_id,
                "topic": topic,
                "data": message,
                "timestamp": datetime.utcnow().isoformat(),
                "metadata": {},
            }

            message_str = json.dumps(message_data)
            await self.redis.publish(f"liminal:{topic}", message_str)
            logger.debug(
                f"Опубликовано сообщение в тему '{topic}': {message_str[:200]}..."
            )

            return message_id

        except Exception as e:
            logger.error(f"Ошибка при публикации сообщения в тему '{topic}': {e}")
            raise

    async def subscribe(
        self, topic: str, callback: Callable[[Dict[str, Any]], Awaitable[None]]
    ):
        """
        Подписка на сообщения из указанной темы.

        Args:
            topic: Название темы для подписки.
            callback: Асинхронная функция-обработчик сообщений.

        Raises:
            RuntimeError: Если соединение с Redis не установлено.
        """
        if not self.pubsub:
            raise RuntimeError("Не установлено соединение с Redis")

        try:
            # Добавляем колбэк в список подписчиков темы
            if topic not in self.subscriptions:
                self.subscriptions[topic] = []

            if callback not in self.subscriptions[topic]:
                self.subscriptions[topic].append(callback)

            # Подписываемся на канал в Redis, если это новая тема
            if len(self.subscriptions[topic]) == 1:
                await self.pubsub.subscribe(f"liminal:{topic}")
                logger.info(f"Подписка на тему: {topic}")

        except Exception as e:
            logger.error(f"Ошибка при подписке на тему '{topic}': {e}")
            raise

    async def unsubscribe(self, topic: str, callback: Optional[Callable] = None):
        """
        Отписка от сообщений из указанной темы.

        Args:
            topic: Название темы для отписки.
            callback: Конкретный обработчик для удаления. Если None, удаляются все обработчики темы.

        Raises:
            RuntimeError: Если соединение с Redis не установлено.
        """
        if not self.pubsub:
            raise RuntimeError("Не установлено соединение с Redis")

        try:
            if topic not in self.subscriptions:
                return

            if callback is None:
                # Отписываемся от всех обработчиков темы
                await self.pubsub.unsubscribe(f"liminal:{topic}")
                del self.subscriptions[topic]
                logger.info(f"Отписка от темы: {topic}")
            else:
                # Удаляем только указанный обработчик
                if callback in self.subscriptions[topic]:
                    self.subscriptions[topic].remove(callback)

                # Если больше нет обработчиков, отписываемся от темы
                if not self.subscriptions[topic]:
                    await self.pubsub.unsubscribe(f"liminal:{topic}")
                    del self.subscriptions[topic]
                    logger.info(f"Отписка от темы: {topic} (последний обработчик)")

        except Exception as e:
            logger.error(f"Ошибка при отписке от темы '{topic}': {e}")
            raise

    async def _listen_messages(self):
        """Фоновый процесс для прослушивания входящих сообщений."""
        if not self.pubsub:
            return

        logger.info("Запуск слушателя сообщений...")

        try:
            while self._running:
                try:
                    message = await self.pubsub.get_message(
                        ignore_subscribe_messages=True, timeout=1.0
                    )

                    if message and message["type"] == "message":
                        await self._process_incoming_message(message)

                    await asyncio.sleep(0.01)

                except asyncio.CancelledError:
                    logger.info("Получен запрос на остановку слушателя сообщений")
                    break
                except Exception as e:
                    logger.error(f"Ошибка при обработке сообщения: {e}", exc_info=True)
                    await asyncio.sleep(1)  # Защита от быстрого цикла ошибок

        except Exception as e:
            logger.critical(
                f"Критическая ошибка в слушателе сообщений: {e}", exc_info=True
            )
        finally:
            logger.info("Остановка слушателя сообщений")

    async def _process_incoming_message(self, message: Dict):
        """
        Обработка входящего сообщения от Redis.

        Args:
            message: Сырое сообщение от Redis Pub/Sub.
        """
        try:
            # Извлекаем данные сообщения
            channel = message["channel"].decode("utf-8")
            topic = channel.replace("liminal:", "", 1)

            # Парсим JSON-данные сообщения
            try:
                message_data = json.loads(message["data"].decode("utf-8"))
            except (json.JSONDecodeError, UnicodeDecodeError) as e:
                logger.error(f"Ошибка декодирования сообщения: {e}")
                return

            # Логируем получение сообщения (ограничиваем длину для логов)
            msg_str = str(message_data)[:200] + (
                "..." if len(str(message_data)) > 200 else ""
            )
            logger.debug(f"Получено сообщение из темы '{topic}': {msg_str}")

            # Вызываем все зарегистрированные обработчики
            if topic in self.subscriptions:
                for callback in self.subscriptions[topic]:
                    try:
                        # Запускаем обработчик в фоновом режиме
                        asyncio.create_task(self._safe_callback(callback, message_data))
                    except Exception as e:
                        logger.error(
                            f"Ошибка при вызове обработчика для темы '{topic}': {e}",
                            exc_info=True,
                        )

        except Exception as e:
            logger.error(
                f"Неожиданная ошибка при обработке сообщения: {e}", exc_info=True
            )

    async def _safe_callback(self, callback: Callable, message: Dict):
        """Безопасный вызов колбэка с обработкой исключений."""
        try:
            await callback(message)
        except asyncio.CancelledError:
            # Игнорируем отмененные задачи
            pass
        except Exception as e:
            logger.error(f"Ошибка в обработчике сообщения: {e}", exc_info=True)
