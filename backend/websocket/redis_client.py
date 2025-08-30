"""
Redis клиент для поддержки масштабируемой WebSocket архитектуры.
Обеспечивает синхронизацию состояния между несколькими инстансами сервера.
"""

import asyncio
import contextlib
import json
import os
import time
import uuid
from collections.abc import Callable
from typing import Any

from loguru import logger

import redis.asyncio as redis
from redis.asyncio.client import Redis
from redis.exceptions import RedisError

# Условный импорт метрик Prometheus
try:
    from metrics import (
        redis_connection_status,
        redis_errors_total,
        redis_operation_duration_seconds,
        redis_operations_total,
        redis_pubsub_messages_total,
    )

    METRICS_ENABLED = True
    logger.info("Prometheus Redis metrics module loaded successfully")
except ImportError:
    logger.warning("Prometheus Redis metrics module not found, metrics collection disabled")
    METRICS_ENABLED = False

    # Заглушки для метрик Redis
    class DummyMetric:
        def __init__(self, *args, **kwargs):
            pass

        def inc(self, *args, **kwargs):
            pass

        def dec(self, *args, **kwargs):
            pass

        def set(self, *args, **kwargs):
            pass

        def observe(self, *args, **kwargs):
            pass

        def labels(self, *args, **kwargs):
            return self

    # Создаем заглушки для Redis метрик
    redis_connection_status = DummyMetric()
    redis_operations_total = DummyMetric()
    redis_operation_duration_seconds = DummyMetric()
    redis_pubsub_messages_total = DummyMetric()
    redis_errors_total = DummyMetric()


class RedisClient:
    """Клиент Redis для синхронизации состояния WebSocket соединений между серверами."""

    # Системные каналы, общие для всех инстансов. Для них НЕ применяется префикс,
    # чтобы все инстансы принимали сообщения из одного и того же канала.
    _SYSTEM_CHANNELS: set[str] = {
        "broadcast",
        "subscribe",
        "unsubscribe",
        "disconnect",
        "personal_message",
    }

    def _full_channel(self, channel: str) -> str:
        """Возвращает имя реального Redis-канала.
        Для системных каналов возвращает исходное имя (без префикса),
        для остальных — добавляет префикс.
        """
        return channel if channel in self._SYSTEM_CHANNELS else self._make_key(channel)

    """Клиент Redis для синхронизации состояния WebSocket соединений между серверами."""

    def __init__(self, url: str = None, prefix: str = "liminal", test_mode: bool = False):
        """
        Инициализирует Redis клиент.

        Args:
            url: URL для подключения к Redis (redis://host:port/db).
                 Если не указан, берется из переменной окружения REDIS_URL.
            prefix: Префикс для ключей в Redis, чтобы избежать конфликтов.
            test_mode: Режим тестирования, если True - получает сообщения от того же инстанса.
        """
        self.url = url or os.environ.get("REDIS_URL", "redis://localhost:6379/0")
        self.prefix = prefix
        self.redis: Redis | None = None
        self.pubsub = None
        self.instance_id = str(uuid.uuid4())[:8]  # Уникальный ID для этого инстанса
        self.subscription_callbacks = {}
        self._pubsub_task = None
        self.test_mode = test_mode
        logger.info(f"Инициализирован Redis клиент с ID: {self.instance_id}")

    async def connect(self) -> bool:
        """
        Устанавливает соединение с Redis.

        Returns:
            bool: True если соединение успешно, иначе False.
        """
        try:
            self.redis = redis.from_url(self.url, decode_responses=True)
            # Проверка соединения
            await self.redis.ping()
            self.pubsub = self.redis.pubsub()

            # Обновляем метрики - соединение установлено
            if METRICS_ENABLED:
                redis_connection_status.labels(instance_id=self.instance_id).set(1)
                logger.debug(
                    f"Redis connection metrics updated: connected (instance: {self.instance_id})"
                )

            logger.info(f"Успешное подключение к Redis: {self.url}")
            return True
        except RedisError as e:
            # Обновляем метрики - ошибка соединения
            if METRICS_ENABLED:
                redis_connection_status.labels(instance_id=self.instance_id).set(0)
                redis_errors_total.labels(type="connection").inc()
                logger.debug(
                    f"Redis connection metrics updated: disconnected (instance: {self.instance_id})"
                )

            logger.error(f"Ошибка подключения к Redis: {str(e)}")
            return False

    async def close(self):
        """Закрывает соединение с Redis."""
        if self.pubsub:
            await self.pubsub.aclose()

        if self._pubsub_task:
            self._pubsub_task.cancel()
            with contextlib.suppress(asyncio.CancelledError):
                await self._pubsub_task

        if self.redis:
            await self.redis.aclose()

            # Обновляем метрики - соединение закрыто
            if METRICS_ENABLED:
                redis_connection_status.labels(instance_id=self.instance_id).set(0)
                logger.debug(
                    f"Redis connection metrics updated: disconnected (instance: {self.instance_id})"
                )

            logger.info("Соединение с Redis закрыто")

    def _make_key(self, key: str) -> str:
        """
        Создает полный ключ с префиксом.

        Args:
            key: Базовый ключ.

        Returns:
            str: Ключ с префиксом.
        """
        return f"{self.prefix}:{key}"

    async def set(self, key: str, value: Any, expire: int | None = None) -> bool:
        """
        Устанавливает значение по ключу.

        Args:
            key: Ключ.
            value: Значение (будет сериализовано в JSON).
            expire: Время истечения в секундах (опционально).

        Returns:
            bool: True если операция успешна, иначе False.
        """
        start_time = time.time()
        try:
            full_key = self._make_key(key)
            serialized = json.dumps(value)
            await self.redis.set(full_key, serialized, ex=expire)

            # Обновляем метрики - успешная операция set
            if METRICS_ENABLED:
                redis_operations_total.labels(operation="set", status="success").inc()
                redis_operation_duration_seconds.labels(operation="set").observe(
                    time.time() - start_time
                )

            return True
        except RedisError as e:
            # Обновляем метрики - ошибка операции set
            if METRICS_ENABLED:
                redis_operations_total.labels(operation="set", status="error").inc()
                redis_errors_total.labels(type="operation").inc()

            logger.error(f"Ошибка установки значения {key}: {str(e)}")
            return False

    async def keys(self, pattern: str, use_prefix: bool = True) -> list[str]:
        """
        Возвращает ключи по шаблону.

        Args:
            pattern: Шаблон ключа.
            use_prefix: Добавлять ли префикс экземпляра к шаблону.

        Returns:
            Список ключей.
        """
        try:
            search_pattern = self._make_key(pattern) if use_prefix else pattern
            # redis-py returns strings by default with recent versions
            return await self.redis.keys(search_pattern)
        except RedisError as e:
            logger.error(f"Ошибка получения ключей по шаблону {pattern}: {str(e)}")
            return []

    async def get(self, key: str) -> Any:
        """
        Получает значение по ключу.

        Args:
            key: Ключ.

        Returns:
            Any: Значение, десериализованное из JSON, или None если ключ не найден.
        """
        start_time = time.time()
        try:
            full_key = self._make_key(key)
            value = await self.redis.get(full_key)

            # Обновляем метрики - успешная операция get
            if METRICS_ENABLED:
                redis_operations_total.labels(operation="get", status="success").inc()
                redis_operation_duration_seconds.labels(operation="get").observe(
                    time.time() - start_time
                )

            if value is not None:
                try:
                    return json.loads(value)
                except json.JSONDecodeError:
                    logger.warning(f"Невозможно десериализовать значение ключа {key}")
                    return value  # Возвращаем сырое значение
            return None
        except RedisError as e:
            # Обновляем метрики - ошибка операции get
            if METRICS_ENABLED:
                redis_operations_total.labels(operation="get", status="error").inc()
                redis_errors_total.labels(type="operation").inc()

            logger.error(f"Ошибка получения значения {key}: {str(e)}")
            return None

    async def delete(self, key: str) -> bool:
        """
        Удаляет ключ из Redis.

        Args:
            key: Ключ.

        Returns:
            bool: True если операция успешна, иначе False.
        """
        start_time = time.time()
        try:
            full_key = self._make_key(key)
            await self.redis.delete(full_key)

            # Обновляем метрики - успешное удаление ключа
            if METRICS_ENABLED:
                redis_operations_total.labels(operation="delete", status="success").inc()
                redis_operation_duration_seconds.labels(operation="delete").observe(
                    time.time() - start_time
                )

            return True
        except RedisError as e:
            # Обновляем метрики - ошибка удаления ключа
            if METRICS_ENABLED:
                redis_operations_total.labels(operation="delete", status="error").inc()
                redis_errors_total.labels(type="operation").inc()

            logger.error(f"Ошибка удаления ключа {key}: {str(e)}")
            return False

    async def publish(self, channel: str, message: Any) -> bool:
        """
        Публикует сообщение в канал Redis.

        Args:
            channel: Название канала.
            message: Сообщение (будет сериализовано в JSON).

        Returns:
            bool: True если операция успешна, иначе False.
        """
        start_time = time.time()
        try:
            full_channel = self._full_channel(channel)
            payload = {"data": message, "sender_id": self.instance_id}
            await self.redis.publish(full_channel, json.dumps(payload))

            # Обновляем метрики - успешная публикация сообщения
            if METRICS_ENABLED:
                redis_operations_total.labels(operation="publish", status="success").inc()
                redis_pubsub_messages_total.labels(channel=channel, direction="published").inc()
                redis_operation_duration_seconds.labels(operation="publish").observe(
                    time.time() - start_time
                )

            return True
        except RedisError as e:
            # Обновляем метрики - ошибка публикации сообщения
            if METRICS_ENABLED:
                redis_operations_total.labels(operation="publish", status="error").inc()
                redis_errors_total.labels(type="operation").inc()

            logger.error(f"Ошибка публикации в канал {channel}: {str(e)}")
            return False

    async def subscribe(self, channel: str, callback: Callable[[Any], None]) -> bool:
        """
        Подписывается на канал Redis.

        Args:
            channel: Название канала.
            callback: Функция обратного вызова для обработки сообщений.
                    Должна принимать один аргумент - данные сообщения.

        Returns:
            bool: True если операция успешна, иначе False.
        """
        start_time = time.time()
        try:
            full_channel = self._full_channel(channel)

            # Регистрируем callback
            self.subscription_callbacks[full_channel] = callback

            # Подписываемся на канал
            await self.pubsub.subscribe(full_channel)

            # Запускаем задачу слушателя, если еще не запущена
            if self._pubsub_task is None or self._pubsub_task.done():
                self._pubsub_task = asyncio.create_task(self._message_listener())

            # Обновляем метрики - успешная подписка на канал
            if METRICS_ENABLED:
                redis_operations_total.labels(operation="subscribe", status="success").inc()
                redis_operation_duration_seconds.labels(operation="subscribe").observe(
                    time.time() - start_time
                )

            logger.info(f"Подписка на канал {channel} успешна")
            return True
        except RedisError as e:
            # Обновляем метрики - ошибка подписки на канал
            if METRICS_ENABLED:
                redis_operations_total.labels(operation="subscribe", status="error").inc()
                redis_errors_total.labels(type="operation").inc()

            logger.error(f"Ошибка подписки на канал {channel}: {str(e)}")
            return False

    async def unsubscribe(self, channel: str) -> bool:
        """
        Отписывается от канала Redis.

        Args:
            channel: Название канала.

        Returns:
            bool: True если операция успешна, иначе False.
        """
        start_time = time.time()
        try:
            full_channel = self._full_channel(channel)

            # Отписываемся от канала
            await self.pubsub.unsubscribe(full_channel)

            # Удаляем callback
            if full_channel in self.subscription_callbacks:
                del self.subscription_callbacks[full_channel]

            # Обновляем метрики - успешная отписка от канала
            if METRICS_ENABLED:
                redis_operations_total.labels(operation="unsubscribe", status="success").inc()
                redis_operation_duration_seconds.labels(operation="unsubscribe").observe(
                    time.time() - start_time
                )

            logger.info(f"Отписка от канала {channel} успешна")
            return True
        except RedisError as e:
            # Обновляем метрики - ошибка отписки от канала
            if METRICS_ENABLED:
                redis_operations_total.labels(operation="unsubscribe", status="error").inc()
                redis_errors_total.labels(type="operation").inc()

            logger.error(f"Ошибка отписки от канала {channel}: {str(e)}")
            return False

    async def _message_listener(self):
        """Фоновая задача для обработки входящих сообщений из Redis PubSub."""
        try:
            async for message in self.pubsub.listen():
                if message["type"] == "message":
                    channel = message["channel"]
                    start_time = time.time()

                    try:
                        payload = json.loads(message["data"])
                        sender_id = payload.get("sender_id")

                        # В тестовом режиме игнорируем сообщения от того же инстанса
                        if self.test_mode and sender_id == self.instance_id:
                            logger.debug(
                                f"Игнорируем сообщение от того же инстанса в тестовом режиме: {channel}"
                            )
                            continue

                        # Обновляем метрики - сообщение получено
                        if METRICS_ENABLED:
                            redis_pubsub_messages_total.labels(
                                channel=channel, direction="received"
                            ).inc()

                        if channel in self.subscription_callbacks:
                            callback = self.subscription_callbacks[channel]
                            await callback(payload.get("data"))

                            # Обновляем метрики - успешная обработка сообщения
                            if METRICS_ENABLED:
                                redis_operation_duration_seconds.labels(
                                    operation="message_process"
                                ).observe(time.time() - start_time)
                        else:
                            logger.warning(
                                f"Получено сообщение для канала {channel}, но обработчик не найден"
                            )
                    except json.JSONDecodeError:
                        # Обновляем метрики - ошибка десериализации сообщения
                        if METRICS_ENABLED:
                            redis_errors_total.labels(type="parse").inc()
                        logger.error(f"Невозможно десериализовать сообщение из канала {channel}")
                    except Exception as e:
                        # Обновляем метрики - ошибка обработки сообщения
                        if METRICS_ENABLED:
                            redis_errors_total.labels(type="processing").inc()
                        logger.error(f"Ошибка обработки сообщения из канала {channel}: {str(e)}")
        except asyncio.CancelledError:
            logger.info("Задача прослушивания сообщений отменена")
        except Exception as e:
            # Обновляем метрики - критическая ошибка прослушивания
            if METRICS_ENABLED:
                redis_errors_total.labels(type="listener").inc()
            logger.error(f"Ошибка в задаче прослушивания сообщений: {str(e)}")
            # Пробуем переподключиться через некоторое время
            await asyncio.sleep(5)
            if self.redis:
                self._pubsub_task = asyncio.create_task(self._message_listener())

    # Методы для работы с списками (для очередей сообщений)
    async def list_push(self, key: str, value: Any) -> bool:
        """
        Добавляет значение в конец списка.

        Args:
            key: Ключ списка.
            value: Значение (будет сериализовано в JSON).

        Returns:
            bool: True если операция успешна, иначе False.
        """
        try:
            full_key = self._make_key(key)
            serialized = json.dumps(value)
            await self.redis.rpush(full_key, serialized)
            return True
        except RedisError as e:
            logger.error(f"Ошибка добавления в список {key}: {str(e)}")
            return False

    async def list_pop(self, key: str) -> Any:
        """
        Извлекает и возвращает первый элемент из списка.

        Args:
            key: Ключ списка.

        Returns:
            Any: Значение, десериализованное из JSON, или None если список пуст.
        """
        try:
            full_key = self._make_key(key)
            value = await self.redis.lpop(full_key)
            if value:
                return json.loads(value)
            return None
        except RedisError as e:
            logger.error(f"Ошибка извлечения из списка {key}: {str(e)}")
            return None

    # Методы для работы с наборами (для каналов подписки)
    async def set_add(self, key: str, value: Any) -> bool:
        """
        Добавляет значение в набор.

        Args:
            key: Ключ набора.
            value: Значение (будет сериализовано в JSON).

        Returns:
            bool: True если операция успешна, иначе False.
        """
        try:
            full_key = self._make_key(key)
            serialized = json.dumps(value)
            await self.redis.sadd(full_key, serialized)
            return True
        except RedisError as e:
            logger.error(f"Ошибка добавления в набор {key}: {str(e)}")
            return False

    async def set_remove(self, key: str, value: Any) -> bool:
        """
        Удаляет значение из набора.

        Args:
            key: Ключ набора.
            value: Значение (будет сериализовано в JSON).

        Returns:
            bool: True если операция успешна, иначе False.
        """
        try:
            full_key = self._make_key(key)
            serialized = json.dumps(value)
            await self.redis.srem(full_key, serialized)
            return True
        except RedisError as e:
            logger.error(f"Ошибка удаления из набора {key}: {str(e)}")
            return False

    async def set_members(self, key: str) -> list[Any]:
        """
        Получает все элементы набора.

        Args:
            key: Ключ набора.

        Returns:
            List[Any]: Список элементов набора, десериализованных из JSON.
        """
        try:
            full_key = self._make_key(key)
            values = await self.redis.smembers(full_key)
            result = []
            for value in values:
                try:
                    result.append(json.loads(value))
                except json.JSONDecodeError:
                    logger.warning(f"Невозможно десериализовать значение из набора {key}")
            return result
        except RedisError as e:
            logger.error(f"Ошибка получения набора {key}: {str(e)}")
            return []
