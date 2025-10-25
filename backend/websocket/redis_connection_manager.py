"""
Расширение ConnectionManager для поддержки масштабирования через Redis.
Обеспечивает синхронизацию состояния WebSocket соединений между несколькими экземплярами сервера.
"""

import asyncio
import json
import logging
from typing import Any, Callable, Dict, List, Optional, Set

from fastapi import WebSocket
from logging_config import get_logger

from .connection_manager import ConnectionManager
from .redis_client import RedisClient

logger = get_logger(__name__)


class RedisConnectionManager(ConnectionManager):
    """
    Расширяет базовый ConnectionManager для работы с Redis.
    Поддерживает синхронизацию состояния между несколькими экземплярами сервера.
    """

    def __init__(
        self,
        redis_url: str = None,
        max_connections: int = 100,
        max_connections_per_ip: int = 10,
        redis_prefix: str = "liminal",
    ):
        """
        Инициализирует RedisConnectionManager.

        Args:
            redis_url: URL для подключения к Redis.
            max_connections: Максимальное общее количество соединений.
            max_connections_per_ip: Максимальное количество соединений с одного IP.
            redis_prefix: Префикс для ключей в Redis.
        """
        super().__init__(max_connections, max_connections_per_ip)
        self.redis = RedisClient(redis_url, redis_prefix)
        self._is_connected = False
        self._local_channels = set()  # Каналы, на которые подписан локальный экземпляр
        self.remote_subscriptions: Dict[str, Set[str]] = {}  # Канал -> {ws_id, ...}

        # Ключи Redis
        self._key_global_users = (
            "global_users"  # Список всех подключенных пользователей
        )
        self._key_channel_format = "channel:{}"  # Формат для ключей каналов
        self._key_user_format = "user:{}"  # Формат для ключей пользователей

        # Системные каналы для синхронизации
        self.SUBSCRIBE_CHANNEL = "subscribe"
        self.UNSUBSCRIBE_CHANNEL = "unsubscribe"
        self.BROADCAST_CHANNEL = "broadcast"
        self.DISCONNECT_CHANNEL = "disconnect"

    async def initialize(self) -> bool:
        """
        Инициализирует подключение к Redis и настраивает подписки на каналы.

        Returns:
            bool: True если успешно, иначе False.
        """
        connected = await self.redis.connect()
        if connected:
            self._is_connected = True

            # Подписываемся на системные каналы Redis (глобальные, без префикса)
            # Добавляем подписку на канал с префиксом и без префикса
            broadcast_channel = f"{self.redis.prefix}:broadcast"
            logger.info(f"[ТЕСТ] Подписываемся на канал: {broadcast_channel}")

            # Подписываемся на канал с префиксом напрямую, чтобы обойти двойной префикс
            await self.redis.redis.pubsub().subscribe(
                **{broadcast_channel: self.redis._message_listener}
            )
            self.redis.subscription_callbacks[broadcast_channel] = (
                self._handle_remote_broadcast
            )

            # Старые подписки оставляем для совместимости
            await self.redis.subscribe("broadcast", self._handle_remote_broadcast)
            await self.redis.subscribe("subscribe", self._handle_remote_subscribe)
            await self.redis.subscribe("unsubscribe", self._handle_remote_unsubscribe)
            await self.redis.subscribe("disconnect", self._handle_remote_disconnect)

            logger.info(
                f"RedisConnectionManager успешно инициализирован с ID: {self.redis.instance_id}"
            )
            return True
        else:
            logger.error(
                "Не удалось инициализировать RedisConnectionManager: ошибка подключения к Redis"
            )
            return False

    async def shutdown(self):
        """Закрывает соединение с Redis при завершении работы приложения."""
        if self._is_connected:
            # Очищаем информацию о локальных пользователях из Redis
            for user_id in self.active_connections:
                user_key = self._key_user_format.format(user_id)
                await self.redis.delete(user_key)

            # Очищаем каналы, на которые подписаны только локальные пользователи
            for channel in self._local_channels:
                channel_key = self._key_channel_format.format(channel)
                users = await self.redis.set_members(channel_key)
                if not users:
                    await self.redis.delete(channel_key)

            # Закрываем соединение с Redis
            await self.redis.close()
            self._is_connected = False
            logger.info("RedisConnectionManager shutdown completed")

    async def connect(self, websocket: WebSocket, user_id: str) -> bool:
        """
        Регистрирует новое подключение пользователя и синхронизирует с Redis.

        Args:
            websocket: WebSocket соединение.
            user_id: ID пользователя.

        Returns:
            bool: True если соединение принято, иначе False.
        """
        # Вызываем базовый метод для локальных проверок и соединения
        connected = await super().connect(websocket, user_id)
        if not connected:
            return False

        if self._is_connected:
            # Регистрируем пользователя в глобальном списке
            user_key = self._key_user_format.format(user_id)
            instance_data = {
                "instance_id": self.redis.instance_id,
                "connection_count": len(self.active_connections.get(user_id, set())),
            }
            await self.redis.set(user_key, instance_data)

            # Добавляем в глобальный список пользователей
            await self.redis.set_add(self._key_global_users, user_id)

            logger.info(f"Пользователь {user_id} синхронизирован с Redis")

        return True

    async def disconnect(self, websocket: WebSocket, user_id: str):
        """
        Удаляет отключившееся соединение и синхронизирует с Redis.

        Args:
            websocket: WebSocket соединение.
            user_id: ID пользователя.
        """
        # Вызываем базовый метод для локальной очистки
        await super().disconnect(websocket, user_id)

        if self._is_connected:
            if user_id not in self.active_connections:
                # Пользователь полностью отключился от этого экземпляра
                logger.debug(
                    f"Пользователь {user_id} полностью отключился от локального экземпляра."
                )

                # Удаляем ключ этого экземпляра для пользователя
                user_instance_key = self._key_user_format.format(user_id)
                await self.redis.delete(user_instance_key)

                # Проверяем, есть ли у пользователя активные соединения на других экземплярах
                user_keys_pattern = f"*:user:{user_id}"
                other_user_connections = await self.redis.keys(
                    user_keys_pattern, use_prefix=False
                )

                logger.debug(
                    f"Найдены другие ключи соединений для {user_id}: {other_user_connections}"
                )

                if not other_user_connections:
                    # Если нет других соединений, публикуем событие отключения
                    logger.info(
                        f"Пользователь {user_id} полностью отключился от всех экземпляров. Публикация события."
                    )
                    await self.redis.publish("disconnect", {"user_id": user_id})
                else:
                    logger.debug(
                        f"Пользователь {user_id} все еще активен на других экземплярах. Событие отключения не публикуется."
                    )

            else:
                # У пользователя остались другие соединения на этом экземпляре
                user_key = self._key_user_format.format(user_id)
                instance_data = {
                    "instance_id": self.redis.instance_id,
                    "connection_count": len(
                        self.active_connections.get(user_id, set())
                    ),
                }
                await self.redis.set(user_key, instance_data)
                logger.debug(
                    f"Обновлено количество соединений пользователя {user_id} в Redis"
                )

    async def subscribe(
        self, websocket: WebSocket, channel: str, user_id: Optional[str] = None
    ):
        """Подписывает WebSocket на канал и уведомляет другие инстансы через Redis."""
        await super().subscribe(websocket, channel, user_id)
        ws_id = self.get_websocket_id(websocket)
        logger.info(f"[Redis] Локальная подписка ws {ws_id} на канал {channel}")
        await self.redis.publish(
            self.SUBSCRIBE_CHANNEL,
            {
                "channel": channel,
                "ws_id": ws_id,
                "user_id": user_id,
                "instance_id": self.redis.instance_id,
            },
        )

        if self._is_connected:
            # Добавляем канал в список локальных каналов
            self._local_channels.add(channel)

            # Синхронизируем с Redis
            channel_key = self._key_channel_format.format(channel)
            await self.redis.set_add(channel_key, user_id)

            # Формируем данные для публикации
            subscribe_data = {"user_id": user_id, "channel": channel}

            # Публикуем событие для других экземпляров
            publish_result = await self.redis.publish("subscribe", subscribe_data)

            logger.debug(
                f"[TEST] Подписка пользователя {user_id} на канал {channel} синхронизирована с Redis. "
                f"Результат публикации: {publish_result}"
            )

        return True

    async def unsubscribe(self, websocket: WebSocket, channel: str):
        """Отписывает WebSocket от канала и уведомляет другие инстансы."""
        ws_id = self.get_websocket_id(websocket)
        await super().unsubscribe(websocket, channel)
        logger.info(f"[Redis] Локальная отписка ws {ws_id} от канала {channel}")
        await self.redis.publish(
            self.UNSUBSCRIBE_CHANNEL,
            {"channel": channel, "ws_id": ws_id, "instance_id": self.redis.instance_id},
        )

        if self._is_connected:
            # Синхронизируем с Redis
            channel_key = self._key_channel_format.format(channel)
            await self.redis.set_remove(channel_key, ws_id)

            # Публикуем событие для других экземпляров
            await self.redis.publish(
                "unsubscribe", {"channel": channel, "ws_id": ws_id}
            )

            # Проверяем, остались ли подписчики канала в локальном экземпляре
            if (
                channel in self.channel_subscriptions
                and not self.channel_subscriptions[channel]
            ):
                self._local_channels.discard(channel)

            logger.debug(
                f"Отписка пользователя от канала {channel} синхронизирована с Redis"
            )

    async def _local_broadcast(self, channel: str, message: Any):
        """
        Отправляет сообщение всем локальным подписчикам канала.
        """
        if channel in self.channel_subscriptions:
            subscribers = self.channel_subscriptions[channel]
            for user_id in list(subscribers):
                if user_id in self.active_connections:
                    for websocket in self.active_connections[user_id]:
                        try:
                            await websocket.send_json(message)
                        except Exception as e:
                            logger.error(
                                f"Ошибка при локальной отправке broadcast: {e}"
                            )

    async def broadcast(
        self, channel: str, message: Any, user_id: Optional[str] = None
    ):
        """
        Отправляет сообщение всем подписчикам канала.
        Если подключен Redis, сообщение отправляется локально и публикуется для других экземпляров.
        В противном случае, используется локальная отправка.
        """
        logger.debug(f"Начало broadcast в канал '{channel}'")
        if self._is_connected:
            # 1. Отправляем локальным подписчикам немедленно, чтобы избежать задержек и проблем с self-messaging.
            await self._local_broadcast(channel, message)

            # 2. Публикуем в Redis для других экземпляров.
            # Сообщения от этого же instance_id будут игнорироваться в _message_listener.
            await self.redis.publish(
                "broadcast", {"channel": channel, "message": message}
            )
            logger.debug(
                f"Сообщение для канала '{channel}' отправлено локально и опубликовано в Redis"
            )
        else:
            # Redis не подключен, используем стандартный broadcast из базового класса.
            await self._local_broadcast(channel, message)
            logger.debug(
                f"Сообщение для канала '{channel}' отправлено локально (Redis отключен)"
            )

    async def send_personal_message(self, user_id: str, message: Dict[str, Any]):
        """
        Отправляет личное сообщение пользователю с учетом его расположения на разных серверах.

        Args:
            user_id: ID пользователя.
            message: Сообщение для отправки.
        """
        # Проверяем, есть ли пользователь локально
        local_sent = False
        if user_id in self.active_connections:
            # Отправляем локально
            await super().send_personal_message(user_id, message)
            local_sent = True

        if self._is_connected:
            # Проверяем, есть ли пользователь на других серверах
            user_key = self._key_user_format.format(user_id)
            user_data = await self.redis.get(user_key)

            if user_data and user_data.get("instance_id") != self.redis.instance_id:
                # Пользователь находится на другом сервере, отправляем через Redis
                await self.redis.publish(
                    "personal_message", {"user_id": user_id, "message": message}
                )
                logger.debug(f"Личное сообщение для {user_id} отправлено через Redis")
                return

    async def _handle_remote_broadcast(self, data: dict):
        """
        Обрабатывает широковещательную отправку и пересылает сообщение локальным подписчикам.

        Args:
            data: Словарь с данными сообщения, содержащий поля 'channel' и 'message'
        """
        if not isinstance(data, dict):
            logger.error(
                f"[Redis] _handle_remote_broadcast получил некорректные данные: {data}"
            )
            return

        channel = data.get("channel")
        message = data.get("message")
        if not channel or not message:
            logger.warning(
                f"[Redis] _handle_remote_broadcast: отсутствует channel или message в данных: {data}"
            )
            return

        logger.debug(
            f"[Redis] Получен remote_broadcast для канала {channel}. Локальные подписчики: {self.subscriptions.get(channel)}"
        )
        if channel in self.subscriptions:
            for websocket in self.subscriptions[channel]:
                logger.debug(
                    f"[Redis] Отправка сообщения ws {self.get_websocket_id(websocket)} в канале {channel}"
                )
                await self.send(websocket, message)

    async def _handle_remote_subscribe(self, data: dict):
        """
        Обрабатывает удаленную подписку, добавляя ws_id в список удаленных подписчиков.

        Args:
            data: Словарь с данными подписки, содержащий поля 'channel' и 'ws_id'
        """
        if not isinstance(data, dict):
            logger.error(
                f"[Redis] _handle_remote_subscribe получил некорректные данные: {data}"
            )
            return

        channel = data.get("channel")
        ws_id = data.get("ws_id")
        if not channel or not ws_id:
            logger.warning(
                f"[Redis] _handle_remote_subscribe: отсутствует channel или ws_id в данных: {data}"
            )
            return

        if channel not in self.remote_subscriptions:
            self.remote_subscriptions[channel] = set()
        self.remote_subscriptions[channel].add(ws_id)
        logger.debug(
            f"[Redis] Удаленная подписка ws {ws_id} на канал {channel} обработана"
        )

    async def _handle_remote_unsubscribe(self, data: dict):
        """
        Обрабатывает удаленную отписку, удаляя ws_id из списка удаленных подписчиков.

        Args:
            data: Словарь с данными отписки, содержащий поля 'channel' и 'ws_id'
        """
        if not isinstance(data, dict):
            logger.error(
                f"[Redis] _handle_remote_unsubscribe получил некорректные данные: {data}"
            )
            return

        channel = data.get("channel")
        ws_id = data.get("ws_id")
        if not channel or not ws_id:
            logger.warning(
                f"[Redis] _handle_remote_unsubscribe: отсутствует channel или ws_id в данных: {data}"
            )
            return

        if (
            channel in self.remote_subscriptions
            and ws_id in self.remote_subscriptions[channel]
        ):
            self.remote_subscriptions[channel].remove(ws_id)
            if not self.remote_subscriptions[channel]:
                del self.remote_subscriptions[channel]
            logger.debug(
                f"[Redis] Удаленная отписка ws {ws_id} от канала {channel} обработана"
            )

    async def _handle_remote_disconnect(self, data: Dict[str, Any]):
        """
        Обрабатывает событие отключения пользователя от другого экземпляра.

        Args:
            data: Данные события, содержащие поле 'user_id'.
        """
        logger.debug(f"[TEST] Получено событие удаленного отключения: {data}")

        if not isinstance(data, dict):
            logger.error(
                f"[Redis] _handle_remote_disconnect получил некорректные данные: {data}"
            )
            return

        if not data:
            logger.warning("[TEST] Пустые данные в _handle_remote_disconnect")
            return

        user_id = data.get("user_id")

        if not user_id:
            logger.warning("[TEST] Отсутствует user_id в данных отключения")
            return

        # Проверяем, есть ли активные соединения пользователя на этом экземпляре
        has_local_connections = (
            user_id in self.active_connections
            and len(self.active_connections[user_id]) > 0
        )
        logger.debug(
            f"[TEST] Пользователь {user_id} имеет локальные соединения: {has_local_connections}"
        )

        # Если у пользователя есть активные соединения на этом экземпляре, не удаляем его из подписок
        if has_local_connections:
            logger.debug(
                f"[TEST] Пользователь {user_id} имеет активные соединения, не удаляем из подписок"
            )
            return

        # Если нет активных соединений, удаляем пользователя из всех каналов
        if user_id in self.user_channels:
            # Удаляем пользователя из всех каналов
            channels = list(self.user_channels[user_id])
            for ws_channel in channels:
                if ws_channel in self.channel_subscriptions:
                    self.channel_subscriptions[ws_channel].discard(user_id)
                    if not self.channel_subscriptions[ws_channel]:
                        del self.channel_subscriptions[ws_channel]
                    logger.debug(
                        f"[TEST] Удален пользователь {user_id} из канала {ws_channel}"
                    )

            del self.user_channels[user_id]
            logger.debug(
                f"[TEST] Синхронизировано удаленное отключение пользователя {user_id}"
            )
        else:
            logger.debug(f"[TEST] Пользователь {user_id} не найден в user_channels")

    async def get_connection_stats(self) -> Dict[str, Any]:
        """
        Получает расширенную статистику соединений с учетом всех экземпляров.

        Returns:
            Dict[str, Any]: Статистика соединений.
        """
        # Получаем локальную статистику
        stats = await super().get_connection_stats()

        if self._is_connected:
            try:
                # Дополняем глобальной статистикой из Redis
                global_users = await self.redis.set_members(self._key_global_users)

                # Собираем информацию по экземплярам
                instances = {}
                total_connections = 0

                for user_id in global_users:
                    user_key = self._key_user_format.format(user_id)
                    user_data = await self.redis.get(user_key)

                    if user_data:
                        instance_id = user_data.get("instance_id")
                        connection_count = user_data.get("connection_count", 0)

                        if instance_id not in instances:
                            instances[instance_id] = {"users": 0, "connections": 0}

                        instances[instance_id]["users"] += 1
                        instances[instance_id]["connections"] += connection_count
                        total_connections += connection_count

                # Добавляем в статистику
                stats.update(
                    {
                        "global_unique_users": len(global_users),
                        "global_total_connections": total_connections,
                        "instances": instances,
                        "is_distributed": True,
                        "current_instance_id": self.redis.instance_id,
                    }
                )

            except Exception as e:
                logger.error(f"Ошибка получения глобальной статистики: {str(e)}")
                stats["redis_error"] = str(e)
        else:
            stats["is_distributed"] = False

        return stats
