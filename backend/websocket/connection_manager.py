"""
Менеджер WebSocket соединений.
Отвечает за хранение активных подключений и широковещательную рассылку.
"""

import asyncio
import logging
import os
import sys
import time
from datetime import datetime
from typing import Callable, Dict, List, Optional, Set

from fastapi import WebSocket
from loguru import logger

try:
    from .redis_client import RedisClient
except Exception:
    # Redis может отсутствовать в среде; предоставляем минимальную заглушку,
    # чтобы базовый функционал WebSocket и readiness не падал на импорте.
    class RedisClient:  # type: ignore
        def __init__(self, *args, **kwargs):
            self.redis = None

# Условный импорт метрик Prometheus
try:
    from metrics import websocket_rate_limit_total  # Новая метрика
    from metrics import websocket_heartbeat_total, websocket_idle_disconnects_total
    from metrics import (
        connection_limits,
        connection_rejections,
        websocket_broadcast_duration_seconds,
        websocket_connections,
        websocket_messages_total,
    )

    METRICS_ENABLED = True
    logger.info("Prometheus metrics module loaded successfully")
except ImportError:
    logger.warning("Prometheus metrics module not found, metrics collection disabled")
    METRICS_ENABLED = False

    # Заглушки для метрик
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

    # Создаем заглушки
    websocket_connections = DummyMetric()
    websocket_messages_total = DummyMetric()
    connection_rejections = DummyMetric()
    connection_limits = DummyMetric()
    websocket_broadcast_duration_seconds = DummyMetric()
    websocket_rate_limit_total = DummyMetric()

logger = logging.getLogger("websocket.connection_manager")


class ConnectionManager:
    """Управляет активными WebSocket соединениями."""

    def __init__(
        self,
        redis_client: Optional[RedisClient] = None,
        max_connections: int = 100,
        max_connections_per_ip: int = 10,
        rate_limit_messages_per_second: int = 10,
        rate_limit_burst: int = 20,
        heartbeat_interval: int = 15,
        heartbeat_timeout: int = 45,
        idle_timeout: int = 60,
    ):
        # Зависимости
        self.redis_client = redis_client
        # Совместимость с health/ready: наличие атрибута redis
        self.redis = getattr(redis_client, "redis", None) if redis_client else None

        # Хранилище активных подключений: {user_id: {websocket1, websocket2, ...}}
        self.active_connections: Dict[str, Set[WebSocket]] = {}
        # Хранилище подписок: {channel: {user_id1, user_id2, ...}}
        self.channel_subscriptions: Dict[str, Set[str]] = {}
        # Хранит каналы, на которые подписан пользователь: {user_id: {channel1, channel2, ...}}
        self.user_channels: Dict[str, Set[str]] = {}
        # Хранит WebSocket соединение для каждого пользователя: {user_id: websocket}
        self.user_to_websocket: Dict[str, WebSocket] = {}

        # Ограничения подключений
        self.max_connections = max_connections
        self.max_connections_per_ip = max_connections_per_ip
        self.connections_per_ip: Dict[str, int] = {}
        self.pending_connections: Dict[WebSocket, datetime] = {}
        self.auth_timeout = 30

        # Rate Limiting (Token Bucket)
        self.rate_limit_messages_per_second = rate_limit_messages_per_second
        self.rate_limit_burst = rate_limit_burst
        self.rate_limit_script_sha = None

        # Heartbeat / idle tracking
        self.heartbeat_interval = heartbeat_interval
        self.heartbeat_timeout = heartbeat_timeout
        self.idle_timeout = idle_timeout
        self._last_activity: Dict[WebSocket, float] = {}
        self._last_pong: Dict[WebSocket, float] = {}
        self._heartbeat_tasks: Dict[WebSocket, asyncio.Task] = {}

        # Инициализируем метрики ограничений подключений
        if METRICS_ENABLED:
            connection_limits.labels(type="total").set(0)
            connection_limits.labels(type="max_total").set(self.max_connections)
            connection_limits.labels(type="max_per_ip").set(self.max_connections_per_ip)

        # Загружаем Lua-скрипт для Rate Limiting (только если доступен Redis)
        if self.redis_client is not None:
            asyncio.create_task(self._load_rate_limit_script())

    async def connect(self, websocket: WebSocket, user_id: str):
        """Регистрирует новое подключение пользователя."""
        ip_address = websocket.client.host
        if ip_address not in self.connections_per_ip:
            self.connections_per_ip[ip_address] = 0
        if self.connections_per_ip[ip_address] >= self.max_connections_per_ip:
            logger.warning(f"Превышено количество подключений с IP {ip_address}")
            if METRICS_ENABLED:
                connection_rejections.labels(reason="max_connections_per_ip").inc()
            await websocket.close(code=1008, reason="Too many connections from this IP")
            return False
        if len(self.active_connections) >= self.max_connections:
            logger.warning("Превышено общее количество подключений")
            if METRICS_ENABLED:
                connection_rejections.labels(reason="max_connections").inc()
            await websocket.close(code=1008, reason="Server connection limit reached")
            return False

        await websocket.accept()

        # Увеличиваем счётчик подключений с этого IP
        self.connections_per_ip[ip_address] += 1

        if user_id not in self.active_connections:
            self.active_connections[user_id] = set()
        self.active_connections[user_id].add(websocket)

        # Сохраняем IP для этого websocket (для отслеживания при отключении)
        websocket.client_ip = ip_address
        # Сохраняем user_id в соединении для удобства
        websocket.user_id = user_id

        # Инициализируем активность/последний pong
        now = time.time()
        self._last_activity[websocket] = now
        self._last_pong[websocket] = now
        # Запускаем heartbeat-монитор
        self._start_heartbeat(websocket)

        # Обновляем метрики
        if METRICS_ENABLED:
            websocket_connections.labels(channel="all", authenticated="true").inc()
            connection_limits.labels(type="total").set(len(self.active_connections))

        logger.info(
            f"Пользователь {user_id} подключился с IP {ip_address}. Всего активных подключений: {len(self.active_connections)}"
        )
        return True

    async def disconnect(self, websocket: WebSocket, user_id: str):
        """Удаляет отключившееся соединение и отписывает от всех каналов."""
        # Останавливаем heartbeat, если запущен
        self._stop_heartbeat(websocket)
        if user_id in self.active_connections:
            self.active_connections[user_id].discard(websocket)
            if not self.active_connections[user_id]:
                del self.active_connections[user_id]

        # Уменьшаем счётчик подключений с этого IP
        if hasattr(websocket, "client_ip"):
            ip_address = websocket.client_ip
            if ip_address in self.connections_per_ip:
                self.connections_per_ip[ip_address] -= 1
                if self.connections_per_ip[ip_address] <= 0:
                    del self.connections_per_ip[ip_address]
                # Удаляем пользователя из всех каналов при отключении
                if user_id in self.user_channels:
                    channels_to_unsubscribe = list(self.user_channels[user_id])
                    logger.debug(
                        f"Отключаем пользователя {user_id} от каналов: {channels_to_unsubscribe}"
                    )
                    for channel in channels_to_unsubscribe:
                        await self.unsubscribe(user_id, channel)
                    # user_channels for this user should be empty now
                    if user_id in self.user_channels:
                        del self.user_channels[user_id]

            # Обновляем метрики
            if METRICS_ENABLED:
                websocket_connections.labels(channel="all", authenticated="true").dec()
                connection_limits.labels(type="total").set(len(self.active_connections))

            logger.info(
                f"Пользователь {user_id} отключился. Осталось подключений: {len(self.active_connections)}"
            )

        # Чистим состояние heartbeat
        self._last_activity.pop(websocket, None)
        self._last_pong.pop(websocket, None)

    async def send_personal_message(self, message: dict, user_id: str):
        """Отправляет личное сообщение конкретному пользователю."""
        if user_id in self.active_connections:
            for connection in self.active_connections[user_id]:
                try:
                    await connection.send_json(message)
                except Exception as e:
                    logger.error(
                        f"Ошибка отправки сообщения пользователю {user_id}: {e}"
                    )

    async def broadcast(self, channel: str, message: dict, sender_id: str = None):
        """
        Отправляет сообщение всем подписчикам канала.
        """
        start_time = time.time()
        logger.info(f"--- BROADCAST START ---")
        logger.info(
            f"SERVER DEBUG: broadcast в канал '{channel}', sender_id='{sender_id}'"
        )
        logger.info(f"SERVER DEBUG: message: {message}")
        logger.info(
            f"SERVER DEBUG: channel_subscriptions: {self.channel_subscriptions}"
        )
        logger.info(
            f"SERVER DEBUG: active_connections: {list(self.active_connections.keys())}"
        )
        logger.info(f"SERVER DEBUG: user_channels: {self.user_channels}")

        if channel not in self.channel_subscriptions:
            logger.warning(
                f"Попытка отправить сообщение в несуществующий канал: {channel}"
            )
            return 0

        sent_count = 0
        subscribers = list(self.channel_subscriptions.get(channel, []))
        logger.info(f"SERVER DEBUG: Subscribers for channel '{channel}': {subscribers}")

        for user_id in subscribers:
            if sender_id and user_id == sender_id:
                continue
            if user_id in self.active_connections:
                connections = list(self.active_connections[user_id])
                for connection in connections:
                    logger.info(
                        f"SERVER DEBUG: Пытаюсь отправить user_id='{user_id}', websocket={connection}"
                    )
                    try:
                        await connection.send_json(message)
                        sent_count += 1
                        logger.info(
                            f"SERVER DEBUG: Успешно отправлено user_id='{user_id}'"
                        )
                    except Exception as e:
                        logger.error(
                            f"SERVER DEBUG: Ошибка при отправке user_id='{user_id}': {e}"
                        )
                        self.active_connections[user_id].discard(connection)
                        if not self.active_connections[user_id]:
                            del self.active_connections[user_id]

        # Обновляем метрики
        if METRICS_ENABLED:
            message_type = message.get("type", "unknown")
            websocket_messages_total.labels(
                type=message_type, direction="out", channel=channel
            ).inc(sent_count)
            websocket_broadcast_duration_seconds.labels(
                message_type=message_type
            ).observe(time.time() - start_time)

        logger.info(
            f"SERVER DEBUG: Сообщение отправлено {sent_count} подписчикам канала '{channel}'"
        )
        logger.info(f"--- BROADCAST END ---")
        return sent_count

    def mark_activity(self, websocket: WebSocket):
        """Помечает, что от клиента пришла активность (любое сообщение)."""
        self._last_activity[websocket] = time.time()

    def mark_pong(self, websocket: WebSocket):
        """Помечает получение pong от клиента."""
        now = time.time()
        self._last_pong[websocket] = now
        if METRICS_ENABLED:
            websocket_heartbeat_total.labels(event="pong_received").inc()

    def _start_heartbeat(self, websocket: WebSocket):
        if websocket in self._heartbeat_tasks:
            return
        task = asyncio.create_task(self._heartbeat_loop(websocket))
        self._heartbeat_tasks[websocket] = task

    def _stop_heartbeat(self, websocket: WebSocket):
        task = self._heartbeat_tasks.pop(websocket, None)
        if task and not task.done():
            task.cancel()

    async def _heartbeat_loop(self, websocket: WebSocket):
        """Периодически отправляет ping и отключает неответившие/бездействующие соединения."""
        try:
            while True:
                await asyncio.sleep(self.heartbeat_interval)
                # Отправляем application-level ping
                try:
                    await websocket.send_json({"type": "ping", "ts": datetime.utcnow().isoformat()})
                    if METRICS_ENABLED:
                        websocket_heartbeat_total.labels(event="ping_sent").inc()
                except Exception as e:
                    logger.warning(f"Не удалось отправить ping: {e}")
                    break

                now = time.time()
                last_pong = self._last_pong.get(websocket, 0)
                last_act = self._last_activity.get(websocket, 0)

                # Проверка на отсутствие pong
                if now - last_pong > self.heartbeat_timeout:
                    reason = "Missing pong"
                    if METRICS_ENABLED:
                        websocket_heartbeat_total.labels(event="timeout_disconnect").inc()
                        websocket_idle_disconnects_total.labels(reason="missing_pong").inc()
                    try:
                        await websocket.close(code=1001, reason=reason)
                    finally:
                        user_id = self.get_user_id_from_websocket(websocket) or "unknown"
                        await self.disconnect(websocket, user_id)
                    break

                # Проверка простоя
                if now - last_act > self.idle_timeout:
                    reason = "Idle timeout"
                    if METRICS_ENABLED:
                        websocket_heartbeat_total.labels(event="timeout_disconnect").inc()
                        websocket_idle_disconnects_total.labels(reason="idle_timeout").inc()
                    try:
                        await websocket.close(code=1001, reason=reason)
                    finally:
                        user_id = self.get_user_id_from_websocket(websocket) or "unknown"
                        await self.disconnect(websocket, user_id)
                    break
        except asyncio.CancelledError:
            # Нормальное завершение
            pass

    def is_user_subscribed(self, user_id: str, channel: str) -> bool:
        """
        Проверяет, подписан ли пользователь на канал.

        Args:
            user_id: ID пользователя
            channel: Название канала

        Returns:
            bool: True если пользователь подписан, False иначе
        """
        return (
            channel in self.channel_subscriptions
            and user_id in self.channel_subscriptions[channel]
        )

    async def subscribe(self, user_id: str, channel: str, websocket: WebSocket):
        """
        Подписывает пользователя на канал.

        Args:
            user_id: ID пользователя
            channel: Название канала
            websocket: WebSocket соединение пользователя
        """
        if not user_id or not channel:
            raise ValueError("User ID and channel are required")

        if channel not in self.channel_subscriptions:
            self.channel_subscriptions[channel] = set()
        self.channel_subscriptions[channel].add(user_id)

        if user_id not in self.user_channels:
            self.user_channels[user_id] = set()
        self.user_channels[user_id].add(channel)

        # Добавляем соединение, если его еще нет
        if user_id not in self.active_connections:
            self.active_connections[user_id] = set()
        self.active_connections[user_id].add(websocket)

        # Обновляем метрики
        if METRICS_ENABLED:
            websocket_connections.labels(channel=channel, authenticated="true").inc()

        logger.info(f"Пользователь {user_id} подписан на канал {channel}")

    async def unsubscribe(self, user_id: str, channel: str):
        """
        Отписывает пользователя от канала.
        """
        if not user_id or not channel:
            raise ValueError("User ID and channel are required")

        logger.info(f"--- UNSUBSCRIBE START ---")
        logger.info(f"SERVER DEBUG: Попытка отписать '{user_id}' от '{channel}'")
        logger.info(f"SERVER DEBUG: user_channels до отписки: {self.user_channels}")
        logger.info(
            f"SERVER DEBUG: channel_subscriptions до отписки: {self.channel_subscriptions}"
        )

        # Проверяем, был ли пользователь подписан на канал
        was_subscribed = (
            channel in self.channel_subscriptions
            and user_id in self.channel_subscriptions[channel]
        )

        if channel in self.channel_subscriptions:
            self.channel_subscriptions[channel].discard(user_id)
            if not self.channel_subscriptions[channel]:
                del self.channel_subscriptions[channel]

        if user_id in self.user_channels:
            self.user_channels[user_id].discard(channel)
            if not self.user_channels[user_id]:
                del self.user_channels[user_id]

        # Обновляем метрики только если пользователь был подписан
        if was_subscribed and METRICS_ENABLED:
            websocket_connections.labels(channel=channel, authenticated="true").dec()

        logger.info(f"Пользователь '{user_id}' отписан от канала '{channel}'")
        logger.info(f"SERVER DEBUG: user_channels ПОСЛЕ отписки: {self.user_channels}")
        logger.info(
            f"SERVER DEBUG: channel_subscriptions ПОСЛЕ отписки: {self.channel_subscriptions}"
        )
        logger.info(f"--- UNSUBSCRIBE END ---")

    def get_user_channels(self, user_id: str) -> List[str]:
        """Возвращает список каналов, на которые подписан пользователь."""
        return list(self.user_channels.get(user_id, set()))

    def get_channel_subscribers(self, channel: str) -> List[str]:
        """Возвращает список подписчиков канала."""
        return list(self.channel_subscriptions.get(channel, set()))

    def get_user_count(self) -> int:
        """Возвращает количество активных пользователей."""
        return len(self.active_connections)

    def get_connection_stats(self) -> dict:
        """Возвращает статистику подключений."""
        total_connections = sum(
            len(connections) for connections in self.active_connections.values()
        )
        return {
            "total_users": len(self.active_connections),
            "total_connections": total_connections,
            "connections_per_ip": dict(self.connections_per_ip),
            "max_connections": self.max_connections,
            "max_connections_per_ip": self.max_connections_per_ip,
            "channels": list(self.channel_subscriptions.keys()),
            "total_channels": len(self.channel_subscriptions),
            "pending_connections": len(self.pending_connections),
        }

    async def accept_pending_connection(self, websocket: WebSocket) -> bool:
        """Принимает WebSocket соединение и помещает его в ожидание аутентификации."""
        ip_address = websocket.client.host

        # Проверяем лимиты
        if ip_address not in self.connections_per_ip:
            self.connections_per_ip[ip_address] = 0
        if self.connections_per_ip[ip_address] >= self.max_connections_per_ip:
            logger.warning(f"Превышено количество подключений с IP {ip_address}")
            await websocket.close(code=1008, reason="Too many connections from this IP")
            return False

        total_connections = len(self.active_connections) + len(self.pending_connections)
        if total_connections >= self.max_connections:
            logger.warning("Превышено общее количество подключений")
            await websocket.close(code=1008, reason="Server connection limit reached")
            return False

        await websocket.accept()

        # Увеличиваем счётчик подключений с этого IP
        self.connections_per_ip[ip_address] += 1

        # Добавляем в ожидающие аутентификации
        self.pending_connections[websocket] = datetime.utcnow()
        websocket.client_ip = ip_address

        logger.info(
            f"WebSocket соединение принято с IP {ip_address}. Ожидает аутентификации."
        )

        # Запускаем таймер для автоматического отключения неаутентифицированных соединений
        asyncio.create_task(self._auth_timeout_handler(websocket))

        return True

    async def authenticate_connection(self, websocket: WebSocket, user_id: str) -> bool:
        """Аутентифицирует WebSocket соединение."""
        if websocket not in self.pending_connections:
            logger.warning("Попытка аутентификации несуществующего соединения")
            return False

        # Удаляем из ожидающих
        del self.pending_connections[websocket]

        # Добавляем в активные соединения
        if user_id not in self.active_connections:
            self.active_connections[user_id] = set()
        self.active_connections[user_id].add(websocket)

        # Сохраняем user_id в websocket для удобства
        websocket.user_id = user_id

        logger.info(f"Пользователь {user_id} успешно аутентифицирован")
        return True

    async def reject_connection(
        self, websocket: WebSocket, reason: str = "Authentication failed"
    ):
        """Отклоняет WebSocket соединение."""
        if websocket in self.pending_connections:
            del self.pending_connections[websocket]

        # Уменьшаем счётчик подключений с этого IP
        if hasattr(websocket, "client_ip"):
            ip_address = websocket.client_ip
            if ip_address in self.connections_per_ip:
                self.connections_per_ip[ip_address] -= 1
                if self.connections_per_ip[ip_address] <= 0:
                    del self.connections_per_ip[ip_address]

        await websocket.close(code=1008, reason=reason)
        logger.info(f"WebSocket соединение отклонено: {reason}")

    async def _auth_timeout_handler(self, websocket: WebSocket):
        """Обработчик таймаута аутентификации."""
        await asyncio.sleep(self.auth_timeout)

        if websocket in self.pending_connections:
            logger.warning(
                f"Таймаут аутентификации для соединения с IP {getattr(websocket, 'client_ip', 'unknown')}"
            )
            await self.reject_connection(websocket, "Authentication timeout")

    def is_authenticated(self, websocket: WebSocket) -> bool:
        """Проверяет, аутентифицировано ли соединение."""
        return hasattr(websocket, "user_id") and websocket.user_id

    def get_user_id_from_websocket(self, websocket: WebSocket) -> Optional[str]:
        """Получает user_id из WebSocket соединения."""
        return getattr(websocket, "user_id", None)

    def get_websocket_id(self, websocket: WebSocket) -> str:
        """Возвращает уникальный идентификатор для WebSocket соединения."""
        return str(id(websocket))

    async def is_rate_limited(
        self,
        user_id: Optional[str] = None,
        ip_address: Optional[str] = None,
        websocket: Optional[WebSocket] = None,
    ) -> bool:
        """Проверяет лимиты для пользователя, IP и конкретного соединения.

        Возвращает True, если превышен лимит по любому из ключей.
        Если скрипт не загружен или Redis недоступен, возвращает False (лимитирование отключено).
        """
        # Если скрипт не загружен, не ограничиваем
        if not self.rate_limit_script_sha or not getattr(self.redis_client, "redis", None):
            return False

        keys: List[str] = []
        if user_id:
            keys.append(self.redis_client._make_key(f"rate_limit:user:{user_id}"))
        if ip_address:
            keys.append(self.redis_client._make_key(f"rate_limit:ip:{ip_address}"))
        if websocket is not None:
            keys.append(
                self.redis_client._make_key(
                    f"rate_limit:conn:{self.get_websocket_id(websocket)}"
                )
            )

        if not keys:
            return False

        now = time.time()
        rate = str(self.rate_limit_messages_per_second)
        burst = str(self.rate_limit_burst)

        for key in keys:
            try:
                allowed = await self.redis_client.redis.evalsha(
                    self.rate_limit_script_sha, 1, key, rate, burst, now
                )
            except Exception as e:
                # Если скрипт удалён из кэша Redis (NOSCRIPT), пробуем загрузить снова и повторить
                if "NOSCRIPT" in str(e):
                    await self._load_rate_limit_script()
                    if not self.rate_limit_script_sha:
                        logger.warning(
                            "Rate limiting disabled: Lua script not available after reload"
                        )
                        return False
                    try:
                        allowed = await self.redis_client.redis.evalsha(
                            self.rate_limit_script_sha, 1, key, rate, burst, now
                        )
                    except Exception as e2:
                        logger.error(f"Rate limiting eval failed after reload: {e2}")
                        return False
                else:
                    logger.error(f"Rate limiting eval failed: {e}")
                    return False

            if allowed == 0 or allowed == "0":
                if METRICS_ENABLED and user_id:
                    try:
                        websocket_rate_limit_total.labels(user_id=user_id).inc()
                    except Exception:
                        pass
                return True

        return False

    async def _load_rate_limit_script(self):
        """Загружает и кэширует Lua-скрипт для Rate Limiting в Redis."""
        # Атомарный Lua-скрипт для Token Bucket алгоритма
        lua_script = """
            local key = KEYS[1]
            local rate = tonumber(ARGV[1])
            local burst = tonumber(ARGV[2])
            local now = tonumber(ARGV[3])

            local data = redis.call('hgetall', key)
            local tokens
            local last_refill

            if #data == 0 then
                tokens = burst
                last_refill = now
            else
                for i=1,#data,2 do
                    if data[i] == 'tokens' then
                        tokens = tonumber(data[i+1])
                    elseif data[i] == 'last_refill' then
                        last_refill = tonumber(data[i+1])
                    end
                end
            end

            local time_passed = now - last_refill
            local new_tokens = time_passed * rate
            tokens = math.min(burst, tokens + new_tokens)
            last_refill = now

            if tokens >= 1 then
                tokens = tokens - 1
                redis.call('hmset', key, 'tokens', tokens, 'last_refill', last_refill)
                redis.call('expire', key, math.ceil(burst / rate) * 2) -- Устанавливаем TTL
                return 1 -- Разрешено
            else
                return 0 -- Запрещено
            end
        """
        try:
            self.rate_limit_script_sha = await self.redis_client.redis.script_load(
                lua_script
            )
            logger.info(
                f"Lua-скрипт для Rate Limiting успешно загружен. SHA: {self.rate_limit_script_sha}"
            )
        except Exception as e:
            logger.error(
                f"Не удалось загрузить Lua-скрипт для Rate Limiting: {e}"
            )
            # В случае ошибки, Rate Limiting будет отключен
            self.rate_limit_script_sha = None
