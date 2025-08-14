"""
Утилиты для работы с WebSocket в нагрузочном тестировании.
"""

import asyncio
import json
import logging
from contextlib import asynccontextmanager
from typing import Any, Awaitable, Callable, Dict, List, Optional

from websockets.client import WebSocketClientProtocol, connect

logger = logging.getLogger(__name__)


class WebSocketManager:
    """Менеджер для управления WebSocket соединениями."""

    def __init__(
        self,
        url: str,
        on_message: Optional[
            Callable[[Dict[str, Any], "WebSocketManager"], Awaitable[None]]
        ] = None,
        on_connect: Optional[Callable[["WebSocketManager"], Awaitable[None]]] = None,
        on_disconnect: Optional[Callable[["WebSocketManager"], Awaitable[None]]] = None,
        reconnect_interval: int = 5,
        max_reconnect_attempts: int = 3,
        **websocket_kwargs,
    ):
        self.url = url
        self.ws: Optional[WebSocketClientProtocol] = None
        self.on_message = on_message or self._default_on_message
        self.on_connect = on_connect or self._default_on_connect
        self.on_disconnect = on_disconnect or self._default_on_disconnect
        self.reconnect_interval = reconnect_interval
        self.max_reconnect_attempts = max_reconnect_attempts
        self.websocket_kwargs = websocket_kwargs
        self._receive_task: Optional[asyncio.Task] = None
        self._reconnect_attempts = 0
        self._is_connected = False
        self._message_handlers: Dict[
            str, List[Callable[[Dict[str, Any]], Awaitable[None]]]
        ] = {}

    @property
    def is_connected(self) -> bool:
        """Проверка подключения WebSocket."""
        return self._is_connected and self.ws is not None and not self.ws.closed

    async def _default_on_message(
        self, message: Dict[str, Any], manager: "WebSocketManager"
    ) -> None:
        """Обработчик сообщений по умолчанию."""
        logger.debug(f"Received message: {message}")

        # Вызываем обработчики для конкретного типа сообщения
        event_type = message.get("event")
        if event_type and event_type in self._message_handlers:
            for handler in self._message_handlers[event_type]:
                await handler(message)

    async def _default_on_connect(self, manager: "WebSocketManager") -> None:
        """Обработчик подключения по умолчанию."""
        logger.info(f"Connected to WebSocket at {self.url}")

    async def _default_on_disconnect(self, manager: "WebSocketManager") -> None:
        """Обработчик отключения по умолчанию."""
        logger.info(f"Disconnected from WebSocket at {self.url}")

    def add_message_handler(
        self, event_type: str, handler: Callable[[Dict[str, Any]], Awaitable[None]]
    ):
        """Добавить обработчик для определенного типа сообщений."""
        if event_type not in self._message_handlers:
            self._message_handlers[event_type] = []
        self._message_handlers[event_type].append(handler)

    async def connect(self) -> bool:
        """Установить соединение с WebSocket сервером."""
        if self.is_connected:
            return True

        while self._reconnect_attempts < self.max_reconnect_attempts:
            try:
                self.ws = await connect(self.url, **self.websocket_kwargs)
                self._is_connected = True
                self._reconnect_attempts = 0

                # Запускаем задачу приема сообщений
                self._receive_task = asyncio.create_task(self._receive_messages())

                # Вызываем обработчик подключения
                await self.on_connect(self)

                return True

            except Exception as e:
                self._reconnect_attempts += 1
                logger.error(
                    f"WebSocket connection attempt {self._reconnect_attempts} failed: {e}"
                )

                if self._reconnect_attempts >= self.max_reconnect_attempts:
                    logger.error(
                        f"Max reconnection attempts ({self.max_reconnect_attempts}) reached"
                    )
                    return False

                await asyncio.sleep(self.reconnect_interval)

        return False

    async def disconnect(self):
        """Закрыть соединение с WebSocket сервером."""
        if self._receive_task and not self._receive_task.done():
            self._receive_task.cancel()
            try:
                await self._receive_task
            except asyncio.CancelledError:
                pass
            self._receive_task = None

        if self.ws and not self.ws.closed:
            await self.ws.close()
            self._is_connected = False
            await self.on_disconnect(self)

    async def send_json(self, data: Dict[str, Any]) -> bool:
        """Отправить JSON данные через WebSocket."""
        if not self.is_connected or not self.ws:
            logger.warning("Cannot send message: WebSocket is not connected")
            return False

        try:
            await self.ws.send(json.dumps(data))
            return True
        except Exception as e:
            logger.error(f"Failed to send WebSocket message: {e}")
            self._is_connected = False
            return False

    async def _receive_messages(self):
        """Принимать сообщения от WebSocket сервера."""
        if not self.ws:
            return

        try:
            async for message in self.ws:
                try:
                    data = json.loads(message)
                    await self.on_message(data, self)
                except json.JSONDecodeError:
                    logger.warning(f"Received invalid JSON: {message}")
                except Exception as e:
                    logger.error(f"Error processing WebSocket message: {e}")
        except Exception as e:
            logger.error(f"WebSocket receive error: {e}")
            self._is_connected = False

    @asynccontextmanager
    async def connection(self):
        """Контекстный менеджер для работы с WebSocket соединением."""
        connected = await self.connect()
        try:
            if connected:
                yield self
            else:
                raise ConnectionError("Failed to establish WebSocket connection")
        finally:
            await self.disconnect()

    async def __aenter__(self):
        await self.connect()
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        await self.disconnect()
