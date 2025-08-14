"""
Сценарий нагрузочного тестирования для WebSocket API.
"""

import asyncio
import json
import logging
import random
from typing import Any, Dict, List, Optional

from locust import between, task

from ..config.settings import TestSettings
from ..utils.monitoring import RequestTimer
from ..utils.websocket_client import WebSocketManager
from .base import BaseScenario

logger = logging.getLogger(__name__)


class WebSocketScenario(BaseScenario):
    """Сценарий для тестирования WebSocket API."""

    # Время ожидания между задачами (секунды)
    wait_time = between(0.5, 2)

    def __init__(self, parent):
        super().__init__(parent)
        self.ws_manager: Optional[WebSocketManager] = None
        self.memory_ids: List[str] = []
        self.messages_received: int = 0

    async def setup(self):
        """Настройка перед началом тестирования."""
        ws_url = TestSettings.HOST.replace("http", "ws") + "/ws/timeline"

        self.ws_manager = WebSocketManager(
            url=ws_url,
            on_message=self._handle_websocket_message,
            on_connect=self._on_websocket_connect,
            on_disconnect=self._on_websocket_disconnect,
            reconnect_interval=5,
            max_reconnect_attempts=3,
            ping_interval=20,
            ping_timeout=10,
        )

        # Подключаемся к WebSocket
        await self.ws_manager.connect()

    async def teardown(self):
        """Очистка после завершения тестирования."""
        if self.ws_manager:
            await self.ws_manager.disconnect()

    async def _on_websocket_connect(self, manager):
        """Обработчик успешного подключения к WebSocket."""
        logger.info(f"WebSocket connected for user {self.user_id}")
        if self.metrics:
            self.metrics.set_websocket_connections(
                self.user.environment.runner.user_count
            )

    async def _on_websocket_disconnect(self, manager):
        """Обработчик отключения от WebSocket."""
        logger.info(f"WebSocket disconnected for user {self.user_id}")
        if self.metrics:
            self.metrics.set_websocket_connections(
                max(0, self.user.environment.runner.user_count - 1)
            )

    async def _handle_websocket_message(self, message: Dict[str, Any], manager):
        """Обработчик входящих WebSocket сообщений."""
        self.messages_received += 1

        # Логируем получение сообщения
        if self.metrics:
            self.metrics.record_request(
                method="WS", endpoint="receive_message", status=200, duration=0
            )

        # Обрабатываем событие добавления памяти
        if message.get("event") == "memory_added" and "id" in message.get("data", {}):
            memory_id = message["data"]["id"]
            self.memory_ids.append(memory_id)
            logger.debug(f"Memory added: {memory_id}")

    @task(3)
    async def add_memory(self):
        """Добавить новую память."""
        if not self.ws_manager or not self.ws_manager.is_connected:
            logger.warning("WebSocket is not connected, skipping add_memory")
            return

        memory_types = ["thought", "event", "fact", "idea"]
        memory_content = f"Test memory from {self.user_id} at {self.user.environment.runner.user_count} users"

        data = {
            "content": memory_content,
            "memory_type": random.choice(memory_types),
            "metadata": {
                "source": "load_test",
                "user_id": self.user_id,
                "test_run": (
                    self.user.environment.parsed_options.test_run_id
                    if hasattr(self.user.environment.parsed_options, "test_run_id")
                    else "unknown"
                ),
            },
        }

        endpoint = "/timeline/memories/"

        async with RequestTimer(
            self.metrics, "POST", endpoint, record_metrics=self.metrics is not None
        ) as timer:
            try:
                response = await self.client.post(
                    endpoint,
                    json=data,
                    headers={"Content-Type": "application/json"},
                    timeout=TestSettings.HTTP_TIMEOUT,
                )

                timer.status_code = response.status_code

                if response.status_code == 201:
                    result = response.json()
                    memory_id = result.get("id")
                    if memory_id:
                        self.memory_ids.append(memory_id)
                    return result
                else:
                    logger.error(f"Failed to add memory: {response.text}")
                    return None

            except Exception as e:
                logger.error(f"Error adding memory: {e}", exc_info=True)
                if self.metrics:
                    self.metrics.record_error("add_memory_failed")
                raise

    @task(1)
    async def get_memories(self):
        """Получить список всех воспоминаний."""
        endpoint = "/timeline/memories/"

        async with RequestTimer(
            self.metrics, "GET", endpoint, record_metrics=self.metrics is not None
        ) as timer:
            try:
                response = await self.client.get(
                    endpoint, timeout=TestSettings.HTTP_TIMEOUT
                )

                timer.status_code = response.status_code

                if response.status_code == 200:
                    return response.json()
                else:
                    logger.error(f"Failed to get memories: {response.text}")
                    return None

            except Exception as e:
                logger.error(f"Error getting memories: {e}", exc_info=True)
                if self.metrics:
                    self.metrics.record_error("get_memories_failed")
                raise

    @task(1)
    async def receive_updates(self):
        """Ожидать обновлений по WebSocket."""
        if not self.ws_manager or not self.ws_manager.is_connected:
            logger.warning("WebSocket is not connected, skipping receive_updates")
            return

        # Ждем некоторое время для получения обновлений
        await asyncio.sleep(random.uniform(1, 3))

        # Записываем метрику о полученных сообщениях
        if self.metrics and self.messages_received > 0:
            self.metrics.record_request(
                method="WS", endpoint="receive_updates", status=200, duration=0
            )
            self.messages_received = 0
