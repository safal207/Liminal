"""
Базовый класс для всех микро-агентов в системе LIMINAL.

Каждый агент наследуется от этого класса и реализует свою логику обработки сообщений.
"""

import logging
import uuid
from abc import ABC, abstractmethod
from collections.abc import Callable
from datetime import datetime
from typing import Any

logger = logging.getLogger(__name__)


class MicroAgent(ABC):
    """
    Базовый класс для всех микро-агентов в системе LIMINAL.

    Атрибуты:
        agent_id (str): Уникальный идентификатор агента
        name (str): Человекочитаемое имя агента
        layer (str): Слой архитектуры, к которому принадлежит агент
        capabilities (List[str]): Список возможностей агента
        config (Dict[str, Any]): Конфигурация агента
        state (str): Текущее состояние агента
        health (str): Текущее состояние здоровья агента
        metrics (Dict[str, Any]): Метрики производительности
    """

    def __init__(
        self,
        agent_id: str | None = None,
        name: str = "BaseAgent",
        layer: str = "base",
        capabilities: list[str] = None,
        config: dict[str, Any] = None,
    ):
        """
        Инициализация агента.

        Args:
            agent_id: Уникальный идентификатор агента. Если не указан, будет сгенерирован автоматически.
            name: Человекочитаемое имя агента.
            layer: Слой архитектуры, к которому принадлежит агент.
            capabilities: Список возможностей агента.
            config: Конфигурация агента.
        """
        self.agent_id = agent_id or f"{name.lower()}_{str(uuid.uuid4())[:8]}"
        self.name = name
        self.layer = layer
        self.capabilities = capabilities or []
        self.config = config or {}
        self.state = "INITIALIZING"
        self.health = "HEALTHY"
        self.metrics = {
            "messages_processed": 0,
            "last_processed": None,
            "average_processing_time": 0.0,
            "errors": 0,
            "start_time": datetime.utcnow().isoformat(),
            "uptime": 0.0,
        }
        self._subscribed_topics: set[str] = set()
        self._message_handlers: dict[str, list[Callable]] = {}

    async def initialize(self):
        """Инициализация ресурсов агента.

        Вызывается при регистрации агента в системе.
        """
        self.state = "IDLE"
        logger.info(f"Агент {self.agent_id} ({self.name}) инициализирован")

    @abstractmethod
    async def process(
        self, message: dict[str, Any], context: dict[str, Any] = None
    ) -> dict[str, Any]:
        """
        Основной метод обработки входящих сообщений.

        Args:
            message: Входящее сообщение для обработки.
            context: Дополнительный контекст выполнения.

        Returns:
            Словарь с результатами обработки сообщения.

        Raises:
            NotImplementedError: Если метод не переопределен в подклассе.
        """
        raise NotImplementedError("Подклассы должны реализовать метод process()")

    async def handle_error(self, error: Exception, context: dict[str, Any] = None):
        """Обработка ошибок, возникающих при работе агента.

        Args:
            error: Исключение, которое было перехвачено.
            context: Дополнительный контекст ошибки.

        Returns:
            Словарь с информацией об ошибке.
        """
        self.health = "ERROR"
        self.metrics["errors"] += 1
        error_id = str(uuid.uuid4())
        error_info = {
            "error_id": error_id,
            "agent_id": self.agent_id,
            "error_type": error.__class__.__name__,
            "error_message": str(error),
            "timestamp": datetime.utcnow().isoformat(),
            "context": context or {},
        }
        logger.error(f"Ошибка в агенте {self.agent_id}: {error}", exc_info=True)
        return error_info

    def get_status(self) -> dict[str, Any]:
        """Возвращает текущее состояние агента.

        Returns:
            Словарь с информацией о состоянии агента.
        """
        uptime = (
            datetime.utcnow() - datetime.fromisoformat(self.metrics["start_time"])
        ).total_seconds()

        return {
            "agent_id": self.agent_id,
            "name": self.name,
            "layer": self.layer,
            "state": self.state,
            "health": self.health,
            "capabilities": self.capabilities,
            "metrics": {
                **self.metrics,
                "uptime": uptime,
                "subscribed_topics": list(self._subscribed_topics),
                "message_handlers": list(self._message_handlers.keys()),
            },
            "timestamp": datetime.utcnow().isoformat(),
        }

    async def shutdown(self):
        """Очистка ресурсов перед завершением работы агента."""
        self.state = "SHUTTING_DOWN"
        logger.info(f"Агент {self.agent_id} завершает работу...")
        # Очищаем подписки и обработчики
        self._subscribed_topics.clear()
        self._message_handlers.clear()
        self.state = "TERMINATED"
        logger.info(f"Агент {self.agent_id} успешно завершил работу")

    def subscribe_to_topic(self, topic: str, handler: Callable | None = None):
        """Подписывает агента на указанную тему.

        Args:
            topic: Название темы, на которую подписываемся.
            handler: Обработчик сообщений для этой темы. Если не указан,
                    будет использован метод process().
        """
        self._subscribed_topics.add(topic)
        if handler:
            if topic not in self._message_handlers:
                self._message_handlers[topic] = []
            self._message_handlers[topic].append(handler)

    def unsubscribe_from_topic(self, topic: str, handler: Callable | None = None):
        """Отписывает агента от указанной темы.

        Args:
            topic: Название темы, от которой отписываемся.
            handler: Конкретный обработчик для удаления. Если не указан,
                    удаляются все обработчики для этой темы.
        """
        if topic in self._subscribed_topics:
            if not handler:
                self._subscribed_topics.remove(topic)
                self._message_handlers.pop(topic, None)
            elif topic in self._message_handlers and handler in self._message_handlers[topic]:
                self._message_handlers[topic].remove(handler)
                if not self._message_handlers[topic]:
                    self._subscribed_topics.remove(topic)
                    self._message_handlers.pop(topic, None)

    async def handle_message(
        self, topic: str, message: dict[str, Any], context: dict[str, Any] = None
    ):
        """Обрабатывает входящее сообщение.

        Args:
            topic: Тема сообщения.
            message: Данные сообщения.
            context: Дополнительный контекст.

        Returns:
            Результат обработки сообщения.
        """
        if topic not in self._subscribed_topics:
            logger.warning(
                f"Агент {self.agent_id} получил сообщение из неподписанной темы: {topic}"
            )
            return None

        context = context or {}
        context["message_topic"] = topic
        context["agent_id"] = self.agent_id

        start_time = datetime.utcnow()
        self.state = "PROCESSING"

        try:
            # Если есть специфические обработчики для этой темы, используем их
            if topic in self._message_handlers and self._message_handlers[topic]:
                results = []
                for handler in self._message_handlers[topic]:
                    result = await handler(message, context)
                    if result is not None:
                        results.append(result)
                response = results[0] if len(results) == 1 else results
            else:
                # Иначе используем основной метод process
                response = await self.process(message, context)

            # Обновляем метрики
            processing_time = (datetime.utcnow() - start_time).total_seconds()
            self.metrics["messages_processed"] += 1
            self.metrics["last_processed"] = datetime.utcnow().isoformat()

            # Обновляем среднее время обработки (скользящее среднее)
            total_time = self.metrics["average_processing_time"] * (
                self.metrics["messages_processed"] - 1
            )
            self.metrics["average_processing_time"] = (total_time + processing_time) / self.metrics[
                "messages_processed"
            ]

            self.state = "IDLE"
            return response

        except Exception as e:
            self.state = "ERROR"
            return await self.handle_error(e, {"topic": topic, "message": message, **context})
