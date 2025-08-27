"""
Модуль agent_manager - управление жизненным циклом агентов и их взаимодействием.

Обеспечивает регистрацию, запуск, остановку и мониторинг агентов,
а также маршрутизацию сообщений между ними через шину событий.
"""

import asyncio
import logging
import signal
from datetime import datetime
from typing import Any, Dict, List, Optional, Type, TypeVar

from .base import MicroAgent
from .utils.event_bus import EventBus

# Тип для аннотаций
T = TypeVar("T", bound=MicroAgent)

logger = logging.getLogger(__name__)


class AgentManager:
    """
    Менеджер агентов, отвечающий за их жизненный цикл и взаимодействие.

    Обеспечивает:
    - Регистрацию и управление агентами
    - Маршрутизацию сообщений через шину событий
    - Мониторинг состояния агентов
    - Обработку ошибок и восстановление
    """

    def __init__(self, redis_url: str = "redis://localhost:6379"):
        """
        Инициализация менеджера агентов.

        Args:
            redis_url: URL для подключения к Redis
        """
        self.agents: Dict[str, MicroAgent] = {}
        self.agent_classes: Dict[str, Type[MicroAgent]] = {}
        self.event_bus = EventBus(redis_url)
        self.initialized = False
        self.running = False
        self._shutdown_event = asyncio.Event()

        # Обработка сигналов завершения
        try:
            loop = asyncio.get_running_loop()
            for sig in (signal.SIGINT, signal.SIGTERM):
                loop.add_signal_handler(
                    sig,
                    lambda s=sig: asyncio.create_task(
                        self.shutdown(f"Получен сигнал {s.name}")
                    ),
                )
        except (NotImplementedError, RuntimeError):
            # Не все платформы поддерживают add_signal_handler
            pass

    async def initialize(self):
        """Инициализация менеджера и подключение к шине событий."""
        if self.initialized:
            return

        logger.info("Инициализация менеджера агентов...")

        try:
            # Подключаемся к шине событий
            await self.event_bus.connect()
            self.initialized = True
            logger.info("Менеджер агентов успешно инициализирован")

        except Exception as e:
            logger.error(f"Ошибка при инициализации менеджера агентов: {e}")
            self.initialized = False
            raise

    async def start(self):
        """Запуск менеджера агентов."""
        if not self.initialized:
            await self.initialize()

        if self.running:
            logger.warning("Менеджер агентов уже запущен")
            return

        logger.info("Запуск менеджера агентов...")
        self.running = True

        # Запускаем все зарегистрированные агенты
        for agent_id in list(self.agents.keys()):
            try:
                await self.start_agent(agent_id)
            except Exception as e:
                logger.error(
                    f"Не удалось запустить агент {agent_id}: {e}", exc_info=True
                )

        logger.info("Менеджер агентов запущен")

        # Ожидаем события завершения
        await self._shutdown_event.wait()

    async def stop(self):
        """Остановка менеджера агентов."""
        if not self.running:
            return

        logger.info("Остановка менеджера агентов...")
        self.running = False

        # Останавливаем все запущенные агенты
        for agent_id in list(self.agents.keys()):
            try:
                await self.stop_agent(agent_id)
            except Exception as e:
                logger.error(
                    f"Ошибка при остановке агента {agent_id}: {e}", exc_info=True
                )

        # Закрываем соединение с шиной событий
        try:
            await self.event_bus.disconnect()
        except Exception as e:
            logger.error(f"Ошибка при отключении от шины событий: {e}")

        logger.info("Менеджер агентов остановлен")
        self.initialized = False

    async def shutdown(self, reason: str = ""):
        """Корректное завершение работы менеджера агентов."""
        if not self.running:
            return

        logger.info(f"Завершение работы менеджера агентов. Причина: {reason}")
        self._shutdown_event.set()
        await self.stop()

    def register_agent_class(
        self, agent_class: Type[T], name: Optional[str] = None
    ) -> str:
        """
        Регистрация класса агента в менеджере.

        Args:
            agent_class: Класс агента для регистрации
            name: Имя для регистрации (если не указано, используется __name__ класса)

        Returns:
            Имя, под которым зарегистрирован класс агента
        """
        agent_name = name or agent_class.__name__
        self.agent_classes[agent_name] = agent_class
        logger.debug(f"Зарегистрирован класс агента: {agent_name}")
        return agent_name

    async def create_agent(
        self,
        agent_class_name: str,
        agent_id: Optional[str] = None,
        config: Optional[Dict[str, Any]] = None,
    ) -> str:
        """
        Создание и регистрация нового экземпляра агента.

        Args:
            agent_class_name: Имя зарегистрированного класса агента
            agent_id: Уникальный идентификатор агента (если не указан, генерируется автоматически)
            config: Конфигурация для создания агента

        Returns:
            Идентификатор созданного агента

        Raises:
            ValueError: Если класс агента с указанным именем не найден
        """
        if agent_class_name not in self.agent_classes:
            raise ValueError(f"Класс агента '{agent_class_name}' не найден")

        agent_class = self.agent_classes[agent_class_name]
        agent = agent_class(agent_id=agent_id, config=config or {})

        # Регистрируем агент
        self.agents[agent.agent_id] = agent
        logger.info(f"Создан агент {agent.agent_id} (класс: {agent_class_name})")

        # Если менеджер запущен, инициализируем агента
        if self.running:
            await self.start_agent(agent.agent_id)

        return agent.agent_id

    async def start_agent(self, agent_id: str):
        """
        Запуск агента.

        Args:
            agent_id: Идентификатор агента

        Raises:
            KeyError: Если агент с указанным идентификатором не найден
        """
        if agent_id not in self.agents:
            raise KeyError(f"Агент с ID '{agent_id}' не найден")

        agent = self.agents[agent_id]

        if hasattr(agent, "is_running") and agent.is_running:
            logger.warning(f"Агент {agent_id} уже запущен")
            return

        try:
            # Инициализируем агента
            await agent.initialize()

            # Подписываемся на события, если агент реализует метод subscribe_to_events
            if hasattr(agent, "subscribe_to_events"):
                await agent.subscribe_to_events(self.event_bus)

            # Устанавливаем флаг запуска, если он есть
            if hasattr(agent, "is_running"):
                agent.is_running = True

            logger.info(f"Агент {agent_id} успешно запущен")

        except Exception as e:
            logger.error(f"Ошибка при запуске агента {agent_id}: {e}", exc_info=True)
            raise

    async def stop_agent(self, agent_id: str):
        """
        Остановка агента.

        Args:
            agent_id: Идентификатор агента

        Raises:
            KeyError: Если агент с указанным идентификатором не найден
        """
        if agent_id not in self.agents:
            raise KeyError(f"Агент с ID '{agent_id}' не найден")

        agent = self.agents[agent_id]

        if hasattr(agent, "is_running") and not agent.is_running:
            logger.warning(f"Агент {agent_id} уже остановлен")
            return

        try:
            # Отписываемся от событий, если агент реализует метод unsubscribe_from_events
            if hasattr(agent, "unsubscribe_from_events"):
                await agent.unsubscribe_from_events(self.event_bus)

            # Останавливаем агента
            await agent.shutdown()

            # Сбрасываем флаг запуска, если он есть
            if hasattr(agent, "is_running"):
                agent.is_running = False

            logger.info(f"Агент {agent_id} успешно остановлен")

        except Exception as e:
            logger.error(f"Ошибка при остановке агента {agent_id}: {e}", exc_info=True)
            raise

    async def remove_agent(self, agent_id: str):
        """
        Удаление агента из менеджера.

        Args:
            agent_id: Идентификатор агента

        Raises:
            KeyError: Если агент с указанным идентификатором не найден
        """
        if agent_id not in self.agents:
            raise KeyError(f"Агент с ID '{agent_id}' не найден")

        # Сначала останавливаем агента, если он запущен
        try:
            await self.stop_agent(agent_id)
        except Exception as e:
            logger.warning(
                f"Не удалось корректно остановить агента {agent_id} перед удалением: {e}"
            )

        # Удаляем агента из словаря
        del self.agents[agent_id]
        logger.info(f"Агент {agent_id} удален из менеджера")

    def get_agent(self, agent_id: str) -> Optional[MicroAgent]:
        """
        Получение экземпляра агента по идентификатору.

        Args:
            agent_id: Идентификатор агента

        Returns:
            Экземпляр агента или None, если агент не найден
        """
        return self.agents.get(agent_id)

    def list_agents(self) -> List[Dict[str, Any]]:
        """
        Получение списка всех зарегистрированных агентов.

        Returns:
            Список словарей с информацией об агентах
        """
        return [
            {
                "agent_id": agent_id,
                "class": agent.__class__.__name__,
                "layer": getattr(agent, "layer", "unknown"),
                "state": getattr(agent, "state", "unknown"),
                "health": getattr(agent, "health", "unknown"),
                "capabilities": getattr(agent, "capabilities", []),
            }
            for agent_id, agent in self.agents.items()
        ]

    async def get_agent_status(self, agent_id: str) -> Dict[str, Any]:
        """
        Получение статуса агента.

        Args:
            agent_id: Идентификатор агента

        Returns:
            Словарь с информацией о статусе агента

        Raises:
            KeyError: Если агент с указанным идентификатором не найден
        """
        if agent_id not in self.agents:
            raise KeyError(f"Агент с ID '{agent_id}' не найден")

        agent = self.agents[agent_id]
        status = {
            "agent_id": agent.agent_id,
            "class": agent.__class__.__name__,
            "layer": getattr(agent, "layer", "unknown"),
            "state": getattr(agent, "state", "unknown"),
            "health": getattr(agent, "health", "unknown"),
            "capabilities": getattr(agent, "capabilities", []),
            "metrics": {},
        }

        # Добавляем метрики, если они есть
        if hasattr(agent, "metrics"):
            status["metrics"] = agent.metrics

        return status

    async def broadcast_message(self, topic: str, message: Dict[str, Any]):
        """
        Отправка сообщения всем подписчикам темы.

        Args:
            topic: Тема сообщения
            message: Данные сообщения
        """
        if not self.initialized:
            raise RuntimeError("Менеджер агентов не инициализирован")

        try:
            await self.event_bus.publish(topic, message)
        except Exception as e:
            logger.error(f"Ошибка при отправке сообщения в тему '{topic}': {e}")
            raise

    async def send_message(self, agent_id: str, message: Dict[str, Any]) -> Any:
        """
        Отправка сообщения конкретному агенту.

        Args:
            agent_id: Идентификатор агента-получателя
            message: Данные сообщения

        Returns:
            Результат обработки сообщения агентом

        Raises:
            KeyError: Если агент с указанным идентификатором не найден
        """
        if agent_id not in self.agents:
            raise KeyError(f"Агент с ID '{agent_id}' не найден")

        agent = self.agents[agent_id]

        try:
            # Если агент реализует метод handle_message, используем его
            if hasattr(agent, "handle_message"):
                return await agent.handle_message(message)
            # Иначе используем стандартный метод process
            else:
                return await agent.process(message)
        except Exception as e:
            logger.error(
                f"Ошибка при обработке сообщения агентом {agent_id}: {e}", exc_info=True
            )
            raise

    async def get_agent_metrics(self) -> Dict[str, Any]:
        """
        Сбор метрик со всех агентов.

        Returns:
            Словарь с метриками агентов
        """
        metrics = {
            "total_agents": len(self.agents),
            "agents_by_layer": {},
            "health_status": {"HEALTHY": 0, "DEGRADED": 0, "ERROR": 0, "UNKNOWN": 0},
            "state_status": {},
            "messages_processed": 0,
            "errors": 0,
            "timestamp": datetime.utcnow().isoformat(),
        }

        # Собираем метрики по каждому агенту
        for agent_id, agent in self.agents.items():
            # Получаем статус агента
            try:
                status = await self.get_agent_status(agent_id)

                # Обновляем счетчики по слоям
                layer = status.get("layer", "unknown")
                metrics["agents_by_layer"][layer] = (
                    metrics["agents_by_layer"].get(layer, 0) + 1
                )

                # Обновляем счетчики состояний здоровья
                health = status.get("health", "UNKNOWN").upper()
                metrics["health_status"][health] = (
                    metrics["health_status"].get(health, 0) + 1
                )

                # Обновляем счетчики состояний
                state = status.get("state", "unknown")
                metrics["state_status"][state] = (
                    metrics["state_status"].get(state, 0) + 1
                )

                # Суммируем метрики
                if "metrics" in status:
                    metrics["messages_processed"] += status["metrics"].get(
                        "messages_processed", 0
                    )
                    metrics["errors"] += status["metrics"].get("errors", 0)

            except Exception as e:
                logger.error(f"Ошибка при сборе метрик агента {agent_id}: {e}")
                metrics["health_status"]["ERROR"] = (
                    metrics["health_status"].get("ERROR", 0) + 1
                )

        return metrics
