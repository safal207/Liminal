"""
Базовый класс для сценариев нагрузочного тестирования.
"""

import asyncio
import logging
import random
from abc import ABC, abstractmethod
from typing import Any, Awaitable, Callable, Dict, List, Optional, Type

from locust import TaskSet, User, between, task
from locust.env import Environment
from locust.runners import WorkerRunner

from ..config.settings import TestSettings
from ..utils.monitoring import MetricsCollector, RequestTimer

logger = logging.getLogger(__name__)


class BaseScenario(ABC, TaskSet):
    """Базовый класс для сценариев нагрузочного тестирования."""

    # Время ожидания между задачами (может быть переопределено в дочерних классах)
    wait_time = between(0.5, 5)

    def __init__(self, parent: User):
        super().__init__(parent)
        self.metrics: Optional[MetricsCollector] = None
        self.user_id: str = f"user_{random.randint(1, 1000000)}"
        self._setup_metrics()

    def _setup_metrics(self):
        """Настроить сбор метрик."""
        env = self.user.environment
        if not hasattr(env, "metrics_collector"):
            env.metrics_collector = MetricsCollector(
                enable_prometheus=TestSettings.ENABLE_PROMETHEUS,
                port=TestSettings.PROMETHEUS_PORT,
            )
        self.metrics = env.metrics_collector

    async def measure_request(
        self,
        method: str,
        endpoint: str,
        request_func: Callable[..., Awaitable[Any]],
        *args,
        **kwargs,
    ) -> Any:
        """Измерить время выполнения запроса и записать метрики."""
        if not self.metrics:
            return await request_func(*args, **kwargs)

        with RequestTimer(self.metrics, method, endpoint) as timer:
            try:
                response = await request_func(*args, **kwargs)
                timer.status = getattr(response, "status_code", 200)
                return response
            except Exception as e:
                timer.status = 500
                logger.error(f"Request failed: {e}", exc_info=True)
                raise

    async def on_start(self):
        """Вызывается при запуске пользователя."""
        if self.metrics:
            self.metrics.set_active_users(self.user.environment.runner.user_count)
        await self.setup()

    async def on_stop(self):
        """Вызывается при остановке пользователя."""
        await self.teardown()

    @abstractmethod
    async def setup(self):
        """Настройка перед началом тестирования."""
        pass

    @abstractmethod
    async def teardown(self):
        """Очистка после завершения тестирования."""
        pass

    @classmethod
    def register(cls, env: Environment, user_classes: List[Type[User]] = None):
        """Зарегистрировать сценарий в среде выполнения."""
        if user_classes is None:
            user_classes = []

        # Создаем класс пользователя для этого сценария
        class ScenarioUser(User):
            abstract = True
            tasks = [cls]
            wait_time = cls.wait_time

            def __init__(self, *args, **kwargs):
                super().__init__(*args, **kwargs)
                self.scenario = None

            async def on_start(self):
                self.scenario = cls(self)
                await self.scenario.on_start()

            async def on_stop(self):
                if self.scenario:
                    await self.scenario.on_stop()

        # Добавляем класс пользователя в среду выполнения
        user_classes.append(ScenarioUser)

        # Если это воркер, регистрируем обработчики событий
        if isinstance(env.runner, WorkerRunner):

            @env.events.test_start.add_listener
            def on_test_start(environment, **kwargs):
                logger.info("Test started")

            @env.events.test_stop.add_listener
            def on_test_stop(environment, **kwargs):
                logger.info("Test stopped")

        return ScenarioUser
