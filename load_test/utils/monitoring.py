"""
Модуль мониторинга для нагрузочного тестирования.

Обеспечивает сбор метрик и интеграцию с системами мониторинга.
"""

import logging
import time
from typing import Any, Dict, Optional

from prometheus_client import Counter, Gauge, Histogram, start_http_server

logger = logging.getLogger(__name__)


class MetricsCollector:
    """Класс для сбора и экспорта метрик."""

    def __init__(self, enable_prometheus: bool = False, port: int = 9090):
        self.enable_prometheus = enable_prometheus
        self.metrics: Dict[str, Any] = {}

        if enable_prometheus:
            self._init_prometheus_metrics()
            start_http_server(port)
            logger.info(f"Prometheus metrics server started on port {port}")

    def _init_prometheus_metrics(self):
        """Инициализировать метрики Prometheus."""
        self.metrics.update(
            {
                "requests_total": Counter(
                    "http_requests_total",
                    "Total number of HTTP requests",
                    ["method", "endpoint", "status"],
                ),
                "request_duration_seconds": Histogram(
                    "http_request_duration_seconds",
                    "HTTP request duration in seconds",
                    ["method", "endpoint"],
                    buckets=(
                        0.005,
                        0.01,
                        0.025,
                        0.05,
                        0.1,
                        0.25,
                        0.5,
                        1.0,
                        2.5,
                        5.0,
                        10.0,
                    ),
                ),
                "active_users": Gauge(
                    "active_users", "Number of currently active users"
                ),
                "websocket_connections": Gauge(
                    "websocket_connections", "Number of active WebSocket connections"
                ),
                "errors_total": Counter(
                    "errors_total", "Total number of errors", ["type"]
                ),
            }
        )

    def record_request(self, method: str, endpoint: str, status: int, duration: float):
        """Записать метрики HTTP-запроса."""
        if self.enable_prometheus:
            self.metrics["requests_total"].labels(
                method=method, endpoint=endpoint, status=status
            ).inc()

            self.metrics["request_duration_seconds"].labels(
                method=method, endpoint=endpoint
            ).observe(duration)

    def record_error(self, error_type: str):
        """Записать ошибку."""
        if self.enable_prometheus:
            self.metrics["errors_total"].labels(type=error_type).inc()

    def set_active_users(self, count: int):
        """Установить количество активных пользователей."""
        if self.enable_prometheus:
            self.metrics["active_users"].set(count)

    def set_websocket_connections(self, count: int):
        """Установить количество активных WebSocket соединений."""
        if self.enable_prometheus:
            self.metrics["websocket_connections"].set(count)


class RequestTimer:
    """Контекстный менеджер для замера времени выполнения запросов."""

    def __init__(self, metrics_collector: MetricsCollector, method: str, endpoint: str):
        self.metrics = metrics_collector
        self.method = method
        self.endpoint = endpoint
        self.start_time = 0.0
        self.status = 500  # По умолчанию ошибка

    def __enter__(self):
        self.start_time = time.monotonic()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        duration = time.monotonic() - self.start_time

        # Если произошло исключение, фиксируем ошибку
        if exc_type is not None:
            self.metrics.record_error(str(exc_type.__name__))
            self.status = 500

        # Записываем метрики
        self.metrics.record_request(
            method=self.method,
            endpoint=self.endpoint,
            status=self.status,
            duration=duration,
        )

        # Пробрасываем исключение дальше, если оно было
        return exc_type is None
