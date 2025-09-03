"""
Настройка и инициализация метрик Prometheus
"""

import time
from typing import Callable

from fastapi import FastAPI, Request, Response
from loguru import logger
from prometheus_client import CONTENT_TYPE_LATEST, generate_latest
from starlette.middleware.base import BaseHTTPMiddleware
from starlette.responses import Response as StarletteResponse

from .collectors import (connection_limits, http_requests_total,
                         websocket_connections)


def setup_metrics(app: FastAPI) -> None:
    """
    Настраивает метрики Prometheus для FastAPI приложения
    """

    # Добавляем эндпоинт для метрик
    @app.get("/metrics")
    async def metrics():
        return Response(content=generate_latest(), media_type=CONTENT_TYPE_LATEST)

    # Инициализируем начальные значения для метрик лимитов соединений
    connection_limits.labels(type="total").set(0)
    connection_limits.labels(type="max_total").set(100)  # Взято из ConnectionManager
    connection_limits.labels(type="max_per_ip").set(10)  # Взято из ConnectionManager

    # Добавляем middleware для отслеживания HTTP запросов
    app.add_middleware(PrometheusMiddleware)

    # Логируем инициализацию метрик
    logger.info("Prometheus metrics initialized at /metrics endpoint")


class PrometheusMiddleware(BaseHTTPMiddleware):
    """
    Middleware для сбора метрик HTTP запросов
    """

    async def dispatch(
        self, request: Request, call_next: Callable
    ) -> StarletteResponse:
        start_time = time.time()

        # Обрабатываем запрос
        try:
            response = await call_next(request)
        except Exception as e:
            # В случае исключения записываем метрику с кодом 500
            http_requests_total.labels(
                method=request.method, endpoint=request.url.path, status_code=500
            ).inc()
            raise e

        # Записываем метрику запроса
        http_requests_total.labels(
            method=request.method,
            endpoint=request.url.path,
            status_code=response.status_code,
        ).inc()

        return response


# Алиас для совместимости
prometheus_middleware = PrometheusMiddleware
