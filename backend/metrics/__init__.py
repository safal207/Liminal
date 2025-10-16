"""
Модуль метрик Prometheus для мониторинга WebSocket сервера и API
"""

from .collectors import (
    message_processing_time,  # Это теперь псевдоним для websocket_broadcast_duration_seconds
)
from .collectors import (
    connection_limits,
    connection_rejections,
    http_requests_total,
    websocket_auth_total,
    websocket_broadcast_duration_seconds,
    websocket_connections,
    websocket_heartbeat_total,
    websocket_idle_disconnects_total,
    websocket_messages_total,
    websocket_rate_limit_total,
)

# Импортируем Redis метрики
from .redis_metrics import (
    redis_connection_status,
    redis_errors_total,
    redis_operation_duration_seconds,
    redis_operations_total,
    redis_pubsub_messages_total,
)
from .registry import REGISTRY
from .setup import prometheus_middleware, setup_metrics

__all__ = [
    # Основные метрики и утилиты
    "setup_metrics",
    "prometheus_middleware",
    # HTTP и WebSocket метрики
    "http_requests_total",
    "websocket_connections",
    "websocket_messages_total",
    "websocket_auth_total",
    "message_processing_time",
    "connection_limits",
    "connection_rejections",
    "websocket_broadcast_duration_seconds",
    "websocket_rate_limit_total",
    "websocket_heartbeat_total",
    "websocket_idle_disconnects_total",
    # Redis метрики
    "redis_connection_status",
    "redis_operations_total",
    "redis_operation_duration_seconds",
    "redis_pubsub_messages_total",
    "redis_errors_total",
    # Registry
    "REGISTRY",
]
