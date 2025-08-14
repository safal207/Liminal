"""
Redis метрики для Prometheus
"""

from prometheus_client import Counter, Gauge, Histogram

# Статус подключения к Redis (1 - подключено, 0 - отключено)
redis_connection_status = Gauge(
    "redis_connection_status",
    "Статус подключения к Redis (1 - подключено, 0 - отключено)",
    ["instance_id"],
)

# Счетчик операций Redis по типам
redis_operations_total = Counter(
    "redis_operations_total",
    "Общее количество операций Redis",
    [
        "operation",
        "status",
    ],  # operation: get, set, publish, subscribe, etc; status: success, error
)

# Гистограмма времени выполнения операций Redis
redis_operation_duration_seconds = Histogram(
    "redis_operation_duration_seconds",
    "Время выполнения операций Redis",
    ["operation"],  # operation: get, set, publish, subscribe, etc
    buckets=(0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5),
)

# Счетчик сообщений Redis PubSub
redis_pubsub_messages_total = Counter(
    "redis_pubsub_messages_total",
    "Общее количество сообщений PubSub",
    ["channel", "direction"],  # direction: published, received
)

# Счетчик ошибок Redis
redis_errors_total = Counter(
    "redis_errors_total", "Количество ошибок Redis", ["type"]  # connection, operation
)
