"""
Коллекторы метрик Prometheus для Resonance Liminal
"""

from prometheus_client import Counter, Gauge, Histogram

# HTTP метрики
http_requests_total = Counter(
    "http_requests_total",
    "Общее количество HTTP запросов",
    ["method", "endpoint", "status_code"],
)

# WebSocket метрики
websocket_connections = Gauge(
    "websocket_connections",
    "Количество активных WebSocket соединений",
    ["channel", "authenticated"],
)

websocket_messages_total = Counter(
    "websocket_messages_total",
    "Общее количество WebSocket сообщений",
    ["type", "direction", "channel"],
)

websocket_auth_total = Counter(
    "websocket_auth_total", "Количество попыток аутентификации WebSocket", ["status"]
)

# Метрики производительности
websocket_broadcast_duration_seconds = Histogram(
    "websocket_broadcast_duration_seconds",
    "Время обработки и рассылки сообщений WebSocket",
    ["message_type"],
    buckets=(0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10),
)

# Обратная совместимость с предыдущим именем метрики
message_processing_time = websocket_broadcast_duration_seconds

# Метрики для ограничений соединений
connection_limits = Gauge(
    "connection_limits",
    "Статистика по ограничениям соединений",
    ["type"],  # ip, total, max_per_ip, max_total
)

# Метрика для тайм-аутов и отклонённых соединений
connection_rejections = Counter(
    "websocket_connection_rejections_total",
    "Total number of rejected WebSocket connections",
    ["reason"],
)

# Новая метрика для отслеживания отклоненных из-за Rate Limit сообщений
websocket_rate_limit_total = Counter(
    "websocket_rate_limit_total",
    "Total number of messages rejected due to rate limiting",
    ["user_id"],
)

# Heartbeat and idle-timeout metrics
websocket_heartbeat_total = Counter(
    "websocket_heartbeat_total",
    "Heartbeat events: ping sent, pong received, timeout disconnect",
    ["event"],  # ping_sent, pong_received, timeout_disconnect
)

websocket_idle_disconnects_total = Counter(
    "websocket_idle_disconnects_total",
    "Connections closed due to idle timeout or missing pong",
    ["reason"],  # idle_timeout, missing_pong
)

# Message validation metrics
websocket_message_validation_errors_total = Counter(
    "websocket_message_validation_errors_total",
    "Количество сообщений отклоненных из-за ошибок валидации",
    ["channel", "error_type"],
)

# Payload size observability
websocket_message_size_bytes = Histogram(
    "websocket_message_size_bytes",
    "Распределение размера WebSocket сообщений в байтах",
    ["direction", "channel"],
    buckets=(
        64,
        128,
        256,
        512,
        1024,
        2048,
        4096,
        8192,
        16384,
    ),
)

# ACK tracking
websocket_message_acks_total = Counter(
    "websocket_message_acks_total",
    "Количество полученных ACK для WebSocket сообщений",
    ["status"],  # success, timeout, duplicate
)

websocket_pending_messages = Gauge(
    "websocket_pending_messages",
    "Количество сообщений, ожидающих ACK",
    ["channel"],
)

websocket_message_retries_total = Counter(
    "websocket_message_retries_total",
    "Количество повторных отправок сообщений WebSocket",
    ["reason"],
)

websocket_ack_response_time = Histogram(
    "websocket_ack_response_time",
    "Время между отправкой сообщения и получением ACK",
    ["channel"],
    buckets=(
        0.005,
        0.01,
        0.025,
        0.05,
        0.1,
        0.25,
        0.5,
        1,
        2.5,
        5,
    ),
)

# Memory timeline observability
memory_timeline_events_total = Counter(
    "memory_timeline_events_total",
    "Количество обработанных событий таймлайна памяти",
    ["event_type"],
)

memory_timeline_processing_seconds = Histogram(
    "memory_timeline_processing_seconds",
    "Время обработки ключевых операций таймлайна памяти",
    ["operation"],
    buckets=(
        0.001,
        0.005,
        0.01,
        0.025,
        0.05,
        0.1,
        0.25,
        0.5,
        1,
        2.5,
        5,
    ),
)

memory_timeline_backlog_size = Gauge(
    "memory_timeline_backlog_size",
    "Размер буфера таймлайна памяти (количество воспоминаний)",
)

memory_timeline_subscribers = Gauge(
    "memory_timeline_subscribers",
    "Количество активных подписчиков таймлайна памяти",
)

# Neo4j saturation metrics
neo4j_operations_total = Counter(
    "neo4j_operations_total",
    "Количество операций Neo4j по статусам",
    ["operation", "status"],
)

neo4j_operation_duration_seconds = Histogram(
    "neo4j_operation_duration_seconds",
    "Длительность операций Neo4j",
    ["operation"],
    buckets=(0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10),
)

neo4j_active_sessions = Gauge(
    "neo4j_active_sessions",
    "Количество активных сессий Neo4j",
)

neo4j_saturation_ratio = Gauge(
    "neo4j_saturation_ratio",
    "Отношение активных сессий Neo4j к предельному размеру пула",
)
