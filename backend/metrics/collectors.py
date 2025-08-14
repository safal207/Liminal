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
