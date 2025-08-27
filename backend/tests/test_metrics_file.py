"""
Тест метрик Prometheus с выводом в файл
"""

import random

from prometheus_client import Counter, Gauge, Histogram, generate_latest
from prometheus_client.core import REGISTRY

# Очистка существующих метрик из реестра
for collector in list(REGISTRY._collector_to_names.keys()):
    REGISTRY.unregister(collector)

# Определение метрик
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

websocket_broadcast_duration_seconds = Histogram(
    "websocket_broadcast_duration_seconds",
    "Время обработки сообщений WebSocket",
    ["message_type"],
    buckets=(0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10),
)

connection_limits = Gauge(
    "connection_limits", "Статистика по ограничениям соединений", ["type"]
)

connection_rejections = Counter(
    "connection_rejections_total",
    "Количество отклонённых WebSocket соединений",
    ["reason"],
)


def save_metrics_to_file(filename):
    """Сохраняет текущие метрики в файл"""
    metrics_data = generate_latest(REGISTRY).decode("utf-8")

    filtered_metrics = []
    for line in metrics_data.split("\n"):
        if any(
            x in line for x in ["websocket_", "connection_"]
        ) and not line.startswith("#"):
            if line:  # Проверка на пустую строку
                filtered_metrics.append(line)

    with open(filename, "w") as f:
        f.write("\n".join(filtered_metrics))

    return filtered_metrics


def run_tests():
    """Выполняет тесты метрик"""
    # Инициализация лимитов
    connection_limits.labels(type="max_total").set(100)
    connection_limits.labels(type="max_per_ip").set(10)
    connection_limits.labels(type="total").set(0)

    # Тест 1: Подключения и каналы
    for i in range(5):
        websocket_connections.labels(channel="all", authenticated="true").inc()
        connection_limits.labels(type="total").set(i + 1)

    channels = ["news", "events", "chat"]
    for channel in channels:
        count = random.randint(1, 3)
        for i in range(count):
            websocket_connections.labels(channel=channel, authenticated="true").inc()

    # Тест 2: Сообщения
    message_types = ["message", "notification", "system", "broadcast"]
    for i in range(10):
        msg_type = random.choice(message_types)
        channel = random.choice(channels)

        websocket_messages_total.labels(
            type=msg_type, direction="in", channel=channel
        ).inc()
        recipients = random.randint(1, 5)
        websocket_messages_total.labels(
            type=msg_type, direction="out", channel=channel
        ).inc(recipients)

        processing_time = random.uniform(0.01, 0.5)
        websocket_broadcast_duration_seconds.labels(message_type=msg_type).observe(
            processing_time
        )

    # Тест 3: Аутентификация
    for i in range(8):
        websocket_auth_total.labels(status="success").inc()

    failure_reasons = {"invalid_token": 3, "expired_token": 2, "missing_token": 1}

    for reason, count in failure_reasons.items():
        for i in range(count):
            websocket_auth_total.labels(status="failure").inc()
            connection_rejections.labels(reason=reason).inc()

    # Тест 4: Лимиты подключений
    successful = 0
    rejected = 0

    for i in range(15):
        if successful < 10:  # Лимит подключений с одного IP
            successful += 1
        else:
            connection_rejections.labels(reason="max_connections_per_ip").inc()
            rejected += 1

    # Сохраняем метрики в файл
    metrics = save_metrics_to_file("metrics_output.txt")
    return metrics


if __name__ == "__main__":
    metrics = run_tests()
    print(f"Тест метрик завершен. Сгенерировано {len(metrics)} строк метрик.")
    print(f"Результаты сохранены в файл 'metrics_output.txt'")
