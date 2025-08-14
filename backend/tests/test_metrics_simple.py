"""
Упрощенный тест метрик Prometheus для WebSocket сервера
Имитирует события WebSocket и выводит метрики в консоль
"""

import random
import time

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

message_processing_time = Histogram(
    "message_processing_time_seconds",
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


def print_metrics():
    """Выводит текущие метрики в консоль"""
    metrics_data = generate_latest(REGISTRY).decode("utf-8")
    print("\n========== ТЕКУЩИЕ МЕТРИКИ ==========")
    for line in metrics_data.split("\n"):
        if any(
            x in line for x in ["websocket_", "http_", "connection_"]
        ) and not line.startswith("#"):
            if line:  # Проверка на пустую строку
                print(line)
    print("=====================================")


def test_websocket_connections():
    """Тестирование метрик подключений WebSocket"""
    print("\n=== ТЕСТ МЕТРИК ПОДКЛЮЧЕНИЙ ===")

    # Инициализация лимитов
    connection_limits.labels(type="max_total").set(100)
    connection_limits.labels(type="max_per_ip").set(10)
    connection_limits.labels(type="total").set(0)

    # Имитация 5 подключений
    print("1. Имитация 5 подключений...")
    for i in range(5):
        websocket_connections.labels(channel="all", authenticated="true").inc()
        connection_limits.labels(type="total").set(i + 1)

    print_metrics()

    # Имитация подписки на каналы
    print("\n2. Имитация подписок на каналы...")
    channels = ["news", "events", "chat"]
    for channel in channels:
        # Случайное количество подключений на канал
        count = random.randint(1, 3)
        for i in range(count):
            websocket_connections.labels(channel=channel, authenticated="true").inc()
        print(f"   Подписано {count} клиентов на канал '{channel}'")

    print_metrics()

    # Имитация отключений
    print("\n3. Имитация отключения 2 клиентов...")
    for i in range(2):
        websocket_connections.labels(channel="all", authenticated="true").dec()

        # Отписка от случайного канала
        channel = random.choice(channels)
        websocket_connections.labels(channel=channel, authenticated="true").dec()

    connection_limits.labels(type="total").set(3)  # 5 - 2 = 3

    print_metrics()


def test_websocket_messages():
    """Тестирование метрик сообщений WebSocket"""
    print("\n=== ТЕСТ МЕТРИК СООБЩЕНИЙ ===")

    # Имитация различных типов сообщений
    message_types = ["message", "notification", "system", "broadcast"]
    channels = ["news", "events", "chat", "personal"]

    print("1. Имитация отправки сообщений разных типов...")
    for i in range(10):
        msg_type = random.choice(message_types)
        channel = random.choice(channels)

        # Имитация входящего сообщения
        websocket_messages_total.labels(
            type=msg_type, direction="in", channel=channel
        ).inc()

        # Имитация исходящих сообщений (broadcast)
        recipients = random.randint(1, 5)
        websocket_messages_total.labels(
            type=msg_type, direction="out", channel=channel
        ).inc(recipients)

        # Имитация времени обработки
        processing_time = random.uniform(0.01, 0.5)
        message_processing_time.labels(message_type=msg_type).observe(processing_time)

        print(
            f"   Сообщение типа '{msg_type}' в канале '{channel}': доставлено {recipients} получателям за {processing_time:.3f}s"
        )
        time.sleep(0.1)  # Небольшая пауза

    print_metrics()


def test_authentication():
    """Тестирование метрик аутентификации WebSocket"""
    print("\n=== ТЕСТ МЕТРИК АУТЕНТИФИКАЦИИ ===")

    # Имитация успешных аутентификаций
    print("1. Имитация 8 успешных аутентификаций...")
    for i in range(8):
        websocket_auth_total.labels(status="success").inc()

    # Имитация неудачных аутентификаций
    print("2. Имитация неудачных аутентификаций...")
    failure_reasons = {"invalid_token": 3, "expired_token": 2, "missing_token": 1}

    for reason, count in failure_reasons.items():
        for i in range(count):
            websocket_auth_total.labels(status="failure").inc()
            connection_rejections.labels(reason=reason).inc()
        print(f"   {count} ошибок аутентификации с причиной '{reason}'")

    print_metrics()


def test_connection_limits():
    """Тестирование метрик лимитов подключений"""
    print("\n=== ТЕСТ МЕТРИК ЛИМИТОВ ПОДКЛЮЧЕНИЙ ===")

    # Сброс счетчиков
    connection_limits.labels(type="total").set(0)

    # Установка лимитов
    max_connections = 100
    max_per_ip = 10
    connection_limits.labels(type="max_total").set(max_connections)
    connection_limits.labels(type="max_per_ip").set(max_per_ip)

    print(f"1. Имитация попыток подключения с одного IP (лимит {max_per_ip})...")
    successful = 0
    rejected = 0

    for i in range(15):  # 15 попыток подключения
        if successful < max_per_ip:
            websocket_connections.labels(channel="all", authenticated="true").inc()
            connection_limits.labels(type="total").set(successful + 1)
            successful += 1
            print(f"   Попытка {i+1}: успешно")
        else:
            connection_rejections.labels(reason="max_connections_per_ip").inc()
            rejected += 1
            print(f"   Попытка {i+1}: отклонено (превышен лимит по IP)")

    print(f"\nРезультаты: {successful} успешных подключений, {rejected} отклонено")
    print_metrics()


def main():
    print("====== ТЕСТИРОВАНИЕ PROMETHEUS МЕТРИК ДЛЯ WEBSOCKET СЕРВЕРА ======")
    print("Время выполнения: " + time.strftime("%Y-%m-%d %H:%M:%S"))

    test_websocket_connections()
    test_websocket_messages()
    test_authentication()
    test_connection_limits()

    print("\n====== ИТОГОВЫЕ МЕТРИКИ ======")
    print_metrics()
    print("\nТестирование метрик завершено успешно!")


if __name__ == "__main__":
    main()
