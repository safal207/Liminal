"""
Тестирование интеграции метрик Prometheus без запуска полного WebSocket сервера.
Этот скрипт имитирует события WebSocket и генерирует соответствующие метрики Prometheus локально.
"""

import argparse
import random
import time

from prometheus_client import REGISTRY, Counter, Gauge, Histogram, start_http_server

# Имитация метрик из backend/metrics/collectors.py
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
message_processing_time = Histogram(
    "message_processing_time_seconds",
    "Время обработки сообщений WebSocket",
    ["message_type"],
    buckets=(0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10),
)

# Метрики для ограничений соединений
connection_limits = Gauge(
    "connection_limits",
    "Статистика по ограничениям соединений",
    ["type"],  # ip, total, max_per_ip, max_total
)

# Метрика для тайм-аутов и отклонённых соединений
connection_rejections = Counter(
    "connection_rejections_total",
    "Количество отклонённых WebSocket соединений",
    ["reason"],
)


def reset_metrics():
    """Сброс всех метрик перед новым тестом"""
    websocket_connections._metrics.clear()

    # Сохраняем метрики, которые не должны быть сброшены полностью, а только уменьшены до начальных значений
    connection_limits.labels(type="total").set(0)
    connection_limits.labels(type="max_total").set(100)
    connection_limits.labels(type="max_per_ip").set(10)


def simulate_single_client():
    """Имитация работы одного клиента с WebSocket"""
    print("\n=== ТЕСТ ОДНОГО КЛИЕНТА ===")

    # Сброс метрик
    reset_metrics()

    # 1. Подключение
    print("Имитация подключения клиента...")
    websocket_connections.labels(channel="all", authenticated="true").inc()
    websocket_auth_total.labels(status="success").inc()
    connection_limits.labels(type="total").set(1)

    # 2. Подписка на канал
    print("Имитация подписки на канал test_channel...")
    websocket_connections.labels(channel="test_channel", authenticated="true").inc()

    # 3. Отправка нескольких сообщений
    print("Имитация отправки сообщений...")
    for i in range(3):
        start_time = time.time()
        # Имитация времени обработки
        processing_time = random.uniform(0.01, 0.2)
        time.sleep(processing_time)

        # Увеличение счетчиков сообщений
        websocket_messages_total.labels(
            type="message", direction="in", channel="test_channel"
        ).inc()
        websocket_messages_total.labels(
            type="message", direction="out", channel="test_channel"
        ).inc(
            5
        )  # broadcast

        # Регистрация времени обработки
        message_processing_time.labels(message_type="message").observe(
            time.time() - start_time
        )
        print(f"  Сообщение {i+1} отправлено, время обработки: {processing_time:.3f}s")

    # 4. Отписка от канала
    print("Имитация отписки от канала...")
    websocket_connections.labels(channel="test_channel", authenticated="true").dec()

    # 5. Отключение
    print("Имитация отключения клиента...")
    websocket_connections.labels(channel="all", authenticated="true").dec()
    connection_limits.labels(type="total").set(0)

    print("\nМетрики успешно сгенерированы")


def simulate_multiple_clients(num_clients=5):
    """Имитация работы нескольких клиентов с WebSocket"""
    print(f"\n=== ТЕСТ {num_clients} КЛИЕНТОВ ===")

    # Сброс метрик
    reset_metrics()

    # Каналы для имитации
    channels = ["news", "events", "updates", "notifications", "chat"]

    # 1. Подключение клиентов
    print(f"Имитация подключения {num_clients} клиентов...")
    for i in range(num_clients):
        websocket_connections.labels(channel="all", authenticated="true").inc()
        websocket_auth_total.labels(status="success").inc()
        # Имитируем случайные подписки на каналы
        num_channels = random.randint(1, 3)
        for _ in range(num_channels):
            channel = random.choice(channels)
            websocket_connections.labels(channel=channel, authenticated="true").inc()
            print(f"  Клиент {i+1} подписан на канал {channel}")

    connection_limits.labels(type="total").set(num_clients)

    # 2. Имитация обмена сообщениями
    print("Имитация обмена сообщениями...")
    for i in range(10):
        channel = random.choice(channels)
        message_type = random.choice(["message", "notification", "system"])

        start_time = time.time()
        # Имитация времени обработки
        processing_time = random.uniform(0.01, 0.5)
        time.sleep(processing_time)

        # Отправка одним клиентом и получение несколькими
        websocket_messages_total.labels(
            type=message_type, direction="in", channel=channel
        ).inc()
        recipients = random.randint(1, num_clients - 1)
        websocket_messages_total.labels(
            type=message_type, direction="out", channel=channel
        ).inc(recipients)

        # Регистрация времени обработки
        message_processing_time.labels(message_type=message_type).observe(
            time.time() - start_time
        )
        print(
            f"  Сообщение {message_type} в канале {channel} доставлено {recipients} получателям за {processing_time:.3f}s"
        )

    # 3. Отключение части клиентов
    disconnected = random.randint(1, num_clients // 2)
    print(f"Имитация отключения {disconnected} клиентов...")
    for i in range(disconnected):
        websocket_connections.labels(channel="all", authenticated="true").dec()
        # Отписка от каналов при отключении
        channels_to_unsubscribe = random.sample(channels, random.randint(1, 3))
        for channel in channels_to_unsubscribe:
            websocket_connections.labels(channel=channel, authenticated="true").dec()

    connection_limits.labels(type="total").set(num_clients - disconnected)
    print(f"Осталось {num_clients - disconnected} подключенных клиентов")


def simulate_connection_limits(num_attempts=15):
    """Имитация работы лимитов подключений"""
    print(f"\n=== ТЕСТ ЛИМИТОВ ПОДКЛЮЧЕНИЙ ({num_attempts} попыток) ===")

    # Сброс метрик
    reset_metrics()

    # Устанавливаем лимиты
    max_connections = 100
    max_per_ip = 10
    connection_limits.labels(type="max_total").set(max_connections)
    connection_limits.labels(type="max_per_ip").set(max_per_ip)

    # Имитация подключений с одного IP
    successful = 0
    rejected = 0

    print(f"Имитация {num_attempts} попыток подключения с одного IP...")
    for i in range(num_attempts):
        if successful < max_per_ip:  # Имитируем лимит подключений с одного IP
            websocket_connections.labels(channel="all", authenticated="true").inc()
            websocket_auth_total.labels(status="success").inc()
            successful += 1
            print(f"  Попытка {i+1}: успешное подключение")
        else:
            connection_rejections.labels(reason="max_connections_per_ip").inc()
            rejected += 1
            print(f"  Попытка {i+1}: отклонено (превышен лимит по IP)")

    connection_limits.labels(type="total").set(successful)

    print(f"Результаты: {successful} успешных подключений, {rejected} отклонено")


def simulate_auth_failures():
    """Имитация ошибок аутентификации"""
    print("\n=== ТЕСТ ОШИБОК АУТЕНТИФИКАЦИИ ===")

    # Сброс метрик
    reset_metrics()

    # Имитация различных ошибок аутентификации
    auth_errors = {"invalid_token": 5, "expired_token": 3, "missing_token": 2}

    for error, count in auth_errors.items():
        print(f"Имитация {count} ошибок аутентификации типа '{error}'...")
        for i in range(count):
            websocket_auth_total.labels(status="failure").inc()
            connection_rejections.labels(reason=error).inc()

    # Добавляем несколько успешных аутентификаций для сравнения
    successful = 8
    print(f"Имитация {successful} успешных аутентификаций...")
    for i in range(successful):
        websocket_auth_total.labels(status="success").inc()
        websocket_connections.labels(channel="all", authenticated="true").inc()

    connection_limits.labels(type="total").set(successful)

    total_failures = sum(auth_errors.values())
    print(
        f"Результаты: {successful} успешных аутентификаций, {total_failures} отклонено"
    )


def main():
    parser = argparse.ArgumentParser(
        description="Тестирование метрик Prometheus для WebSocket"
    )
    parser.add_argument(
        "--port", type=int, default=8000, help="Порт для HTTP сервера метрик"
    )
    parser.add_argument(
        "--test",
        default="all",
        choices=["single", "multiple", "limits", "auth", "all"],
        help="Тип теста для запуска",
    )
    parser.add_argument(
        "--clients",
        type=int,
        default=5,
        help="Количество клиентов для теста множественных подключений",
    )
    parser.add_argument(
        "--attempts",
        type=int,
        default=15,
        help="Количество попыток подключений для теста лимитов",
    )

    args = parser.parse_args()

    # Запуск HTTP сервера метрик
    print(f"Запуск HTTP сервера метрик Prometheus на порту {args.port}...")
    start_http_server(args.port)
    print(
        f"Сервер метрик запущен. Доступен по адресу http://localhost:{args.port}/metrics"
    )

    # Инициализация базовых метрик
    connection_limits.labels(type="max_total").set(100)
    connection_limits.labels(type="max_per_ip").set(10)
    connection_limits.labels(type="total").set(0)

    try:
        if args.test == "single" or args.test == "all":
            simulate_single_client()

        if args.test == "multiple" or args.test == "all":
            simulate_multiple_clients(args.clients)

        if args.test == "limits" or args.test == "all":
            simulate_connection_limits(args.attempts)

        if args.test == "auth" or args.test == "all":
            simulate_auth_failures()

        print(
            "\nВсе тесты завершены. Метрики доступны по адресу http://localhost:{args.port}/metrics"
        )
        print("Нажмите Ctrl+C для завершения работы сервера метрик...")

        # Держим сервер запущенным для просмотра метрик
        while True:
            time.sleep(1)

    except KeyboardInterrupt:
        print("\nСервер метрик остановлен")


if __name__ == "__main__":
    main()
