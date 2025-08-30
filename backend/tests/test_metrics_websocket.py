"""
Тестовый скрипт для проверки метрик Prometheus в WebSocket соединениях
"""

import argparse
import asyncio
import json
import random
from datetime import datetime

import requests
import websockets

# Токен для аутентификации
DEFAULT_TOKEN = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJ0ZXN0X3VzZXIiLCJuYW1lIjoiVGVzdCBVc2VyIiwiZXhwIjoxOTk5OTk5OTk5fQ.dF-J2q5zUzjF9TG-wBS-GkZNu82ZuYw5TZiMJ0U3ngQ"


class WebSocketClient:
    """
    Клиент WebSocket для генерации метрик и тестирования
    """

    def __init__(self, server_url, client_id, token=None):
        self.server_url = server_url
        self.client_id = client_id
        self.token = token or DEFAULT_TOKEN
        self.websocket = None
        self.connected = False
        self.authenticated = False
        self.subscribed_channels = set()

    async def connect(self):
        """Подключение к WebSocket серверу"""
        url = f"{self.server_url}/ws/{self.client_id}"
        try:
            self.websocket = await websockets.connect(url)
            self.connected = True
            print(f"[{self.client_id}] Подключен к {url}")

            # Получаем запрос на аутентификацию
            message = await self.websocket.recv()
            print(f"[{self.client_id}] Получено: {message}")

            # Отправляем токен для аутентификации
            auth_message = {"token": self.token}
            await self.websocket.send(json.dumps(auth_message))
            print(f"[{self.client_id}] Отправлен токен аутентификации")

            # Получаем ответ аутентификации
            auth_response = await self.websocket.recv()
            print(f"[{self.client_id}] Ответ аутентификации: {auth_response}")

            auth_data = json.loads(auth_response)
            if auth_data.get("type") == "auth_success":
                self.authenticated = True
                return True
            else:
                print(f"[{self.client_id}] Ошибка аутентификации")
                return False

        except Exception as e:
            print(f"[{self.client_id}] Ошибка подключения: {e}")
            return False

    async def subscribe(self, channel):
        """Подписка на канал"""
        if not self.connected or not self.authenticated:
            print(f"[{self.client_id}] Не подключен или не аутентифицирован")
            return False

        try:
            subscribe_message = {
                "type": "subscribe",
                "user_id": self.client_id,
                "channel": channel,
            }
            await self.websocket.send(json.dumps(subscribe_message))
            print(f"[{self.client_id}] Отправлен запрос на подписку на канал: {channel}")

            # Получаем ответ подписки
            response = await self.websocket.recv()
            print(f"[{self.client_id}] Ответ подписки: {response}")

            self.subscribed_channels.add(channel)
            return True
        except Exception as e:
            print(f"[{self.client_id}] Ошибка подписки: {e}")
            return False

    async def send_message(self, channel, message):
        """Отправка сообщения в канал"""
        if not self.connected or not self.authenticated:
            print(f"[{self.client_id}] Не подключен или не аутентифицирован")
            return False

        if channel not in self.subscribed_channels:
            print(f"[{self.client_id}] Не подписан на канал: {channel}")
            return False

        try:
            msg = {
                "type": "message",
                "user_id": self.client_id,
                "channel": channel,
                "message": {"text": message, "timestamp": datetime.now().isoformat()},
            }
            await self.websocket.send(json.dumps(msg))
            print(f"[{self.client_id}] Отправлено сообщение в канал {channel}: {message}")
            return True
        except Exception as e:
            print(f"[{self.client_id}] Ошибка отправки сообщения: {e}")
            return False

    async def unsubscribe(self, channel):
        """Отписка от канала"""
        if not self.connected or not self.authenticated:
            print(f"[{self.client_id}] Не подключен или не аутентифицирован")
            return False

        if channel not in self.subscribed_channels:
            print(f"[{self.client_id}] Не подписан на канал: {channel}")
            return False

        try:
            unsubscribe_message = {
                "type": "unsubscribe",
                "user_id": self.client_id,
                "channel": channel,
            }
            await self.websocket.send(json.dumps(unsubscribe_message))
            print(f"[{self.client_id}] Отправлен запрос на отписку от канала: {channel}")

            # Получаем ответ отписки
            response = await self.websocket.recv()
            print(f"[{self.client_id}] Ответ отписки: {response}")

            self.subscribed_channels.remove(channel)
            return True
        except Exception as e:
            print(f"[{self.client_id}] Ошибка отписки: {e}")
            return False

    async def disconnect(self):
        """Отключение от WebSocket сервера"""
        if self.connected:
            try:
                await self.websocket.close()
                print(f"[{self.client_id}] Отключен")
            except Exception as e:
                print(f"[{self.client_id}] Ошибка отключения: {e}")
            finally:
                self.connected = False
                self.authenticated = False
                self.subscribed_channels.clear()

    async def listener(self):
        """Прослушивание сообщений"""
        if not self.connected:
            return

        try:
            while self.connected:
                message = await self.websocket.recv()
                print(f"[{self.client_id}] Получено: {message}")
        except websockets.exceptions.ConnectionClosedOK:
            print(f"[{self.client_id}] Соединение закрыто")
        except Exception as e:
            print(f"[{self.client_id}] Ошибка при прослушивании: {e}")
        finally:
            self.connected = False


def print_metrics(url):
    """Печатает метрики Prometheus"""
    try:
        response = requests.get(url)
        if response.status_code == 200:
            metrics = response.text
            print("=" * 50)
            print("PROMETHEUS METRICS")
            print("=" * 50)
            # Фильтруем только метрики WebSocket и HTTP
            for line in metrics.split("\n"):
                if any(x in line for x in ["websocket_", "http_", "connection_"]):
                    if line and not line.startswith("#"):
                        print(line)
            print("=" * 50)
        else:
            print(f"Ошибка получения метрик: {response.status_code}")
    except Exception as e:
        print(f"Ошибка при запросе метрик: {e}")


async def test_single_client(server_url, metrics_url):
    """Тест одного клиента"""
    client = WebSocketClient(server_url, "test_user_1")

    # Подключаемся
    success = await client.connect()
    if not success:
        return

    # Запускаем прослушивание в фоновом режиме
    listener_task = asyncio.create_task(client.listener())

    # Подписываемся на канал
    await client.subscribe("test_channel")

    # Отправляем несколько сообщений
    for i in range(3):
        await client.send_message("test_channel", f"Тестовое сообщение {i + 1}")
        await asyncio.sleep(1)

    # Отписываемся от канала
    await client.unsubscribe("test_channel")

    # Получаем метрики
    print_metrics(metrics_url)

    # Отключаемся
    await client.disconnect()

    # Останавливаем прослушивание
    listener_task.cancel()


async def test_multiple_clients(server_url, metrics_url, num_clients=5):
    """Тест множественных клиентов"""
    clients = []
    listener_tasks = []

    print(f"Создание {num_clients} клиентов...")
    for i in range(num_clients):
        client = WebSocketClient(server_url, f"test_user_{i + 1}")
        clients.append(client)

        # Подключаем клиентов
        success = await client.connect()
        if success:
            # Запускаем прослушивание
            listener_task = asyncio.create_task(client.listener())
            listener_tasks.append(listener_task)

            # Подписываемся на каналы
            channels = [f"test_channel_{j + 1}" for j in range(random.randint(1, 3))]
            for channel in channels:
                await client.subscribe(channel)

    print("Отправка сообщений...")
    # Отправка сообщений от случайных клиентов
    for _ in range(10):
        client = random.choice(clients)
        if client.subscribed_channels:
            channel = random.choice(list(client.subscribed_channels))
            await client.send_message(channel, f"Тестовое сообщение в {channel}")
        await asyncio.sleep(0.5)

    # Получаем метрики
    print_metrics(metrics_url)

    print("Отключение клиентов...")
    # Отключаем клиентов
    for client in clients:
        await client.disconnect()

    # Останавливаем задачи прослушивания
    for task in listener_tasks:
        task.cancel()

    # Получаем метрики еще раз после отключения
    await asyncio.sleep(1)
    print_metrics(metrics_url)


async def test_connection_limits(server_url, metrics_url, num_clients=15):
    """Тест лимитов подключений"""
    clients = []

    print(f"Тестирование лимитов подключений с {num_clients} клиентами...")
    for i in range(num_clients):
        # Используем один и тот же IP для провоцирования лимита подключений с одного IP
        client = WebSocketClient(server_url, f"test_user_{i + 1}")
        print(f"Попытка подключения клиента {i + 1}...")
        success = await client.connect()
        if success:
            clients.append(client)
        await asyncio.sleep(0.2)

    # Получаем метрики
    print(f"Успешно подключено {len(clients)} из {num_clients} клиентов")
    print_metrics(metrics_url)

    # Отключаем клиентов
    for client in clients:
        await client.disconnect()

    # Получаем метрики еще раз после отключения
    await asyncio.sleep(1)
    print_metrics(metrics_url)


async def test_invalid_auth(server_url, metrics_url):
    """Тест неверной аутентификации"""
    # Клиент с неверным токеном
    invalid_client = WebSocketClient(server_url, "invalid_user", token="invalid_token")
    await invalid_client.connect()
    await asyncio.sleep(1)

    # Получаем метрики
    print_metrics(metrics_url)


async def main():
    parser = argparse.ArgumentParser(description="Тестирование метрик WebSocket")
    parser.add_argument("--server", default="ws://localhost:8000", help="URL WebSocket сервера")
    parser.add_argument(
        "--metrics",
        default="http://localhost:8000/metrics",
        help="URL метрик Prometheus",
    )
    parser.add_argument(
        "--test",
        default="all",
        choices=["single", "multiple", "limits", "auth", "all"],
        help="Тип теста",
    )

    args = parser.parse_args()

    if args.test == "single" or args.test == "all":
        print("=== ТЕСТ ОДНОГО КЛИЕНТА ===")
        await test_single_client(args.server, args.metrics)

    if args.test == "multiple" or args.test == "all":
        print("\n=== ТЕСТ МНОЖЕСТВЕННЫХ КЛИЕНТОВ ===")
        await test_multiple_clients(args.server, args.metrics)

    if args.test == "limits" or args.test == "all":
        print("\n=== ТЕСТ ЛИМИТОВ ПОДКЛЮЧЕНИЙ ===")
        await test_connection_limits(args.server, args.metrics)

    if args.test == "auth" or args.test == "all":
        print("\n=== ТЕСТ НЕВЕРНОЙ АУТЕНТИФИКАЦИИ ===")
        await test_invalid_auth(args.server, args.metrics)


if __name__ == "__main__":
    asyncio.run(main())
