"""
Интеграционный тест метрик Prometheus для Resonance Liminal WebSocket сервера.
Проверяет правильность сбора метрик во время работы с WebSocket соединениями.

Запуск: python tests/test_metrics_integration.py
"""

import argparse
import asyncio
import json
import random
import uuid
from datetime import datetime

import requests
import websockets
from colorama import Fore, Style, init

# Инициализация colorama для цветного вывода
init()


class WebSocketClient:
    def __init__(self, server_url, client_id, token=None):
        self.server_url = server_url
        self.client_id = client_id
        self.token = token
        self.websocket = None
        self.connected = False
        self.authenticated = False
        self.subscribed_channels = set()

    async def connect(self):
        """Подключение к WebSocket серверу"""
        try:
            self.websocket = await websockets.connect(self.server_url)
            self.connected = True
            print(f"{Fore.GREEN}[{self.client_id}] Подключен к {self.server_url}{Style.RESET_ALL}")

            # Аутентификация с токеном, если предоставлен
            if self.token:
                auth_result = await self.authenticate()
                if not auth_result:
                    await self.disconnect()
                    return False

            return True
        except Exception as e:
            print(f"{Fore.RED}[{self.client_id}] Ошибка подключения: {e}{Style.RESET_ALL}")
            self.connected = False
            return False

    async def authenticate(self):
        """Аутентификация на WebSocket сервере с использованием JWT"""
        if not self.connected:
            print(f"{Fore.RED}[{self.client_id}] Не подключен к серверу{Style.RESET_ALL}")
            return False

        try:
            auth_message = {
                "type": "authenticate",
                "token": self.token or f"test-token-{self.client_id}-{uuid.uuid4()}",
            }
            await self.websocket.send(json.dumps(auth_message))
            print(f"{Fore.CYAN}[{self.client_id}] Отправлен запрос аутентификации{Style.RESET_ALL}")

            response = await self.websocket.recv()
            response_data = json.loads(response)
            print(
                f"{Fore.CYAN}[{self.client_id}] Ответ аутентификации: {response_data}{Style.RESET_ALL}"
            )

            # Проверяем успешность аутентификации
            if response_data.get("type") == "auth_success":
                self.authenticated = True
                print(f"{Fore.GREEN}[{self.client_id}] Аутентификация успешна{Style.RESET_ALL}")
                return True
            else:
                print(
                    f"{Fore.RED}[{self.client_id}] Ошибка аутентификации: {response_data}{Style.RESET_ALL}"
                )
                return False

        except Exception as e:
            print(f"{Fore.RED}[{self.client_id}] Ошибка при аутентификации: {e}{Style.RESET_ALL}")
            return False

    async def subscribe(self, channel):
        """Подписка на канал"""
        if not self.connected or not self.authenticated:
            print(
                f"{Fore.RED}[{self.client_id}] Не подключен или не аутентифицирован{Style.RESET_ALL}"
            )
            return False

        try:
            subscribe_message = {
                "type": "subscribe",
                "user_id": self.client_id,
                "channel": channel,
            }
            await self.websocket.send(json.dumps(subscribe_message))
            print(
                f"{Fore.CYAN}[{self.client_id}] Отправлен запрос на подписку на канал: {channel}{Style.RESET_ALL}"
            )

            # Получаем ответ подписки
            response = await self.websocket.recv()
            print(f"{Fore.CYAN}[{self.client_id}] Ответ подписки: {response}{Style.RESET_ALL}")

            self.subscribed_channels.add(channel)
            return True
        except Exception as e:
            print(f"{Fore.RED}[{self.client_id}] Ошибка подписки: {e}{Style.RESET_ALL}")
            return False

    async def send_message(self, channel, message):
        """Отправка сообщения в канал"""
        if not self.connected or not self.authenticated:
            print(
                f"{Fore.RED}[{self.client_id}] Не подключен или не аутентифицирован{Style.RESET_ALL}"
            )
            return False

        if channel not in self.subscribed_channels:
            print(
                f"{Fore.YELLOW}[{self.client_id}] Не подписан на канал: {channel}{Style.RESET_ALL}"
            )
            return False

        try:
            msg = {
                "type": "message",
                "user_id": self.client_id,
                "channel": channel,
                "message": {"text": message, "timestamp": datetime.now().isoformat()},
            }
            await self.websocket.send(json.dumps(msg))
            print(
                f"{Fore.CYAN}[{self.client_id}] Отправлено сообщение в канал {channel}: {message}{Style.RESET_ALL}"
            )
            return True
        except Exception as e:
            print(f"{Fore.RED}[{self.client_id}] Ошибка отправки сообщения: {e}{Style.RESET_ALL}")
            return False

    async def unsubscribe(self, channel):
        """Отписка от канала"""
        if not self.connected or not self.authenticated:
            print(
                f"{Fore.RED}[{self.client_id}] Не подключен или не аутентифицирован{Style.RESET_ALL}"
            )
            return False

        if channel not in self.subscribed_channels:
            print(
                f"{Fore.YELLOW}[{self.client_id}] Не подписан на канал: {channel}{Style.RESET_ALL}"
            )
            return False

        try:
            unsubscribe_message = {
                "type": "unsubscribe",
                "user_id": self.client_id,
                "channel": channel,
            }
            await self.websocket.send(json.dumps(unsubscribe_message))
            print(
                f"{Fore.CYAN}[{self.client_id}] Отправлен запрос на отписку от канала: {channel}{Style.RESET_ALL}"
            )

            # Получаем ответ отписки
            response = await self.websocket.recv()
            print(f"{Fore.CYAN}[{self.client_id}] Ответ отписки: {response}{Style.RESET_ALL}")

            self.subscribed_channels.remove(channel)
            return True
        except Exception as e:
            print(f"{Fore.RED}[{self.client_id}] Ошибка отписки: {e}{Style.RESET_ALL}")
            return False

    async def disconnect(self):
        """Отключение от WebSocket сервера"""
        if self.connected:
            try:
                await self.websocket.close()
                print(f"{Fore.GREEN}[{self.client_id}] Отключен{Style.RESET_ALL}")
            except Exception as e:
                print(f"{Fore.RED}[{self.client_id}] Ошибка отключения: {e}{Style.RESET_ALL}")
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
                print(f"{Fore.BLUE}[{self.client_id}] Получено: {message}{Style.RESET_ALL}")
        except websockets.exceptions.ConnectionClosedOK:
            print(f"{Fore.YELLOW}[{self.client_id}] Соединение закрыто{Style.RESET_ALL}")
        except websockets.exceptions.ConnectionClosedError as e:
            print(
                f"{Fore.RED}[{self.client_id}] Соединение закрыто с ошибкой: {e.code} {e.reason}{Style.RESET_ALL}"
            )
        except Exception as e:
            print(f"{Fore.RED}[{self.client_id}] Ошибка при прослушивании: {e}{Style.RESET_ALL}")
        finally:
            self.connected = False


def print_metrics(url):
    """Печатает метрики Prometheus из указанного URL"""
    try:
        response = requests.get(url)
        if response.status_code == 200:
            metrics = response.text
            print(f"\n{Fore.MAGENTA}{'=' * 50}")
            print("PROMETHEUS METRICS")
            print("=" * 50)

            # Фильтруем только нужные метрики и исключаем комментарии
            filtered_metrics = []
            metrics_of_interest = [
                "websocket_connections",
                "websocket_messages_total",
                "websocket_auth_total",
                "websocket_broadcast_duration_seconds",
                "connection_limits",
                "connection_rejections",
            ]

            for line in metrics.split("\n"):
                if any(x in line for x in metrics_of_interest) and not line.startswith("#"):
                    if line.strip():
                        filtered_metrics.append(line)
                        print(f"{Fore.CYAN}{line}{Style.RESET_ALL}")

            print(f"{Fore.MAGENTA}{'=' * 50}{Style.RESET_ALL}")
            print(f"Найдено {len(filtered_metrics)} строк метрик")

            return filtered_metrics
        else:
            print(f"{Fore.RED}Ошибка получения метрик: {response.status_code}{Style.RESET_ALL}")
            return []
    except Exception as e:
        print(f"{Fore.RED}Ошибка при запросе метрик: {e}{Style.RESET_ALL}")
        return []


async def test_single_client(server_url, metrics_url):
    """Тест одного клиента - основной сценарий использования"""
    print(f"\n{Fore.GREEN}=== ТЕСТ ОДНОГО КЛИЕНТА ==={Style.RESET_ALL}")

    client = WebSocketClient(server_url, f"test_user_{uuid.uuid4().hex[:8]}")

    # Подключаемся и аутентифицируемся
    success = await client.connect()
    if not success:
        return

    await client.authenticate()

    # Запускаем прослушивание в фоновом режиме
    listener_task = asyncio.create_task(client.listener())

    # Проверяем метрики после аутентификации
    print(f"\n{Fore.GREEN}Метрики после аутентификации:{Style.RESET_ALL}")
    print_metrics(metrics_url)

    # Подписываемся на канал
    test_channel = f"test_channel_{uuid.uuid4().hex[:6]}"
    await client.subscribe(test_channel)

    # Проверяем метрики после подписки
    print(f"\n{Fore.GREEN}Метрики после подписки на канал:{Style.RESET_ALL}")
    print_metrics(metrics_url)

    # Отправляем несколько сообщений
    for i in range(3):
        await client.send_message(test_channel, f"Тестовое сообщение {i + 1}")
        await asyncio.sleep(0.5)

    # Проверяем метрики после отправки сообщений
    print(f"\n{Fore.GREEN}Метрики после отправки сообщений:{Style.RESET_ALL}")
    print_metrics(metrics_url)

    # Отписываемся от канала
    await client.unsubscribe(test_channel)

    # Отключаемся
    await client.disconnect()

    # Проверяем метрики после отключения
    print(f"\n{Fore.GREEN}Метрики после отключения:{Style.RESET_ALL}")
    print_metrics(metrics_url)

    # Останавливаем прослушивание
    listener_task.cancel()


async def test_multiple_clients(server_url, metrics_url, num_clients=5):
    """Тест множественных клиентов для проверки счетчиков соединений"""
    print(f"\n{Fore.GREEN}=== ТЕСТ {num_clients} КЛИЕНТОВ ==={Style.RESET_ALL}")

    clients = []
    listener_tasks = []

    print(f"Создание {num_clients} клиентов...")
    for i in range(num_clients):
        client = WebSocketClient(server_url, f"multi_user_{i + 1}")
        clients.append(client)

        # Подключаем клиентов
        success = await client.connect()
        if success:
            await client.authenticate()

            # Запускаем прослушивание
            listener_task = asyncio.create_task(client.listener())
            listener_tasks.append(listener_task)

            # Подписываемся на каналы
            channels = [f"multi_channel_{j + 1}" for j in range(random.randint(1, 3))]
            for channel in channels:
                await client.subscribe(channel)
                await asyncio.sleep(0.1)  # Небольшая пауза между подписками

    # Проверяем метрики после подключения всех клиентов
    print(f"\n{Fore.GREEN}Метрики после подключения {len(clients)} клиентов:{Style.RESET_ALL}")
    print_metrics(metrics_url)

    # Отправка сообщений от случайных клиентов
    print("\nОтправка сообщений от случайных клиентов...")
    for _ in range(min(10, num_clients * 2)):
        client = random.choice(clients)
        if client.connected and client.authenticated and client.subscribed_channels:
            channel = random.choice(list(client.subscribed_channels))
            await client.send_message(channel, f"Тестовое сообщение в {channel}")
        await asyncio.sleep(0.2)  # Небольшая пауза между сообщениями

    # Проверяем метрики после отправки сообщений
    print(f"\n{Fore.GREEN}Метрики после отправки сообщений:{Style.RESET_ALL}")
    print_metrics(metrics_url)

    # Отключаем половину клиентов
    disconnect_count = len(clients) // 2
    print(f"\nОтключение {disconnect_count} клиентов...")
    for client in clients[:disconnect_count]:
        await client.disconnect()

    # Проверяем метрики после отключения части клиентов
    print(f"\n{Fore.GREEN}Метрики после отключения части клиентов:{Style.RESET_ALL}")
    print_metrics(metrics_url)

    # Отключаем оставшихся клиентов
    print("\nОтключение оставшихся клиентов...")
    for client in clients[disconnect_count:]:
        await client.disconnect()

    # Останавливаем задачи прослушивания
    for task in listener_tasks:
        task.cancel()

    # Финальная проверка метрик
    print(f"\n{Fore.GREEN}Финальные метрики после отключения всех клиентов:{Style.RESET_ALL}")
    print_metrics(metrics_url)


async def test_connection_limits(server_url, metrics_url, num_clients=12):
    """Тест лимитов подключений для проверки защиты от DoS"""
    print(
        f"\n{Fore.GREEN}=== ТЕСТ ЛИМИТОВ ПОДКЛЮЧЕНИЙ ({num_clients} попыток) ==={Style.RESET_ALL}"
    )

    clients = []
    client_prefix = f"limit_test_{uuid.uuid4().hex[:6]}"

    print(f"Тестирование лимитов подключений с {num_clients} клиентами с одинаковым префиксом...")

    for i in range(num_clients):
        # Используем один префикс ID для имитации одного IP
        client = WebSocketClient(server_url, f"{client_prefix}_{i + 1}")
        print(f"Попытка подключения клиента {i + 1}...")

        success = await client.connect()
        if success:
            clients.append(client)
            print(f"{Fore.GREEN}Клиент {i + 1} успешно подключен{Style.RESET_ALL}")
            await client.authenticate()
        else:
            print(
                f"{Fore.YELLOW}Клиент {i + 1} не смог подключиться (возможно, достигнут лимит){Style.RESET_ALL}"
            )

        await asyncio.sleep(0.2)  # Пауза между попытками подключения

    # Проверяем метрики для лимитов подключений
    print(f"\n{Fore.GREEN}Метрики лимитов подключений:{Style.RESET_ALL}")
    metrics = print_metrics(metrics_url)

    # Находим значения лимитов в метриках
    [m for m in metrics if "connection_limits" in m]
    rejection_metrics = [m for m in metrics if "connection_rejections" in m]

    print(
        f"\n{Fore.GREEN}Успешно подключено {len(clients)} из {num_clients} клиентов{Style.RESET_ALL}"
    )

    if rejection_metrics:
        print(
            f"{Fore.YELLOW}Обнаружены отклонения подключений: {rejection_metrics}{Style.RESET_ALL}"
        )

    # Отключаем клиентов
    for client in clients:
        await client.disconnect()

    # Финальная проверка метрик после отключения
    print(f"\n{Fore.GREEN}Финальные метрики после отключения всех клиентов:{Style.RESET_ALL}")
    print_metrics(metrics_url)


async def test_invalid_auth(server_url, metrics_url):
    """Тест неверной аутентификации"""
    print(f"\n{Fore.GREEN}=== ТЕСТ НЕВЕРНОЙ АУТЕНТИФИКАЦИИ ==={Style.RESET_ALL}")

    # Клиент с неверным токеном
    invalid_client = WebSocketClient(server_url, "invalid_user", token="invalid_token")
    success = await invalid_client.connect()

    if success:
        auth_success = await invalid_client.authenticate()
        if not auth_success:
            print(
                f"{Fore.YELLOW}Аутентификация с неверным токеном корректно отклонена{Style.RESET_ALL}"
            )
        else:
            print(
                f"{Fore.RED}ОШИБКА: Аутентификация с неверным токеном неожиданно прошла успешно!{Style.RESET_ALL}"
            )

    # Проверяем метрики аутентификации
    print(f"\n{Fore.GREEN}Метрики аутентификации:{Style.RESET_ALL}")
    metrics = print_metrics(metrics_url)

    # Ищем метрики аутентификации
    auth_metrics = [m for m in metrics if "websocket_auth_total" in m]

    if auth_metrics:
        print(f"{Fore.CYAN}Метрики аутентификации: {auth_metrics}{Style.RESET_ALL}")

    # Отключаем клиента
    await invalid_client.disconnect()


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
        help="Тип теста для запуска",
    )
    parser.add_argument(
        "--clients",
        type=int,
        default=5,
        help="Количество клиентов для теста множественных подключений",
    )

    args = parser.parse_args()

    print(f"{Fore.GREEN}=========================================")
    print("ТЕСТИРОВАНИЕ МЕТРИК PROMETHEUS WEBSOCKET СЕРВЕРА")
    print("=========================================")
    print(f"WebSocket сервер: {args.server}")
    print(f"Metrics endpoint: {args.metrics}")
    print(f"Выбранные тесты: {args.test}{Style.RESET_ALL}")

    if args.test == "single" or args.test == "all":
        await test_single_client(args.server, args.metrics)

    if args.test == "multiple" or args.test == "all":
        await test_multiple_clients(args.server, args.metrics, args.clients)

    if args.test == "limits" or args.test == "all":
        await test_connection_limits(args.server, args.metrics)

    if args.test == "auth" or args.test == "all":
        await test_invalid_auth(args.server, args.metrics)

    print(f"\n{Fore.GREEN}=========================================")
    print("ТЕСТИРОВАНИЕ ЗАВЕРШЕНО")
    print("=========================================")
    print(f"Все тесты выполнены{Style.RESET_ALL}")


if __name__ == "__main__":
    asyncio.run(main())
