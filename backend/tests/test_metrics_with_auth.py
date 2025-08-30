"""
Интеграционный тест метрик Prometheus с JWT аутентификацией.
Этот скрипт:
1. Получает JWT токен от API
2. Устанавливает WebSocket соединение с токеном
3. Проверяет метрики Prometheus

Запуск: python tests/test_metrics_with_auth.py
"""

import argparse
import asyncio
import json
import uuid
from datetime import datetime

import requests
import websockets
from colorama import Fore, Style, init

# Инициализация colorama для цветного вывода
init()


class AuthenticatedClient:
    def __init__(self, server_url, metrics_url, api_url):
        self.server_url = server_url  # WebSocket URL
        self.metrics_url = metrics_url  # Metrics URL
        self.api_url = api_url  # API URL для получения токена
        self.client_id = f"test_user_{uuid.uuid4().hex[:8]}"
        self.token = None
        self.websocket = None
        self.connected = False
        self.subscribed_channels = set()

    async def get_token(self, username="testuser", password="testpass"):
        """Получение JWT токена через API endpoint"""
        try:
            # Предполагаем, что у вас есть эндпоинт /token для получения JWT
            token_url = f"{self.api_url}/token"

            # Для отладки используем хардкодированное значение
            print(f"{Fore.YELLOW}Пытаемся получить JWT токен из {token_url}{Style.RESET_ALL}")

            try:
                # Используем json= вместо data= для отправки JSON
                response = requests.post(
                    token_url,
                    json={"username": username, "password": password},
                    headers={"Content-Type": "application/json"},
                )

                if response.status_code == 200:
                    data = response.json()
                    self.token = data.get("access_token")
                    print(f"{Fore.GREEN}JWT токен получен успешно{Style.RESET_ALL}")
                    return True
                else:
                    print(
                        f"{Fore.RED}Ошибка получения токена: {response.status_code} - {response.text}{Style.RESET_ALL}"
                    )

                    # Создаем тестовый токен для отладки, если сервер его примет
                    # НЕ ИСПОЛЬЗОВАТЬ В ПРОДАКШЕНЕ
                    print(f"{Fore.YELLOW}Создаем тестовый токен для отладки{Style.RESET_ALL}")
                    self.token = "test_token_for_debugging"
                    return False
            except Exception as e:
                print(f"{Fore.RED}Исключение при запросе токена: {e}{Style.RESET_ALL}")
                # Создаем тестовый токен для отладки
                self.token = "test_token_for_debugging"
                return False

        except Exception as e:
            print(f"{Fore.RED}Ошибка получения токена: {e}{Style.RESET_ALL}")
            return False

    async def connect(self):
        """Подключение к WebSocket с JWT токеном"""
        if not self.token:
            print(f"{Fore.RED}Нет токена для подключения{Style.RESET_ALL}")
            return False

        try:
            # Формируем URL для подключения
            base_url = self.server_url.rstrip("/")
            # Пробуем подключиться с токеном в URL
            ws_url = f"{base_url}/timeline?token={self.token}"

            print(f"{Fore.CYAN}Попытка подключения к WebSocket: {ws_url}{Style.RESET_ALL}")

            try:
                # Увеличиваем таймаут подключения до 10 секунд
                # Убираем extra_headers, так как они не поддерживаются в текущей версии websockets
                self.websocket = await websockets.connect(
                    ws_url,
                    ping_interval=10,
                    ping_timeout=10,
                    close_timeout=10,
                    max_size=10 * 1024 * 1024,  # 10MB max message size
                )
                print(f"{Fore.GREEN}WebSocket подключение установлено{Style.RESET_ALL}")
                self.connected = True
                self.connected = True
                print(f"{Fore.GREEN}Подключен к {ws_url}{Style.RESET_ALL}")

                # Аутентификация с токеном через сообщение
                auth_result = await self.send_auth()
                if not auth_result:
                    await self.disconnect()
                    return False

                return True
            except websockets.exceptions.InvalidStatusCode as e:
                print(
                    f"{Fore.RED}Сервер отклонил подключение: HTTP {e.status_code}{Style.RESET_ALL}"
                )
                return False
            except Exception as e:
                print(f"{Fore.RED}Ошибка подключения: {e}{Style.RESET_ALL}")
                return False
        except Exception as e:
            print(f"{Fore.RED}Исключение при подключении: {e}{Style.RESET_ALL}")
            self.connected = False
            return False

    async def send_auth(self):
        """Отправка аутентификационного сообщения"""
        if not self.websocket or not self.connected:
            print(f"{Fore.RED}Нет активного WebSocket соединения{Style.RESET_ALL}")
            return False

        try:
            # Проверяем, не закрыто ли соединение (совместимость с разными версиями websockets)
            try:
                is_closed = self.websocket.closed
            except AttributeError:
                # В некоторых версиях websockets нет атрибута closed
                is_closed = False

            if is_closed:
                print(f"{Fore.RED}WebSocket соединение закрыто{Style.RESET_ALL}")
                return False

            auth_message = {"type": "auth", "access_token": self.token}

            print(
                f"{Fore.CYAN}Отправка аутентификационного сообщения: {auth_message}{Style.RESET_ALL}"
            )

            try:
                await self.websocket.send(json.dumps(auth_message))
                print(f"{Fore.GREEN}Запрос на аутентификацию отправлен{Style.RESET_ALL}")

                # Увеличиваем таймаут ожидания ответа до 10 секунд
                try:
                    response = await asyncio.wait_for(self.websocket.recv(), timeout=10.0)
                    print(f"{Fore.CYAN}Получен ответ: {response}{Style.RESET_ALL}")

                    try:
                        response_data = json.loads(response)
                        if (
                            response_data.get("type") == "auth_ok"
                            or response_data.get("type") == "auth_success"
                        ):
                            print(f"{Fore.GREEN}Аутентификация успешна{Style.RESET_ALL}")
                            return True
                        else:
                            print(
                                f"{Fore.YELLOW}Неожиданный тип ответа: {response_data.get('type')}{Style.RESET_ALL}"
                            )
                            print(f"{Fore.YELLOW}Полный ответ: {response_data}{Style.RESET_ALL}")
                            return False
                    except json.JSONDecodeError as je:
                        print(f"{Fore.RED}Ошибка декодирования JSON: {je}{Style.RESET_ALL}")
                        print(f"{Fore.RED}Полученные данные: {response}{Style.RESET_ALL}")
                        return False

                except TimeoutError:
                    print(f"{Fore.RED}Таймаут ожидания ответа на аутентификацию{Style.RESET_ALL}")
                    return False

            except websockets.exceptions.ConnectionClosed as e:
                print(
                    f"{Fore.RED}Соединение закрыто при отправке аутентификации: {e}{Style.RESET_ALL}"
                )
                print(f"{Fore.YELLOW}Код: {e.code}, причина: {e.reason}{Style.RESET_ALL}")
                return False

        except Exception as e:
            print(f"{Fore.RED}Критическая ошибка при аутентификации: {e}{Style.RESET_ALL}")
            import traceback

            traceback.print_exc()
            return False

    async def subscribe(self, channel):
        """Подписка на канал"""
        if not self.connected:
            print(f"{Fore.RED}Не подключен к серверу{Style.RESET_ALL}")
            return False

        try:
            subscribe_message = {
                "type": "subscribe",
                "user_id": self.client_id,
                "channel": channel,
            }
            await self.websocket.send(json.dumps(subscribe_message))
            print(f"{Fore.CYAN}Отправлен запрос на подписку на канал: {channel}{Style.RESET_ALL}")

            # Получаем ответ подписки (может быть асинхронным)
            try:
                response = await asyncio.wait_for(self.websocket.recv(), timeout=3.0)
                print(f"{Fore.CYAN}Ответ подписки: {response}{Style.RESET_ALL}")
            except TimeoutError:
                print(f"{Fore.YELLOW}Тайм-аут ожидания ответа подписки{Style.RESET_ALL}")

            self.subscribed_channels.add(channel)
            return True
        except Exception as e:
            print(f"{Fore.RED}Ошибка подписки: {e}{Style.RESET_ALL}")
            return False

    async def send_message(self, channel, message_text):
        """Отправка сообщения в канал"""
        if not self.connected:
            print(f"{Fore.RED}Не подключен к серверу{Style.RESET_ALL}")
            return False

        if channel not in self.subscribed_channels:
            print(f"{Fore.YELLOW}Не подписан на канал: {channel}{Style.RESET_ALL}")
            return False

        try:
            msg = {
                "type": "message",
                "user_id": self.client_id,
                "channel": channel,
                "message": {
                    "text": message_text,
                    "timestamp": datetime.now().isoformat(),
                },
            }
            await self.websocket.send(json.dumps(msg))
            print(
                f"{Fore.CYAN}Отправлено сообщение в канал {channel}: {message_text}{Style.RESET_ALL}"
            )
            return True
        except Exception as e:
            print(f"{Fore.RED}Ошибка отправки сообщения: {e}{Style.RESET_ALL}")
            return False

    async def unsubscribe(self, channel):
        """Отписка от канала"""
        if not self.connected:
            print(f"{Fore.RED}Не подключен к серверу{Style.RESET_ALL}")
            return False

        try:
            unsubscribe_message = {
                "type": "unsubscribe",
                "user_id": self.client_id,
                "channel": channel,
            }
            await self.websocket.send(json.dumps(unsubscribe_message))
            print(f"{Fore.CYAN}Отправлен запрос на отписку от канала: {channel}{Style.RESET_ALL}")

            # Получаем ответ отписки (может быть асинхронным)
            try:
                response = await asyncio.wait_for(self.websocket.recv(), timeout=3.0)
                print(f"{Fore.CYAN}Ответ отписки: {response}{Style.RESET_ALL}")
            except TimeoutError:
                print(f"{Fore.YELLOW}Тайм-аут ожидания ответа отписки{Style.RESET_ALL}")

            if channel in self.subscribed_channels:
                self.subscribed_channels.remove(channel)
            return True
        except Exception as e:
            print(f"{Fore.RED}Ошибка отписки: {e}{Style.RESET_ALL}")
            return False

    async def disconnect(self):
        """Отключение от WebSocket сервера"""
        if self.connected:
            try:
                await self.websocket.close()
                print(f"{Fore.GREEN}Отключен{Style.RESET_ALL}")
            except Exception as e:
                print(f"{Fore.RED}Ошибка отключения: {e}{Style.RESET_ALL}")
            finally:
                self.connected = False
                self.subscribed_channels.clear()

    async def listener(self, timeout=30):
        """Прослушивание сообщений"""
        if not self.connected:
            return

        try:
            end_time = asyncio.get_event_loop().time() + timeout
            while self.connected and asyncio.get_event_loop().time() < end_time:
                try:
                    message = await asyncio.wait_for(self.websocket.recv(), timeout=1.0)
                    print(f"{Fore.BLUE}[{self.client_id}] Получено: {message}{Style.RESET_ALL}")
                except TimeoutError:
                    # Тайм-аут просто для проверки подключения
                    continue
        except websockets.exceptions.ConnectionClosedOK:
            print(f"{Fore.YELLOW}Соединение закрыто{Style.RESET_ALL}")
        except websockets.exceptions.ConnectionClosedError as e:
            print(f"{Fore.RED}Соединение закрыто с ошибкой: {e.code} {e.reason}{Style.RESET_ALL}")
        except Exception as e:
            print(f"{Fore.RED}Ошибка при прослушивании: {e}{Style.RESET_ALL}")
        finally:
            self.connected = False

    def check_metrics(self):
        """Проверяет метрики Prometheus из эндпоинта"""
        try:
            response = requests.get(self.metrics_url)
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


async def test_with_auth(server_url, metrics_url, api_url):
    """Тест с аутентификацией"""
    print(f"\n{Fore.GREEN}=== ТЕСТ JWT АУТЕНТИФИКАЦИИ И МЕТРИК ==={Style.RESET_ALL}")

    client = AuthenticatedClient(server_url, metrics_url, api_url)

    # Получаем токен
    await client.get_token()

    # Подключаемся с токеном
    success = await client.connect()
    if not success:
        return

    # Запускаем прослушивание в фоновом режиме
    listener_task = asyncio.create_task(client.listener())

    # Проверяем метрики после аутентификации
    print(f"\n{Fore.GREEN}Метрики после аутентификации:{Style.RESET_ALL}")
    client.check_metrics()

    # Подписываемся на канал
    test_channel = f"test_channel_{uuid.uuid4().hex[:6]}"
    await client.subscribe(test_channel)

    # Проверяем метрики после подписки
    print(f"\n{Fore.GREEN}Метрики после подписки на канал:{Style.RESET_ALL}")
    client.check_metrics()

    # Отправляем несколько сообщений
    for i in range(3):
        await client.send_message(test_channel, f"Тестовое сообщение {i + 1}")
        await asyncio.sleep(0.5)

    # Проверяем метрики после отправки сообщений
    print(f"\n{Fore.GREEN}Метрики после отправки сообщений:{Style.RESET_ALL}")
    client.check_metrics()

    # Отписываемся от канала
    await client.unsubscribe(test_channel)

    # Отключаемся
    await client.disconnect()

    # Проверяем метрики после отключения
    print(f"\n{Fore.GREEN}Метрики после отключения:{Style.RESET_ALL}")
    client.check_metrics()

    # Останавливаем прослушивание
    listener_task.cancel()


async def main():
    parser = argparse.ArgumentParser(
        description="Тестирование метрик WebSocket с JWT аутентификацией"
    )
    parser.add_argument(
        "--server", default="ws://localhost:8000/ws/test", help="WebSocket URL сервера"
    )
    parser.add_argument(
        "--metrics",
        default="http://localhost:8000/metrics",
        help="URL метрик Prometheus",
    )
    parser.add_argument(
        "--api",
        default="http://localhost:8000",
        help="URL API для получения JWT токена",
    )

    args = parser.parse_args()

    print(f"{Fore.GREEN}=========================================")
    print("ТЕСТИРОВАНИЕ МЕТРИК PROMETHEUS С JWT АУТЕНТИФИКАЦИЕЙ")
    print("=========================================")
    print(f"WebSocket сервер: {args.server}")
    print(f"Metrics endpoint: {args.metrics}")
    print(f"API endpoint: {args.api}{Style.RESET_ALL}")

    await test_with_auth(args.server, args.metrics, args.api)

    print(f"\n{Fore.GREEN}=========================================")
    print("ТЕСТИРОВАНИЕ ЗАВЕРШЕНО")
    print("=========================================")
    print(f"Все тесты выполнены{Style.RESET_ALL}")


if __name__ == "__main__":
    asyncio.run(main())
