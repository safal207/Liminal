#!/usr/bin/env python3
"""
Простой тест WebSocket соединения с JWT аутентификацией
"""

import asyncio
import json

import requests
import websockets
from colorama import Fore, Style, init

# Инициализация colorama для Windows
init()


async def test_websocket_connection():
    """Простой тест WebSocket соединения"""

    print(f"{Fore.CYAN}=== ПРОСТОЙ ТЕСТ WEBSOCKET С JWT ==={Style.RESET_ALL}")

    # 1. Получаем JWT токен
    print(f"{Fore.YELLOW}1. Получение JWT токена...{Style.RESET_ALL}")
    try:
        response = requests.post(
            "http://localhost:8000/token",
            json={"username": "testuser", "password": "testpass"},
        )

        if response.status_code == 200:
            token_data = response.json()
            token = token_data.get("access_token")
            print(f"{Fore.GREEN}✓ Токен получен успешно{Style.RESET_ALL}")
        else:
            print(f"{Fore.RED}✗ Ошибка получения токена: {response.status_code}{Style.RESET_ALL}")
            print(f"Ответ: {response.text}")
            return False

    except Exception as e:
        print(f"{Fore.RED}✗ Ошибка при запросе токена: {e}{Style.RESET_ALL}")
        return False

    # 2. Подключаемся к WebSocket
    print(f"{Fore.YELLOW}2. Подключение к WebSocket...{Style.RESET_ALL}")

    ws_url = f"ws://localhost:8000/ws/timeline?token={token}"
    print(f"URL: {ws_url}")

    try:
        async with websockets.connect(ws_url) as websocket:
            print(f"{Fore.GREEN}✓ WebSocket подключение установлено{Style.RESET_ALL}")

            # 3. Ждем сообщение от сервера
            print(f"{Fore.YELLOW}3. Ожидание сообщения от сервера...{Style.RESET_ALL}")

            try:
                # Ждем первое сообщение (обычно подтверждение аутентификации)
                message = await asyncio.wait_for(websocket.recv(), timeout=5.0)
                print(f"{Fore.CYAN}Получено сообщение: {message}{Style.RESET_ALL}")

                # Пробуем распарсить как JSON
                try:
                    msg_data = json.loads(message)
                    msg_type = msg_data.get("type", "unknown")
                    print(f"{Fore.GREEN}✓ Тип сообщения: {msg_type}{Style.RESET_ALL}")
                except json.JSONDecodeError:
                    print(f"{Fore.YELLOW}Сообщение не в формате JSON{Style.RESET_ALL}")

                # 4. Отправляем тестовое сообщение
                print(f"{Fore.YELLOW}4. Отправка тестового сообщения...{Style.RESET_ALL}")

                test_message = {"type": "subscribe", "channel": "timeline"}

                await websocket.send(json.dumps(test_message))
                print(f"{Fore.GREEN}✓ Тестовое сообщение отправлено{Style.RESET_ALL}")

                # Ждем ответ
                try:
                    response_msg = await asyncio.wait_for(websocket.recv(), timeout=5.0)
                    print(f"{Fore.CYAN}Ответ сервера: {response_msg}{Style.RESET_ALL}")
                except TimeoutError:
                    print(f"{Fore.YELLOW}Тайм-аут ожидания ответа (это нормально){Style.RESET_ALL}")

                print(f"{Fore.GREEN}✓ Тест завершен успешно!{Style.RESET_ALL}")
                return True

            except TimeoutError:
                print(f"{Fore.YELLOW}Тайм-аут ожидания первого сообщения{Style.RESET_ALL}")
                print(f"{Fore.GREEN}✓ Подключение работает (сервер молчит){Style.RESET_ALL}")
                return True

    except websockets.exceptions.InvalidStatusCode as e:
        print(f"{Fore.RED}✗ Ошибка статуса WebSocket: {e}{Style.RESET_ALL}")
        return False

    except websockets.exceptions.ConnectionClosed as e:
        print(f"{Fore.RED}✗ Соединение закрыто: код {e.code}, причина: {e.reason}{Style.RESET_ALL}")
        return False

    except Exception as e:
        print(f"{Fore.RED}✗ Неожиданная ошибка: {e}{Style.RESET_ALL}")
        import traceback

        traceback.print_exc()
        return False


async def test_metrics_endpoint():
    """Тест эндпоинта метрик"""
    print(f"{Fore.YELLOW}5. Проверка эндпоинта метрик...{Style.RESET_ALL}")

    try:
        response = requests.get("http://localhost:8000/metrics")
        if response.status_code == 200:
            metrics_text = response.text
            print(f"{Fore.GREEN}✓ Метрики доступны ({len(metrics_text)} символов){Style.RESET_ALL}")

            # Ищем WebSocket метрики
            if "websocket" in metrics_text.lower():
                print(f"{Fore.GREEN}✓ WebSocket метрики найдены{Style.RESET_ALL}")
            else:
                print(f"{Fore.YELLOW}WebSocket метрики не найдены{Style.RESET_ALL}")

            return True
        else:
            print(f"{Fore.RED}✗ Ошибка доступа к метрикам: {response.status_code}{Style.RESET_ALL}")
            return False

    except Exception as e:
        print(f"{Fore.RED}✗ Ошибка при запросе метрик: {e}{Style.RESET_ALL}")
        return False


async def main():
    """Главная функция теста"""
    print(f"{Fore.CYAN}{'=' * 50}{Style.RESET_ALL}")
    print(f"{Fore.CYAN}ПРОСТОЙ ТЕСТ WEBSOCKET + JWT + МЕТРИКИ{Style.RESET_ALL}")
    print(f"{Fore.CYAN}{'=' * 50}{Style.RESET_ALL}")

    # Тест WebSocket
    ws_success = await test_websocket_connection()

    # Тест метрик
    metrics_success = await test_metrics_endpoint()

    print(f"{Fore.CYAN}{'=' * 50}{Style.RESET_ALL}")
    if ws_success and metrics_success:
        print(f"{Fore.GREEN}🎉 ВСЕ ТЕСТЫ ПРОШЛИ УСПЕШНО! 🎉{Style.RESET_ALL}")
    else:
        print(f"{Fore.RED}❌ НЕКОТОРЫЕ ТЕСТЫ НЕ ПРОШЛИ{Style.RESET_ALL}")
        print(f"WebSocket: {'✓' if ws_success else '✗'}")
        print(f"Метрики: {'✓' if metrics_success else '✗'}")
    print(f"{Fore.CYAN}{'=' * 50}{Style.RESET_ALL}")


if __name__ == "__main__":
    asyncio.run(main())
