#!/usr/bin/env python3

"""
Resonance Liminal - Простой диагностический инструмент для WebSocket relay
-------------------------------------------------------------------------
Philosophy-First подход к тестированию состояний сознания без внешних зависимостей
"""

import asyncio
import json
import time
from datetime import datetime

import requests
import websockets

# Конфигурация
WS_URL = "ws://localhost:8080/ws"
API_URL = "http://localhost:8080"
DIVIDER = "=" * 70


async def test_websocket_connection():
    """Проверка подключения к WebSocket серверу"""
    print(f"\n{DIVIDER}")
    print("ТЕСТ: WebSocket соединение")
    print(f"{DIVIDER}")

    try:
        print(f"Подключение к {WS_URL}...")
        connection_start = time.time()
        ws = await websockets.connect(WS_URL, ping_interval=None, close_timeout=5)
        connection_time = time.time() - connection_start

        print(f"✓ Соединение установлено за {connection_time:.2f} сек")

        # Отправка ping для проверки
        await ws.send(json.dumps({"type": "ping", "timestamp": datetime.now().isoformat()}))
        print("Отправлен ping...")

        try:
            # Пробуем получить ответ в течение 5 секунд
            response = await asyncio.wait_for(ws.recv(), timeout=5.0)
            print(f"✓ Получен ответ: {response[:100]}...")
        except TimeoutError:
            print("⚠ Не получен ответ на ping (это нормально, если сервер не настроен на ответ)")

        await ws.close()
        print("✓ Соединение успешно закрыто")
        return True, "WebSocket соединение работает"
    except Exception as e:
        print(f"✗ Ошибка WebSocket: {str(e)}")
        return False, f"Ошибка: {str(e)}"


def test_graphql_endpoint():
    """Проверка доступности GraphQL endpoint"""
    print(f"\n{DIVIDER}")
    print("ТЕСТ: GraphQL API")
    print(f"{DIVIDER}")

    try:
        print(f"Запрос к {API_URL}/graphql...")
        response = requests.get(f"{API_URL}/graphql", timeout=5)

        if response.status_code == 200:
            print(f"✓ GraphQL API доступен (HTTP {response.status_code})")
            return True, "GraphQL API доступен"
        else:
            print(f"⚠ GraphQL API вернул код {response.status_code}")
            return False, f"HTTP код {response.status_code}"
    except Exception as e:
        print(f"✗ Ошибка доступа к GraphQL API: {str(e)}")
        return False, f"Ошибка: {str(e)}"


def test_consciousness_graph_api():
    """Проверка API для графа сознания"""
    print(f"\n{DIVIDER}")
    print("ТЕСТ: API графа сознания")
    print(f"{DIVIDER}")

    try:
        print(f"Запрос к {API_URL}/api/consciousness/graph...")
        response = requests.get(f"{API_URL}/api/consciousness/graph", timeout=5)

        if response.status_code == 200:
            graph_data = response.json()
            node_count = len(graph_data.get("nodes", []))
            link_count = len(graph_data.get("links", []))

            print(f"✓ Граф получен: {node_count} узлов, {link_count} связей")

            # Вывод списка состояний
            if node_count > 0:
                print("\nСостояния сознания:")
                print("-" * 40)
                print(f"{'ID':<20} | {'Название':<25} | Описание")
                print("-" * 100)

                for node in graph_data.get("nodes", [])[:5]:  # Показываем только первые 5
                    desc = node.get("description", "")
                    if len(desc) > 40:
                        desc = desc[:40] + "..."

                    print(f"{node.get('id', ''):<20} | {node.get('label', ''):<25} | {desc}")

            return True, f"{node_count} узлов, {link_count} связей"
        else:
            print(f"⚠ API графа вернул код {response.status_code}")
            return False, f"HTTP код {response.status_code}"
    except Exception as e:
        print(f"✗ Ошибка доступа к API графа: {str(e)}")
        return False, f"Ошибка: {str(e)}"


def test_events_api():
    """Проверка API для отправки событий"""
    print(f"\n{DIVIDER}")
    print("ТЕСТ: API событий")
    print(f"{DIVIDER}")

    # Создаем тестовое событие перехода сознания
    test_event = {
        "source": "TRANSITION_LIMINAL",
        "target": "PRESENCE_NOW",
        "type": "CONSCIOUSNESS_TRANSITION",
        "trigger": "DEEP_BREATH",
        "timestamp": datetime.now().isoformat(),
        "description": "Диагностический тест перехода сознания",
    }

    try:
        print(f"Отправка события на {API_URL}/events...")
        print(f"Данные: {json.dumps(test_event, indent=2)}")

        response = requests.post(
            f"{API_URL}/events",
            json=test_event,
            headers={"Content-Type": "application/json"},
            timeout=5,
        )

        if response.status_code == 200:
            print(f"✓ Событие успешно отправлено (HTTP {response.status_code})")
            try:
                response_data = response.json()
                print(f"Ответ: {json.dumps(response_data, indent=2)}")
            except:
                print(f"Ответ: {response.text[:100]}...")

            return True, "Событие успешно отправлено"
        else:
            print(f"⚠ API событий вернул код {response.status_code}")
            print(f"Ответ: {response.text[:100]}...")
            return False, f"HTTP код {response.status_code}"
    except Exception as e:
        print(f"✗ Ошибка отправки события: {str(e)}")
        return False, f"Ошибка: {str(e)}"


async def run_diagnostics():
    """Запуск всех диагностических тестов"""
    print("\n" + "=" * 100)
    print("RESONANCE LIMINAL - ДИАГНОСТИКА PHILOSOPHY-FIRST WEBSOCKET RELAY")
    print("=" * 100)

    # Сохраняем результаты всех тестов
    results = {}

    # Тест 1: Проверка GraphQL API
    results["GraphQL API"] = test_graphql_endpoint()

    # Тест 2: Проверка API графа сознания
    results["Graph API"] = test_consciousness_graph_api()

    # Тест 3: Проверка WebSocket соединения
    results["WebSocket"] = await test_websocket_connection()

    # Тест 4: Проверка API событий
    results["Events API"] = test_events_api()

    # Итоговая таблица
    print(f"\n{DIVIDER}")
    print("ИТОГИ ДИАГНОСТИКИ")
    print(f"{DIVIDER}")
    print(f"{'Компонент':<20} | {'Статус':<15} | Описание")
    print("-" * 100)

    all_success = True
    for component, (success, message) in results.items():
        status = "✓ Работает" if success else "✗ Ошибка"
        print(f"{component:<20} | {status:<15} | {message}")
        all_success = all_success and success

    print(f"\n{DIVIDER}")
    if all_success:
        print("✓ ВСЕ ТЕСТЫ УСПЕШНО ПРОЙДЕНЫ! WEBSOCKET RELAY ГОТОВ К РАБОТЕ.")
    else:
        print("⚠ ОБНАРУЖЕНЫ ПРОБЛЕМЫ В РАБОТЕ НЕКОТОРЫХ КОМПОНЕНТОВ.")
    print(f"{DIVIDER}")


if __name__ == "__main__":
    asyncio.run(run_diagnostics())
