#!/usr/bin/env python3

"""
Resonance Liminal - Диагностический инструмент для WebSocket relay
----------------------------------------------------------------
Philosophy-First подход к тестированию состояний сознания
"""

import asyncio
import json
import logging
import time
from datetime import datetime

import pytest
import requests
import websockets
from rich.console import Console
from rich.panel import Panel
from rich.table import Table

# Маркер: это интеграционные диагностические проверки, требуют живого сервера
pytestmark = pytest.mark.integration

# Настройка логирования
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[logging.StreamHandler()],
)

# Красивый вывод с rich
console = Console()

# Конфигурация
WS_URL = "ws://localhost:8080/ws"
API_URL = "http://localhost:8080"


@pytest.mark.asyncio
async def test_websocket_connection():
    """Проверка подключения к WebSocket серверу"""
    console.print("[bold blue]Тестирование WebSocket соединения[/bold blue]")

    try:
        console.print(f"Подключение к {WS_URL}...")
        connection_start = time.time()
        ws = await websockets.connect(WS_URL, ping_interval=None, close_timeout=5)
        connection_time = time.time() - connection_start

        console.print(f"[green]✓[/green] Соединение установлено за {connection_time:.2f} сек")

        # Отправка ping для проверки
        await ws.send(json.dumps({"type": "ping", "timestamp": datetime.now().isoformat()}))
        console.print("Отправлен ping...")

        try:
            # Пробуем получить ответ в течение 5 секунд
            response = await asyncio.wait_for(ws.recv(), timeout=5.0)
            console.print(f"[green]✓[/green] Получен ответ: {response[:100]}...")
        except TimeoutError:
            console.print(
                "[yellow]⚠[/yellow] Не получен ответ на ping (это нормально, если сервер не настроен на ответ)"
            )

        await ws.close()
        console.print("[green]✓[/green] Соединение успешно закрыто")
        assert True, "WebSocket соединение работает"
    except Exception as e:
        console.print(f"[red]✗[/red] Ошибка WebSocket: {str(e)}")
        assert False, f"Ошибка: {str(e)}"


def test_graphql_endpoint():
    """Проверка доступности GraphQL endpoint"""
    console.print("\n[bold blue]Тестирование GraphQL API[/bold blue]")

    try:
        console.print(f"Запрос к {API_URL}/graphql...")
        response = requests.get(f"{API_URL}/graphql", timeout=5)

        if response.status_code == 200:
            console.print(f"[green]✓[/green] GraphQL API доступен (HTTP {response.status_code})")
            assert True, "GraphQL API доступен"
        else:
            console.print(f"[yellow]⚠[/yellow] GraphQL API вернул код {response.status_code}")
            assert False, f"HTTP код {response.status_code}"
    except Exception as e:
        console.print(f"[red]✗[/red] Ошибка доступа к GraphQL API: {str(e)}")
        assert False, f"Ошибка: {str(e)}"


def test_consciousness_graph_api():
    """Проверка API для графа сознания"""
    console.print("\n[bold blue]Тестирование API графа сознания[/bold blue]")

    try:
        console.print(f"Запрос к {API_URL}/api/consciousness/graph...")
        response = requests.get(f"{API_URL}/api/consciousness/graph", timeout=5)

        if response.status_code == 200:
            graph_data = response.json()
            node_count = len(graph_data.get("nodes", []))
            link_count = len(graph_data.get("links", []))

            console.print(f"[green]✓[/green] Граф получен: {node_count} узлов, {link_count} связей")

            # Вывод списка состояний
            if node_count > 0:
                table = Table(title="Состояния сознания")
                table.add_column("ID", style="cyan")
                table.add_column("Название", style="green")
                table.add_column("Описание")

                for node in graph_data.get("nodes", [])[:5]:  # Показываем только первые 5
                    table.add_row(
                        node.get("id", ""),
                        node.get("label", ""),
                        (
                            node.get("description", "")[:50] + "..."
                            if len(node.get("description", "")) > 50
                            else node.get("description", "")
                        ),
                    )

                console.print(table)

            assert True, f"{node_count} узлов, {link_count} связей"
        else:
            console.print(f"[yellow]⚠[/yellow] API графа вернул код {response.status_code}")
            assert False, f"HTTP код {response.status_code}"
    except Exception as e:
        console.print(f"[red]✗[/red] Ошибка доступа к API графа: {str(e)}")
        assert False, f"Ошибка: {str(e)}"


def test_events_api():
    """Проверка API для отправки событий"""
    console.print("\n[bold blue]Тестирование API событий[/bold blue]")

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
        console.print(f"Отправка события на {API_URL}/events...")
        console.print(f"Данные: {json.dumps(test_event, indent=2)}")

        response = requests.post(
            f"{API_URL}/events",
            json=test_event,
            headers={"Content-Type": "application/json"},
            timeout=5,
        )

        if response.status_code == 200:
            console.print(
                f"[green]✓[/green] Событие успешно отправлено (HTTP {response.status_code})"
            )
            try:
                response_data = response.json()
                console.print(f"Ответ: {json.dumps(response_data, indent=2)}")
            except:
                console.print(f"Ответ: {response.text[:100]}...")

            assert True, "Событие успешно отправлено"
        else:
            console.print(f"[yellow]⚠[/yellow] API событий вернул код {response.status_code}")
            console.print(f"Ответ: {response.text[:100]}...")
            assert False, f"HTTP код {response.status_code}"
    except Exception as e:
        console.print(f"[red]✗[/red] Ошибка отправки события: {str(e)}")
        assert False, f"Ошибка: {str(e)}"


async def run_diagnostics():
    """Запуск всех диагностических тестов"""
    console.clear()
    console.print(
        Panel.fit(
            "[bold green]Resonance Liminal[/bold green] - [bold]Диагностика Philosophy-First WebSocket Relay[/bold]",
            border_style="blue",
        )
    )

    # Сохраняем результаты всех тестов
    results = {}

    # Тест 1: Проверка GraphQL API
    try:
        test_graphql_endpoint()
        results["graphql"] = (True, "GraphQL API доступен")
    except AssertionError as e:
        results["graphql"] = (False, str(e))


    # Тест 2: Проверка API графа сознания
    try:
        test_consciousness_graph_api()
        results["graph"] = (True, "API графа сознания доступно")
    except AssertionError as e:
        results["graph"] = (False, str(e))

    # Тест 3: Проверка WebSocket соединения
    try:
        await test_websocket_connection()
        results["websocket"] = (True, "WebSocket соединение работает")
    except AssertionError as e:
        results["websocket"] = (False, str(e))

    # Тест 4: Проверка API событий
    try:
        test_events_api()
        results["events"] = (True, "API событий работает")
    except AssertionError as e:
        results["events"] = (False, str(e))


    # Итоговая таблица
    console.print("\n[bold]Итоги диагностики:[/bold]")
    table = Table(show_header=True, header_style="bold")
    table.add_column("Компонент", style="cyan")
    table.add_column("Статус", style="green")
    table.add_column("Описание")

    for component, (success, message) in results.items():
        status = "[green]✓ Работает[/green]" if success else "[red]✗ Ошибка[/red]"
        table.add_row(component, status, message)

    console.print(table)

    # Общий вывод
    all_success = all(success for success, _ in results.values())
    if all_success:
        console.print(
            Panel(
                "[green bold]Все тесты успешно пройдены![/green bold] WebSocket relay готов к работе.",
                border_style="green",
            )
        )
    else:
        console.print(
            Panel(
                "[yellow bold]Обнаружены проблемы[/yellow bold] в работе некоторых компонентов.",
                border_style="yellow",
            )
        )


if __name__ == "__main__":
    # Проверяем наличие rich
    try:
        pass
    except ImportError:
        print("Установка пакета rich для красивого вывода...")
        import subprocess

        subprocess.call(["pip", "install", "rich"])

    # Запускаем диагностику
    asyncio.run(run_diagnostics())
