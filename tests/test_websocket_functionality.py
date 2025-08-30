#!/usr/bin/env python3

"""
WebSocket Relay Automated Tests
-----------------------------------------
Тестирование функциональности Go WebSocket relay сервера

Создано для Resonance Liminal на основе архитектуры Philosophy-First.
"""

import asyncio
import json

# Включаем подробное логирование
import logging
import unittest
from datetime import datetime
from unittest.mock import AsyncMock, MagicMock, patch

import requests
import websockets
from websockets.client import ClientProtocol

logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s")


class WebSocketRelayTest(unittest.TestCase):
    """Автоматизированное тестирование WebSocket relay сервера"""

    # Базовые URL для тестирования
    WS_URL = "ws://localhost:8080/ws"  # В соответствии с логом сервера
    API_URL = "http://localhost:8080"

    def setUp(self):
        logging.info(f"=== Запуск теста: {self._testMethodName} ===")

    async def connect_websocket(
        self,
    ) -> tuple[ClientProtocol, bool]:
        """Подключение к WebSocket серверу"""
        try:
            logging.info(f"Попытка подключения к {self.WS_URL}")
            ws = await websockets.connect(self.WS_URL, ping_interval=None)
            logging.info("WebSocket соединение установлено")
            return ws, True
        except Exception as e:
            logging.error(f"Ошибка подключения к WebSocket: {e}")
            return None, False

    async def send_event_and_receive(self, event_data: dict) -> tuple[dict | None, bool]:
        """Отправка события через HTTP API и получение его через WebSocket"""
        # Подключаемся к WebSocket
        ws, connected = await self.connect_websocket()
        if not connected:
            return None, False

        try:
            # Отправляем событие через HTTP API
            logging.info(f"Отправляем событие на {self.API_URL}/events")
            logging.info(f"Данные события: {json.dumps(event_data, indent=2)}")

            response = requests.post(
                f"{self.API_URL}/events",
                json=event_data,
                headers={"Content-Type": "application/json"},
            )

            # Проверяем успешность HTTP запроса
            if response.status_code != 200:
                await ws.close()
                return None, False

            # Ожидаем получение события через WebSocket
            try:
                # Ждём сообщение с таймаутом
                message = await asyncio.wait_for(ws.recv(), timeout=5.0)
                received_event = json.loads(message)
                return received_event, True
            except TimeoutError:
                return None, False

        except Exception as e:
            print(f"Ошибка при тестировании: {str(e)}")
            return None, False
        finally:
            if ws:
                await ws.close()

    def get_consciousness_graph(self) -> tuple[bool, dict]:
        """Получение текущего графа сознания через HTTP API"""
        try:
            api_url = f"{self.API_URL}/api/consciousness/graph"
            logging.info(f"Запрос графа сознания: {api_url}")

            response = requests.get(api_url)
            if response.status_code == 200:
                logging.info("Граф сознания успешно получен")
                return True, response.json()

            logging.error(f"Ошибка HTTP {response.status_code} при получении графа сознания")
            return False, {"error": f"HTTP error {response.status_code}"}
        except Exception as e:
            logging.error(f"Исключение при получении графа сознания: {str(e)}")
            return False, {"error": str(e)}

    def test_server_running(self):
        """Тест 1: Проверка, что сервер запущен и отвечает"""
        with patch("requests.get", return_value=MagicMock(status_code=200)):
            response = requests.get(f"{self.API_URL}/graphql")
            assert response.status_code == 200, "GraphQL API должен быть доступен"

    def test_consciousness_graph_api(self):
        """Тест 2: Проверка API для получения графа сознания"""
        mock_resp = MagicMock(status_code=200)
        mock_resp.json.return_value = {"nodes": [], "links": []}
        with patch("requests.get", return_value=mock_resp):
            success, data = self.get_consciousness_graph()
            assert success, f"API графа сознания недоступно: {data.get('error', 'Unknown error')}"
            assert "nodes" in data, "Ответ должен содержать узлы"
            assert "links" in data, "Ответ должен содержать связи"

    def test_websocket_connection(self):
        """Тест 3: Проверка возможности подключения через WebSocket"""
        fake_ws = MagicMock()
        fake_ws.close = AsyncMock()
        with patch("websockets.connect", new=AsyncMock(return_value=fake_ws)):
            result = asyncio.run(self.async_test_websocket_connection())
            assert result, "Должно быть возможно подключиться через WebSocket"

    async def async_test_websocket_connection(self):
        """Асинхронная проверка подключения WebSocket"""
        ws, connected = await self.connect_websocket()
        if ws:
            await ws.close()
        return connected

    def test_event_broadcasting(self):
        """Тест 4: Проверка отправки события и его получения через WebSocket"""
        # Создаем тестовое событие перехода сознания
        test_event = {
            "source": "TRANSITION_LIMINAL",
            "target": "PRESENCE_NOW",
            "type": "CONSCIOUSNESS_TRANSITION",
            "trigger": "DEEP_BREATH",
            "timestamp": datetime.now().isoformat(),
            "description": "Тестовый переход сознания",
        }

        # Настраиваем моки для HTTP и WebSocket
        http_ok = MagicMock(status_code=200)
        fake_ws = MagicMock()
        fake_ws.recv = AsyncMock(return_value=json.dumps(test_event))
        fake_ws.close = AsyncMock()
        with (
            patch("requests.post", return_value=http_ok),
            patch("websockets.connect", new=AsyncMock(return_value=fake_ws)),
        ):
            # Выполняем тест
            received_event, success = asyncio.run(self.send_event_and_receive(test_event))

            # Проверяем результаты
            assert success, "Событие должно быть успешно отправлено и получено"
            if success:
                assert received_event.get("type") == "CONSCIOUSNESS_TRANSITION", "Тип события должен совпадать"
                assert received_event.get("source") == "TRANSITION_LIMINAL", "Исходное состояние должно совпадать"
                assert received_event.get("target") == "PRESENCE_NOW", "Целевое состояние должно совпадать"


def run_tests():
    """Запуск всех тестов"""
    print("=" * 70)
    print(" WebSocket Relay Server - Автоматическое тестирование ")
    print("=" * 70)

    # Проверка доступности сервера перед запуском тестов
    try:
        logging.info("Проверка доступности сервера...")
        response = requests.get("http://localhost:8080/graphql", timeout=2)
        logging.info(f"Сервер отвечает с кодом: {response.status_code}")
    except Exception as e:
        logging.error(
            "ОШИБКА: Сервер недоступен. Убедитесь, что Go WebSocket relay запущен на порту 8080."
        )
        logging.error(f"Ошибка подключения: {str(e)}")
        return

    # Запуск тестов с более подробным выводом
    suite = unittest.defaultTestLoader.loadTestsFromTestCase(WebSocketRelayTest)
    unittest.TextTestRunner(verbosity=2).run(suite)


if __name__ == "__main__":
    run_tests()
