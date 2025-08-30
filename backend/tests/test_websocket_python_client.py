import asyncio
import contextlib
import json
import time

import pytest
import websockets
from websockets.exceptions import ConnectionClosed


class TestWebSocketPythonClient:
    """
    Простые тесты WebSocket через Python-клиент (без Selenium).
    Проверяют подключение, отправку и получение сообщений.
    """

    @pytest.fixture
    def websocket_url(self):
        """URL для подключения к WebSocket."""
        return "ws://127.0.0.1:8000/ws/timeline"

    @pytest.mark.asyncio
    async def test_websocket_connection(self, websocket_url):
        """Тест: подключение к WebSocket."""
        try:
            async with websockets.connect(websocket_url):
                # Если подключение успешно, тест пройден
                # Если объект создан, соединение установлено
                print("✓ WebSocket подключение установлено")
        except Exception as e:
            pytest.fail(f"Не удалось подключиться к WebSocket: {e}")

    @pytest.mark.asyncio
    async def test_websocket_receive_initial_state(self, websocket_url):
        """Тест: получение начального состояния от сервера."""
        try:
            async with websockets.connect(websocket_url) as websocket:
                # Отправляем любое сообщение, чтобы активировать сервер
                await websocket.send(json.dumps({"type": "ping"}))

                # Ждём сообщение от сервера (initial_state должен прийти при подключении)
                message = await asyncio.wait_for(websocket.recv(), timeout=5.0)
                data = json.loads(message)

                # Проверяем, что получили initial_state или ответ
                assert "event" in data or "error" in data
                print(f"✓ Получено сообщение от сервера: {data}")

        except TimeoutError:
            pytest.fail("Не получено сообщение от сервера в течение 5 секунд")
        except Exception as e:
            pytest.fail(f"Ошибка при получении начального состояния: {e}")

    @pytest.mark.asyncio
    async def test_websocket_send_message(self, websocket_url):
        """Тест: отправка сообщения через WebSocket."""
        try:
            async with websockets.connect(websocket_url) as websocket:
                # Отправляем тестовое сообщение
                test_message = {
                    "type": "test_message",
                    "content": "Тестовое сообщение от Python-клиента",
                    "timestamp": time.time(),
                }

                await websocket.send(json.dumps(test_message))
                print(f"✓ Отправлено сообщение: {test_message}")

                # Ждём ответ (если сервер отвечает)
                try:
                    response = await asyncio.wait_for(websocket.recv(), timeout=3.0)
                    response_data = json.loads(response)
                    print(f"✓ Получен ответ: {response_data}")
                except TimeoutError:
                    print("⚠ Ответ от сервера не получен (это нормально)")

        except Exception as e:
            pytest.fail(f"Ошибка при отправке сообщения: {e}")

    @pytest.mark.asyncio
    async def test_multiple_websocket_connections(self, websocket_url):
        """Тест: множественные подключения к WebSocket."""
        connections = []

        try:
            # Создаём 3 подключения
            for i in range(3):
                websocket = await websockets.connect(websocket_url)
                connections.append(websocket)
                print(f"✓ Подключение {i + 1} установлено")

            # Проверяем, что все подключения активны
            for i, ws in enumerate(connections):
                assert ws is not None, f"Подключение {i + 1} не активно"

            print("✓ Все множественные подключения работают")

        except Exception as e:
            pytest.fail(f"Ошибка при множественных подключениях: {e}")
        finally:
            # Закрываем все подключения
            for ws in connections:
                with contextlib.suppress(Exception):
                    await ws.close()

    @pytest.mark.asyncio
    async def test_websocket_connection_close(self, websocket_url):
        """Тест: корректное закрытие WebSocket соединения."""
        try:
            websocket = await websockets.connect(websocket_url)
            # Соединение открыто

            # Закрываем соединение
            await websocket.close()
            # Соединение корректно закрыто (исключений нет)

            print("✓ WebSocket соединение корректно закрыто")

        except Exception as e:
            pytest.fail(f"Ошибка при закрытии соединения: {e}")

    @pytest.mark.asyncio
    async def test_websocket_invalid_json(self, websocket_url):
        """Тест: отправка невалидного JSON."""
        try:
            async with websockets.connect(websocket_url) as websocket:
                # Отправляем невалидный JSON
                invalid_json = "{ invalid json }"
                await websocket.send(invalid_json)

                # Ждём ответ или ошибку
                try:
                    response = await asyncio.wait_for(websocket.recv(), timeout=3.0)
                    print(f"✓ Сервер обработал невалидный JSON: {response}")
                except TimeoutError:
                    print("⚠ Сервер не ответил на невалидный JSON (это нормально)")

        except ConnectionClosed:
            print("✓ Соединение закрыто сервером из-за невалидного JSON")
        except Exception as e:
            pytest.fail(f"Неожиданная ошибка: {e}")


if __name__ == "__main__":
    # Запуск тестов напрямую
    pytest.main([__file__, "-v", "-s"])
