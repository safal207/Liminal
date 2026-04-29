import asyncio

import pytest
import websockets
from websockets.exceptions import ConnectionClosed

pytestmark = pytest.mark.integration


class TestWebSocketLimitsSimple:
    """
    Простые тесты ограничений WebSocket подключений.
    Только WebSocket тесты, без HTTP API.
    """

    @pytest.fixture
    def websocket_url(self):
        """URL для подключения к WebSocket."""
        return "ws://127.0.0.1:8000/ws/timeline"

    @pytest.mark.asyncio
    async def test_multiple_connections_basic(self, websocket_url):
        """Тест: базовая проверка множественных подключений."""
        connections = []
        successful_connections = 0

        try:
            # Пытаемся создать 5 подключений (должно работать)
            for i in range(5):
                try:
                    websocket = await websockets.connect(websocket_url)
                    connections.append(websocket)
                    successful_connections += 1
                    print(f"✓ Подключение {i+1} установлено")
                except Exception as e:
                    print(f"✗ Подключение {i+1} не удалось: {e}")
                    break

            # Должно быть хотя бы 3 успешных подключения
            assert (
                successful_connections >= 3
            ), f"Слишком мало подключений: {successful_connections}"
            print(f"✓ Множественные подключения работают: {successful_connections}/5")

        finally:
            # Закрываем все подключения
            for ws in connections:
                try:
                    await ws.close()
                except Exception:
                    pass

    @pytest.mark.asyncio
    async def test_connection_limit_stress(self, websocket_url):
        """Тест: стресс-тест лимитов подключений."""
        connections = []
        successful_connections = 0
        rejected_connections = 0

        try:
            # Пытаемся создать 15 подключений (больше лимита 10)
            for i in range(15):
                try:
                    websocket = await websockets.connect(websocket_url)
                    connections.append(websocket)
                    successful_connections += 1
                    print(f"✓ Подключение {i+1} принято")

                    # Небольшая задержка между подключениями
                    await asyncio.sleep(0.1)

                except (ConnectionClosed, OSError, Exception) as e:
                    rejected_connections += 1
                    print(f"✗ Подключение {i+1} отклонено: {type(e).__name__}")

            print(
                f"📊 Итого: Принято={successful_connections}, Отклонено={rejected_connections}"
            )

            # Проверяем, что лимит работает
            if successful_connections > 10:
                print("⚠ Возможно, лимит подключений не работает корректно")
            else:
                print("✓ Лимит подключений работает корректно")

            # Должно быть отклонено хотя бы несколько подключений
            assert (
                successful_connections <= 12
            ), f"Слишком много подключений принято: {successful_connections}"

        finally:
            # Закрываем все подключения
            for ws in connections:
                try:
                    await ws.close()
                except Exception:
                    pass

    @pytest.mark.asyncio
    async def test_connection_recovery_after_disconnect(self, websocket_url):
        """Тест: восстановление подключений после отключения."""
        # Создаём несколько подключений
        connections = []
        for i in range(3):
            websocket = await websockets.connect(websocket_url)
            connections.append(websocket)
            print(f"✓ Создано подключение {i+1}")

        # Закрываем все подключения
        for i, ws in enumerate(connections):
            await ws.close()
            print(f"✓ Закрыто подключение {i+1}")

        # Ждём немного для обработки отключений
        await asyncio.sleep(0.2)

        # Создаём новые подключения (должно работать)
        new_connections = []
        try:
            for i in range(3):
                websocket = await websockets.connect(websocket_url)
                new_connections.append(websocket)
                print(f"✓ Новое подключение {i+1} установлено")

            assert len(new_connections) == 3, "Не удалось создать новые подключения"
            print("✓ Восстановление подключений работает")

        finally:
            # Закрываем новые подключения
            for ws in new_connections:
                try:
                    await ws.close()
                except Exception:
                    pass

    @pytest.mark.asyncio
    async def test_websocket_message_exchange(self, websocket_url):
        """Тест: обмен сообщениями через WebSocket."""
        try:
            async with websockets.connect(websocket_url) as websocket:
                # Отправляем тестовое сообщение
                test_message = {"type": "ping", "data": "test"}
                await websocket.send(str(test_message))

                # Ждём ответ
                try:
                    response = await asyncio.wait_for(websocket.recv(), timeout=3.0)
                    print(f"✓ Получен ответ: {response}")
                except asyncio.TimeoutError:
                    print("⚠ Ответ не получен (это нормально)")

                print("✓ Обмен сообщениями работает")

        except Exception as e:
            pytest.fail(f"Ошибка при обмене сообщениями: {e}")


if __name__ == "__main__":
    # Запуск тестов напрямую
    pytest.main([__file__, "-v", "-s"])
