import asyncio
import json

import pytest
import websockets
from websockets.exceptions import ConnectionClosed


class TestConnectionLimits:
    """
    Тесты ограничений подключений WebSocket.
    Проверяют защиту от DoS-атак через множественные подключения.
    """

    @pytest.fixture
    def websocket_url(self):
        """URL для подключения к WebSocket."""
        return "ws://127.0.0.1:8000/ws/timeline"

    @pytest.mark.asyncio
    async def test_connection_limit_per_ip(self, websocket_url):
        """Тест: ограничение количества подключений с одного IP."""
        connections = []
        successful_connections = 0

        try:
            # Пытаемся создать 15 подключений (лимит 10 на IP)
            for i in range(15):
                try:
                    websocket = await websockets.connect(websocket_url)
                    connections.append(websocket)
                    successful_connections += 1
                    print(f"✓ Подключение {i+1} установлено")
                except ConnectionClosed:
                    print(f"✗ Подключение {i+1} отклонено (лимит достигнут)")
                    break
                except Exception as e:
                    print(f"✗ Подключение {i+1} не удалось: {e}")
                    break

            # Должно быть не больше 10 успешных подключений
            assert (
                successful_connections <= 10
            ), f"Превышен лимит подключений: {successful_connections}"
            print(f"✓ Лимит подключений работает: {successful_connections}/10")

        finally:
            # Закрываем все подключения
            for ws in connections:
                try:
                    await ws.close()
                except Exception:
                    pass

    @pytest.mark.asyncio
    async def test_connection_stats_endpoint(self, websocket_url):
        """Тест: эндпоинт статистики подключений."""
        import httpx

        # Создаём несколько подключений
        connections = []
        try:
            for i in range(3):
                websocket = await websockets.connect(websocket_url)
                connections.append(websocket)

            # Проверяем статистику через HTTP API
            async with httpx.AsyncClient() as client:
                response = await client.get(
                    "http://127.0.0.1:8000/debug/connections/stats"
                )
                assert response.status_code == 200

                stats = response.json()
                assert "total_users" in stats
                assert "total_connections" in stats
                assert "connections_per_ip" in stats
                assert "max_connections" in stats
                assert "max_connections_per_ip" in stats

                print(f"✓ Статистика подключений: {stats}")

                # Проверяем, что есть подключения
                assert stats["total_connections"] >= 3
                assert stats["max_connections"] == 100
                assert stats["max_connections_per_ip"] == 10

        finally:
            # Закрываем все подключения
            for ws in connections:
                try:
                    await ws.close()
                except Exception:
                    pass

    @pytest.mark.asyncio
    async def test_connection_cleanup_on_disconnect(self, websocket_url):
        """Тест: корректная очистка счётчиков при отключении."""
        import httpx

        # Создаём подключение
        websocket = await websockets.connect(websocket_url)

        # Проверяем, что счётчик увеличился
        async with httpx.AsyncClient() as client:
            response = await client.get("http://127.0.0.1:8000/debug/connections/stats")
            stats_before = response.json()
            connections_before = stats_before["total_connections"]

        # Закрываем подключение
        await websocket.close()

        # Ждём немного для обработки отключения
        await asyncio.sleep(0.1)

        # Проверяем, что счётчик уменьшился
        async with httpx.AsyncClient() as client:
            response = await client.get("http://127.0.0.1:8000/debug/connections/stats")
            stats_after = response.json()
            connections_after = stats_after["total_connections"]

        assert (
            connections_after < connections_before
        ), "Счётчик подключений не уменьшился"
        print(
            f"✓ Счётчики корректно обновились: {connections_before} → {connections_after}"
        )

    @pytest.mark.asyncio
    async def test_rejected_connection_handling(self, websocket_url):
        """Тест: обработка отклонённых подключений."""
        connections = []
        rejected_count = 0

        try:
            # Создаём много подключений до достижения лимита
            for i in range(12):  # Больше лимита 10
                try:
                    websocket = await websockets.connect(websocket_url)
                    connections.append(websocket)
                    print(f"✓ Подключение {i+1} принято")
                except (ConnectionClosed, OSError) as e:
                    rejected_count += 1
                    print(f"✗ Подключение {i+1} отклонено: {e}")
                except Exception as e:
                    print(f"? Подключение {i+1} ошибка: {e}")

            print(f"✓ Принято: {len(connections)}, Отклонено: {rejected_count}")

            # Должно быть отклонено хотя бы 2 подключения
            assert (
                rejected_count >= 2 or len(connections) <= 10
            ), "Лимит подключений не работает"

        finally:
            # Закрываем все подключения
            for ws in connections:
                try:
                    await ws.close()
                except Exception:
                    pass


if __name__ == "__main__":
    # Запуск тестов напрямую
    pytest.main([__file__, "-v", "-s"])
