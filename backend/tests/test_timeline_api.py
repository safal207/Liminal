"""
Тесты для API временной шкалы памяти.
"""

import pytest


# Базовый тест для проверки работоспособности
def test_health_check(client):
    """Проверка доступности API."""
    response = client.get("/")
    assert response.status_code == 200
    assert "message" in response.json()


def test_add_and_get_memory(client):
    """Тестирование добавления и получения воспоминания."""
    # Добавляем воспоминание
    test_data = {"content": "Тестовое воспоминание", "memory_type": "test"}

    # Проверяем добавление
    response = client.post("/timeline/memories/", json=test_data)
    assert response.status_code == 201
    data = response.json()
    assert "id" in data

    # Проверяем получение списка
    response = client.get("/timeline/memories/")
    assert response.status_code == 200
    memories = response.json()
    assert len(memories) == 1
    assert memories[0]["content"] == test_data["content"]


def test_websocket_connection(client):
    """Проверка подключения WebSocket."""
    with client.websocket_connect("/ws/timeline") as websocket:
        # Проверяем, что соединение установлено
        assert websocket.client_state.value == 1  # WebSocketState.CONNECTED


# Запуск тестов
if __name__ == "__main__":
    import sys

    import pytest

    sys.exit(pytest.main(["-v", __file__]))
