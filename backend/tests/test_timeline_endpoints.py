"""
Тесты для эндпоинтов временной шкалы памяти.
"""

import asyncio
import os
import sys
from datetime import datetime, timedelta
from pathlib import Path

import pytest

# Устанавливаем переменную окружения для тестового режима
os.environ["TESTING"] = "1"

# Добавляем корневую директорию в PYTHONPATH
project_root = str(Path(__file__).parent.parent)
sys.path.insert(0, project_root)

# Монтируем мок-модуль в sys.modules до импорта приложения
import types

# Импортируем мок-реализацию neo4j_writer
from tests.mock_neo4j_writer import Neo4jDateTime, Neo4jWriter

mock_neo4j = types.ModuleType("neo4j_writer")
mock_neo4j.Neo4jWriter = Neo4jWriter
mock_neo4j.Neo4jDateTime = Neo4jDateTime
sys.modules["neo4j_writer"] = mock_neo4j

from api import app, memory_timeline
# Теперь можно импортировать приложение
from fastapi.testclient import TestClient


# Фикстура для тестового клиента
@pytest.fixture
def client():
    with TestClient(app) as test_client:
        yield test_client


# Фикстура для очистки временной шкалы перед каждым тестом
@pytest.fixture(autouse=True)
def reset_timeline():
    """Очищает временную шкалу перед каждым тестом."""
    memory_timeline.timeline = []
    memory_timeline.subscribers = []


# Тестовые данные
TEST_MEMORY = {
    "content": "Тестовое воспоминание",
    "memory_type": "test",
    "metadata": {"key": "value"},
}


@pytest.mark.asyncio
async def test_add_memory(client):
    """Тестирование добавления нового воспоминания."""
    response = client.post("/timeline/memories/", json=TEST_MEMORY)
    assert response.status_code == 201  # 201 Created для успешного создания
    data = response.json()
    assert "id" in data
    assert data["content"] == TEST_MEMORY["content"]
    assert data["type"] == TEST_MEMORY["memory_type"]
    assert "timestamp" in data


@pytest.mark.asyncio
async def test_get_memories(client):
    """Тестирование получения списка воспоминаний."""
    # Сначала добавляем тестовое воспоминание
    response = client.post("/timeline/memories/", json=TEST_MEMORY)
    assert response.status_code == 201

    # Получаем список воспоминаний
    response = client.get("/timeline/memories/")
    assert response.status_code == 200
    memories = response.json()
    assert isinstance(memories, list)
    assert len(memories) == 1  # Должно быть ровно одно воспоминание
    assert memories[0]["content"] == TEST_MEMORY["content"]
    assert memories[0]["type"] == TEST_MEMORY["memory_type"]


@pytest.mark.asyncio
async def test_get_memories_with_filters(client):
    """Тестирование фильтрации воспоминаний по типу."""
    # Добавляем тестовые данные
    test_memories = [
        {"content": "Воспоминание 1", "memory_type": "old"},
        {"content": "Воспоминание 2", "memory_type": "new"},
        {"content": "Воспоминание 3", "memory_type": "old"},
    ]

    for memory in test_memories:
        response = client.post("/timeline/memories/", json=memory)
        assert response.status_code == 201

    # Фильтр по типу old
    response = client.get("/timeline/memories/?memory_type=old")
    assert response.status_code == 200
    memories = response.json()
    assert len(memories) == 2
    assert all(m["type"] == "old" for m in memories)

    # Фильтр по типу new
    response = client.get("/timeline/memories/?memory_type=new")
    assert response.status_code == 200
    memories = response.json()
    assert len(memories) == 1
    assert all(m["type"] == "new" for m in memories)


@pytest.mark.asyncio
async def test_websocket_updates():
    """Тестирование обновлений через WebSocket."""
    # Используем отдельный клиент для WebSocket теста
    with TestClient(app) as client:
        with client.websocket_connect("/ws/timeline") as websocket:
            # Отправляем новое сообщение
            response = client.post(
                "/timeline/memories/",
                json={"content": "WebSocket тест", "memory_type": "test"},
            )
            assert response.status_code == 201

            # Проверяем, что получили обновление через WebSocket
            data = websocket.receive_json()
            assert data["event"] == "memory_added"
            assert data["data"]["content"] == "WebSocket тест"
            assert data["data"]["type"] == "test"
