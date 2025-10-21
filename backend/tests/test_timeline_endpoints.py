"""Tests for timeline-related REST endpoints."""
from __future__ import annotations

from datetime import UTC, datetime, timedelta

import pytest

from backend.memory_timeline import timeline as memory_timeline

TEST_MEMORY = {
    "content": "Тестовое воспоминание",
    "memory_type": "test",
    "metadata": {"key": "value"},
}


@pytest.mark.asyncio
async def test_add_memory(client):
    """Тестирование добавления нового воспоминания."""

    response = client.post("/timeline/memories/", json=TEST_MEMORY)
    assert response.status_code == 201
    data = response.json()
    assert "id" in data
    assert data["content"] == TEST_MEMORY["content"]
    assert data["type"] == TEST_MEMORY["memory_type"]
    assert "timestamp" in data


@pytest.mark.asyncio
async def test_get_memories(client):
    """Тестирование получения списка воспоминаний."""

    response = client.post("/timeline/memories/", json=TEST_MEMORY)
    assert response.status_code == 201

    response = client.get("/timeline/memories/")
    assert response.status_code == 200
    memories = response.json()
    assert isinstance(memories, list)
    assert len(memories) == 1
    assert memories[0]["content"] == TEST_MEMORY["content"]
    assert memories[0]["type"] == TEST_MEMORY["memory_type"]


@pytest.mark.asyncio
async def test_get_memories_with_filters(client):
    """Тестирование фильтрации воспоминаний по типу."""

    test_memories = [
        {"content": "Воспоминание 1", "memory_type": "old"},
        {"content": "Воспоминание 2", "memory_type": "new"},
        {"content": "Воспоминание 3", "memory_type": "old"},
    ]

    for memory in test_memories:
        response = client.post("/timeline/memories/", json=memory)
        assert response.status_code == 201

    response = client.get("/timeline/memories/?memory_type=old")
    assert response.status_code == 200
    memories = response.json()
    assert len(memories) == 2
    assert all(m["type"] == "old" for m in memories)

    response = client.get("/timeline/memories/?memory_type=new")
    assert response.status_code == 200
    memories = response.json()
    assert len(memories) == 1
    assert all(m["type"] == "new" for m in memories)


@pytest.mark.asyncio
async def test_websocket_updates(client):
    """Проверяем, что уведомления таймлайна отправляются подписчикам."""

    events = []
    original_notify = memory_timeline._notify_subscribers

    async def fake_notify(event_type, data):
        events.append({"event": event_type, "data": data})

    memory_timeline._notify_subscribers = fake_notify

    try:
        response = client.post(
            "/timeline/memories/",
            json={"content": "WebSocket тест", "memory_type": "test"},
        )
        assert response.status_code == 201
        assert events
        assert events[-1]["event"] == "memory_added"
        assert events[-1]["data"]["content"] == "WebSocket тест"
        assert events[-1]["data"]["type"] == "test"
    finally:
        memory_timeline._notify_subscribers = original_notify


@pytest.mark.asyncio
async def test_get_memories_with_time_filters(client):
    """Проверяем фильтрацию по временным границам."""

    now = datetime.now(UTC)
    older_memory = {
        "content": "Старое воспоминание",
        "memory_type": "test",
        "metadata": {},
    }
    newer_memory = {
        "content": "Новое воспоминание",
        "memory_type": "test",
        "metadata": {},
    }

    response = client.post("/timeline/memories/", json=older_memory)
    assert response.status_code == 201
    response = client.post("/timeline/memories/", json=newer_memory)
    assert response.status_code == 201

    start_time = (now + timedelta(seconds=1)).isoformat()
    response = client.get("/timeline/memories/", params={"start_time": start_time})
    assert response.status_code == 200
    memories = response.json()
    start_dt = datetime.fromisoformat(start_time)
    assert all(datetime.fromisoformat(m["timestamp"]) >= start_dt for m in memories)
