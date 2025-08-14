"""
Простые тесты для проверки работоспособности приложения.
"""

from fastapi.testclient import TestClient


def test_root_endpoint():
    """Проверяем, что корневой эндпоинт работает."""
    from api import app

    client = TestClient(app)
    response = client.get("/")
    assert response.status_code == 200
    assert "message" in response.json()


def test_memory_timeline():
    """Проверяем работу с временной шкалой памяти с таймаутами."""
    import pytest
    from api import app, memory_timeline
    from fastapi.testclient import TestClient

    # Используем контекстный менеджер для тестового клиента
    with TestClient(app) as client:
        # 1. Проверяем пустой список с таймаутом
        try:
            response = client.get("/timeline/memories/", timeout=5.0)
            assert response.status_code == 200
            assert response.json() == []
        except Exception as e:
            pytest.fail(f"GET /timeline/memories/ failed: {str(e)}")

        # 2. Добавляем тестовое воспоминание
        test_memory = {
            "content": "Test memory content",
            "memory_type": "test",
            "metadata": {"test": "data"},
        }

        try:
            response = client.post("/timeline/memories/", json=test_memory, timeout=5.0)
            assert response.status_code in [200, 201]
            memory_data = response.json()
            assert memory_data["content"] == test_memory["content"]
        except Exception as e:
            pytest.fail(f"POST /timeline/memories/ failed: {str(e)}")

        # 3. Проверяем, что воспоминание добавилось
        try:
            response = client.get("/timeline/memories/", timeout=5.0)
            assert response.status_code == 200
            memories = response.json()
            assert len(memories) == 1
            assert memories[0]["content"] == test_memory["content"]
            assert memories[0]["type"] == test_memory["memory_type"]
        except Exception as e:
            pytest.fail(f"Second GET /timeline/memories/ failed: {str(e)}")

        # 4. Очищаем тестовые данные
        memory_timeline.timeline = []
