"""
Базовые тесты для проверки работоспособности приложения.
"""

from api import app
from fastapi.testclient import TestClient


def test_root_endpoint():
    """Проверяем, что корневой эндпоинт работает."""
    client = TestClient(app)
    response = client.get("/")
    assert response.status_code == 200
    assert "message" in response.json()


def test_memory_timeline_endpoint():
    """Проверяем эндпоинт временной шкалы памяти."""
    print("\n=== Starting test_memory_timeline_endpoint ===")

    # Используем TestClient для синхронных запросов
    print("1. Creating TestClient...")
    client = TestClient(app)

    try:
        # Тестируем получение пустого списка
        print("\n2. Testing GET /timeline/memories/...")
        response = client.get("/timeline/memories/")
        print(f"   Response status: {response.status_code}")
        assert (
            response.status_code == 200
        ), f"Expected status 200, got {response.status_code}"

        data = response.json()
        print(f"   Response data: {data}")
        assert isinstance(data, list), f"Expected list, got {type(data)}"
        print("   ✓ GET /timeline/memories/ passed")

        # Тестируем добавление воспоминания
        print("\n3. Testing POST /timeline/memories/...")
        test_data = {"content": "Test memory", "memory_type": "test"}
        print(f"   Sending data: {test_data}")

        response = client.post("/timeline/memories/", json=test_data)
        print(f"   Response status: {response.status_code}")
        print(f"   Response data: {response.text}")
        assert response.status_code in [
            200,
            201,
        ], f"Expected status 200 or 201, got {response.status_code}"
        print("   ✓ POST /timeline/memories/ passed")

        # Проверяем, что воспоминание добавилось
        print("\n4. Verifying GET /timeline/memories/ after POST...")
        response = client.get("/timeline/memories/")
        print(f"   Response status: {response.status_code}")
        assert (
            response.status_code == 200
        ), f"Expected status 200, got {response.status_code}"

        memories = response.json()
        print(f"   Response data: {memories}")
        assert isinstance(memories, list), f"Expected list, got {type(memories)}"
        assert len(memories) > 0, "Expected at least one memory in the list"

        if memories:
            print(f"   First memory: {memories[0]}")
            assert (
                memories[0]["content"] == test_data["content"]
            ), f"Expected content '{test_data['content']}', got '{memories[0]['content']}'"
        print("   ✓ GET /timeline/memories/ verification passed")

    except Exception as e:
        print(f"\n❌ Test failed: {str(e)}")
        raise

    print("\n=== Test completed successfully ===\n")


def test_websocket_disconnect():
    """Проверяем корректное отключение WebSocket клиента."""
    print("\n=== Starting test_websocket_disconnect ===")
    client = TestClient(app)

    try:
        # Подключаем WebSocket клиента
        print("1. Connecting WebSocket...")
        with client.websocket_connect("/ws/timeline") as websocket:
            # Проверяем, что клиент добавился в подписчики
            print("2. Verifying subscription...")
            response = client.get("/debug/subscribers/count")
            assert response.status_code == 200
            assert response.json()["count"] == 1

            # Имитируем отправку сообщения
            print("3. Sending test message...")
            test_data = {"content": "Test disconnect", "memory_type": "test"}
            response = client.post("/timeline/memories/", json=test_data)
            assert response.status_code in [200, 201]

            # Проверяем, что сообщение пришло через WebSocket
            print("4. Verifying WebSocket message...")
            data = websocket.receive_json()
            assert data["event"] == "memory_added"
            assert data["data"]["content"] == test_data["content"]

        # После выхода из контекстного менеджера соединение должно закрыться
        print("5. Verifying WebSocket disconnection...")
        response = client.get("/debug/subscribers/count")
        assert response.status_code == 200
        assert response.json()["count"] == 0

    except Exception as e:
        print(f"\n❌ Test failed: {str(e)}")
        raise

    print("\n=== Test completed successfully ===\n")
