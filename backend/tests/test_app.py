"""Базовые тесты для проверки работы FastAPI приложения."""


def test_root_endpoint(client):
    """Проверяем корневой эндпоинт."""

    response = client.get("/")
    assert response.status_code == 200
    data = response.json()
    assert "message" in data
