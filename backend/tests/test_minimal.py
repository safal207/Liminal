"""Минимальный тест для проверки работы FastAPI приложения."""


def test_root(client):
    """Проверяем корневой эндпоинт."""

    response = client.get("/")
    assert response.status_code == 200
    data = response.json()
    assert "message" in data
