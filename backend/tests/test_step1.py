"""
Тест для проверки первого шага работы с временной шкалой.
"""

import pytest
from api import app
from fastapi.testclient import TestClient


def test_get_empty_timeline():
    """Проверяем получение пустого списка воспоминаний."""
    with TestClient(app) as client:
        print("1. Отправляем GET запрос на /timeline/memories/")
        response = client.get("/timeline/memories/")
        print(f"   Получен ответ: {response.status_code}")
        print(f"   Тело ответа: {response.text}")

        assert response.status_code == 200
        assert response.json() == []
        print("   Тест успешно пройден!")
