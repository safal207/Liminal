"""
Базовые тесты для проверки работы FastAPI приложения.
"""

import os
import sys
from pathlib import Path

# Устанавливаем переменную окружения для тестового режима ДО ВСЕХ ИМПОРТОВ
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

from api import app
# Теперь можно импортировать приложение
from fastapi.testclient import TestClient


def test_root_endpoint():
    """Проверяем корневой эндпоинт."""
    client = TestClient(app)
    response = client.get("/")
    assert response.status_code == 200
    data = response.json()
    assert "message" in data
