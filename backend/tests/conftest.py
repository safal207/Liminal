"""
Конфигурация тестов для работы с FastAPI приложением.
"""

import os
import sys
import types
from pathlib import Path

import pytest
from fastapi.testclient import TestClient

# Импортируем мок-реализацию neo4j_writer
from tests.mock_neo4j_writer import Neo4jDateTime, Neo4jWriter

# Добавляем корневую директорию в PYTHONPATH
project_root = str(Path(__file__).parent.parent)
if project_root not in sys.path:
    sys.path.insert(0, project_root)

# Устанавливаем переменную окружения для тестового режима
os.environ["TESTING"] = "1"

# Монтируем мок-модуль в sys.modules до импорта приложения
# Создаем мок-модуль neo4j_writer
mock_neo4j = types.ModuleType("neo4j_writer")
mock_neo4j.Neo4jWriter = Neo4jWriter
mock_neo4j.Neo4jDateTime = Neo4jDateTime
sys.modules["neo4j_writer"] = mock_neo4j

# Теперь можно импортировать приложение
from api import app, memory_timeline


# Фикстура для тестового клиента
@pytest.fixture
def client():
    """Создает тестовый клиент для FastAPI приложения."""
    # Очищаем временную шкалу перед каждым тестом
    memory_timeline.timeline = []

    # Возвращаем тестовый клиент
    with TestClient(app) as test_client:
        yield test_client
