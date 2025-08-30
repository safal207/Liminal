"""
Минимальный тест для проверки работы FastAPI приложения.
"""
import os
import sys
import types
from pathlib import Path

# Устанавливаем переменную окружения для тестового режима
os.environ["TESTING"] = "1"

# Добавляем корневую директорию в PYTHONPATH
project_root = str(Path(__file__).parent.parent)
sys.path.insert(0, project_root)

# Мокаем neo4j_writer
sys.modules["neo4j_writer"] = types.ModuleType("neo4j_writer")
sys.modules["neo4j_writer"].Neo4jWriter = type(
    "Neo4jWriter",
    (),
    {
        "__init__": lambda self, *args, **kwargs: None,
        "connect": lambda self: self,
        "close": lambda self: None,
        "execute_query": lambda self, *args, **kwargs: [],
    },
)

# Импортируем приложение
try:
    from fastapi.testclient import TestClient

    from api import app
except Exception as e:
    print(f"ERROR: Failed to import app: {e}")
    raise


def test_root():
    """Проверяем корневой эндпоинт."""
    try:
        client = TestClient(app)
        response = client.get("/")
        assert response.status_code == 200
        data = response.json()
        assert "message" in data
    except Exception as e:
        print(f"ERROR in test_root: {e}")
        raise


if __name__ == "__main__":
    test_root()
