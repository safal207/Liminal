"""
Минимальный тест для проверки работы FastAPI приложения.
"""

print("TEST: Starting test script")
import os
import sys
from pathlib import Path

# Устанавливаем переменную окружения для тестового режима
print("TEST: Setting TESTING environment variable")
os.environ["TESTING"] = "1"

# Добавляем корневую директорию в PYTHONPATH
print("TEST: Setting up Python path")
project_root = str(Path(__file__).parent.parent)
print(f"TEST: Project root: {project_root}")
sys.path.insert(0, project_root)
print(f"TEST: sys.path: {sys.path}")

# Мокаем neo4j_writer
print("TEST: Creating mock neo4j_writer module")
import types

sys.modules["neo4j_writer"] = types.ModuleType("neo4j_writer")
sys.modules["neo4j_writer"].Neo4jWriter = type(
    "Neo4jWriter",
    (),
    {
        "__init__": lambda self, *args, **kwargs: print(
            "MOCK: Neo4jWriter.__init__ called"
        ),
        "connect": lambda self: (print("MOCK: Neo4jWriter.connect called"), self)[1],
        "close": lambda self: print("MOCK: Neo4jWriter.close called"),
        "execute_query": lambda self, *args, **kwargs: (
            print(f"MOCK: Neo4jWriter.execute_query called with {args}, {kwargs}"),
            [],
        )[1],
    },
)

# Импортируем приложение
print("TEST: Importing FastAPI and app")
try:
    from fastapi.testclient import TestClient

    print("TEST: Successfully imported TestClient")
    from api import app

    print("TEST: Successfully imported app")
except Exception as e:
    print(f"ERROR: Failed to import app: {e}")
    raise


def test_root():
    """Проверяем корневой эндпоинт."""
    print("TEST: Starting test_root")
    try:
        client = TestClient(app)
        print("TEST: Created TestClient")
        response = client.get("/")
        print(f"TEST: Got response: {response.status_code}")
        assert response.status_code == 200
        data = response.json()
        print(f"TEST: Response data: {data}")
        assert "message" in data
        print("TEST: test_root passed")
    except Exception as e:
        print(f"ERROR in test_root: {e}")
        raise


if __name__ == "__main__":
    print("TEST: Running directly")
    test_root()
