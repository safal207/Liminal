#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Интеграционные тесты для Database API endpoints.

Тестирует REST API для DatabaseAdapter через FastAPI TestClient.
"""

import sys
from pathlib import Path
from unittest.mock import AsyncMock, MagicMock, patch

import pytest
from fastapi.testclient import TestClient

# Репозиторий должен быть на sys.path, чтобы `from backend...` совпадал с приложением в CI.
_REPO_ROOT = Path(__file__).resolve().parents[2]
_repo = str(_REPO_ROOT)
if _repo not in sys.path:
    sys.path.insert(0, _repo)

from backend.app.main import app  # noqa: E402
from backend.database_adapter import DataType  # noqa: E402

_DATABASE_API_MOUNTED = "/api/database/store" in {
    getattr(r, "path", None) for r in app.router.routes
}


class TestDatabaseAPIIntegration:
    """Интеграционные тесты для Database API."""

    @pytest.fixture
    def db_api_client(self):
        """TestClient: имя не `client`, чтобы не пересекаться с фикстурой из conftest."""
        if not _DATABASE_API_MOUNTED:
            pytest.skip(
                "database_api router is not mounted on backend.app.main (no /api/database/*)"
            )
        with TestClient(app) as test_client:
            yield test_client

    @pytest.fixture
    def mock_adapter(self):
        """Мок для DatabaseAdapter."""
        mock = MagicMock()

        # Настраиваем асинхронные методы
        mock.store_data = AsyncMock(return_value="test-record-123")
        mock.query_data = AsyncMock(
            return_value=[
                {
                    "emotion": "радость",
                    "intensity": 0.8,
                    "timestamp": "2025-08-21T19:30:00",
                },
                {
                    "emotion": "спокойствие",
                    "intensity": 0.6,
                    "timestamp": "2025-08-21T19:25:00",
                },
            ]
        )

        # Настраиваем синхронные методы
        mock._choose_database.return_value = "datomic"
        mock._get_available_database.return_value = "datomic"
        mock.get_health_status.return_value = {
            "status": "healthy",
            "databases": {
                "datomic": {
                    "available": True,
                    "uri": "http://localhost:8080",
                    "db_name": "liminal",
                },
                "neo4j": {
                    "available": True,
                    "uri": "bolt://localhost:7687",
                    "database": "neo4j",
                },
            },
            "stats": {
                "datomic_queries": 5,
                "neo4j_queries": 3,
                "fallback_uses": 0,
                "errors": 0,
            },
            "fallback_enabled": True,
        }

        return mock

    def test_store_emotion_data(self, db_api_client, mock_adapter):
        """Тест сохранения эмоциональных данных."""
        with patch(
            "backend.database_api.get_database_adapter", return_value=mock_adapter
        ):
            response = db_api_client.post(
                "/api/database/store",
                json={
                    "data": {
                        "emotion": "радость",
                        "intensity": 0.8,
                        "context": "успешное завершение задачи",
                    },
                    "data_type": "emotion_history",
                    "user_id": "user-123",
                    "session_id": "session-456",
                },
            )

        assert response.status_code == 200
        data = response.json()

        assert data["success"] is True
        assert data["record_id"] == "test-record-123"
        assert (
            "эмоциональных данных" in data["message"]
            or "emotion_history" in data["message"]
        )
        assert data["stored_in"] == "datomic"

        # Проверяем, что метод был вызван с правильными параметрами
        mock_adapter.store_data.assert_called_once()
        call_args = mock_adapter.store_data.call_args
        assert call_args.kwargs["data_type"] == DataType.EMOTION_HISTORY
        assert call_args.kwargs["user_id"] == "user-123"
        assert call_args.kwargs["session_id"] == "session-456"

    def test_store_philosophy_data(self, db_api_client, mock_adapter):
        """Тест сохранения философских данных."""
        mock_adapter._choose_database.return_value = "neo4j"
        mock_adapter._get_available_database.return_value = "neo4j"

        with patch(
            "backend.database_api.get_database_adapter", return_value=mock_adapter
        ):
            response = db_api_client.post(
                "/api/database/store",
                json={
                    "data": {"state": "resonance", "depth": 0.9, "clarity": 0.7},
                    "data_type": "philosophy",
                    "user_id": "user-123",
                },
            )

        assert response.status_code == 200
        data = response.json()

        assert data["success"] is True
        assert data["record_id"] == "test-record-123"
        assert data["stored_in"] == "neo4j"

    def test_store_invalid_data_type(self, db_api_client, mock_adapter):
        """Тест с неподдерживаемым типом данных."""
        with patch(
            "backend.database_api.get_database_adapter", return_value=mock_adapter
        ):
            response = db_api_client.post(
                "/api/database/store",
                json={"data": {"test": "data"}, "data_type": "invalid_type"},
            )

        assert response.status_code == 400
        data = response.json()
        assert "Неподдерживаемый тип данных" in data["detail"]

    def test_query_emotion_history(self, db_api_client, mock_adapter):
        """Тест запроса истории эмоций."""
        with patch(
            "backend.database_api.get_database_adapter", return_value=mock_adapter
        ):
            response = db_api_client.post(
                "/api/database/query",
                json={
                    "data_type": "emotion_history",
                    "filters": {"user_id": "user-123"},
                    "limit": 10,
                },
            )

        assert response.status_code == 200
        data = response.json()

        assert data["success"] is True
        assert data["count"] == 2
        assert len(data["data"]) == 2
        assert data["data"][0]["emotion"] == "радость"
        assert data["data"][1]["emotion"] == "спокойствие"
        assert data["queried_from"] == "datomic"

        # Проверяем вызов метода
        mock_adapter.query_data.assert_called_once_with(
            data_type=DataType.EMOTION_HISTORY,
            filters={"user_id": "user-123"},
            limit=10,
        )

    def test_query_invalid_data_type(self, db_api_client, mock_adapter):
        """Тест запроса с неподдерживаемым типом данных."""
        with patch(
            "backend.database_api.get_database_adapter", return_value=mock_adapter
        ):
            response = db_api_client.post(
                "/api/database/query",
                json={"data_type": "invalid_type", "filters": {}, "limit": 10},
            )

        assert response.status_code == 400
        data = response.json()
        assert "Неподдерживаемый тип данных" in data["detail"]

    def test_database_health(self, db_api_client, mock_adapter):
        """Тест проверки здоровья БД."""
        with patch(
            "backend.database_api.get_database_adapter", return_value=mock_adapter
        ):
            response = db_api_client.get("/api/database/health")

        assert response.status_code == 200
        data = response.json()

        assert data["status"] == "healthy"
        assert "datomic" in data["databases"]
        assert "neo4j" in data["databases"]
        assert data["databases"]["datomic"]["available"] is True
        assert data["databases"]["neo4j"]["available"] is True
        assert data["stats"]["datomic_queries"] == 5
        assert data["stats"]["neo4j_queries"] == 3
        assert data["fallback_enabled"] is True

    def test_get_supported_data_types(self, db_api_client):
        """Тест получения поддерживаемых типов данных."""
        response = db_api_client.get("/api/database/data-types")

        assert response.status_code == 200
        data = response.json()

        assert "supported_types" in data
        assert "routing_rules" in data

        # Проверяем некоторые типы данных
        supported_types = data["supported_types"]
        assert "emotion_history" in supported_types
        assert "philosophy" in supported_types
        assert supported_types["emotion_history"]["preferred_database"] == "datomic"
        assert supported_types["philosophy"]["preferred_database"] == "neo4j"

        # Проверяем правила роутинга
        routing_rules = data["routing_rules"]
        assert "datomic" in routing_rules
        assert "neo4j" in routing_rules

    def test_get_api_examples(self, db_api_client):
        """Тест получения примеров использования API."""
        response = db_api_client.get("/api/database/examples")

        assert response.status_code == 200
        data = response.json()

        assert "store_emotion" in data
        assert "store_philosophy" in data
        assert "query_emotions" in data
        assert "health_check" in data

        # Проверяем структуру примера
        store_emotion = data["store_emotion"]
        assert store_emotion["method"] == "POST"
        assert "/api/database/store" in store_emotion["url"]
        assert "body" in store_emotion
        assert store_emotion["body"]["data_type"] == "emotion_history"

    def test_store_data_error_handling(self, db_api_client, mock_adapter):
        """Тест обработки ошибок при сохранении данных."""
        # Настраиваем мок для генерации исключения
        mock_adapter.store_data.side_effect = Exception("Database connection failed")

        with patch(
            "backend.database_api.get_database_adapter", return_value=mock_adapter
        ):
            response = db_api_client.post(
                "/api/database/store",
                json={"data": {"test": "data"}, "data_type": "emotion_history"},
            )

        assert response.status_code == 500
        data = response.json()
        assert "Ошибка сохранения данных" in data["detail"]

    def test_query_data_error_handling(self, db_api_client, mock_adapter):
        """Тест обработки ошибок при запросе данных."""
        # Настраиваем мок для генерации исключения
        mock_adapter.query_data.side_effect = Exception("Query execution failed")

        with patch(
            "backend.database_api.get_database_adapter", return_value=mock_adapter
        ):
            response = db_api_client.post(
                "/api/database/query",
                json={"data_type": "emotion_history", "filters": {}, "limit": 10},
            )

        assert response.status_code == 500
        data = response.json()
        assert "Ошибка запроса данных" in data["detail"]

    def test_health_check_error_handling(self, db_api_client, mock_adapter):
        """Тест обработки ошибок при проверке здоровья."""
        # Настраиваем мок для генерации исключения
        mock_adapter.get_health_status.side_effect = Exception("Health check failed")

        with patch(
            "backend.database_api.get_database_adapter", return_value=mock_adapter
        ):
            response = db_api_client.get("/api/database/health")

        assert response.status_code == 500
        data = response.json()
        assert "Ошибка получения статуса БД" in data["detail"]

    def test_request_validation(self, db_api_client, mock_adapter):
        """Тест валидации запросов."""
        with patch(
            "backend.database_api.get_database_adapter", return_value=mock_adapter
        ):
            # Запрос без обязательных полей
            response = db_api_client.post(
                "/api/database/store",
                json={
                    "data": {"test": "data"}
                    # Отсутствует data_type
                },
            )

        assert response.status_code == 422  # Validation Error

        # Запрос с некорректным типом данных для limit
        with patch(
            "backend.database_api.get_database_adapter", return_value=mock_adapter
        ):
            response = db_api_client.post(
                "/api/database/query",
                json={"data_type": "emotion_history", "limit": "not_a_number"},
            )

        assert response.status_code == 422  # Validation Error


# Функциональные тесты (можно запускать вручную)
class TestDatabaseAPIFunctional:
    """Функциональные тесты для проверки реального API."""

    @pytest.mark.functional
    def test_api_documentation_available(self):
        """Тест доступности документации API."""
        if not _DATABASE_API_MOUNTED:
            pytest.skip(
                "database_api router is not mounted on backend.app.main (no /api/database/*)"
            )
        with TestClient(app) as tc:
            # Проверяем, что Swagger UI доступен
            response = tc.get("/docs")
            assert response.status_code == 200

            # Проверяем, что OpenAPI схема доступна
            response = tc.get("/openapi.json")
            assert response.status_code == 200

            openapi_data = response.json()
            assert "paths" in openapi_data
            assert "/api/database/store" in openapi_data["paths"]
            assert "/api/database/query" in openapi_data["paths"]
            assert "/api/database/health" in openapi_data["paths"]


if __name__ == "__main__":
    # Запуск тестов
    pytest.main([__file__, "-v", "--tb=short"])
