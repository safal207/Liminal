#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Тесты для GraphQL API PersonalityAdapter.

Проверяет работу GraphQL запросов, мутаций и интеграцию с FastAPI.
"""

import json
import os
import sys
from datetime import datetime
from unittest.mock import AsyncMock, MagicMock, patch

import pytest
from fastapi.testclient import TestClient

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from backend.auth.dependencies import token_verifier
from backend.main import app
from backend.personality import PersonalityAdapter
from backend.personality.schema import schema


def _auth_override():
    async def _fake_token():
        return {"sub": "test-user-123", "username": "testuser"}

    return _fake_token


class TestPersonalityGraphQL:
    """Тесты для GraphQL API PersonalityAdapter."""

    @pytest.fixture
    def mock_personality_adapter(self):
        """Мок для PersonalityAdapter."""
        mock = MagicMock()
        mock.get_profile = AsyncMock()
        mock.store_emotion = AsyncMock()
        mock.update_preference = AsyncMock()
        mock.get_recommendations = AsyncMock()

        # Настраиваем возвращаемые значения
        mock.get_profile.return_value = {
            "user_id": "test-user-123",
            "emotional_history": [
                {
                    "type": "радость",
                    "intensity": 0.8,
                    "context": "тестирование GraphQL",
                    "timestamp": datetime.now().isoformat(),
                }
            ],
            "preferences": [
                {
                    "category": "обучение",
                    "value": "визуальный стиль",
                    "strength": 0.9,
                    "last_updated": datetime.now().isoformat(),
                }
            ],
            "recommendations": [],
        }

        mock.store_emotion.return_value = {
            "type": "спокойствие",
            "intensity": 0.7,
            "context": "тестирование мутации",
            "timestamp": datetime.now().isoformat(),
        }

        mock.update_preference.return_value = {
            "category": "музыка",
            "value": "классическая",
            "strength": 0.85,
            "last_updated": datetime.now().isoformat(),
        }

        mock.get_recommendations.return_value = [
            {
                "id": "rec-1",
                "content": "Тестовая рекомендация",
                "confidence": 0.85,
                "source": "test",
                "context": "тестирование",
                "created_at": datetime.now().isoformat(),
            }
        ]

        return mock

    @pytest.fixture
    def client(self):
        """TestClient для FastAPI с валидным payload JWT dependency."""

        app.dependency_overrides[token_verifier] = _auth_override()
        yield TestClient(app)
        app.dependency_overrides.pop(token_verifier, None)

    @pytest.fixture
    def auth_headers(self):
        """Заголовки авторизации для тестов."""
        # В реальном тесте здесь был бы настоящий JWT токен
        return {"Authorization": "Bearer test_token"}

    def test_graphql_schema(self):
        """Тест структуры GraphQL схемы."""
        for name in (
            "Emotion",
            "Preference",
            "Recommendation",
            "PersonalityProfile",
            "Query",
            "Mutation",
            "Subscription",
        ):
            assert schema.get_type_by_name(name) is not None

    @patch("backend.personality.adapter.PersonalityAdapter")
    def test_query_personality_profile(
        self, mock_adapter_class, mock_personality_adapter, client, auth_headers
    ):
        """Тест GraphQL запроса personalityProfile."""
        mock_adapter_class.return_value = mock_personality_adapter

        # GraphQL запрос
        query = """
        query {
          personalityProfile(userId: "test-user-123") {
            userId
            emotionalHistory {
              type
              intensity
              context
            }
            currentMood {
              type
              intensity
            }
            preferences {
              category
              value
              strength
            }
          }
        }
        """

        # Отправляем запрос
        response = client.post(
            "/personality/graphql", json={"query": query}, headers=auth_headers
        )

        # Проверяем ответ
        assert response.status_code == 200
        data = response.json()
        assert "data" in data
        assert "personalityProfile" in data["data"]
        profile = data["data"]["personalityProfile"]

        assert profile["userId"] == "test-user-123"
        assert len(profile["emotionalHistory"]) == 1
        assert profile["emotionalHistory"][0]["type"] == "радость"
        assert profile["currentMood"]["type"] == "радость"
        assert len(profile["preferences"]) == 1
        assert profile["preferences"][0]["category"] == "обучение"

        # Проверяем, что метод get_profile был вызван
        mock_personality_adapter.get_profile.assert_called_once()

    @patch("backend.personality.adapter.PersonalityAdapter")
    def test_query_recommendations(
        self, mock_adapter_class, mock_personality_adapter, client, auth_headers
    ):
        """Тест GraphQL запроса recommendations."""
        mock_adapter_class.return_value = mock_personality_adapter

        # GraphQL запрос
        query = """
        query {
          recommendations(userId: "test-user-123", limit: 3, context: "тестирование") {
            id
            content
            confidence
            source
            context
          }
        }
        """

        # Отправляем запрос
        response = client.post(
            "/personality/graphql", json={"query": query}, headers=auth_headers
        )

        # Проверяем ответ
        assert response.status_code == 200
        data = response.json()
        assert "data" in data
        assert "recommendations" in data["data"]
        recommendations = data["data"]["recommendations"]

        assert len(recommendations) == 1
        assert recommendations[0]["id"] == "rec-1"
        assert recommendations[0]["content"] == "Тестовая рекомендация"
        assert recommendations[0]["context"] == "тестирование"

        # Проверяем, что метод get_recommendations был вызван с правильными параметрами
        mock_personality_adapter.get_recommendations.assert_called_once_with(
            3, "тестирование"
        )

    @patch("backend.personality.adapter.PersonalityAdapter")
    def test_mutation_store_emotion(
        self, mock_adapter_class, mock_personality_adapter, client, auth_headers
    ):
        """Тест GraphQL мутации storeEmotion."""
        mock_adapter_class.return_value = mock_personality_adapter

        # GraphQL мутация
        mutation = """
        mutation {
          storeEmotion(
            userId: "test-user-123",
            emotionType: "спокойствие",
            intensity: 0.7,
            context: "тестирование мутации"
          ) {
            type
            intensity
            context
            timestamp
          }
        }
        """

        # Отправляем запрос
        response = client.post(
            "/personality/graphql", json={"query": mutation}, headers=auth_headers
        )

        # Проверяем ответ
        assert response.status_code == 200
        data = response.json()
        assert "data" in data
        assert "storeEmotion" in data["data"]
        emotion = data["data"]["storeEmotion"]

        assert emotion["type"] == "спокойствие"
        assert emotion["intensity"] == 0.7
        assert emotion["context"] == "тестирование мутации"
        assert "timestamp" in emotion

        # Проверяем, что метод store_emotion был вызван с правильными параметрами
        mock_personality_adapter.store_emotion.assert_called_once_with(
            "спокойствие", 0.7, "тестирование мутации"
        )

    @patch("backend.personality.adapter.PersonalityAdapter")
    def test_mutation_update_preference(
        self, mock_adapter_class, mock_personality_adapter, client, auth_headers
    ):
        """Тест GraphQL мутации updatePreference."""
        mock_adapter_class.return_value = mock_personality_adapter

        # GraphQL мутация
        mutation = """
        mutation {
          updatePreference(
            userId: "test-user-123",
            preference: {
              category: "музыка",
              value: "классическая",
              strength: 0.85
            }
          ) {
            category
            value
            strength
            lastUpdated
          }
        }
        """

        # Отправляем запрос
        response = client.post(
            "/personality/graphql", json={"query": mutation}, headers=auth_headers
        )

        # Проверяем ответ
        assert response.status_code == 200
        data = response.json()
        assert "data" in data
        assert "updatePreference" in data["data"]
        preference = data["data"]["updatePreference"]

        assert preference["category"] == "музыка"
        assert preference["value"] == "классическая"
        assert preference["strength"] == 0.85
        assert "lastUpdated" in preference

        # Проверяем, что метод update_preference был вызван с правильными параметрами
        mock_personality_adapter.update_preference.assert_called_once_with(
            "музыка", "классическая", 0.85
        )


class TestPersonalityRESTAPI:
    """Тесты для REST API PersonalityAdapter."""

    @pytest.fixture
    def mock_personality_adapter(self):
        """Мок для PersonalityAdapter."""
        mock = MagicMock()
        mock.get_profile = AsyncMock()
        mock.store_emotion = AsyncMock()
        mock.get_recommendations = AsyncMock()

        # Настраиваем возвращаемые значения
        mock.get_profile.return_value = {
            "user_id": "test-user-123",
            "emotional_history": [],
            "preferences": [],
            "recommendations": [],
        }

        mock.store_emotion.return_value = {
            "type": "радость",
            "intensity": 0.8,
            "context": "тестирование REST",
            "timestamp": datetime.now().isoformat(),
        }

        mock.get_recommendations.return_value = []

        return mock

    @pytest.fixture
    def client(self):
        """TestClient для FastAPI с валидным JWT dependency (без реального Bearer)."""

        app.dependency_overrides[token_verifier] = _auth_override()
        yield TestClient(app)
        app.dependency_overrides.pop(token_verifier, None)

    @pytest.fixture
    def auth_headers(self):
        """Заголовки авторизации для тестов."""
        # В реальном тесте здесь был бы настоящий JWT токен
        return {"Authorization": "Bearer test_token"}

    @patch("backend.personality.adapter.PersonalityAdapter")
    def test_rest_store_emotion(
        self, mock_adapter_class, mock_personality_adapter, client, auth_headers
    ):
        """Тест REST эндпоинта /personality/emotion."""
        mock_adapter_class.return_value = mock_personality_adapter

        # Отправляем запрос
        response = client.post(
            "/personality/emotion?emotion_type=радость&intensity=0.8&context=тестирование REST",
            headers=auth_headers,
        )

        # Проверяем ответ
        assert response.status_code == 200
        data = response.json()
        assert data["type"] == "радость"
        assert data["intensity"] == 0.8
        assert data["context"] == "тестирование REST"
        assert "timestamp" in data

        # Проверяем, что метод store_emotion был вызван с правильными параметрами
        mock_personality_adapter.store_emotion.assert_called_once_with(
            "радость", 0.8, "тестирование REST"
        )

    @patch("backend.personality.adapter.PersonalityAdapter")
    def test_rest_get_profile(
        self, mock_adapter_class, mock_personality_adapter, client, auth_headers
    ):
        """Тест REST эндпоинта /personality/profile."""
        mock_adapter_class.return_value = mock_personality_adapter

        # Отправляем запрос
        response = client.get("/personality/profile", headers=auth_headers)

        # Проверяем ответ
        assert response.status_code == 200
        data = response.json()
        assert data["user_id"] == "test-user-123"
        assert "emotional_history" in data
        assert "preferences" in data
        assert "recommendations" in data

        # Проверяем, что метод get_profile был вызван
        mock_personality_adapter.get_profile.assert_called_once()

    @patch("backend.personality.adapter.PersonalityAdapter")
    def test_rest_get_recommendations(
        self, mock_adapter_class, mock_personality_adapter, client, auth_headers
    ):
        """Тест REST эндпоинта /personality/recommendations."""
        mock_adapter_class.return_value = mock_personality_adapter

        # Отправляем запрос
        response = client.get(
            "/personality/recommendations?limit=3&context=тестирование",
            headers=auth_headers,
        )

        # Проверяем ответ
        assert response.status_code == 200
        data = response.json()
        assert isinstance(data, list)

        # Проверяем, что метод get_recommendations был вызван с правильными параметрами
        mock_personality_adapter.get_recommendations.assert_called_once_with(
            3, "тестирование"
        )


if __name__ == "__main__":
    # Запуск тестов
    pytest.main([__file__, "-v"])
