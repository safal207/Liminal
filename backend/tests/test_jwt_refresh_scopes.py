"""
Тесты для refresh токенов, scopes и roles в JWT.
"""

import pytest
from fastapi.testclient import TestClient
from unittest.mock import patch
import json

from backend.main import app


def test_login_returns_both_tokens():
    """Тест что /auth/token возвращает access и refresh токены."""
    client = TestClient(app)
    
    response = client.post(
        "/auth/token",
        data={"username": "testuser", "password": "testpass"},
        headers={"Content-Type": "application/x-www-form-urlencoded"}
    )
    
    assert response.status_code == 200
    data = response.json()
    assert "access_token" in data
    assert "refresh_token" in data
    assert data["token_type"] == "bearer"
    assert data["access_token"] != data["refresh_token"]


def test_refresh_token_endpoint():
    """Тест обновления токена через /auth/refresh."""
    client = TestClient(app)
    
    # Получаем токены
    login_response = client.post(
        "/auth/token",
        data={"username": "admin", "password": "admin123"},
        headers={"Content-Type": "application/x-www-form-urlencoded"}
    )
    tokens = login_response.json()
    
    # Обновляем токены
    refresh_response = client.post(
        "/auth/refresh",
        json={"refresh_token": tokens["refresh_token"]}
    )
    
    assert refresh_response.status_code == 200
    new_tokens = refresh_response.json()
    assert "access_token" in new_tokens
    assert "refresh_token" in new_tokens
    assert new_tokens["token_type"] == "bearer"
    
    # Новые токены должны отличаться от старых
    assert new_tokens["access_token"] != tokens["access_token"]
    assert new_tokens["refresh_token"] != tokens["refresh_token"]


def test_invalid_refresh_token():
    """Тест с недействительным refresh токеном."""
    client = TestClient(app)
    
    response = client.post(
        "/auth/refresh",
        json={"refresh_token": "invalid_token"}
    )
    
    assert response.status_code == 401
    assert "Недействительный refresh токен" in response.json()["detail"]


def test_user_scopes_and_roles():
    """Тест что /auth/me возвращает scopes и roles."""
    client = TestClient(app)
    
    # Логин как admin
    login_response = client.post(
        "/auth/token",
        data={"username": "admin", "password": "admin123"},
        headers={"Content-Type": "application/x-www-form-urlencoded"}
    )
    tokens = login_response.json()
    
    # Получаем информацию о пользователе
    me_response = client.get(
        "/auth/me",
        headers={"Authorization": f"Bearer {tokens['access_token']}"}
    )
    
    assert me_response.status_code == 200
    user_info = me_response.json()
    
    assert user_info["username"] == "admin"
    assert "scopes" in user_info
    assert "roles" in user_info
    assert "admin" in user_info["scopes"]
    assert "read" in user_info["scopes"]
    assert "write" in user_info["scopes"]
    assert "admin" in user_info["roles"]
    assert "user" in user_info["roles"]


def test_testuser_scopes_and_roles():
    """Тест scopes и roles для обычного пользователя."""
    client = TestClient(app)
    
    # Логин как testuser
    login_response = client.post(
        "/auth/token",
        data={"username": "testuser", "password": "testpass"},
        headers={"Content-Type": "application/x-www-form-urlencoded"}
    )
    tokens = login_response.json()
    
    # Получаем информацию о пользователе
    me_response = client.get(
        "/auth/me",
        headers={"Authorization": f"Bearer {tokens['access_token']}"}
    )
    
    assert me_response.status_code == 200
    user_info = me_response.json()
    
    assert user_info["username"] == "testuser"
    assert user_info["scopes"] == ["read", "write"]
    assert user_info["roles"] == ["user"]
    assert "admin" not in user_info["scopes"]
    assert "admin" not in user_info["roles"]


def test_token_contains_scopes_and_roles():
    """Тест что токен содержит scopes и roles в payload."""
    from backend.auth.jwt_utils import jwt_manager
    
    client = TestClient(app)
    
    # Получаем токен
    login_response = client.post(
        "/auth/token",
        data={"username": "admin", "password": "admin123"},
        headers={"Content-Type": "application/x-www-form-urlencoded"}
    )
    tokens = login_response.json()
    
    # Декодируем токен и проверяем payload
    payload = jwt_manager.verify_token(tokens["access_token"])
    
    assert payload is not None
    assert payload["sub"] == "admin"
    assert payload["username"] == "admin"
    assert "scopes" in payload
    assert "roles" in payload
    assert "admin" in payload["scopes"]
    assert "admin" in payload["roles"]
    assert payload["type"] == "access"


def test_refresh_token_type():
    """Тест что refresh токен имеет правильный тип."""
    from backend.auth.jwt_utils import jwt_manager
    
    client = TestClient(app)
    
    # Получаем токены
    login_response = client.post(
        "/auth/token",
        data={"username": "testuser", "password": "testpass"},
        headers={"Content-Type": "application/x-www-form-urlencoded"}
    )
    tokens = login_response.json()
    
    # Проверяем типы токенов
    access_payload = jwt_manager.verify_token(tokens["access_token"])
    refresh_payload = jwt_manager.verify_token(tokens["refresh_token"])
    
    assert access_payload["type"] == "access"
    assert refresh_payload["type"] == "refresh"


def test_access_token_cannot_be_used_for_refresh():
    """Тест что access токен нельзя использовать для refresh."""
    client = TestClient(app)
    
    # Получаем токены
    login_response = client.post(
        "/auth/token",
        data={"username": "testuser", "password": "testpass"},
        headers={"Content-Type": "application/x-www-form-urlencoded"}
    )
    tokens = login_response.json()
    
    # Пытаемся использовать access токен для refresh
    refresh_response = client.post(
        "/auth/refresh",
        json={"refresh_token": tokens["access_token"]}
    )
    
    assert refresh_response.status_code == 401
    assert "Недействительный refresh токен" in refresh_response.json()["detail"]
