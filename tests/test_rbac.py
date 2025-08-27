"""Тесты для системы RBAC."""
import pytest
from fastapi import HTTPException
from fastapi.testclient import TestClient

from backend.rbac import (
    Role,
    Permission,
    Resource,
    check_permission,
    rbac_matrix,
    User
)

def test_rbac_matrix():
    """Тест матрицы прав доступа."""
    # Проверяем права администратора
    assert rbac_matrix.has_permission(Role.ADMIN, Resource.SYSTEM, Permission.WRITE)
    assert rbac_matrix.has_permission(Role.ADMIN, Resource.ML_MODELS, Permission.EXECUTE)
    
    # Проверяем права обычного пользователя
    assert rbac_matrix.has_permission(Role.USER, Resource.ML_MODELS, Permission.READ)
    assert not rbac_matrix.has_permission(Role.USER, Resource.SYSTEM, Permission.WRITE)
    
    # Проверяем права ML инженера
    assert rbac_matrix.has_permission(Role.ML_ENGINEER, Resource.ML_MODELS, Permission.EXECUTE)
    assert not rbac_matrix.has_permission(Role.ML_ENGINEER, Resource.SYSTEM, Permission.DELETE)

def test_admin_access(client: TestClient, admin_token: str):
    """Тест доступа администратора."""
    headers = {"Authorization": f"Bearer {admin_token}"}
    
    # Тест доступа к системным настройкам
    response = client.get("/api/system/settings", headers=headers)
    assert response.status_code == 200
    
    # Тест создания нового пользователя
    response = client.post(
        "/api/users",
        headers=headers,
        json={
            "username": "new_user",
            "password": "password123",
            "role": "user"
        }
    )
    assert response.status_code == 201

def test_user_access(client: TestClient, user_token: str):
    """Тест доступа обычного пользователя."""
    headers = {"Authorization": f"Bearer {user_token}"}
    
    # Тест доступа к ML моделям (только чтение)
    response = client.get("/api/ml/models", headers=headers)
    assert response.status_code == 200
    
    # Тест попытки изменения ML модели (должно быть запрещено)
    response = client.post(
        "/api/ml/models",
        headers=headers,
        json={"name": "test_model", "type": "classification"}
    )
    assert response.status_code == 403

def test_permission_checking():
    """Тест функции проверки разрешений."""
    admin = User(username="admin", role=Role.ADMIN)
    user = User(username="user", role=Role.USER)
    
    # Проверяем права администратора
    assert check_permission(Resource.SYSTEM, Permission.WRITE, admin)
    assert check_permission(Resource.ML_MODELS, Permission.EXECUTE, admin)
    
    # Проверяем права пользователя
    assert check_permission(Resource.ML_MODELS, Permission.READ, user)
    assert not check_permission(Resource.SYSTEM, Permission.WRITE, user)

@pytest.mark.asyncio
async def test_require_permission_decorator(client: TestClient):
    """Тест декоратора проверки прав."""
    from backend.api import app
    from backend.rbac import require_permission
    
    @app.get("/test-endpoint")
    @require_permission(Resource.ML_MODELS, Permission.READ)
    async def test_endpoint():
        return {"message": "success"}
    
    # Тест с токеном пользователя
    response = client.get(
        "/test-endpoint",
        headers={"Authorization": f"Bearer {user_token}"}
    )
    assert response.status_code == 200
    
    # Тест без токена
    response = client.get("/test-endpoint")
    assert response.status_code == 401
