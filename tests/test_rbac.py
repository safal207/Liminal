"""Тесты для системы RBAC."""
from types import SimpleNamespace

import pytest
from fastapi import Depends, FastAPI, HTTPException
from fastapi.security import SecurityScopes
from fastapi.testclient import TestClient

from backend.rbac import (
    Role,
    Permission,
    Resource,
    User,
    check_permission,
    get_current_user,
    oauth2_scheme,
    rbac_matrix,
    require_permission,
)


@pytest.fixture(autouse=True)
def stub_vault_client(monkeypatch):
    """Подменяем клиента Vault, чтобы не требовать hvac в тестах."""

    def _fake_client():
        return SimpleNamespace(read_secret=lambda _: {"secret_key": "test"})

    monkeypatch.setattr("backend.vault_client.get_vault_client", _fake_client)


@pytest.fixture
def admin_token() -> str:
    """Токен администратора."""
    return "admin-token"


@pytest.fixture
def user_token() -> str:
    """Токен обычного пользователя."""
    return "user-token"


@pytest.fixture
def app(admin_token: str, user_token: str) -> FastAPI:
    """Минимальное приложение FastAPI с RBAC-декораторами для тестов."""
    test_app = FastAPI()

    async def override_current_user(
        security_scopes: SecurityScopes,
        token: str = Depends(oauth2_scheme),
    ) -> User:
        users = {
            admin_token: User(username="admin", role=Role.ADMIN, disabled=False),
            user_token: User(username="user", role=Role.USER, disabled=False),
        }

        user = users.get(token)
        if user is None:
            raise HTTPException(
                status_code=401,
                detail="Could not validate credentials",
                headers={"WWW-Authenticate": "Bearer"},
            )

        return user

    test_app.dependency_overrides[get_current_user] = override_current_user

    @test_app.get("/api/system/settings")
    @require_permission(Resource.SYSTEM, Permission.WRITE)
    async def system_settings():
        return {"status": "ok"}

    @test_app.post("/api/users", status_code=201)
    @require_permission(Resource.USERS, Permission.WRITE)
    async def create_user():
        return {"status": "created"}

    @test_app.get("/api/ml/models")
    @require_permission(Resource.ML_MODELS, Permission.READ)
    async def list_models():
        return {"models": []}

    @test_app.post("/api/ml/models")
    @require_permission(Resource.ML_MODELS, Permission.WRITE)
    async def create_model():
        return {"status": "created"}

    yield test_app

    test_app.dependency_overrides.clear()


@pytest.fixture
def client(app: FastAPI) -> TestClient:
    """Тестовый клиент FastAPI."""
    with TestClient(app) as test_client:
        yield test_client

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
async def test_require_permission_decorator(client: TestClient, user_token: str):
    """Тест декоратора проверки прав."""

    response = client.get(
        "/api/ml/models",
        headers={"Authorization": f"Bearer {user_token}"},
    )
    assert response.status_code == 200

    response = client.post(
        "/api/ml/models",
        headers={"Authorization": f"Bearer {user_token}"},
        json={"name": "test_model"},
    )
    assert response.status_code == 403

    response = client.get("/api/ml/models")
    assert response.status_code == 401
