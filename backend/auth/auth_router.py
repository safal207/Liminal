"""
Маршруты (роутеры) для аутентификации через JWT.
"""

from datetime import timedelta
from typing import Dict, Optional, Any

from fastapi import APIRouter, Depends, HTTPException, status
from fastapi.security import OAuth2PasswordBearer, OAuth2PasswordRequestForm
from pydantic import BaseModel

from backend.auth.jwt_utils import (
    authenticate_user,
    create_access_token_for_user,
    create_tokens_for_user,
    refresh_access_token,
    jwt_manager,
    ACCESS_TOKEN_EXPIRE_MINUTES
)

# Создаем роутер для аутентификации
router = APIRouter(prefix="/auth", tags=["authentication"])

# OAuth2 с поддержкой Password flow для Bearer токенов
oauth2_scheme = OAuth2PasswordBearer(tokenUrl="/auth/token")


# Модели данных для ответов API
class Token(BaseModel):
    access_token: str
    refresh_token: str
    token_type: str


class RefreshTokenRequest(BaseModel):
    refresh_token: str


class UserInfo(BaseModel):
    username: str
    email: str
    is_active: bool
    scopes: list[str]
    roles: list[str]


# Функция для получения текущего пользователя из токена
async def get_current_user(token: str = Depends(oauth2_scheme)) -> Dict[str, Any]:
    credentials_exception = HTTPException(
        status_code=status.HTTP_401_UNAUTHORIZED,
        detail="Недействительные учетные данные",
        headers={"WWW-Authenticate": "Bearer"},
    )
    
    user_id = jwt_manager.extract_user_id_from_token(token)
    if user_id is None:
        raise credentials_exception
    
    # Здесь должен быть код получения пользователя из БД
    # В текущей реализации используем временную БД из jwt_utils
    from backend.auth.jwt_utils import fake_users_db
    
    user = fake_users_db.get(user_id)
    if user is None:
        raise credentials_exception
        
    return user


# Функция-зависимость для проверки активности пользователя
async def get_current_active_user(
    current_user: Dict[str, Any] = Depends(get_current_user)
) -> Dict[str, Any]:
    if not current_user.get("is_active", False):
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST, 
            detail="Пользователь неактивен"
        )
    return current_user


# Endpoint для получения токена
@router.post("/token", response_model=Token)
async def login_for_access_token(
    form_data: OAuth2PasswordRequestForm = Depends()
) -> Dict[str, str]:
    user = authenticate_user(form_data.username, form_data.password)
    if not user:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Неверное имя пользователя или пароль",
            headers={"WWW-Authenticate": "Bearer"},
        )
    
    tokens = create_tokens_for_user(user)
    
    return {
        "access_token": tokens["access_token"],
        "refresh_token": tokens["refresh_token"],
        "token_type": "bearer"
    }


# Endpoint для обновления токена
@router.post("/refresh", response_model=Token)
async def refresh_token(
    refresh_request: RefreshTokenRequest
) -> Dict[str, str]:
    tokens = refresh_access_token(refresh_request.refresh_token)
    if not tokens:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Недействительный refresh токен",
            headers={"WWW-Authenticate": "Bearer"},
        )
    
    return {
        "access_token": tokens["access_token"],
        "refresh_token": tokens["refresh_token"],
        "token_type": "bearer"
    }


# Endpoint для получения информации о пользователе
@router.get("/me", response_model=UserInfo)
async def read_users_me(
    current_user: Dict[str, Any] = Depends(get_current_active_user)
) -> Dict[str, Any]:
    return {
        "username": current_user["username"],
        "email": current_user["email"],
        "is_active": current_user["is_active"],
        "scopes": current_user.get("scopes", []),
        "roles": current_user.get("roles", [])
    }


# Проверка аутентификации для тестирования
@router.get("/check")
async def check_auth(
    current_user: Dict[str, Any] = Depends(get_current_active_user)
) -> Dict[str, Any]:
    return {
        "authenticated": True,
        "user_id": current_user["user_id"],
        "username": current_user["username"]
    }
