"""
Pydantic модели для аутентификации.
"""

from typing import Optional

from pydantic import BaseModel, Field


class UserLogin(BaseModel):
    """Модель для логина пользователя."""

    username: str = Field(..., description="Имя пользователя")
    password: str = Field(..., description="Пароль")


class UserCreate(BaseModel):
    """Модель для создания пользователя."""

    username: str = Field(..., description="Имя пользователя")
    email: str = Field(..., description="Email")
    password: str = Field(..., description="Пароль")


class Token(BaseModel):
    """Модель для JWT токена."""

    access_token: str = Field(..., description="JWT токен")
    token_type: str = Field(default="bearer", description="Тип токена")


class TokenData(BaseModel):
    """Модель для данных токена."""

    user_id: Optional[str] = None
    username: Optional[str] = None


class User(BaseModel):
    """Модель пользователя."""

    user_id: str = Field(..., description="ID пользователя")
    username: str = Field(..., description="Имя пользователя")
    email: str = Field(..., description="Email")
    is_active: bool = Field(default=True, description="Активен ли пользователь")


class WebSocketAuthMessage(BaseModel):
    """Модель для аутентификации через WebSocket."""

    type: str = Field(..., description="Тип сообщения (должен быть 'auth')")
    token: str = Field(..., description="JWT токен")
