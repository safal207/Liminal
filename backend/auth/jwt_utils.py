"""
JWT Authentication utilities for WebSocket and API endpoints.
"""

import logging
import os
from datetime import datetime, timedelta
from typing import Any, Dict, Optional

from fastapi import HTTPException, status
from jose import JWTError, jwt

# Безопасный импорт CryptContext с обработкой ошибок
try:
    from passlib.context import CryptContext

    # Проверка, что bcrypt работает корректно
    pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")
    CRYPTO_ENABLED = True
except (ImportError, AttributeError) as e:
    logger = logging.getLogger("auth.jwt_utils")
    logger.warning(f"Ошибка при импорте passlib/bcrypt: {e}")
    logger.warning(
        "JWT аутентификация будет работать в тестовом режиме без проверки паролей"
    )
    CRYPTO_ENABLED = False

    # Заглушка для CryptContext
    class DummyCryptContext:
        def __init__(self, **kwargs):
            pass

        def verify(self, plain_password, hashed_password):
            # В тестовом режиме всегда возвращаем True
            return True

        def hash(self, password):
            # В тестовом режиме просто возвращаем хеш-подобную строку
            return f"$2b$12$DUMMY_HASH_FOR_TESTING_{password}"

    pwd_context = DummyCryptContext()

logger = logging.getLogger("auth.jwt_utils")

FALLBACK_HASH_PREFIX = "$2b$12$DUMMY_HASH_FOR_TESTING_"

# Конфигурация JWT
SECRET_KEY = os.getenv(
    "JWT_SECRET_KEY", "resonance-liminal-secret-key-change-in-production"
)
ALGORITHM = "HS256"
ACCESS_TOKEN_EXPIRE_MINUTES = 30

# pwd_context уже определен выше в блоке импорта


class JWTManager:
    """Менеджер для работы с JWT токенами."""

    def __init__(self):
        self.secret_key = SECRET_KEY
        self.algorithm = ALGORITHM
        self.access_token_expire_minutes = ACCESS_TOKEN_EXPIRE_MINUTES

    def verify_password(self, plain_password: str, hashed_password: str) -> bool:
        """Проверяет пароль против хеша."""
        try:
            return pwd_context.verify(plain_password, hashed_password)
        except ValueError as exc:
            logger.warning(
                "Ошибка проверки bcrypt: %s. Используется резервная проверка пароля.",
                exc,
            )
            if (
                isinstance(hashed_password, str)
                and hashed_password.startswith(FALLBACK_HASH_PREFIX)
            ):
                stored_password = hashed_password[len(FALLBACK_HASH_PREFIX) :]
                return stored_password == plain_password
            return False

    def get_password_hash(self, password: str) -> str:
        """Создает хеш пароля."""
        try:
            return pwd_context.hash(password)
        except ValueError as exc:
            logger.warning(
                "Ошибка генерации bcrypt-хеша: %s. Используется резервный хеш.", exc
            )
            return f"{FALLBACK_HASH_PREFIX}{password}"

    def create_access_token(
        self, data: Dict[str, Any], expires_delta: Optional[timedelta] = None
    ) -> str:
        """
        Создает JWT токен.

        Args:
            data: Данные для включения в токен
            expires_delta: Время жизни токена

        Returns:
            str: JWT токен
        """
        to_encode = data.copy()
        if expires_delta:
            expire = datetime.utcnow() + expires_delta
        else:
            expire = datetime.utcnow() + timedelta(
                minutes=self.access_token_expire_minutes
            )

        to_encode.update({"exp": expire})
        encoded_jwt = jwt.encode(to_encode, self.secret_key, algorithm=self.algorithm)

        logger.info(f"JWT токен создан для пользователя: {data.get('sub', 'unknown')}")
        return encoded_jwt

    def verify_token(self, token: str) -> Optional[Dict[str, Any]]:
        """
        Проверяет JWT токен.

        Args:
            token: JWT токен для проверки

        Returns:
            Dict[str, Any]: Payload токена или None если токен недействителен
        """
        try:
            payload = jwt.decode(token, self.secret_key, algorithms=[self.algorithm])
            user_id: str = payload.get("sub")
            if user_id is None:
                logger.warning("JWT токен не содержит user_id")
                return None

            logger.debug(f"JWT токен успешно проверен для пользователя: {user_id}")
            return payload
        except JWTError as e:
            logger.warning(f"Ошибка проверки JWT токена: {e}")
            return None

    def extract_user_id_from_token(self, token: str) -> Optional[str]:
        """
        Извлекает user_id из JWT токена.

        Args:
            token: JWT токен

        Returns:
            str: user_id или None если токен недействителен
        """
        payload = self.verify_token(token)
        if payload:
            return payload.get("sub")
        return None


# Глобальный экземпляр JWT менеджера
jwt_manager = JWTManager()

# Временное хранилище пользователей (в продакшене заменить на БД)
fake_users_db = {
    "testuser": {
        "user_id": "testuser",
        "username": "testuser",
        "hashed_password": jwt_manager.get_password_hash("testpass"),
        "email": "test@example.com",
        "is_active": True,
    },
    "admin": {
        "user_id": "admin",
        "username": "admin",
        "hashed_password": jwt_manager.get_password_hash("admin123"),
        "email": "admin@example.com",
        "is_active": True,
    },
}


def authenticate_user(username: str, password: str) -> Optional[Dict[str, Any]]:
    """
    Аутентифицирует пользователя по логину и паролю.

    Args:
        username: Имя пользователя
        password: Пароль

    Returns:
        Dict[str, Any]: Данные пользователя или None если аутентификация неудачна
    """
    user = fake_users_db.get(username)
    if not user:
        logger.warning(f"Пользователь {username} не найден")
        return None

    if not jwt_manager.verify_password(password, user["hashed_password"]):
        logger.warning(f"Неверный пароль для пользователя {username}")
        return None

    logger.info(f"Пользователь {username} успешно аутентифицирован")
    return user


def create_access_token_for_user(user_data: Dict[str, Any]) -> str:
    """
    Создает access token для пользователя.

    Args:
        user_data: Данные пользователя

    Returns:
        str: JWT токен
    """
    access_token_expires = timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES)
    access_token = jwt_manager.create_access_token(
        data={"sub": user_data["user_id"], "username": user_data["username"]},
        expires_delta=access_token_expires,
    )
    return access_token


def verify_websocket_token(token: str) -> Optional[str]:
    """
    Проверяет токен для WebSocket соединения.

    Args:
        token: JWT токен

    Returns:
        str: user_id или None если токен недействителен
    """
    return jwt_manager.extract_user_id_from_token(token)
