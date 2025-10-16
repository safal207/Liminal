"""
JWT Authentication utilities for WebSocket and API endpoints.
"""

import hashlib
import logging
from datetime import datetime, timedelta
from typing import Any, Dict, Optional

from fastapi import HTTPException, status
from jose import JWTError, jwt

try:  # pragma: no cover - compatibility shim for different PYTHONPATH setups
    from core.settings import settings
except ImportError:  # pragma: no cover - fallback for package usage
    from backend.core.settings import settings

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
            return f"$2b$12$DUMMY_HASH_FOR_TESTING_{password[:5]}"

    pwd_context = DummyCryptContext()

logger = logging.getLogger("auth.jwt_utils")

# pwd_context уже определен выше в блоке импорта


class JWTManager:
    """Менеджер для работы с JWT токенами."""

    def __init__(self):
        self.secret_key = settings.jwt_secret_key
        self.algorithm = settings.jwt_algorithm
        self.access_token_expire_minutes = settings.jwt_access_token_expire_minutes

    def _strip_bearer_prefix(self, token: str) -> str:
        """Remove a Bearer prefix if present."""

        if token.lower().startswith("bearer "):
            return token.split(" ", 1)[1].strip()
        return token

    def verify_password(self, plain_password: str, hashed_password: str) -> bool:
        """Проверяет пароль против хеша."""
        if hashed_password.startswith("sha256$"):
            expected = hashed_password.split("$", 1)[1]
            actual = hashlib.sha256(plain_password.encode("utf-8")).hexdigest()
            return actual == expected

        return pwd_context.verify(plain_password, hashed_password)

    def get_password_hash(self, password: str) -> str:
        """Создает хеш пароля."""
        try:
            return pwd_context.hash(password)
        except ValueError as exc:
            logger.warning(
                "Ошибка при хешировании пароля через bcrypt: %s."
                " Переключаемся на sha256 для тестового режима.",
                exc,
            )
            return f"sha256${hashlib.sha256(password.encode('utf-8')).hexdigest()}"

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
            if not token:
                logger.warning("Попытка проверить пустой JWT токен")
                return None

            normalized_token = self._strip_bearer_prefix(token)
            payload = jwt.decode(
                normalized_token, self.secret_key, algorithms=[self.algorithm]
            )
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
    access_token_expires = timedelta(
        minutes=jwt_manager.access_token_expire_minutes
    )
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
    if not token:
        return None

    return jwt_manager.extract_user_id_from_token(token)
