"""JWT authentication utilities for WebSocket and API endpoints."""

from __future__ import annotations

import logging
import os
from datetime import UTC, datetime, timedelta
from functools import lru_cache
from typing import Any, Dict, Optional

import jwt
from jwt import InvalidTokenError

from backend.core.settings import DEFAULT_SECRET, Settings, get_settings

logger = logging.getLogger("auth.jwt_utils")

try:
    from passlib.context import CryptContext

    pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")
    CRYPTO_ENABLED = True
except (ImportError, AttributeError) as exc:
    logger.warning("passlib/bcrypt is unavailable: %s", exc)
    CRYPTO_ENABLED = False

    class _UnavailableCryptContext:
        def __init__(self, error: Exception) -> None:
            self._error = error

        def verify(self, plain_password: str, hashed_password: str) -> bool:
            raise RuntimeError(
                "passlib/bcrypt failed to import; password verification is unavailable"
            ) from self._error

        def hash(self, password: str) -> str:
            raise RuntimeError(
                "passlib/bcrypt failed to import; password hashing is unavailable"
            ) from self._error

    pwd_context = _UnavailableCryptContext(exc)


VALID_TOKEN_TYPES = frozenset({"access", "refresh"})
MIN_PRODUCTION_SECRET_LENGTH = 32


class JWTManager:
    """Create and validate purpose-bound JWT tokens."""

    def __init__(self, config: Settings):
        self._config = config
        self._validate_configuration()

    def _validate_configuration(self) -> None:
        environment = os.getenv("ENV", "development").strip().lower()
        secret = self.secret_key.strip()
        if environment == "production" and (
            not secret
            or secret == DEFAULT_SECRET
            or len(secret) < MIN_PRODUCTION_SECRET_LENGTH
        ):
            raise RuntimeError(
                "JWT_SECRET_KEY must be a non-default secret of at least 32 characters "
                "when ENV=production"
            )

    @property
    def secret_key(self) -> str:
        return self._config.jwt.secret_key

    @property
    def algorithm(self) -> str:
        return self._config.jwt.algorithm

    @property
    def access_token_expire_minutes(self) -> int:
        return self._config.jwt.access_token_expire_minutes

    @staticmethod
    def _strip_bearer_prefix(token: str) -> str:
        if token.lower().startswith("bearer "):
            return token.split(" ", 1)[1].strip()
        return token

    def verify_password(self, plain_password: str, hashed_password: str) -> bool:
        """Verify a password without accepting legacy weak hashes."""
        if hashed_password.startswith("sha256$"):
            logger.warning("Rejected legacy SHA-256 password hash")
            return False
        return pwd_context.verify(plain_password, hashed_password)

    def get_password_hash(self, password: str) -> str:
        """Hash a password; cryptographic failures are propagated fail-closed."""
        return pwd_context.hash(password)

    def create_access_token(
        self, data: Dict[str, Any], expires_delta: Optional[timedelta] = None
    ) -> str:
        """Create a signed token with an explicit access or refresh purpose."""
        to_encode = data.copy()
        token_type = str(to_encode.setdefault("token_type", "access"))
        if token_type not in VALID_TOKEN_TYPES:
            raise ValueError(f"Unsupported token_type: {token_type}")

        expire = datetime.now(UTC) + (
            expires_delta
            if expires_delta is not None
            else timedelta(minutes=self.access_token_expire_minutes)
        )
        to_encode["exp"] = expire
        encoded_jwt = jwt.encode(to_encode, self.secret_key, algorithm=self.algorithm)
        logger.info(
            "JWT token created for user=%s type=%s",
            data.get("sub", "unknown"),
            token_type,
        )
        return encoded_jwt

    def verify_token(
        self, token: str, expected_type: Optional[str] = None
    ) -> Optional[Dict[str, Any]]:
        """Validate signature, expiry, subject and token purpose."""
        if expected_type is not None and expected_type not in VALID_TOKEN_TYPES:
            raise ValueError(f"Unsupported expected token type: {expected_type}")
        if not token:
            return None

        try:
            payload = jwt.decode(
                self._strip_bearer_prefix(token),
                self.secret_key,
                algorithms=[self.algorithm],
                options={"require": ["exp", "sub", "token_type"]},
            )
        except InvalidTokenError as exc:
            logger.warning("JWT validation failed: %s", exc)
            return None

        token_type = payload.get("token_type")
        if token_type not in VALID_TOKEN_TYPES:
            logger.warning("JWT token has unsupported token_type")
            return None
        if expected_type is not None and token_type != expected_type:
            logger.warning(
                "JWT token purpose mismatch: expected=%s actual=%s",
                expected_type,
                token_type,
            )
            return None
        return payload

    def extract_user_id_from_token(
        self, token: str, expected_type: str = "access"
    ) -> Optional[str]:
        payload = self.verify_token(token, expected_type=expected_type)
        return str(payload["sub"]) if payload else None


@lru_cache()
def get_jwt_manager() -> JWTManager:
    return JWTManager(get_settings())


jwt_manager = get_jwt_manager()

# In-memory user store populated from environment variables.
# Replace with a durable user database before production use.
_test_user_pass = os.getenv("LIMINAL_TEST_USER_PASS", "")
_admin_pass = os.getenv("LIMINAL_ADMIN_PASS", "")

fake_users_db: Dict[str, Any] = {}
if _test_user_pass:
    fake_users_db["testuser"] = {
        "user_id": "testuser",
        "username": "testuser",
        "hashed_password": jwt_manager.get_password_hash(_test_user_pass),
        "email": "test@example.com",
        "is_active": True,
    }
if _admin_pass:
    fake_users_db["admin"] = {
        "user_id": "admin",
        "username": "admin",
        "hashed_password": jwt_manager.get_password_hash(_admin_pass),
        "email": "admin@example.com",
        "is_active": True,
    }
if not fake_users_db:
    logger.warning(
        "No users configured. Set LIMINAL_TEST_USER_PASS / LIMINAL_ADMIN_PASS "
        "or replace fake_users_db with a durable database backend."
    )


def authenticate_user(username: str, password: str) -> Optional[Dict[str, Any]]:
    user = fake_users_db.get(username)
    if not user:
        logger.warning("Authentication failed for unknown user=%s", username)
        return None
    if not jwt_manager.verify_password(password, user["hashed_password"]):
        logger.warning("Authentication failed for user=%s", username)
        return None
    return user


def create_access_token_for_user(user_data: Dict[str, Any]) -> str:
    return jwt_manager.create_access_token(
        data={
            "sub": user_data["user_id"],
            "username": user_data["username"],
            "token_type": "access",
        },
        expires_delta=timedelta(minutes=jwt_manager.access_token_expire_minutes),
    )


def verify_websocket_token(token: str) -> Optional[str]:
    if not token:
        return None
    return jwt_manager.extract_user_id_from_token(token, expected_type="access")


ACCESS_TOKEN_EXPIRE_MINUTES = jwt_manager.access_token_expire_minutes


def create_tokens_for_user(user_data: Dict[str, Any]) -> Dict[str, str]:
    access_token = create_access_token_for_user(user_data)
    refresh_token = jwt_manager.create_access_token(
        data={
            "sub": user_data["user_id"],
            "username": user_data["username"],
            "token_type": "refresh",
        },
        expires_delta=timedelta(days=7),
    )
    return {"access_token": access_token, "refresh_token": refresh_token}


def refresh_access_token(refresh_token: str) -> Optional[Dict[str, str]]:
    payload = jwt_manager.verify_token(refresh_token, expected_type="refresh")
    if not payload:
        return None
    user = fake_users_db.get(str(payload["sub"]))
    return create_tokens_for_user(user) if user else None
