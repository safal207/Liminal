"""Authentication service abstractions for the FastAPI routers."""

from __future__ import annotations

from typing import Any, Dict, Optional

from backend.auth.jwt_utils import (
    authenticate_user,
    create_access_token_for_user,
    jwt_manager,
    verify_websocket_token,
)
from backend.auth.models import Token, UserLogin


class AuthService:
    """Encapsulate authentication workflows used by the API layer."""

    def login(self, credentials: UserLogin) -> Token:
        user = authenticate_user(credentials.username, credentials.password)
        if not user:
            raise PermissionError("invalid-credentials")

        access_token = create_access_token_for_user(user)
        return Token(access_token=access_token, token_type="bearer")

    def issue_token(self, credentials: UserLogin) -> Token:
        return self.login(credentials)

    def verify_token(self, token: str) -> Optional[Dict[str, Any]]:
        """Return payload only for a valid access token."""
        return jwt_manager.verify_token(token, expected_type="access")

    def verify_websocket_token(self, token: str) -> Optional[str]:
        return verify_websocket_token(token)
