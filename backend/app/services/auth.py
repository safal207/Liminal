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
    """Encapsulates authentication workflows used by the API layer."""

    def login(self, credentials: UserLogin) -> Token:
        """Authenticate a user and return a bearer token."""

        user = authenticate_user(credentials.username, credentials.password)
        if not user:
            raise PermissionError("invalid-credentials")

        access_token = create_access_token_for_user(user)
        return Token(access_token=access_token, token_type="bearer")

    def issue_token(self, credentials: UserLogin) -> Token:
        """Compatibility wrapper for OAuth style token issuance."""

        return self.login(credentials)

    def verify_token(self, token: str) -> Optional[Dict[str, Any]]:
        """Return payload for a valid token, ``None`` otherwise."""

        return jwt_manager.verify_token(token)

    def verify_websocket_token(self, token: str) -> Optional[str]:
        """Validate a WebSocket token and return the associated user id."""

        return verify_websocket_token(token)

