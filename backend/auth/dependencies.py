"""Reusable authentication dependencies for HTTP and WebSocket handlers."""

from __future__ import annotations

from typing import Any, Dict, Optional

from fastapi import Depends, HTTPException, WebSocket, status
from fastapi.security import HTTPAuthorizationCredentials, HTTPBearer

from backend.auth.jwt_utils import JWTManager, get_jwt_manager

_bearer_scheme = HTTPBearer(auto_error=False)


class TokenVerifier:
    """Validate purpose-bound access tokens for HTTP and WebSocket flows."""

    def __init__(
        self,
        manager: Optional[JWTManager] = None,
        expected_type: str = "access",
    ) -> None:
        self._manager = manager or get_jwt_manager()
        self._expected_type = expected_type

    def _validate(self, token: Optional[str]) -> Dict[str, Any]:
        if not token:
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="Authorization token missing",
            )

        payload = self._manager.verify_token(
            token,
            expected_type=self._expected_type,
        )
        if not payload:
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="Invalid, expired, or wrong-purpose token",
            )
        return payload

    async def __call__(
        self,
        credentials: HTTPAuthorizationCredentials = Depends(_bearer_scheme),
    ) -> Dict[str, Any]:
        token = credentials.credentials if credentials else None
        return self._validate(token)

    async def ensure_websocket(
        self,
        websocket: WebSocket,
        token: Optional[str] = None,
    ) -> Optional[Dict[str, Any]]:
        token_value = token or websocket.headers.get("Authorization")
        try:
            payload = self._validate(token_value)
        except HTTPException:
            await websocket.close(code=1008, reason="Invalid access token")
            return None

        websocket.state.jwt_payload = payload
        return payload

    def manager(self) -> JWTManager:
        return self._manager


token_verifier = TokenVerifier(expected_type="access")
refresh_token_verifier = TokenVerifier(expected_type="refresh")

__all__ = ["TokenVerifier", "token_verifier", "refresh_token_verifier"]
