"""Reusable authentication dependencies for HTTP and WebSocket handlers."""

from __future__ import annotations

from typing import Any, Dict, Optional

from fastapi import Depends, HTTPException, WebSocket, status
from fastapi.security import HTTPAuthorizationCredentials, HTTPBearer

from backend.auth.jwt_utils import JWTManager, get_jwt_manager


_bearer_scheme = HTTPBearer(auto_error=False)


class TokenVerifier:
    """Validate Bearer tokens for both HTTP and WebSocket flows."""

    def __init__(self, manager: Optional[JWTManager] = None) -> None:
        self._manager = manager or get_jwt_manager()

    def _validate(self, token: Optional[str]) -> Dict[str, Any]:
        if not token:
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="Authorization token missing",
            )

        payload = self._manager.verify_token(token)
        if not payload:
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="Invalid or expired token",
            )
        return payload

    async def __call__(
        self, credentials: HTTPAuthorizationCredentials = Depends(_bearer_scheme)
    ) -> Dict[str, Any]:
        token = credentials.credentials if credentials else None
        return self._validate(token)

    async def ensure_websocket(
        self, websocket: WebSocket, token: Optional[str] = None
    ) -> Optional[Dict[str, Any]]:
        token_value = token or websocket.headers.get("Authorization")
        try:
            payload = self._validate(token_value)
        except HTTPException as exc:  # pragma: no cover - handshake path
            await websocket.close(code=1008, reason=exc.detail)
            return None

        websocket.state.jwt_payload = payload
        return payload

    def manager(self) -> JWTManager:
        """Expose the underlying JWT manager for auxiliary operations."""

        return self._manager


token_verifier = TokenVerifier()

__all__ = ["TokenVerifier", "token_verifier"]
