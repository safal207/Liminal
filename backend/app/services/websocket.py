"""WebSocket connection manager helpers."""

from __future__ import annotations

import json
import logging
import os
import re
import time
from datetime import UTC, datetime
from typing import TYPE_CHECKING, Any, Dict, Optional, Set, Tuple

from fastapi import WebSocket, WebSocketDisconnect

from backend.redis_client import RedisClient
from backend.websocket.connection_manager import ConnectionManager

if TYPE_CHECKING:  # pragma: no cover
    from .auth import AuthService
    from .memory import MemoryTimelineService
    from .ml import MLService

logger = logging.getLogger(__name__)

MAX_MESSAGE_BYTES = int(os.getenv("WS_MAX_MESSAGE_BYTES", "16384"))
MAX_CHANNEL_LENGTH = int(os.getenv("WS_MAX_CHANNEL_LENGTH", "128"))
MAX_CONTENT_LENGTH = int(os.getenv("WS_MAX_CONTENT_LENGTH", "8192"))
MAX_TOKEN_LENGTH = int(os.getenv("WS_MAX_TOKEN_LENGTH", "8192"))
CHANNEL_PATTERN = re.compile(r"^[A-Za-z0-9:_-]+$")

_MESSAGE_FIELDS: Dict[str, Tuple[Set[str], Set[str]]] = {
    "auth": ({"type", "token"}, {"type", "token"}),
    "subscribe": ({"type", "channel"}, {"type", "channel"}),
    "unsubscribe": ({"type", "channel"}, {"type", "channel"}),
    "broadcast": ({"type", "channel", "content"}, {"type", "channel", "content"}),
    "pong": ({"type"}, {"type"}),
}


class WebSocketMessageError(ValueError):
    """Stable public validation error for client-originated messages."""

    def __init__(self, code: str, message: str) -> None:
        super().__init__(message)
        self.code = code
        self.public_message = message


def parse_client_message(
    data: str,
    expected_types: Optional[Set[str]] = None,
) -> Dict[str, Any]:
    """Parse and strictly validate a client WebSocket message."""
    if len(data.encode("utf-8")) > MAX_MESSAGE_BYTES:
        raise WebSocketMessageError("message_too_large", "Message exceeds size limit")

    try:
        message = json.loads(data)
    except json.JSONDecodeError as exc:
        raise WebSocketMessageError("invalid_json", "Invalid JSON") from exc

    if not isinstance(message, dict):
        raise WebSocketMessageError("invalid_message", "Message must be an object")

    message_type = message.get("type")
    if not isinstance(message_type, str) or message_type not in _MESSAGE_FIELDS:
        raise WebSocketMessageError("unknown_type", "Unknown message type")
    if expected_types is not None and message_type not in expected_types:
        raise WebSocketMessageError("unexpected_type", "Unexpected message type")

    allowed, required = _MESSAGE_FIELDS[message_type]
    message_fields = set(message)
    if not required.issubset(message_fields) or not message_fields.issubset(allowed):
        raise WebSocketMessageError("invalid_schema", "Invalid message schema")

    if "token" in message:
        token = message["token"]
        if not isinstance(token, str) or not token or len(token) > MAX_TOKEN_LENGTH:
            raise WebSocketMessageError("invalid_token", "Invalid token value")

    if "channel" in message:
        channel = message["channel"]
        if (
            not isinstance(channel, str)
            or not channel
            or len(channel) > MAX_CHANNEL_LENGTH
            or CHANNEL_PATTERN.fullmatch(channel) is None
        ):
            raise WebSocketMessageError("invalid_channel", "Invalid channel value")

    if "content" in message:
        content = message["content"]
        if (
            not isinstance(content, str)
            or not content
            or len(content) > MAX_CONTENT_LENGTH
        ):
            raise WebSocketMessageError("invalid_content", "Invalid content value")

    return message


class LocalTokenBucket:
    """Per-connection fallback limiter used even when Redis is unavailable."""

    def __init__(self, rate: int, burst: int) -> None:
        self.rate = max(1, rate)
        self.burst = max(1, burst)
        self._state: Dict[str, Tuple[float, float]] = {}

    def is_limited(self, key: str) -> bool:
        now = time.monotonic()
        tokens, updated_at = self._state.get(key, (float(self.burst), now))
        tokens = min(float(self.burst), tokens + (now - updated_at) * self.rate)
        if tokens < 1.0:
            self._state[key] = (tokens, now)
            return True
        self._state[key] = (tokens - 1.0, now)
        return False

    def clear(self, key: str) -> None:
        self._state.pop(key, None)


class ConnectionManagerService:
    """Factory for the appropriate connection manager implementation."""

    def __init__(self) -> None:
        self._manager: Optional[ConnectionManager] = None

    def _create_manager(self) -> ConnectionManager:
        max_connections = int(os.getenv("WS_MAX_CONNECTIONS", "100"))
        max_connections_per_ip = int(os.getenv("WS_MAX_CONNECTIONS_PER_IP", "10"))
        rate_limit_messages_per_second = int(
            os.getenv("WS_RATE_LIMIT_PER_SECOND", "10")
        )
        rate_limit_burst = int(os.getenv("WS_RATE_LIMIT_BURST", "20"))

        use_redis = os.getenv("USE_REDIS", "false").lower() == "true"
        if use_redis:
            from backend.websocket.redis_connection_manager import (
                RedisConnectionManager,
            )

            redis_url = os.getenv("REDIS_URL", "redis://localhost:6379/0")
            return RedisConnectionManager(
                redis_url=redis_url,
                max_connections=max_connections,
                max_connections_per_ip=max_connections_per_ip,
            )

        redis_client = None
        if os.getenv("USE_REDIS_CLIENT", "false").lower() == "true":
            redis_client = RedisClient()

        return ConnectionManager(
            redis_client=redis_client,
            max_connections=max_connections,
            max_connections_per_ip=max_connections_per_ip,
            rate_limit_messages_per_second=rate_limit_messages_per_second,
            rate_limit_burst=rate_limit_burst,
        )

    def get_manager(self) -> ConnectionManager:
        if self._manager is None:
            self._manager = self._create_manager()
        return self._manager

    def get_connection_stats(self) -> dict:
        return self.get_manager().get_connection_stats()


class TimelineWebSocketService:
    """Handle the timeline WebSocket contract with strict ingress validation."""

    def __init__(
        self,
        manager_service: ConnectionManagerService,
        memory_service: "MemoryTimelineService",
        ml_service: "MLService",
        auth_service: "AuthService",
    ) -> None:
        self._manager_service = manager_service
        self._memory_service = memory_service
        self._ml_service = ml_service
        self._auth_service = auth_service
        self._local_limiter = LocalTokenBucket(
            rate=int(os.getenv("WS_RATE_LIMIT_PER_SECOND", "10")),
            burst=int(os.getenv("WS_RATE_LIMIT_BURST", "20")),
        )

    @staticmethod
    async def _send_error(
        websocket: WebSocket,
        code: str,
        message: str,
    ) -> None:
        await websocket.send_json({"type": "error", "code": code, "message": message})

    async def _is_rate_limited(
        self,
        manager: ConnectionManager,
        websocket: WebSocket,
        user_id: str,
    ) -> bool:
        ip_address = getattr(getattr(websocket, "client", None), "host", None)
        distributed_limited = await manager.is_rate_limited(
            user_id=user_id,
            ip_address=ip_address,
            websocket=websocket,
        )
        local_key = f"{user_id}:{manager.get_websocket_id(websocket)}"
        return distributed_limited or self._local_limiter.is_limited(local_key)

    async def handle_connection(
        self,
        websocket: WebSocket,
        token: Optional[str] = None,
    ) -> None:
        manager = self._manager_service.get_manager()
        timeline = self._memory_service.get_timeline()
        local_rate_key: Optional[str] = None

        if not await manager.accept_pending_connection(websocket):
            return

        authenticated = False
        user_id: Optional[str] = None

        try:
            if token:
                user_id = self._auth_service.verify_websocket_token(token)
                if user_id:
                    authenticated = await manager.authenticate_connection(
                        websocket, user_id
                    )
                    if authenticated:
                        self._ml_service.register_auth_event(user_id, True)
                        await websocket.send_json(
                            {
                                "type": "auth_success",
                                "message": "Authentication successful",
                            }
                        )

            if not authenticated:
                await websocket.send_json(
                    {
                        "type": "auth_required",
                        "message": "Send an access JWT token",
                    }
                )
                try:
                    auth_message = parse_client_message(
                        await websocket.receive_text(),
                        expected_types={"auth"},
                    )
                except WebSocketMessageError:
                    await manager.reject_connection(
                        websocket, "Invalid authentication message"
                    )
                    return

                user_id = self._auth_service.verify_websocket_token(
                    auth_message["token"]
                )
                if user_id:
                    authenticated = await manager.authenticate_connection(
                        websocket, user_id
                    )

                self._ml_service.register_auth_event(user_id or "unknown", authenticated)
                if not authenticated:
                    await manager.reject_connection(websocket, "Invalid access token")
                    return

                await websocket.send_json(
                    {"type": "auth_success", "message": "Authentication successful"}
                )

            if not user_id:
                await manager.reject_connection(websocket, "Authentication failed")
                return

            local_rate_key = f"{user_id}:{manager.get_websocket_id(websocket)}"

            while True:
                data = await websocket.receive_text()
                if await self._is_rate_limited(manager, websocket, user_id):
                    await self._send_error(
                        websocket,
                        "rate_limited",
                        "Message rate limit exceeded",
                    )
                    await websocket.close(code=1008, reason="Rate limit exceeded")
                    return

                try:
                    message = parse_client_message(data)
                except WebSocketMessageError as exc:
                    await self._send_error(websocket, exc.code, exc.public_message)
                    if exc.code == "message_too_large":
                        await websocket.close(code=1009, reason="Message too large")
                        return
                    continue

                message_type = message["type"]
                if message_type == "pong":
                    manager.mark_pong(websocket)
                    continue

                if message_type == "subscribe":
                    channel = message["channel"]
                    await manager.subscribe(user_id, channel, websocket)
                    if channel == "timeline":
                        await timeline.subscribe(websocket)
                    self._ml_service.register_channel_activity(channel)
                    await websocket.send_json(
                        {"type": "subscribed", "channel": channel}
                    )
                elif message_type == "unsubscribe":
                    channel = message["channel"]
                    await manager.unsubscribe(user_id, channel)
                    if channel == "timeline":
                        await timeline.unsubscribe(websocket)
                    await websocket.send_json(
                        {"type": "unsubscribed", "channel": channel}
                    )
                elif message_type == "broadcast":
                    await manager.broadcast(
                        message["channel"],
                        {
                            "type": "message",
                            "content": message["content"],
                            "sender": user_id,
                            "timestamp": datetime.now(UTC)
                            .isoformat()
                            .replace("+00:00", "Z"),
                        },
                        sender_id=user_id,
                    )

        except WebSocketDisconnect:
            pass
        except Exception:
            logger.exception("Unexpected WebSocket handler failure")
            try:
                if not authenticated:
                    await manager.reject_connection(websocket, "Internal server error")
                else:
                    await websocket.close(code=1011, reason="Internal server error")
            except Exception:
                logger.debug("WebSocket already closed during error handling")
        finally:
            if local_rate_key:
                self._local_limiter.clear(local_rate_key)
            try:
                if authenticated and user_id:
                    await timeline.unsubscribe(websocket)
                    await manager.disconnect(websocket, user_id)
                elif websocket in manager.pending_connections:
                    await manager.reject_connection(websocket, "Connection closed")
            except Exception:
                logger.exception("WebSocket cleanup failed")
