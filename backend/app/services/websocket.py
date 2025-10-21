"""WebSocket connection manager helpers."""
from __future__ import annotations

import json
import os
from datetime import datetime
from typing import TYPE_CHECKING, Optional

from fastapi import WebSocket, WebSocketDisconnect

from backend.redis_client import RedisClient
from backend.websocket.connection_manager import ConnectionManager

if TYPE_CHECKING:  # pragma: no cover - used only for type hints
    from .auth import AuthService
    from .memory import MemoryTimelineService
    from .ml import MLService


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
            from backend.websocket.redis_connection_manager import (  # type: ignore
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
    """Handle the timeline WebSocket contract."""

    def __init__(
        self,
        manager_service: ConnectionManagerService,
        memory_service: "MemoryTimelineService",
        ml_service: "MLService",
        auth_service: "AuthService",
    ) -> None:
        self._manager_service: ConnectionManagerService = manager_service
        self._memory_service: "MemoryTimelineService" = memory_service
        self._ml_service: "MLService" = ml_service
        self._auth_service: "AuthService" = auth_service

    async def handle_connection(self, websocket: WebSocket, token: Optional[str] = None) -> None:
        manager = self._manager_service.get_manager()
        timeline = self._memory_service.get_timeline()

        connection_accepted = await manager.accept_pending_connection(websocket)
        if not connection_accepted:
            return

        authenticated = False
        user_id: Optional[str] = None

        try:
            if token:
                user_id = self._auth_service.verify_websocket_token(token)
                if user_id:
                    authenticated = await manager.authenticate_connection(websocket, user_id)
                    if authenticated:
                        self._ml_service.register_auth_event(user_id, True)
                        await websocket.send_json(
                            {
                                "type": "auth_success",
                                "message": f"Пользователь {user_id} успешно аутентифицирован через URL токен",
                            }
                        )

            auth_message_data: Optional[str] = None
            if not authenticated:
                await websocket.send_json(
                    {
                        "type": "auth_required",
                        "message": "Необходима аутентификация. Отправьте JWT токен.",
                    }
                )
                auth_message_data = await websocket.receive_text()

            if not authenticated:
                try:
                    payload = json.loads(auth_message_data or "{}")
                except json.JSONDecodeError:
                    await manager.reject_connection(websocket, "Invalid JSON format")
                    return

                if payload.get("type") == "auth" and "token" in payload:
                    candidate = payload["token"]
                    user_id = self._auth_service.verify_websocket_token(candidate)
                    if user_id:
                        authenticated = await manager.authenticate_connection(
                            websocket, user_id
                        )
                        if authenticated:
                            self._ml_service.register_auth_event(user_id, True)
                            await websocket.send_json(
                                {
                                    "type": "auth_success",
                                    "message": f"Пользователь {user_id} успешно аутентифицирован",
                                }
                            )
                    if not authenticated:
                        self._ml_service.register_auth_event(user_id or "unknown", False)
                        await manager.reject_connection(websocket, "Invalid token")
                        return
                else:
                    await manager.reject_connection(websocket, "Invalid auth message format")
                    return

            if not user_id:
                await manager.reject_connection(websocket, "Authentication failed")
                return

            while True:
                data = await websocket.receive_text()
                try:
                    message = json.loads(data)
                except json.JSONDecodeError:
                    await websocket.send_json({"type": "error", "message": "Неверный формат JSON"})
                    continue

                message_type = message.get("type")
                if message_type == "subscribe":
                    channel = message.get("channel")
                    if channel:
                        await manager.subscribe(user_id, channel, websocket)
                        if channel == "timeline":
                            await timeline.subscribe(websocket)
                        self._ml_service.register_channel_activity(channel)
                        await websocket.send_json({"type": "subscribed", "channel": channel})
                elif message_type == "unsubscribe":
                    channel = message.get("channel")
                    if channel:
                        await manager.unsubscribe(user_id, channel)
                        if channel == "timeline":
                            await timeline.unsubscribe(websocket)
                        await websocket.send_json({"type": "unsubscribed", "channel": channel})
                elif message_type == "broadcast":
                    channel = message.get("channel")
                    content = message.get("content")
                    if channel and content:
                        await manager.broadcast(
                            channel,
                            {
                                "type": "message",
                                "content": content,
                                "sender": user_id,
                                "timestamp": datetime.utcnow().isoformat(),
                            },
                            sender_id=user_id,
                        )
                else:
                    await websocket.send_json(
                        {
                            "type": "error",
                            "message": f"Неизвестный тип сообщения: {message_type}",
                        }
                    )

        except WebSocketDisconnect:
            if authenticated and user_id:
                await timeline.unsubscribe(websocket)
                await manager.disconnect(websocket, user_id)
            else:
                await manager.reject_connection(websocket, "Disconnected during auth")
        except Exception as exc:
            if authenticated and user_id:
                await timeline.unsubscribe(websocket)
                await manager.disconnect(websocket, user_id)
            else:
                await manager.reject_connection(websocket, f"Error: {exc}")
