"""WebSocket routes."""
from __future__ import annotations

import json
from datetime import datetime
from typing import Optional

from fastapi import APIRouter, Depends, WebSocket, WebSocketDisconnect

from backend.auth.dependencies import token_verifier

from ..dependencies import (
    get_connection_manager,
    get_memory_timeline,
    get_ml_service,
)

router = APIRouter()


@router.websocket("/ws/timeline")
async def websocket_timeline(
    websocket: WebSocket,
    token: Optional[str] = None,
    manager=Depends(get_connection_manager),
    timeline=Depends(get_memory_timeline),
    ml_service=Depends(get_ml_service),
):
    connection_accepted = await manager.accept_pending_connection(websocket)
    if not connection_accepted:
        return

    authenticated = False
    user_id: Optional[str] = None

    try:
        if token:
            payload = await token_verifier.ensure_websocket(websocket, token)
            if payload is None:
                return
            user_id = payload.get("sub")
            if user_id:
                authenticated = await manager.authenticate_connection(websocket, user_id)
                if authenticated:
                    ml_service.register_auth_event(user_id, True)
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
                token_payload = await token_verifier.ensure_websocket(websocket, candidate)
                if token_payload is None:
                    ml_service.register_auth_event(user_id or "unknown", False)
                    return
                user_id = token_payload.get("sub")
                if user_id:
                    authenticated = await manager.authenticate_connection(websocket, user_id)
                    if authenticated:
                        ml_service.register_auth_event(user_id, True)
                        await websocket.send_json(
                            {
                                "type": "auth_success",
                                "message": f"Пользователь {user_id} успешно аутентифицирован",
                            }
                        )
                if not authenticated:
                    ml_service.register_auth_event(user_id or "unknown", False)
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
                await websocket.send_json(
                    {"type": "error", "message": "Неверный формат JSON"}
                )
                continue

            message_type = message.get("type")
            if message_type == "subscribe":
                channel = message.get("channel")
                if channel:
                    await manager.subscribe(user_id, channel, websocket)
                    if channel == "timeline":
                        await timeline.subscribe(websocket)
                    ml_service.register_channel_activity(channel)
                    await websocket.send_json(
                        {"type": "subscribed", "channel": channel}
                    )
            elif message_type == "unsubscribe":
                channel = message.get("channel")
                if channel:
                    await manager.unsubscribe(user_id, channel)
                    if channel == "timeline":
                        await timeline.unsubscribe(websocket)
                    await websocket.send_json(
                        {"type": "unsubscribed", "channel": channel}
                    )
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
