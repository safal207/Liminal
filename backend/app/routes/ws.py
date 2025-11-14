"""WebSocket routes."""
from __future__ import annotations

from typing import Optional

from fastapi import APIRouter, Depends, WebSocket

from ..dependencies import get_websocket_service

router = APIRouter()


@router.websocket("/ws/timeline")
async def websocket_timeline(
    websocket: WebSocket,
    token: Optional[str] = None,
    service=Depends(get_websocket_service),
):
    """Delegate WebSocket lifecycle handling to the service layer."""

    await service.handle_connection(websocket, token)
