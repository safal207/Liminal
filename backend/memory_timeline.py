"""Event-emitting memory timeline with WebSocket broadcasting support."""

from __future__ import annotations

import asyncio
import json
from dataclasses import dataclass, field
from datetime import UTC, datetime
from typing import Any, Awaitable, Callable, Dict, List, Optional

from fastapi import HTTPException, WebSocket

from backend.auth.jwt_utils import jwt_manager

def verify_jwt_token(token: str):
    payload = jwt_manager.verify_token(token)
    if not payload:
        raise HTTPException(status_code=401, detail="Invalid token")
    return payload


@dataclass
class TimelineEvent:
    """Structured message emitted by the memory timeline."""

    type: str
    payload: Dict[str, Any]
    source: str = "memory.timeline"
    version: str = "1.0"
    occurred_at: str = field(
        default_factory=lambda: datetime.now(UTC).isoformat().replace("+00:00", "Z")
    )


MemoryTimelineEventListener = Callable[[TimelineEvent], Awaitable[None]]


class MemoryTimeline:
    def __init__(self):
        self.timeline: List[Dict[str, Any]] = []
        self._subscribers: List[WebSocket] = []
        self._lock = asyncio.Lock()
        self._event_listeners: List[MemoryTimelineEventListener] = []

    @property
    def subscribers(self):
        """Доступ к списку подписчиков (только для тестирования)."""
        return self._subscribers.copy()

    async def add_memory(
        self, content: str, memory_type: str, metadata: Optional[Dict] = None
    ) -> Dict[str, Any]:
        """Добавляет новое воспоминание в таймлайн."""
        memory = {
            "id": f"mem_{len(self.timeline) + 1}",
            "timestamp": datetime.now(UTC).isoformat().replace("+00:00", "Z"),
            "content": content,
            "type": memory_type,
            "metadata": metadata or {},
        }

        async with self._lock:
            self.timeline.append(memory)
            await self._notify_subscribers("memory_added", memory)

        await self._emit_event(
            TimelineEvent(
                type="memory.fragment.created",
                payload={
                    **memory,
                    "metadata": metadata or {},
                },
            )
        )

        return memory

    async def subscribe(self, websocket: WebSocket):
        """Подписывает WebSocket на обновления таймлайна."""
        token = websocket.headers.get("Authorization")
        if token:
            try:
                verify_jwt_token(token)
            except HTTPException as e:
                await websocket.close(code=1008, reason=e.detail)
                return
        elif not getattr(websocket, "user_id", None):
            await websocket.close(code=1008, reason="Authorization token missing")
            return

        async with self._lock:
            self._subscribers.append(websocket)

        # Отправляем историю при подключении
        await websocket.send_json(
            {
                "event": "initial_state",
                "data": self.timeline[-100:],  # Последние 100 записей
            }
        )

    async def unsubscribe(self, websocket: WebSocket):
        """Отписывает WebSocket от обновлений."""
        async with self._lock:
            if websocket in self._subscribers:
                self._subscribers.remove(websocket)

    async def _notify_subscribers(self, event_type: str, data: Dict):
        """Отправляет уведомление всем подписчикам."""
        # Если нет подписчиков, не создаем сообщение и не блокируем
        if not self._subscribers:
            return

        message = {
            "event": event_type,
            "data": data,
            "timestamp": datetime.now(UTC).isoformat().replace("+00:00", "Z"),
        }

        message_json = json.dumps(message)

        async with self._lock:
            # Проверяем снова под локом, так как список мог измениться
            if not self._subscribers:
                return

            disconnected = []
            for subscriber in self._subscribers:
                try:
                    await subscriber.send_text(message_json)
                except Exception as e:
                    print(f"Error sending to WebSocket: {e}")
                    disconnected.append(subscriber)

            # Удаляем отключившихся подписчиков, если они еще не были удалены
            if disconnected:
                self._subscribers = [
                    sub for sub in self._subscribers if sub not in disconnected
                ]

    def register_listener(self, listener: MemoryTimelineEventListener) -> None:
        """Register a coroutine listener for timeline events."""

        if listener not in self._event_listeners:
            self._event_listeners.append(listener)

    def remove_listener(self, listener: MemoryTimelineEventListener) -> None:
        """Remove a previously registered listener."""

        self._event_listeners = [
            existing for existing in self._event_listeners if existing != listener
        ]

    def clear_listeners(self) -> None:
        """Remove all registered event listeners."""

        self._event_listeners.clear()

    async def _emit_event(self, event: TimelineEvent) -> None:
        """Dispatch an event to all registered listeners."""

        if not self._event_listeners:
            return

        for listener in list(self._event_listeners):
            try:
                await listener(event)
            except Exception as exc:  # pragma: no cover - defensive logging
                print(f"Error emitting event {event.type}: {exc}")

    def get_timeline(
        self,
        start_time: Optional[datetime] = None,
        end_time: Optional[datetime] = None,
        memory_type: Optional[str] = None,
        limit: int = 100,
    ) -> List[Dict]:
        """Возвращает отфильтрованный таймлайн."""
        result = self.timeline

        if start_time:
            result = [
                m
                for m in result
                if datetime.fromisoformat(m["timestamp"]) >= start_time
            ]
        if end_time:
            result = [
                m for m in result if datetime.fromisoformat(m["timestamp"]) <= end_time
            ]
        if memory_type:
            result = [m for m in result if m["type"] == memory_type]

        return result[-limit:]


# Глобальный экземпляр таймлайна
timeline = MemoryTimeline()
