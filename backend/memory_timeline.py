"""Event-emitting memory timeline with WebSocket broadcasting support."""

import asyncio
import json
import logging
from dataclasses import dataclass, field
from datetime import UTC, datetime
from time import perf_counter
from typing import Any, Awaitable, Callable, Dict, List, Optional

from fastapi import WebSocket

from backend.auth.dependencies import token_verifier
from backend.core.settings import get_settings

logger = logging.getLogger(__name__)

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
        settings = get_settings()
        self._initial_state_limit = settings.memory_timeline.initial_state_limit
        self._max_retained_events = settings.memory_timeline.max_retained_events
        print("DEBUG: MemoryTimeline instance initialized")

    @property
    def subscribers(self):
        """Доступ к списку подписчиков (только для тестирования)."""
        return self._subscribers.copy()

    async def add_memory(
        self, content: str, memory_type: str, metadata: Optional[Dict] = None
    ) -> Dict[str, Any]:
        """Добавляет новое воспоминание в таймлайн."""
        start_time = perf_counter()
        memory = {
            "id": f"mem_{len(self.timeline) + 1}",
            "timestamp": datetime.now(UTC).isoformat().replace("+00:00", "Z"),
            "content": content,
            "type": memory_type,
            "metadata": metadata or {},
        }

        async with self._lock:
            self.timeline.append(memory)
            if len(self.timeline) > self._max_retained_events:
                self.timeline = self.timeline[-self._max_retained_events :]
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
        payload = await token_verifier.ensure_websocket(websocket, token)
        if payload is None and not getattr(websocket, "user_id", None):
            return

        async with self._lock:
            self._subscribers.append(websocket)
            memory_timeline_subscribers.set(len(self._subscribers))

        memory_timeline_events_total.labels(event_type="subscriber_joined").inc()
        # Отправляем историю при подключении
        await websocket.send_json(
            {
                "event": "initial_state",
                "data": self.timeline[-self._initial_state_limit :],
            }
        )

    async def unsubscribe(self, websocket: WebSocket):
        """Отписывает WebSocket от обновлений."""
        async with self._lock:
            if websocket in self._subscribers:
                self._subscribers.remove(websocket)
                memory_timeline_subscribers.set(len(self._subscribers))
                memory_timeline_events_total.labels(event_type="subscriber_left").inc()

    async def _notify_subscribers(self, event_type: str, data: Dict):
        """Отправляет уведомление всем подписчикам."""
        # Если нет подписчиков, не создаем сообщение и не блокируем
        if not self._subscribers:
            memory_timeline_events_total.labels(event_type="notification_skipped").inc()
            return

        start_time = perf_counter()
        message = {
            "event": event_type,
            "data": data,
            "timestamp": datetime.now(UTC).isoformat().replace("+00:00", "Z"),
        }

        message_json = json.dumps(message)

        delivered = 0
        failed = 0
        async with self._lock:
            # Проверяем снова под локом, так как список мог измениться
            if not self._subscribers:
                memory_timeline_events_total.labels(event_type="notification_skipped").inc()
                return

            disconnected = []
            for subscriber in self._subscribers:
                try:
                    await subscriber.send_text(message_json)
                    delivered += 1
                except Exception as e:
                    print(f"Error sending to WebSocket: {e}")
                    disconnected.append(subscriber)
                    failed += 1

            # Удаляем отключившихся подписчиков, если они еще не были удалены
            if disconnected:
                self._subscribers = [
                    sub for sub in self._subscribers if sub not in disconnected
                ]
                memory_timeline_events_total.labels(
                    event_type="subscriber_dropped"
                ).inc(len(disconnected))
                memory_timeline_subscribers.set(len(self._subscribers))

        if delivered:
            memory_timeline_events_total.labels(event_type="notification_sent").inc(
                delivered
            )
        if failed:
            memory_timeline_events_total.labels(event_type="notification_failed").inc(
                failed
            )
        memory_timeline_processing_seconds.labels(
            operation="notify_subscribers"
        ).observe(perf_counter() - start_time)

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
        limit: Optional[int] = None,
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

        effective_limit = limit if limit is not None else self._initial_state_limit
        return result[-effective_limit:]


# Глобальный экземпляр таймлайна
timeline = MemoryTimeline()
