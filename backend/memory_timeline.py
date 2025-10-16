"""Memory timeline primitives and observability hooks."""

import asyncio
import json
import logging
from datetime import datetime
from time import perf_counter
from typing import Any, Dict, List, Optional

from fastapi import HTTPException, WebSocket

from auth.jwt_utils import jwt_manager
from backend.metrics.collectors import (
    memory_timeline_backlog_size,
    memory_timeline_events_total,
    memory_timeline_processing_seconds,
    memory_timeline_subscribers,
)

logger = logging.getLogger(__name__)

def verify_jwt_token(token: str):
    payload = jwt_manager.verify_token(token)
    if not payload:
        raise HTTPException(status_code=401, detail="Invalid token")
    return payload


class MemoryTimeline:
    def __init__(self):
        logger.debug("Initializing MemoryTimeline instance")
        self.timeline: List[Dict[str, Any]] = []
        self._subscribers: List[WebSocket] = []
        self._lock = asyncio.Lock()
        logger.debug("MemoryTimeline instance initialized")

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
            "timestamp": datetime.utcnow().astimezone().isoformat(),
            "content": content,
            "type": memory_type,
            "metadata": metadata or {},
        }

        async with self._lock:
            self.timeline.append(memory)
            backlog_size = len(self.timeline)

        memory_timeline_backlog_size.set(backlog_size)
        await self._notify_subscribers("memory_added", memory)

        memory_timeline_events_total.labels(event_type="memory_added").inc()
        memory_timeline_processing_seconds.labels(operation="add_memory").observe(
            perf_counter() - start_time
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
            memory_timeline_subscribers.set(len(self._subscribers))

        memory_timeline_events_total.labels(event_type="subscriber_joined").inc()
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
            "timestamp": datetime.utcnow().isoformat(),
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
                except Exception:
                    logger.exception("Error sending notification to WebSocket")
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
