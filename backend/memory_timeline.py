"""
MemoryTimeline - динамически обновляемая временная шкала для работы с памятью.
Поддерживает подписку на обновления в реальном времени.
"""

print("DEBUG: Starting memory_timeline.py imports")
import asyncio
from datetime import datetime
from typing import Any, Dict, List, Optional

print("DEBUG: Importing fastapi.WebSocket")
import json

from fastapi import HTTPException, WebSocket

from auth.jwt_utils import jwt_manager

print("DEBUG: All imports completed in memory_timeline.py")

def verify_jwt_token(token: str):
    payload = jwt_manager.verify_token(token)
    if not payload:
        raise HTTPException(status_code=401, detail="Invalid token")
    return payload


class MemoryTimeline:
    def __init__(self):
        print("DEBUG: Initializing MemoryTimeline instance")
        self.timeline: List[Dict[str, Any]] = []
        self._subscribers: List[WebSocket] = []
        self._lock = asyncio.Lock()
        print("DEBUG: MemoryTimeline instance initialized")

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
            "timestamp": datetime.utcnow().astimezone().isoformat(),
            "content": content,
            "type": memory_type,
            "metadata": metadata or {},
        }

        async with self._lock:
            self.timeline.append(memory)
            await self._notify_subscribers("memory_added", memory)

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
            "timestamp": datetime.utcnow().isoformat(),
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
print("DEBUG: Creating global timeline instance")
timeline = MemoryTimeline()
print("DEBUG: Global timeline instance created")
