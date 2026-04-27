"""Memory timeline utilities."""

from __future__ import annotations

from datetime import datetime
from typing import Dict, List, Optional

from backend.memory_timeline import (
    MemoryTimeline,
    MemoryTimelineEventListener,
)
from backend.memory_timeline import timeline as global_timeline


class MemoryTimelineService:
    """Provide access to the shared MemoryTimeline instance."""

    def __init__(self, timeline: Optional[MemoryTimeline] = None) -> None:
        self._timeline = timeline or global_timeline

    def get_timeline(self) -> MemoryTimeline:
        return self._timeline

    async def add_memory(
        self, content: str, memory_type: str, metadata: Optional[Dict] = None
    ) -> Dict:
        """Proxy writes to the underlying timeline implementation."""

        return await self._timeline.add_memory(content, memory_type, metadata)

    def list_memories(
        self,
        start_time: Optional[datetime] = None,
        end_time: Optional[datetime] = None,
        memory_type: Optional[str] = None,
        limit: int = 100,
    ) -> List[Dict]:
        """Proxy filtered reads to the underlying timeline implementation."""

        return self._timeline.get_timeline(start_time, end_time, memory_type, limit)

    def register_listener(self, listener: MemoryTimelineEventListener) -> None:
        self._timeline.register_listener(listener)

    def remove_listener(self, listener: MemoryTimelineEventListener) -> None:
        self._timeline.remove_listener(listener)

    def clear_listeners(self) -> None:
        self._timeline.clear_listeners()
