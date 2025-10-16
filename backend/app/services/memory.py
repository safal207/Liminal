"""Memory timeline utilities."""
from __future__ import annotations

from datetime import datetime
from typing import Dict, List, Optional

from backend.memory_timeline import MemoryTimeline, timeline as global_timeline


class MemoryTimelineService:
    """Provide access to the shared MemoryTimeline instance."""

    def __init__(self, timeline: Optional[MemoryTimeline] = None) -> None:
        self._timeline = timeline or global_timeline

    def get_timeline(self) -> MemoryTimeline:
        return self._timeline

    async def add_memory(
        self, content: str, memory_type: str, metadata: Optional[Dict] = None
    ) -> Dict:
        return await self._timeline.add_memory(content, memory_type, metadata)

    def list_memories(
        self,
        start_time: Optional[datetime] = None,
        end_time: Optional[datetime] = None,
        memory_type: Optional[str] = None,
        limit: int = 100,
    ) -> List[Dict]:
        return self._timeline.get_timeline(start_time, end_time, memory_type, limit)

    def subscriber_count(self) -> int:
        timeline = self.get_timeline()
        if hasattr(timeline, "subscribers"):
            return len(timeline.subscribers)
        if hasattr(timeline, "_subscribers"):
            return len(timeline._subscribers)
        return 0
