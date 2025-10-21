"""Memory timeline utilities."""
from __future__ import annotations

from typing import Optional

from backend.memory_timeline import (
    MemoryTimeline,
    MemoryTimelineEventListener,
    timeline as global_timeline,
)


class MemoryTimelineService:
    """Provide access to the shared MemoryTimeline instance."""

    def __init__(self, timeline: Optional[MemoryTimeline] = None) -> None:
        self._timeline = timeline or global_timeline

    def get_timeline(self) -> MemoryTimeline:
        return self._timeline

    def register_listener(self, listener: MemoryTimelineEventListener) -> None:
        self._timeline.register_listener(listener)

    def remove_listener(self, listener: MemoryTimelineEventListener) -> None:
        self._timeline.remove_listener(listener)

    def clear_listeners(self) -> None:
        self._timeline.clear_listeners()
