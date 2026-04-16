"""Memory timeline utilities."""

from __future__ import annotations

import builtins
import logging
from importlib import import_module
from typing import Optional

# Some legacy modules reference `logging` without importing it explicitly.
# Expose stdlib logging via builtins before importing memory_timeline.
builtins.logging = logging

_memory_timeline = import_module("backend.memory_timeline")
MemoryTimeline = _memory_timeline.MemoryTimeline
MemoryTimelineEventListener = _memory_timeline.MemoryTimelineEventListener
global_timeline = _memory_timeline.timeline


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
