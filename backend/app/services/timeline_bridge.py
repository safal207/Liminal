"""Bridge between the in-memory timeline and the Neo4j gateway."""

from __future__ import annotations

from typing import Any, Dict

from backend.infrastructure.neo4j import Neo4jGateway
from backend.memory_timeline import TimelineEvent


class TimelineGraphBridge:
    """Translate timeline events into Neo4j gateway operations."""

    def __init__(self, gateway: Neo4jGateway) -> None:
        self._gateway = gateway

    @property
    def gateway(self) -> Neo4jGateway:
        return self._gateway

    async def handle_event(self, event: TimelineEvent) -> None:
        """Dispatch supported timeline events to the gateway."""

        if event.type == "memory.fragment.created":
            self._ingest_memory_fragment(event.payload)

    # Internal helpers -------------------------------------------------
    def _ingest_memory_fragment(self, payload: Dict[str, Any]) -> None:
        fragment = {
            "id": payload.get("id"),
            "content": payload.get("content"),
            "type": payload.get("type"),
            "growth_stage": payload.get("metadata", {}).get(
                "growth_stage", "observed"
            ),
            "timestamp": payload.get("timestamp"),
            "metadata": payload.get("metadata", {}),
        }
        self._gateway.create_memory_fragment_node(fragment)


__all__ = ["TimelineGraphBridge"]
