"""In-memory mock implementation of the Neo4j gateway for tests."""

from __future__ import annotations

from dataclasses import dataclass
from datetime import UTC, datetime
from typing import Any, Dict, List, Optional

from . import Neo4jGateway


@dataclass
class MockNeo4jDateTime:
    """Lightweight replacement for neo4j.time.DateTime with isoformat support."""

    value: datetime

    def isoformat(self) -> str:
        return self.value.isoformat() + "Z"


class MockNeo4jGateway(Neo4jGateway):
    """Simple in-memory Neo4j gateway suitable for unit tests."""

    def __init__(self) -> None:
        self.dunewaves: List[Dict[str, Any]] = []
        self.memory_fragments: List[Dict[str, Any]] = []
        self.relationships: List[Dict[str, Any]] = []

    def create_indexes(self) -> None:  # pragma: no cover - nothing to do here
        return None

    def close(self) -> None:  # pragma: no cover - nothing to do here
        return None

    def _timestamp(self) -> str:
        return datetime.now(UTC).isoformat().replace("+00:00", "Z")

    def create_dunewave_node(self, wave_data: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        node = {
            **wave_data,
            "id": wave_data.get("id", f"wave_{len(self.dunewaves) + 1}"),
            "timestamp": wave_data.get("timestamp", self._timestamp()),
        }
        self.dunewaves.append(node)
        return node

    def list_dunewaves(self, limit: int = 10) -> List[Dict[str, Any]]:
        return sorted(self.dunewaves, key=lambda item: item["timestamp"], reverse=True)[
            :limit
        ]

    def create_memory_fragment_node(
        self, memory_data: Dict[str, Any]
    ) -> Optional[Dict[str, Any]]:
        node = {
            **memory_data,
            "id": memory_data.get("id", f"mem_{len(self.memory_fragments) + 1}"),
            "timestamp": memory_data.get("timestamp", self._timestamp()),
            "metadata": memory_data.get("metadata", {}),
        }
        self.memory_fragments.append(node)
        return node

    def list_memory_fragments(self, limit: int = 10) -> List[Dict[str, Any]]:
        return sorted(
            self.memory_fragments, key=lambda item: item["timestamp"], reverse=True
        )[:limit]

    def link_dunewave_to_memory(self, wave_id: str, memory_id: str) -> bool:
        self.relationships.append(
            {"type": "CONTAINS", "from": wave_id, "to": memory_id}
        )
        return True

    def create_mentorship(self, younger_id: str, mentor_id: str) -> bool:
        self.relationships.append(
            {"type": "MENTORED_BY", "from": younger_id, "to": mentor_id}
        )
        return True

    def find_wisdom_fragments(
        self, emotion: Optional[str] = None, limit: int = 10
    ) -> List[Dict[str, Any]]:
        fragments: List[Dict[str, Any]] = []
        for wave in self.dunewaves:
            if emotion is not None and wave.get("emotion") != emotion:
                continue
            for fragment in self.memory_fragments:
                fragments.append({**fragment, "emotion": wave.get("emotion")})
        return fragments[:limit]


__all__ = ["MockNeo4jGateway", "MockNeo4jDateTime"]
