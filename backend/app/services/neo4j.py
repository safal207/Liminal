"""Neo4j integration helpers used by the FastAPI application."""
from __future__ import annotations

from typing import Any, Dict, List, Optional

from backend.infrastructure.neo4j import Neo4jGateway, get_default_gateway


class Neo4jService:
    """Wrapper around the Neo4j gateway implementation."""

    def __init__(self, gateway: Optional[Neo4jGateway] = None) -> None:
        self._gateway = gateway or get_default_gateway()
        self._indexes_initialised = False

    @property
    def gateway(self) -> Neo4jGateway:
        return self._gateway

    def ensure_indexes(self) -> None:
        if not self._indexes_initialised:
            self._gateway.create_indexes()
            self._indexes_initialised = True

    # Utility helpers -------------------------------------------------
    def create_wave(self, wave_data: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        self.ensure_indexes()
        return self._gateway.create_dunewave_node(wave_data)

    def create_fragment(self, fragment_data: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        self.ensure_indexes()
        return self._gateway.create_memory_fragment_node(fragment_data)

    def link_wave_to_memory(self, wave_id: str, memory_id: str) -> bool:
        self.ensure_indexes()
        return self._gateway.link_dunewave_to_memory(wave_id, memory_id)

    def create_mentorship(self, younger_id: str, mentor_id: str) -> bool:
        self.ensure_indexes()
        return self._gateway.create_mentorship(younger_id, mentor_id)

    def list_waves(self, limit: int = 10) -> List[Dict[str, Any]]:
        self.ensure_indexes()
        return self._gateway.list_dunewaves(limit)

    def list_fragments(self, limit: int = 10) -> List[Dict[str, Any]]:
        self.ensure_indexes()
        return self._gateway.list_memory_fragments(limit)
