"""Neo4j gateway abstractions and implementations."""

from __future__ import annotations

from functools import lru_cache
from typing import Any, Dict, List, Optional, Protocol

from backend.core.settings import get_settings


class Neo4jGateway(Protocol):
    """Protocol describing the operations required from a Neo4j gateway."""

    def create_indexes(self) -> None: ...

    def close(self) -> None: ...

    def create_dunewave_node(self, wave_data: Dict[str, Any]) -> Optional[Dict[str, Any]]: ...

    def list_dunewaves(self, limit: int = 10) -> List[Dict[str, Any]]: ...

    def create_memory_fragment_node(
        self, memory_data: Dict[str, Any]
    ) -> Optional[Dict[str, Any]]: ...

    def list_memory_fragments(self, limit: int = 10) -> List[Dict[str, Any]]: ...

    def link_dunewave_to_memory(self, wave_id: str, memory_id: str) -> bool: ...

    def create_mentorship(self, younger_id: str, mentor_id: str) -> bool: ...

    def find_wisdom_fragments(
        self, emotion: Optional[str] = None, limit: int = 10
    ) -> List[Dict[str, Any]]: ...


def _build_default_gateway() -> Neo4jGateway:
    from .client import Neo4jClient
    from .mock import MockNeo4jGateway

    integrations = get_settings().integrations
    uri = integrations.neo4j_uri
    user = integrations.neo4j_user
    password = integrations.neo4j_password

    if os.getenv("NEO4J_USE_MOCK", "false").lower() == "true" or os.getenv("TESTING"):
        return MockNeo4jGateway()

    try:
        return Neo4jClient(uri=uri, user=user, password=password)
    except Exception:
        # Fall back to an in-memory mock when a connection cannot be established
        return MockNeo4jGateway()


@lru_cache()
def get_default_gateway() -> Neo4jGateway:
    """Return a cached instance of the default Neo4j gateway."""

    return _build_default_gateway()


__all__ = [
    "Neo4jGateway",
    "get_default_gateway",
]
