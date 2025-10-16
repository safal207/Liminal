"""Neo4j integration helpers used by the FastAPI application."""
from __future__ import annotations

import os
from datetime import datetime
from typing import Any, Dict, List


class Neo4jService:
    """Wrapper around the Neo4j writer implementation with graceful fallbacks."""

    def __init__(self, writer: Any | None = None) -> None:
        self._writer = writer
        self._use_mock = writer is not None

    def _create_writer(self):
        if os.environ.get("TESTING"):
            self._use_mock = True
            from backend.neo4j_writer import Neo4jWriter  # type: ignore

            return Neo4jWriter()

        try:
            from neo4j_writer import Neo4jWriter  # type: ignore

            uri = os.getenv("NEO4J_URI", "bolt://localhost:7687")
            user = os.getenv("NEO4J_USER", "neo4j")
            password = os.getenv("NEO4J_PASSWORD", "password")
            return Neo4jWriter(uri=uri, user=user, password=password)
        except Exception:
            self._use_mock = True
            from backend.neo4j_writer import Neo4jWriter  # type: ignore

            return Neo4jWriter()

    def get_writer(self):
        """Return the underlying writer instance."""
        if self._writer is None:
            self._writer = self._create_writer()
        return self._writer

    # Utility helpers -------------------------------------------------
    def create_wave(self, wave_data: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        payload = {**wave_data}
        payload.setdefault("id", f"wave_{int(datetime.utcnow().timestamp())}")
        payload.setdefault("timestamp", datetime.utcnow().isoformat())

        writer = self.get_writer()
        if hasattr(writer, "create_dunewave_node"):
            return writer.create_dunewave_node(payload)
        raise AttributeError("Configured writer does not support dune wave creation")

    def create_fragment(self, fragment_data: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        payload = {**fragment_data}
        payload.setdefault("id", f"mem_{int(datetime.utcnow().timestamp())}")
        payload.setdefault("timestamp", datetime.utcnow().isoformat())

        writer = self.get_writer()
        if hasattr(writer, "create_memory_fragment_node"):
            return writer.create_memory_fragment_node(payload)
        raise AttributeError("Configured writer does not support memory fragments")

    def link_wave_to_memory(self, wave_id: str, memory_id: str) -> bool:
        writer = self.get_writer()
        if hasattr(writer, "link_dunewave_to_memory"):
            return bool(writer.link_dunewave_to_memory(wave_id, memory_id))
        raise AttributeError("Configured writer does not support linking waves to memories")

    def create_mentorship(self, younger_id: str, mentor_id: str) -> bool:
        writer = self.get_writer()
        if hasattr(writer, "create_mentorship"):
            return bool(writer.create_mentorship(younger_id, mentor_id))
        raise AttributeError("Configured writer does not support mentorship relationships")

    def list_waves(self, limit: int = 10) -> List[Dict[str, Any]]:
        writer = self.get_writer()
        if hasattr(writer, "driver"):
            try:
                with writer.driver.session() as session:  # type: ignore[attr-defined]
                    result = session.run(
                        "MATCH (d:DuneWave) RETURN d ORDER BY d.timestamp DESC LIMIT $limit",
                        limit=limit,
                    )
                    waves: List[Dict[str, Any]] = []
                    for record in result:
                        data = dict(record["d"])
                        for key, value in data.items():
                            if hasattr(value, "isoformat"):
                                data[key] = value.isoformat()
                        waves.append(data)
                    return waves
            except Exception:
                # fall back to mock behaviour
                pass
        if hasattr(writer, "list_dunewaves"):
            return writer.list_dunewaves(limit)  # type: ignore[attr-defined]
        if hasattr(writer, "get_waves"):
            return writer.get_waves(limit)  # type: ignore[attr-defined]
        return []

    def list_fragments(self, limit: int = 10) -> List[Dict[str, Any]]:
        writer = self.get_writer()
        if hasattr(writer, "driver"):
            try:
                with writer.driver.session() as session:  # type: ignore[attr-defined]
                    result = session.run(
                        "MATCH (m:MemoryFragment) RETURN m ORDER BY m.timestamp DESC LIMIT $limit",
                        limit=limit,
                    )
                    fragments: List[Dict[str, Any]] = []
                    for record in result:
                        data = dict(record["m"])
                        for key, value in data.items():
                            if hasattr(value, "isoformat"):
                                data[key] = value.isoformat()
                        fragments.append(data)
                    return fragments
            except Exception:
                pass
        if hasattr(writer, "list_memory_fragments"):
            return writer.list_memory_fragments(limit)  # type: ignore[attr-defined]
        if hasattr(writer, "get_memories"):
            return writer.get_memories(limit)  # type: ignore[attr-defined]
        return []
