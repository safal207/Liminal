"""Production Neo4j gateway implementation backed by the official driver."""

from __future__ import annotations

import os
from contextlib import contextmanager
from threading import Lock
from time import perf_counter
from typing import Any, Dict, List, Optional

from neo4j import GraphDatabase
from neo4j.time import DateTime as Neo4jDateTime

from backend.metrics.collectors import (
    neo4j_active_sessions,
    neo4j_operation_duration_seconds,
    neo4j_operations_total,
    neo4j_saturation_ratio,
)

from . import Neo4jGateway


def _serialize_node(record: Dict[str, Any]) -> Dict[str, Any]:
    serialized: Dict[str, Any] = {}
    for key, value in record.items():
        if isinstance(value, Neo4jDateTime):
            serialized[key] = value.isoformat()
        else:
            serialized[key] = value
    return serialized


class Neo4jClient(Neo4jGateway):
    """Gateway that communicates with a Neo4j instance via the official driver."""

    def __init__(self, uri: str, user: str, password: str):
        self.driver = GraphDatabase.driver(uri, auth=(user, password))
        try:
            self._max_pool_size = int(os.getenv("NEO4J_MAX_POOL_SIZE", "100"))
        except ValueError:
            self._max_pool_size = 100
        self._active_sessions = 0
        self._lock = Lock()
        neo4j_active_sessions.set(0)
        self._update_saturation(0)

    def _update_saturation(self, active_sessions: int) -> None:
        if self._max_pool_size <= 0:
            neo4j_saturation_ratio.set(0)
            return

        ratio = min(active_sessions / self._max_pool_size, 1.0)
        neo4j_saturation_ratio.set(ratio)

    @contextmanager
    def _track_operation(self, operation: str):
        start_time = perf_counter()
        with self._lock:
            self._active_sessions += 1
            current_active = self._active_sessions
        neo4j_active_sessions.set(current_active)
        self._update_saturation(current_active)

        try:
            yield
        except Exception:
            neo4j_operations_total.labels(operation=operation, status="error").inc()
            raise
        else:
            neo4j_operations_total.labels(operation=operation, status="success").inc()
        finally:
            duration = perf_counter() - start_time
            neo4j_operation_duration_seconds.labels(operation=operation).observe(duration)
            with self._lock:
                self._active_sessions = max(self._active_sessions - 1, 0)
                current_active = self._active_sessions
            neo4j_active_sessions.set(current_active)
            self._update_saturation(current_active)

    def create_indexes(self) -> None:
        with self._track_operation("create_indexes"):
            with self.driver.session() as session:
                session.run("CREATE INDEX IF NOT EXISTS FOR (d:DuneWave) ON (d.id)")
                session.run("CREATE INDEX IF NOT EXISTS FOR (m:MemoryFragment) ON (m.id)")
                session.run("CREATE INDEX IF NOT EXISTS FOR (d:DuneWave) ON (d.emotion)")
                session.run("CREATE INDEX IF NOT EXISTS FOR (d:DuneWave) ON (d.timestamp)")
                session.run("CREATE INDEX IF NOT EXISTS FOR (m:MemoryFragment) ON (m.type)")
                session.run(
                    "CREATE CONSTRAINT IF NOT EXISTS FOR (d:DuneWave) REQUIRE d.id IS UNIQUE"
                )
                session.run(
                    "CREATE CONSTRAINT IF NOT EXISTS FOR (m:MemoryFragment) REQUIRE m.id IS UNIQUE"
                )

    def close(self) -> None:
        self.driver.close()

    def create_dunewave_node(self, wave_data: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        with self._track_operation("create_dunewave_node"):
            with self.driver.session() as session:
                query = """
                MERGE (dw:DuneWave {id: $id})
                SET dw.phase = $phase,
                    dw.emotion = $emotion,
                    dw.intensity = $intensity,
                    dw.context = $context,
                    dw.source = $source,
                    dw.timestamp = datetime($timestamp)
                RETURN dw
                """
                result = session.run(query, **wave_data)
                record = result.single()
                if record and "dw" in record:
                    return _serialize_node(dict(record["dw"]))
                return None

    def list_dunewaves(self, limit: int = 10) -> List[Dict[str, Any]]:
        with self._track_operation("list_dunewaves"):
            with self.driver.session() as session:
                result = session.run(
                    "MATCH (d:DuneWave) RETURN d ORDER BY d.timestamp DESC LIMIT $limit",
                    limit=limit,
                )
                waves: List[Dict[str, Any]] = []
                for record in result:
                    waves.append(_serialize_node(dict(record["d"])))
                return waves

    def create_memory_fragment_node(
        self, memory_data: Dict[str, Any]
    ) -> Optional[Dict[str, Any]]:
        with self.driver.session() as session:
            query = """
            MERGE (mf:MemoryFragment {id: $id})
            SET mf.content = $content,
                mf.type = $type,
                mf.growth_stage = $growth_stage,
                mf.timestamp = datetime($timestamp),
                mf.metadata = $metadata
            RETURN mf
            """
            result = session.run(query, **memory_data)
            record = result.single()
            if record and "mf" in record:
                return _serialize_node(dict(record["mf"]))
            return None

    def list_memory_fragments(self, limit: int = 10) -> List[Dict[str, Any]]:
        with self._track_operation("list_memory_fragments"):
            with self.driver.session() as session:
                result = session.run(
                    "MATCH (m:MemoryFragment) RETURN m ORDER BY m.timestamp DESC LIMIT $limit",
                    limit=limit,
                )
                fragments: List[Dict[str, Any]] = []
                for record in result:
                    fragments.append(_serialize_node(dict(record["m"])))
                return fragments

    def link_dunewave_to_memory(self, wave_id: str, memory_id: str) -> bool:
        with self._track_operation("link_dunewave_to_memory"):
            with self.driver.session() as session:
                query = """
                MATCH (dw:DuneWave {id: $wave_id})
                MATCH (mf:MemoryFragment {id: $memory_id})
                MERGE (dw)-[r:CONTAINS]->(mf)
                SET r.linked_at = datetime()
                RETURN r
                """
                result = session.run(query, wave_id=wave_id, memory_id=memory_id)
                return result.single() is not None

    def create_mentorship(self, younger_id: str, mentor_id: str) -> bool:
        with self._track_operation("create_mentorship"):
            with self.driver.session() as session:
                query = """
                MATCH (young:MemoryFragment {id: $younger_id})
                MATCH (mentor:MemoryFragment {id: $mentor_id})
                MERGE (young)-[r:MENTORED_BY]->(mentor)
                SET r.created_at = datetime()
                RETURN r
                """
                result = session.run(
                    query, younger_id=younger_id, mentor_id=mentor_id
                )
                return result.single() is not None

    def find_wisdom_fragments(
        self, emotion: Optional[str] = None, limit: int = 10
    ) -> List[Dict[str, Any]]:
        with self._track_operation("find_wisdom_fragments"):
            with self.driver.session() as session:
                query = """
                MATCH (d:DuneWave)
                WHERE $emotion IS NULL OR d.emotion = $emotion
                MATCH (d)-[:CONTAINS]->(m:MemoryFragment)
                RETURN m, d.emotion as emotion
                ORDER BY d.timestamp DESC
                LIMIT $limit
                """
                result = session.run(query, emotion=emotion, limit=limit)
                fragments: List[Dict[str, Any]] = []
                for record in result:
                    fragment = _serialize_node(dict(record["m"]))
                    fragment["emotion"] = record["emotion"]
                    fragments.append(fragment)
                return fragments
