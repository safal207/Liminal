"""
🌿✨ Emotime Neo4j Storage — хранение эмоциональных временных рядов

Интеграция Emotime с Neo4j для персистентного хранения:
- Эмоциональные точки и временные ряды
- Режимы и переходы между ними
- Паттерны и тренды
- Резонансные следы пользователей

"В графе души каждая эмоция связана с каждой"
"""

import json
import os
import uuid
import time
from datetime import datetime
from typing import Dict, List, Optional, Any, Tuple

try:
    from neo4j import GraphDatabase

    NEO4J_AVAILABLE = True
except ImportError:
    NEO4J_AVAILABLE = False

try:
    from .utils import safe_logger
except ImportError:
    # Fallback если utils не доступен
    class FallbackLogger:
        def warning(self, msg):
            print(f"WARNING: {msg}")

        def error(self, msg):
            print(f"ERROR: {msg}")

        def info(self, msg):
            print(f"INFO: {msg}")

    safe_logger = FallbackLogger()

from .timeseries import EmotionalPoint, TrendAnalysis
from .modes import EmotionalMode, ModeType
from .fusion import EmotionalFeatures


class EmotimeNeo4jStorage:
    """
    Система хранения Emotime в Neo4j.

    Схема данных:
    - (:User)-[:HAS_SESSION]->(:EmotimeSession)
    - (:EmotimeSession)-[:CONTAINS]->(:EmotionalPoint)
    - (:EmotionalPoint)-[:NEXT]->(:EmotionalPoint) // временная последовательность
    - (:EmotionalPoint)-[:IN_MODE]->(:EmotionalMode)
    - (:EmotionalMode)-[:TRANSITIONS_TO]->(:EmotionalMode)
    """

    def __init__(
        self,
        uri: str = None,
        user: str = None,
        password: str = None,
        database: str = "neo4j",
    ):
        if not NEO4J_AVAILABLE:
            safe_logger.warning("Neo4j storage disabled - driver not available")
            self.driver = None
            return

        # Security: No default credentials - fail fast if not provided
        self.uri = uri or os.getenv("NEO4J_URI")
        self.user = user or os.getenv("NEO4J_USER")
        self.password = password or os.getenv("NEO4J_PASSWORD")
        self.database = database

        # Validate required credentials
        if not self.uri:
            raise ValueError("NEO4J_URI environment variable must be set")
        if not self.user:
            raise ValueError("NEO4J_USER environment variable must be set")
        if not self.password:
            raise ValueError("NEO4J_PASSWORD environment variable must be set")
        if (
            self.password == "password"
            or self.password == "admin"
            or len(self.password) < 8
        ):
            raise ValueError(
                "NEO4J_PASSWORD must be secure (min 8 chars, not default values)"
            )

        try:
            self.driver = GraphDatabase.driver(
                self.uri,
                auth=(self.user, self.password),
                max_connection_lifetime=30 * 60,  # 30 минут
                max_connection_pool_size=50,
                connection_acquisition_timeout=60.0,
            )
            safe_logger.info("Neo4j storage initialized")
            self._initialize_schema()
        except Exception as e:
            safe_logger.error(f"Neo4j connection failed: {e}")
            self.driver = None

    def _safe_execute(self, query: str, parameters: Dict = None, max_retries: int = 3):
        """Безопасное выполнение запроса с retry логикой."""
        if not self.driver:
            return None

        for attempt in range(max_retries):
            try:
                with self.driver.session(database=self.database) as session:
                    result = session.run(query, parameters or {})
                    return result
            except Exception as e:
                safe_logger.warning(f"Query attempt {attempt + 1} failed: {e}")
                if attempt < max_retries - 1:
                    time.sleep(0.5 * (attempt + 1))  # Exponential backoff
                else:
                    safe_logger.error(f"All query attempts failed: {e}")
                    return None
        return None

    def _initialize_schema(self):
        """Инициализирует схему базы данных."""
        if not self.driver:
            return

        schema_queries = [
            # Упрощенные индексы без сложных символов
            "CREATE INDEX emotime_user_id_idx IF NOT EXISTS FOR (u:User) ON (u.user_id)",
            "CREATE INDEX emotime_session_id_idx IF NOT EXISTS FOR (s:EmotimeSession) ON (s.session_id)",
            "CREATE INDEX emotime_point_ts_idx IF NOT EXISTS FOR (p:EmotionalPoint) ON (p.timestamp)",
            "CREATE INDEX emotime_mode_type_idx IF NOT EXISTS FOR (m:EmotionalMode) ON (m.type)",
        ]

        for query in schema_queries:
            try:
                result = self._safe_execute(query)
                if result is None:
                    safe_logger.warning(f"Schema query may have failed: {query}")
                else:
                    safe_logger.info(f"Schema query executed: {query}")
            except Exception as e:
                if "already exists" not in str(e).lower():
                    safe_logger.warning(f"Schema query failed: {query}, error: {e}")

    async def store_emotional_point(
        self,
        user_id: str,
        session_id: str,
        point: EmotionalPoint,
        mode: EmotionalMode,
        previous_point_id: Optional[str] = None,
    ) -> str:
        """
        Сохраняет эмоциональную точку в Neo4j.

        Returns:
            ID созданной точки
        """
        if not self.driver:
            return str(uuid.uuid4())  # Фиктивный ID

        point_id = str(uuid.uuid4())

        # Упрощенный запрос без сложных символов
        query = """
        MERGE (u:User {user_id: $user_id})
        MERGE (s:EmotimeSession {session_id: $session_id})
        MERGE (u)-[:HAS_SESSION]->(s)
        
        CREATE (p:EmotionalPoint {
            point_id: $point_id,
            timestamp: $timestamp,
            valence: $valence,
            arousal: $arousal,
            dominance: $dominance,
            tempo: $tempo,
            intensity: $intensity,
            trend: $trend,
            is_peak: $is_peak,
            confidence: $confidence
        })
        
        MERGE (s)-[:CONTAINS]->(p)
        
        MERGE (m:EmotionalMode {
            type: $mode_type,
            name: $mode_name
        })
        ON CREATE SET 
            m.description = $mode_description
        
        MERGE (p)-[:IN_MODE {
            intensity: $mode_intensity,
            confidence: $mode_confidence,
            duration: $mode_duration
        }]->(m)
        
        RETURN p.point_id as point_id
        """

        params = {
            "user_id": str(user_id),
            "session_id": str(session_id),
            "point_id": point_id,
            "timestamp": point.timestamp.isoformat(),
            "valence": float(point.valence),
            "arousal": float(point.arousal),
            "dominance": float(point.dominance),
            "tempo": float(point.tempo),
            "intensity": float(point.intensity),
            "trend": str(point.trend) if point.trend else "stable",
            "is_peak": bool(point.is_peak),
            "confidence": float(point.confidence),
            "mode_type": str(mode.type.value),
            "mode_name": str(mode.name),
            "mode_description": str(mode.description),
            "mode_intensity": float(mode.intensity),
            "mode_confidence": float(mode.confidence),
            "mode_duration": int(mode.duration),
        }

        result = self._safe_execute(query, params)
        if result:
            try:
                record = result.single()

                # Создаем временную связь с предыдущей точкой
                if previous_point_id:
                    await self._link_temporal_sequence(previous_point_id, point_id)

                return record["point_id"] if record else point_id

            except Exception as e:
                safe_logger.error(f"Failed to process Neo4j result: {e}")
                return point_id
        else:
            safe_logger.warning("Failed to store emotional point - using fallback ID")
            return point_id

    async def _link_temporal_sequence(self, prev_point_id: str, curr_point_id: str):
        """Связывает точки в временную последовательность."""
        if not self.driver:
            return

        query = """
        MATCH (prev:EmotionalPoint {point_id: $prev_point_id})
        MATCH (curr:EmotionalPoint {point_id: $curr_point_id})
        MERGE (prev)-[:NEXT]->(curr)
        """

        result = self._safe_execute(
            query, {"prev_point_id": prev_point_id, "curr_point_id": curr_point_id}
        )
        if not result:
            safe_logger.warning("Failed to link temporal sequence")

    async def store_mode_transition(
        self,
        user_id: str,
        from_mode: ModeType,
        to_mode: ModeType,
        transition_time: datetime,
        duration_seconds: float,
    ):
        """Сохраняет переход между эмоциональными режимами."""
        if not self.driver:
            return

        query = """
        MATCH (from_mode:EmotionalMode {type: $from_mode_type})
        MATCH (to_mode:EmotionalMode {type: $to_mode_type})
        
        MERGE (from_mode)-[t:TRANSITIONS_TO]->(to_mode)
        ON CREATE SET 
            t.first_transition = $transition_time,
            t.count = 1,
            t.total_duration = $duration_seconds,
            t.avg_duration = $duration_seconds
        ON MATCH SET
            t.count = t.count + 1,
            t.total_duration = t.total_duration + $duration_seconds,
            t.avg_duration = t.total_duration / t.count,
            t.last_transition = $transition_time
        
        // Также создаем связь для пользователя
        MERGE (u:User {user_id: $user_id})
        MERGE (u)-[ut:USER_TRANSITION {
            from_mode: $from_mode_type,
            to_mode: $to_mode_type,
            timestamp: $transition_time,
            duration: $duration_seconds
        }]->(to_mode)
        """

        result = self._safe_execute(
            query,
            {
                "user_id": str(user_id),
                "from_mode_type": str(from_mode.value),
                "to_mode_type": str(to_mode.value),
                "transition_time": transition_time.isoformat(),
                "duration_seconds": float(duration_seconds),
            },
        )
        if not result:
            safe_logger.warning("Failed to store mode transition")

    async def get_emotional_timeline(
        self, user_id: str, session_id: Optional[str] = None, limit: int = 100
    ) -> List[Dict[str, Any]]:
        """Получает временную линию эмоций пользователя."""
        if not self.driver:
            return []

        # Запрос зависит от того, указана ли сессия
        if session_id:
            query = """
            MATCH (u:User {user_id: $user_id})-[:HAS_SESSION]->(s:EmotimeSession {session_id: $session_id})
            MATCH (s)-[:CONTAINS]->(p:EmotionalPoint)
            MATCH (p)-[:IN_MODE]->(m:EmotionalMode)
            RETURN p, m
            ORDER BY p.timestamp DESC
            LIMIT $limit
            """
            params = {"user_id": user_id, "session_id": session_id, "limit": limit}
        else:
            query = """
            MATCH (u:User {user_id: $user_id})-[:HAS_SESSION]->(s:EmotimeSession)
            MATCH (s)-[:CONTAINS]->(p:EmotionalPoint)
            MATCH (p)-[:IN_MODE]->(m:EmotionalMode)
            RETURN p, m, s.session_id as session_id
            ORDER BY p.timestamp DESC  
            LIMIT $limit
            """
            params = {"user_id": user_id, "limit": limit}

        result = self._safe_execute(query, params)
        if not result:
            safe_logger.warning("Failed to get emotional timeline")
            return []

        try:
            timeline = []
            for record in result:
                point = dict(record["p"])
                mode = dict(record["m"])

                timeline_item = {
                    "point": point,
                    "mode": mode,
                    "session_id": (
                        record.get("session_id") if not session_id else session_id
                    ),
                }
                timeline.append(timeline_item)

            return timeline
        except Exception as e:
            safe_logger.error(f"Failed to process timeline results: {e}")
            return []

    async def get_emotional_patterns(
        self, user_id: str, days_back: int = 7
    ) -> Dict[str, Any]:
        """Анализирует эмоциональные паттерны пользователя."""
        if not self.driver:
            return {"status": "storage_unavailable"}

        # Дата для фильтрации
        since_date = datetime.now().replace(
            hour=0, minute=0, second=0, microsecond=0
        ) - datetime.timedelta(days=days_back)

        query = """
        MATCH (u:User {user_id: $user_id})-[:HAS_SESSION]->(s:EmotimeSession)
        MATCH (s)-[:CONTAINS]->(p:EmotionalPoint)
        WHERE datetime(p.timestamp) >= datetime($since_date)
        MATCH (p)-[:IN_MODE]->(m:EmotionalMode)
        
        RETURN 
            collect({
                timestamp: p.timestamp,
                valence: p.valence,
                arousal: p.arousal,
                mode: m.type,
                is_peak: p.is_peak
            }) as points,
            
            // Статистика по режимам
            m.type as mode_type,
            count(p) as mode_count,
            avg(p.valence) as avg_valence,
            avg(p.arousal) as avg_arousal,
            sum(CASE WHEN p.is_peak THEN 1 ELSE 0 END) as peak_count
            
        ORDER BY mode_count DESC
        """

        result = self._safe_execute(
            query, {"user_id": str(user_id), "since_date": since_date.isoformat()}
        )

        if not result:
            safe_logger.warning("Failed to get emotional patterns")
            return {"status": "query_failed"}

        try:
            mode_stats = []
            all_points = []

            for record in result:
                mode_stats.append(
                    {
                        "mode_type": record["mode_type"],
                        "count": record["mode_count"],
                        "avg_valence": record["avg_valence"],
                        "avg_arousal": record["avg_arousal"],
                        "peak_count": record["peak_count"],
                    }
                )
                all_points.extend(record["points"])

            # Анализируем переходы
            transitions = await self._analyze_mode_transitions(user_id, since_date)

            return {
                "user_id": user_id,
                "period_days": days_back,
                "total_points": len(all_points),
                "mode_statistics": mode_stats,
                "mode_transitions": transitions,
                "emotional_baseline": self._calculate_baseline(all_points),
            }

        except Exception as e:
            safe_logger.error(f"Failed to process patterns results: {e}")
            return {"status": "processing_failed", "error": str(e)}

    async def _analyze_mode_transitions(
        self, user_id: str, since_date: datetime
    ) -> List[Dict]:
        """Анализирует переходы между режимами."""
        if not self.driver:
            return []

        query = """
        MATCH (u:User {user_id: $user_id})-[ut:USER_TRANSITION]->(m:EmotionalMode)
        WHERE datetime(ut.timestamp) >= datetime($since_date)
        RETURN 
            ut.from_mode as from_mode,
            ut.to_mode as to_mode,
            count(*) as transition_count,
            avg(ut.duration) as avg_duration
        ORDER BY transition_count DESC
        LIMIT 10
        """

        result = self._safe_execute(
            query, {"user_id": str(user_id), "since_date": since_date.isoformat()}
        )

        if not result:
            safe_logger.warning("Failed to analyze transitions")
            return []

        try:
            return [
                {
                    "from_mode": record["from_mode"],
                    "to_mode": record["to_mode"],
                    "count": record["transition_count"],
                    "avg_duration": record["avg_duration"],
                }
                for record in result
            ]
        except Exception as e:
            safe_logger.error(f"Failed to process transitions: {e}")
            return []

    def _calculate_baseline(self, points: List[Dict]) -> Dict[str, float]:
        """Вычисляет эмоциональный базис."""
        if not points:
            return {"valence": 0.0, "arousal": 0.5}

        total_valence = sum(p["valence"] for p in points)
        total_arousal = sum(p["arousal"] for p in points)
        count = len(points)

        return {
            "valence": total_valence / count,
            "arousal": total_arousal / count,
            "peak_frequency": sum(1 for p in points if p["is_peak"]) / count,
        }

    def close(self):
        """Закрывает соединение с Neo4j."""
        if self.driver:
            self.driver.close()
            safe_logger.info("Neo4j storage closed")
