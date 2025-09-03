#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Philosophy Neo4j Writer
-----------------------
Neo4j интеграция для Philosophy First подхода
"""

import json
import os
import uuid
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional, Tuple

try:
    from neo4j import GraphDatabase
    from neo4j.exceptions import ServiceUnavailable
except ImportError:
    print("Neo4j driver не установлен. Установите: pip install neo4j")
    raise

from philosophy_schema import (ConsciousnessNode, ConsciousnessState,
                               StateTransition, TransitionTrigger)


class PhilosophyNeo4jWriter:
    """
    Класс для работы с Neo4j и философскими состояниями сознания.
    Реализует Philosophy First подход с акцентом на Home State, Resonance,
    Question Space и другие философские концепции.
    """

    def __init__(
        self,
        uri: str = None,
        user: str = None,
        password: str = None,
        database: str = "neo4j",
    ):
        """
        Инициализирует подключение к Neo4j с параметрами для Docker.

        Args:
            uri: URI для подключения к Neo4j (по умолчанию берется из NEO4J_URI)
            user: Имя пользователя Neo4j (по умолчанию берется из NEO4J_USER)
            password: Пароль Neo4j (по умолчанию берется из NEO4J_PASSWORD)
            database: Имя базы данных Neo4j
        """
        self.uri = uri or os.environ.get("NEO4J_URI", "bolt://localhost:7687")
        self.user = user or os.environ.get("NEO4J_USER", "neo4j")
        self.password = password or os.environ.get(
            "NEO4J_PASSWORD", "NewStrongPass123!"
        )
        self.database = database

        self.driver = GraphDatabase.driver(self.uri, auth=(self.user, self.password))
        self._init_database()

    def _init_database(self):
        """Инициализирует базу данных, создавая необходимые индексы."""
        with self.driver.session(database=self.database) as session:
            # Создаем индексы для быстрого поиска
            session.run(
                "CREATE INDEX IF NOT EXISTS FOR (n:ConsciousnessNode) ON (n.id)"
            )
            session.run(
                "CREATE INDEX IF NOT EXISTS FOR (n:ConsciousnessNode) ON (n.state)"
            )
            session.run(
                "CREATE INDEX IF NOT EXISTS FOR (n:ConsciousnessNode) ON (n.user_id)"
            )

    def close(self):
        """Закрывает соединение с Neo4j."""
        if hasattr(self, "driver") and self.driver:
            self.driver.close()

    def create_consciousness_node(self, node: ConsciousnessNode) -> str:
        """
        Создает узел сознания в Neo4j.

        Args:
            node: Объект ConsciousnessNode для сохранения

        Returns:
            ID созданного узла
        """
        node_dict = node.to_dict()

        # Преобразуем мета-данные в JSON
        if "meta" in node_dict and node_dict["meta"]:
            node_dict["meta"] = json.dumps(node_dict["meta"])

        query = """
        CREATE (n:ConsciousnessNode {
            id: $id,
            state: $state,
            timestamp: datetime($timestamp),
            presence_level: $presence_level,
            harmony_index: $harmony_index,
            authenticity_score: $authenticity_score,
            home_resonance: $home_resonance,
            question_clarity: $question_clarity,
            resonance_strength: $resonance_strength,
            user_id: $user_id,
            label: $label
        })
        RETURN n.id as id
        """

        with self.driver.session(database=self.database) as session:
            result = session.run(query, **node_dict)
            return result.single()["id"]

    def create_state_transition(self, transition: StateTransition) -> str:
        """
        Создает переход между состояниями сознания в Neo4j.

        Args:
            transition: Объект StateTransition для сохранения

        Returns:
            ID созданного перехода
        """
        transition_dict = transition.to_dict()

        # Преобразуем мета-данные в JSON
        if "meta" in transition_dict and transition_dict["meta"]:
            transition_dict["meta"] = json.dumps(transition_dict["meta"])

        query = """
        MATCH (source:ConsciousnessNode {id: $source_id})
        MATCH (target:ConsciousnessNode {id: $target_id})
        CREATE (source)-[t:TRANSITIONS_TO {
            id: $id,
            trigger: $trigger,
            timestamp: datetime($timestamp),
            presence_delta: $presence_delta,
            harmony_delta: $harmony_delta,
            authenticity_delta: $authenticity_delta,
            resonance_delta: $resonance_delta,
            philosophical_significance: $philosophical_significance,
            user_id: $user_id
        }]->(target)
        RETURN t.id as id
        """

        with self.driver.session(database=self.database) as session:
            result = session.run(query, **transition_dict)
            return result.single()["id"]

    def get_consciousness_timeline(
        self, hours: int = 24, user_id: str = None
    ) -> Dict[str, Any]:
        """
        Получает временную линию сознания для анализа и визуализации.

        Args:
            hours: Количество часов для анализа
            user_id: ID пользователя для фильтрации (опционально)

        Returns:
            Словарь с узлами и связями для визуализации
        """
        # Базовый запрос для узлов
        node_query = """
        MATCH (n:ConsciousnessNode)
        WHERE n.timestamp >= datetime() - duration({hours: $hours})
        """

        # Добавляем фильтр по пользователю, если указан
        if user_id:
            node_query += " AND n.user_id = $user_id"

        node_query += """
        RETURN n.id as id, n.state as state, n.label as label,
               n.presence_level as presenceLevel, n.harmony_index as harmonyIndex,
               n.authenticity_score as authenticityScore, n.home_resonance as homeResonance,
               n.question_clarity as questionClarity, n.resonance_strength as resonanceStrength,
               n.timestamp as timestamp, n.user_id as userId
        """

        # Запрос для связей
        link_query = """
        MATCH (source:ConsciousnessNode)-[t:TRANSITIONS_TO]->(target:ConsciousnessNode)
        WHERE t.timestamp >= datetime() - duration({hours: $hours})
        """

        # Добавляем фильтр по пользователю, если указан
        if user_id:
            link_query += " AND t.user_id = $user_id"

        link_query += """
        RETURN t.id as id, source.id as source, target.id as target,
               t.trigger as trigger, t.timestamp as timestamp,
               t.philosophical_significance as philosophical_significance,
               t.presence_delta as presence_delta, t.harmony_delta as harmony_delta,
               t.authenticity_delta as authenticity_delta, t.resonance_delta as resonance_delta,
               t.user_id as userId
        """

        params = {"hours": hours, "user_id": user_id} if user_id else {"hours": hours}

        with self.driver.session(database=self.database) as session:
            nodes = list(session.run(node_query, **params))
            links = list(session.run(link_query, **params))

            # Преобразуем результат в формат для фронтенда
            nodes_list = []
            for record in nodes:
                nodes_list.append(
                    {
                        "id": record["id"],
                        "state": record["state"],
                        "label": record["label"],
                        "presenceLevel": record["presenceLevel"],
                        "harmonyIndex": record["harmonyIndex"],
                        "authenticityScore": record["authenticityScore"],
                        "homeResonance": record["homeResonance"],
                        "questionClarity": record["questionClarity"],
                        "resonanceStrength": record["resonanceStrength"],
                        "timestamp": (
                            record["timestamp"].isoformat()
                            if record["timestamp"]
                            else None
                        ),
                        "userId": record["userId"],
                    }
                )

            links_list = []
            for record in links:
                links_list.append(
                    {
                        "id": record["id"],
                        "source": record["source"],
                        "target": record["target"],
                        "trigger": record["trigger"],
                        "timestamp": (
                            record["timestamp"].isoformat()
                            if record["timestamp"]
                            else None
                        ),
                        "philosophical_significance": record[
                            "philosophical_significance"
                        ],
                        "presence_delta": record["presence_delta"],
                        "harmony_delta": record["harmony_delta"],
                        "authenticity_delta": record["authenticity_delta"],
                        "resonance_delta": record["resonance_delta"],
                        "userId": record["userId"],
                    }
                )

            return {"nodes": nodes_list, "links": links_list}

    def analyze_temporal_patterns(self, days: int = 7) -> List[Dict[str, Any]]:
        """
        Анализирует временные паттерны в переходах сознания.
        Не использует APOC для совместимости с базовой Neo4j.

        Args:
            days: Количество дней для анализа

        Returns:
            Список обнаруженных паттернов
        """
        query = """
        MATCH (source:ConsciousnessNode)-[t:TRANSITIONS_TO]->(target:ConsciousnessNode)
        WHERE t.timestamp >= datetime() - duration({days: $days})
        WITH source.state as source_state, target.state as target_state,
             count(*) as frequency,
             min(t.timestamp) as first_occurrence,
             max(t.timestamp) as last_occurrence,
             collect(t.philosophical_significance) as significances,
             collect(t.timestamp) as timestamps
        WHERE frequency > 1
        RETURN 
            source_state as source, 
            target_state as target, 
            frequency,
            first_occurrence,
            last_occurrence,
            significances[0] as philosophical_significance,
            CASE 
                WHEN frequency >= 5 THEN 'Frequent Pattern'
                WHEN frequency >= 3 THEN 'Emerging Pattern'
                ELSE 'Occasional Pattern'
            END as pattern_type
        ORDER BY frequency DESC
        """

        with self.driver.session(database=self.database) as session:
            result = session.run(query, days=days)
            patterns = []

            for record in result:
                pattern = {
                    "source": record["source"],
                    "target": record["target"],
                    "frequency": record["frequency"],
                    "first_occurrence": (
                        record["first_occurrence"].isoformat()
                        if record["first_occurrence"]
                        else None
                    ),
                    "last_occurrence": (
                        record["last_occurrence"].isoformat()
                        if record["last_occurrence"]
                        else None
                    ),
                    "philosophical_significance": record["philosophical_significance"],
                    "pattern_type": record["pattern_type"],
                }
                patterns.append(pattern)

            return patterns

    def find_resonance_moments(
        self, hours: int = 24, max_time_diff_seconds: int = 300
    ) -> List[Dict[str, Any]]:
        """
        Находит моменты резонанса между пользователями, когда они
        переходили в одинаковое состояние сознания примерно в одно время.

        Args:
            hours: Количество часов для анализа
            max_time_diff_seconds: Максимальная разница во времени (в секундах)

        Returns:
            Список моментов резонанса
        """
        query = """
        MATCH (n1:ConsciousnessNode)-[t1:TRANSITIONS_TO]->(n2:ConsciousnessNode),
              (n3:ConsciousnessNode)-[t2:TRANSITIONS_TO]->(n4:ConsciousnessNode)
        WHERE n2.state = n4.state
        AND t1.user_id <> t2.user_id
        AND n2.timestamp >= datetime() - duration({hours: $hours})
        AND n4.timestamp >= datetime() - duration({hours: $hours})
        AND abs(duration.between(n2.timestamp, n4.timestamp).seconds) <= $max_seconds
        WITH n2.state as shared_state, t1.user_id as user1, t2.user_id as user2,
             n2.timestamp as timestamp1, n4.timestamp as timestamp2,
             abs(duration.between(n2.timestamp, n4.timestamp).seconds) as time_diff
        RETURN 
            shared_state as to_state,
            user1,
            user2,
            timestamp1,
            timestamp2,
            time_diff as time_diff_seconds,
            CASE
                WHEN time_diff <= 60 THEN 'Strong Resonance'
                WHEN time_diff <= 180 THEN 'Moderate Resonance'
                ELSE 'Weak Resonance'
            END as resonance_type,
            CASE shared_state
                WHEN 'HOME_AUTHENTIC' THEN 'Resonance in authentic home state'
                WHEN 'PRESENCE_NOW' THEN 'Synchronous presence in the moment'
                WHEN 'HARMONY_BALANCE' THEN 'Harmonious balance alignment'
                WHEN 'QUESTION_SPACE' THEN 'Shared question space exploration'
                WHEN 'RESONANCE_MOMENT' THEN 'Meta-resonance amplification'
                ELSE 'Consciousness state alignment'
            END as philosophical_meaning
        ORDER BY time_diff
        """

        with self.driver.session(database=self.database) as session:
            result = session.run(query, hours=hours, max_seconds=max_time_diff_seconds)
            moments = []

            for record in result:
                moment = {
                    "to_state": record["to_state"],
                    "user1": record["user1"],
                    "user2": record["user2"],
                    "timestamp1": (
                        record["timestamp1"].isoformat()
                        if record["timestamp1"]
                        else None
                    ),
                    "timestamp2": (
                        record["timestamp2"].isoformat()
                        if record["timestamp2"]
                        else None
                    ),
                    "time_diff_seconds": record["time_diff_seconds"],
                    "resonance_type": record["resonance_type"],
                    "philosophical_meaning": record["philosophical_meaning"],
                }
                moments.append(moment)

            return moments

    def get_transition_details(self, transition_id: str) -> Optional[Dict[str, Any]]:
        """
        Получает подробности о переходе для отправки через WebSocket.

        Args:
            transition_id: ID перехода

        Returns:
            Словарь с деталями перехода или None, если переход не найден
        """
        query = """
        MATCH (source:ConsciousnessNode)-[t:TRANSITIONS_TO {id: $id}]->(target:ConsciousnessNode)
        RETURN 
            source.state as source_state,
            target.state as target_state,
            t.trigger as trigger,
            t.presence_delta as presence_delta,
            t.harmony_delta as harmony_delta,
            t.authenticity_delta as authenticity_delta,
            t.resonance_delta as resonance_delta,
            t.philosophical_significance as philosophical_significance,
            t.user_id as user_id
        """

        with self.driver.session(database=self.database) as session:
            result = session.run(query, id=transition_id)
            record = result.single()

            if not record:
                return None

            return {
                "source_state": record["source_state"],
                "target_state": record["target_state"],
                "trigger": record["trigger"],
                "presence_delta": record["presence_delta"],
                "harmony_delta": record["harmony_delta"],
                "authenticity_delta": record["authenticity_delta"],
                "resonance_delta": record["resonance_delta"],
                "philosophical_significance": record["philosophical_significance"],
                "user_id": record["user_id"],
            }

    def get_last_transition(self) -> Optional[Dict[str, Any]]:
        """
        Получает последний созданный переход для демонстрации.

        Returns:
            Словарь с деталями перехода или None, если переходов нет
        """
        query = """
        MATCH (source:ConsciousnessNode)-[t:TRANSITIONS_TO]->(target:ConsciousnessNode)
        RETURN t.id as id, source.state as source_state, target.state as target_state
        ORDER BY t.timestamp DESC
        LIMIT 1
        """

        with self.driver.session(database=self.database) as session:
            result = session.run(query)
            record = result.single()

            if not record:
                return None

            return {
                "id": record["id"],
                "source_state": record["source_state"],
                "target_state": record["target_state"],
            }
