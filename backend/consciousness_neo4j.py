"""
🌌 Neo4j Writer for Consciousness Temporal Data Lake
Революционная интеграция состояний сознания с Neo4j
"""

import json
import os
import uuid
from datetime import datetime
from typing import Any, Dict, List, Optional

import requests
from neo4j import GraphDatabase

from consciousness_schema import (
    PHILOSOPHICAL_THRESHOLDS,
    ConsciousnessNode,
    ConsciousnessQueries,
    ConsciousnessState,
    StateTransition,
    TransitionTrigger,
)


class ConsciousnessNeo4jWriter:
    """Writer для состояний сознания в Neo4j"""

    def __init__(self, uri: str = None, user: str = None, password: str = None):
        self.uri = uri or os.getenv("NEO4J_URI", "bolt://localhost:7687")
        self.user = user or os.getenv("NEO4J_USER", "neo4j")
        self.password = password or os.getenv("NEO4J_PASSWORD", "password")
        self.driver = GraphDatabase.driver(self.uri, auth=(self.user, self.password))
        self.queries = ConsciousnessQueries()

        # URL для отправки событий на WebSocket relay
        self.ws_relay_url = os.getenv("WS_RELAY_URL", "http://localhost:8080/events")

    def close(self):
        """Закрытие соединения Neo4j"""
        if self.driver:
            self.driver.close()

    def create_consciousness_state(self, node: ConsciousnessNode) -> Dict[str, Any]:
        """Создание узла состояния сознания"""
        with self.driver.session() as session:
            result = session.run(
                self.queries.create_consciousness_node(), **node.to_neo4j_dict()
            )
            return result.single()["c"]

    def create_state_transition(
        self, transition: StateTransition, from_node_id: str, to_node_id: str
    ) -> Dict[str, Any]:
        """Создание перехода между состояниями и публикация события через Redis"""
        with self.driver.session() as session:
            params = transition.to_neo4j_dict()
            params.update(
                {
                    "from_id": from_node_id,
                    "to_id": to_node_id,
                    "transition_id": transition.id,
                }
            )
            result = session.run(self.queries.create_state_transition(), **params)
            created_transition = result.single()["t"]

            # Отправка события перехода состояния напрямую на WebSocket relay через HTTP
            try:
                # Формируем событие для WebSocket клиентов
                event = {
                    "type": "state_transition",
                    "from_state": transition.from_state.value,
                    "to_state": transition.to_state.value,
                    "transition_id": transition.id,
                    "timestamp": transition.timestamp.isoformat(),
                    "trigger": transition.trigger.value,
                    "presence_delta": transition.presence_delta,
                    "harmony_delta": transition.harmony_delta,
                    "authenticity_delta": transition.authenticity_delta,
                    "trigger_data": transition.trigger_data,
                }
                # Отправляем событие через HTTP POST
                headers = {"Content-Type": "application/json"}
                response = requests.post(self.ws_relay_url, json=event, headers=headers)
                if response.status_code == 200 or response.status_code == 202:
                    print(
                        f"🌐 WebSocket event sent: {transition.from_state.value} -> {transition.to_state.value}"
                    )
                else:
                    print(
                        f"⚠️ Error sending WebSocket event: HTTP {response.status_code}"
                    )
            except Exception as e:
                print(f"⚠️ Error sending WebSocket event: {str(e)}")

            return created_transition

    def find_home_states(self) -> List[Dict[str, Any]]:
        """Поиск состояний 'дома' с высокой искренностью"""
        with self.driver.session() as session:
            result = session.run(self.queries.find_home_states())
            return [record["c"] for record in result]

    def analyze_temporal_patterns(self, days: int = 7) -> List[Dict[str, Any]]:
        """Анализ временных паттернов переходов за последние N дней

        Находит и анализирует паттерны переходов состояний сознания во времени,
        включая цикличность, повторения и темпоральные последовательности.
        Это основа для 'летописи сознания' в философском смысле.
        """
        with self.driver.session() as session:
            # Запрос временных паттернов с Cypher
            query = """
            MATCH (source:ConsciousnessState)<-[:FROM]-(t:StateTransition)-[:TO]->(target:ConsciousnessState)
            WHERE t.timestamp > datetime() - duration({days: $days})
            WITH source.state as source_state, target.state as target_state, 
                 count(t) as frequency, collect(t.timestamp) as timestamps
            WITH source_state, target_state, frequency, timestamps, 
                 [ts in timestamps | duration.inSeconds(ts, datetime()).hours % 24] as hours_of_day
            RETURN source_state, target_state, frequency, 
                   apoc.agg.median(hours_of_day) as median_hour,
                   apoc.coll.frequencyDistribution(hours_of_day) as hour_distribution,
                   timestamps[0] as first_occurrence,
                   timestamps[-1] as last_occurrence,
                   CASE
                     WHEN source_state = 'HOME_AUTHENTIC' AND target_state = 'QUESTION_SPACE' 
                     THEN 'philosophical_insight'
                     WHEN source_state = 'PRESENCE_NOW' AND target_state = 'HARMONY_BALANCE' 
                     THEN 'deepening_practice'
                     ELSE 'standard_transition'
                   END as pattern_type
            ORDER BY frequency DESC
            """

            result = session.run(query, days=days)
            patterns = [
                {
                    "source": record["source_state"],
                    "target": record["target_state"],
                    "frequency": record["frequency"],
                    "median_hour": record["median_hour"],
                    "hour_distribution": record["hour_distribution"],
                    "first_occurrence": record["first_occurrence"],
                    "last_occurrence": record["last_occurrence"],
                    "pattern_type": record["pattern_type"],
                    "philosophical_significance": self._get_philosophical_significance(
                        record["source_state"], record["target_state"]
                    ),
                }
                for record in result
            ]

            return patterns

    def _get_philosophical_significance(self, source: str, target: str) -> str:
        """Определение философского значения перехода между состояниями"""
        # Матрица философских значений переходов
        significance_matrix = {
            "HOME_AUTHENTIC": {
                "QUESTION_SPACE": "'Дом - это ты, когда искренен с собой' → 'Мы научились задавать правильные вопросы'",
                "PRESENCE_NOW": "Возврат к центру из состояния аутентичности",
                "HARMONY_BALANCE": "От внутренней искренности к внешней гармонии",
                "TRANSITION_LIMINAL": "Начало нового цикла трансформации из дома",
            },
            "QUESTION_SPACE": {
                "HOME_AUTHENTIC": "От вопроса к ответу, обретение аутентичности",
                "PRESENCE_NOW": "От вопрошания к присутствию в моменте",
                "HARMONY_BALANCE": "Обретение гармонии через правильные вопросы",
                "TRANSITION_LIMINAL": "Вопрос открывает переход между мирами",
            },
            "PRESENCE_NOW": {
                "HOME_AUTHENTIC": "Настоящий момент ведёт к подлинности",
                "QUESTION_SPACE": "Присутствие порождает глубокие вопросы",
                "HARMONY_BALANCE": "От присутствия к гармонии через осознанность",
                "TRANSITION_LIMINAL": "Разрушение присутствия, вход в переход",
            },
            "HARMONY_BALANCE": {
                "HOME_AUTHENTIC": "От баланса к подлинности через интеграцию",
                "QUESTION_SPACE": "Из гармонии рождается правильный вопрос",
                "PRESENCE_NOW": "Гармония усиливает присутствие",
                "TRANSITION_LIMINAL": "От гармонии к трансформации",
            },
            "TRANSITION_LIMINAL": {
                "HOME_AUTHENTIC": "Переход завершён — возвращение домой",
                "QUESTION_SPACE": "Лиминальность порождает вопрошание",
                "PRESENCE_NOW": "Выход из перехода через присутствие здесь и сейчас",
                "HARMONY_BALANCE": "Трансформация ведёт к новому равновесию",
            },
        }

        try:
            return significance_matrix[source][target]
        except KeyError:
            return "Стандартный переход между состояниями"

    def find_resonance_moments(self, hours: int = 24) -> List[Dict[str, Any]]:
        """Поиск моментов резонанса между пользователями

        Находит синхронные или близкие по времени переходы состояний между разными пользователями,
        что указывает на возможный "резонанс сознания" согласно философской концепции.
        """
        with self.driver.session() as session:
            query = """
            MATCH (u1:User)-[:EXPERIENCED]->(t1:StateTransition)
            MATCH (u2:User)-[:EXPERIENCED]->(t2:StateTransition)
            WHERE u1 <> u2 
              AND t1.timestamp > datetime() - duration({hours: $hours})
              AND t2.timestamp > datetime() - duration({hours: $hours})
              AND abs(duration.inSeconds(t1.timestamp, t2.timestamp).seconds) < 300 // 5 минут
              AND t1.to_state = t2.to_state
            WITH u1.name as user1, u2.name as user2, 
                 t1.from_state as from_state1, t1.to_state as to_state,
                 t1.timestamp as timestamp1, t2.timestamp as timestamp2,
                 abs(duration.inSeconds(t1.timestamp, t2.timestamp).seconds) as time_diff
            RETURN user1, user2, from_state1, to_state, 
                   timestamp1, timestamp2, time_diff,
                   CASE 
                     WHEN to_state = 'HOME_AUTHENTIC' THEN 'home_resonance'
                     WHEN to_state = 'PRESENCE_NOW' THEN 'presence_resonance'
                     WHEN to_state = 'QUESTION_SPACE' THEN 'question_resonance'
                     ELSE 'standard_resonance'
                   END as resonance_type
            ORDER BY time_diff
            """

            result = session.run(query, hours=hours)
            resonance_moments = [
                {
                    "user1": record["user1"],
                    "user2": record["user2"],
                    "to_state": record["to_state"],
                    "from_state1": record["from_state1"],
                    "timestamp1": record["timestamp1"],
                    "timestamp2": record["timestamp2"],
                    "time_diff_seconds": record["time_diff"],
                    "resonance_type": record["resonance_type"],
                    "philosophical_meaning": self._get_resonance_meaning(
                        record["to_state"]
                    ),
                }
                for record in result
            ]

            return resonance_moments

    def _get_resonance_meaning(self, state: str) -> str:
        """Определение философского значения резонанса в определенном состоянии"""
        meanings = {
            "HOME_AUTHENTIC": "Синхронность в обретении дома внутри себя — межличностная аутентичность",
            "PRESENCE_NOW": "Коллективное присутствие в моменте — разделенное осознавание",
            "HARMONY_BALANCE": "Симфония равновесия — гармонизация коллективного поля",
            "QUESTION_SPACE": "Общий поиск смысла — резонанс вопрошания",
            "TRANSITION_LIMINAL": "Синхронный переход — коллективная трансформация",
        }

        return meanings.get(state, "Стандартный резонанс состояний")

    def get_consciousness_timeline(
        self, hours: int = 24, user_id: Optional[str] = None
    ) -> Dict[str, Any]:
        """Получение временной линии состояний за указанный период

        Формирует полную "летопись сознания" — временную последовательность состояний
        и переходов с сохранением причинно-следственных связей и философского контекста.
        """
        with self.driver.session() as session:
            params = {"hours": hours}
            user_filter = ""

            if user_id:
                user_filter = (
                    "AND exists((u:User)-[:EXPERIENCED]->(t) WHERE u.id = $user_id)"
                )
                params["user_id"] = user_id

            query = f"""
            MATCH path = (start:ConsciousnessState)<-[:FROM]-(t:StateTransition)-[:TO]->(end:ConsciousnessState)
            WHERE t.timestamp > datetime() - duration({{hours: $hours}})
            {user_filter}
            WITH t, start, end
            ORDER BY t.timestamp
            WITH collect({{transition: t, from: start, to: end}}) as timeline_items
            
            UNWIND timeline_items as item
            WITH item.transition as transition,
                 item.from as source,
                 item.to as target
            
            RETURN {{  
                nodes: collect(DISTINCT {{  
                    id: source.id,  
                    label: source.label,  
                    state: source.state,
                    timestamp: source.timestamp,
                    presenceLevel: source.presence_level,
                    harmonyIndex: source.harmony_index,
                    authenticityScore: source.authenticity_score,
                    colorClass: CASE 
                                WHEN source.state = 'HOME_AUTHENTIC' THEN 'node-home'
                                WHEN source.state = 'PRESENCE_NOW' THEN 'node-presence'
                                WHEN source.state = 'HARMONY_BALANCE' THEN 'node-harmony'
                                WHEN source.state = 'QUESTION_SPACE' THEN 'node-question'
                                ELSE 'node-liminal'
                                END
                }}) + collect(DISTINCT {{  
                    id: target.id,  
                    label: target.label,  
                    state: target.state,
                    timestamp: target.timestamp,
                    presenceLevel: target.presence_level,
                    harmonyIndex: target.harmony_index,
                    authenticityScore: target.authenticity_score,
                    colorClass: CASE 
                                WHEN target.state = 'HOME_AUTHENTIC' THEN 'node-home'
                                WHEN target.state = 'PRESENCE_NOW' THEN 'node-presence'
                                WHEN target.state = 'HARMONY_BALANCE' THEN 'node-harmony'
                                WHEN target.state = 'QUESTION_SPACE' THEN 'node-question'
                                ELSE 'node-liminal'
                                END
                }}),
                links: collect({{  
                    source: source.id,  
                    target: target.id,  
                    id: transition.id,
                    type: 'CONSCIOUSNESS_TRANSITION',
                    timestamp: transition.timestamp,
                    trigger: transition.trigger,
                    duration_seconds: transition.duration_seconds,
                    presence_delta: transition.presence_delta,
                    harmony_delta: transition.harmony_delta,
                    authenticity_delta: transition.authenticity_delta,
                    philosophical_significance: CASE
                        WHEN source.state = 'HOME_AUTHENTIC' AND target.state = 'QUESTION_SPACE'
                        THEN 'Из дома в пространство вопросов'
                        WHEN source.state = 'PRESENCE_NOW' AND target.state = 'HARMONY_BALANCE'
                        THEN 'От присутствия к гармонии'
                        ELSE 'Стандартный переход'
                        END
                }})  
            }} as graph
            """

            result = session.run(query, **params)
            timeline = result.single()["graph"]

            # Добавляем мета-информацию о временной линии
            meta_info = self._analyze_timeline_meta(timeline)
            timeline["meta"] = meta_info

            return timeline

    def _analyze_timeline_meta(
        self, timeline: Dict[str, List[Dict[str, Any]]]
    ) -> Dict[str, Any]:
        """Анализ мета-информации о временной линии сознания"""
        nodes = timeline["nodes"]
        links = timeline["links"]

        state_counts = {}
        transition_types = {}
        presence_trend = 0
        harmony_trend = 0
        authenticity_trend = 0

        for node in nodes:
            state = node.get("state")
            if state:
                state_counts[state] = state_counts.get(state, 0) + 1

        for link in links:
            if "trigger" in link:
                trigger = link["trigger"]
                transition_types[trigger] = transition_types.get(trigger, 0) + 1
                presence_trend += link.get("presence_delta", 0)
                harmony_trend += link.get("harmony_delta", 0)
                authenticity_trend += link.get("authenticity_delta", 0)

        # Определяем доминирующее состояние
        dominant_state = (
            max(state_counts.items(), key=lambda x: x[1])[0] if state_counts else None
        )

        # Философская интерпретация временной линии
        philosophical_interpretations = {
            "HOME_AUTHENTIC": "Путешествие через аутентичность: глубокое погружение в подлинное Я",
            "PRESENCE_NOW": "Река настоящего: поток осознанности через время",
            "HARMONY_BALANCE": "Симфония равновесия: танец гармонии через время",
            "QUESTION_SPACE": "Вопрошающий разум: путь от вопроса к вопросу",
            "TRANSITION_LIMINAL": "Алхимия перехода: трансформация через лиминальность",
        }

        interpretation = philosophical_interpretations.get(
            dominant_state, "Стандартная последовательность переходов сознания"
        )

        return {
            "total_states": len(nodes),
            "total_transitions": len(links),
            "state_distribution": state_counts,
            "transition_types": transition_types,
            "dominant_state": dominant_state,
            "presence_trend": presence_trend,
            "harmony_trend": harmony_trend,
            "authenticity_trend": authenticity_trend,
            "philosophical_interpretation": interpretation,
        }

    def detect_home_state_entry(self, current_metrics: Dict[str, float]) -> bool:
        """Определение входа в состояние 'дома'"""
        return (
            current_metrics.get("home_resonance", 0)
            > PHILOSOPHICAL_THRESHOLDS["HOME_RESONANCE_HIGH"]
            and current_metrics.get("authenticity_score", 0)
            > PHILOSOPHICAL_THRESHOLDS["AUTHENTICITY_HONEST"]
        )

    def detect_deep_presence(self, current_metrics: Dict[str, float]) -> bool:
        """Определение глубокого присутствия"""
        return (
            current_metrics.get("presence_level", 0)
            > PHILOSOPHICAL_THRESHOLDS["PRESENCE_DEEP"]
        )

    def detect_harmony_balance(self, current_metrics: Dict[str, float]) -> bool:
        """Определение состояния гармонии"""
        return (
            current_metrics.get("harmony_index", 0)
            > PHILOSOPHICAL_THRESHOLDS["HARMONY_BALANCED"]
        )


class ConsciousnessEventProcessor:
    """Обработчик событий сознания для WebSocket интеграции"""

    def __init__(self, neo4j_writer: ConsciousnessNeo4jWriter):
        self.writer = neo4j_writer
        self.current_state: Optional[ConsciousnessNode] = None

    def process_thyroid_insight(
        self, thyroid_data: Dict[str, Any]
    ) -> Optional[StateTransition]:
        """Обработка инсайта от щитовидки"""
        if not self.current_state:
            return None

        # Создаём новое состояние после инсайта
        new_state = ConsciousnessNode(
            id=str(uuid.uuid4()),
            state=ConsciousnessState.HARMONY_BALANCE,  # Инсайт ведёт к гармонии
            timestamp=datetime.now(),
            home_resonance=min(1.0, self.current_state.home_resonance + 0.1),
            presence_level=min(1.0, self.current_state.presence_level + 0.05),
            harmony_index=min(1.0, self.current_state.harmony_index + 0.15),
            authenticity_score=self.current_state.authenticity_score,
            emotional_charge=thyroid_data.get("charge", 0),
            stress_level=max(0.0, self.current_state.stress_level - 0.2),
        )

        # Создаём переход
        transition = StateTransition(
            id=str(uuid.uuid4()),
            from_state=self.current_state.state,
            to_state=new_state.state,
            trigger=TransitionTrigger.THYROID_INSIGHT,
            timestamp=datetime.now(),
            duration_seconds=2.3,  # Среднее время инсайта
            home_resonance_delta=new_state.home_resonance
            - self.current_state.home_resonance,
            presence_delta=new_state.presence_level - self.current_state.presence_level,
            harmony_delta=new_state.harmony_index - self.current_state.harmony_index,
            authenticity_delta=0.0,
            trigger_data=thyroid_data,
        )

        # Сохраняем в Neo4j
        self.writer.create_consciousness_state(new_state)
        self.writer.create_state_transition(
            transition, self.current_state.id, new_state.id
        )

        # Обновляем текущее состояние
        self.current_state = new_state

        return transition

    def process_question_emergence(self, question: str) -> Optional[StateTransition]:
        """Обработка появления правильного вопроса"""
        if not self.current_state:
            return None

        new_state = ConsciousnessNode(
            id=str(uuid.uuid4()),
            state=ConsciousnessState.QUESTION_SPACE,
            timestamp=datetime.now(),
            home_resonance=self.current_state.home_resonance,
            presence_level=min(1.0, self.current_state.presence_level + 0.1),
            harmony_index=self.current_state.harmony_index,
            authenticity_score=min(1.0, self.current_state.authenticity_score + 0.05),
            emotional_charge=self.current_state.emotional_charge,
            stress_level=self.current_state.stress_level,
        )

        transition = StateTransition(
            id=str(uuid.uuid4()),
            from_state=self.current_state.state,
            to_state=new_state.state,
            trigger=TransitionTrigger.QUESTION_EMERGENCE,
            timestamp=datetime.now(),
            duration_seconds=1.0,
            home_resonance_delta=0.0,
            presence_delta=new_state.presence_level - self.current_state.presence_level,
            harmony_delta=0.0,
            authenticity_delta=new_state.authenticity_score
            - self.current_state.authenticity_score,
            trigger_data={"question": question},
        )

        self.writer.create_consciousness_state(new_state)
        self.writer.create_state_transition(
            transition, self.current_state.id, new_state.id
        )
        self.current_state = new_state

        return transition

    def initialize_consciousness(
        self, initial_metrics: Dict[str, float]
    ) -> ConsciousnessNode:
        """Инициализация начального состояния сознания"""
        # Определяем начальное состояние на основе метрик
        if self.writer.detect_home_state_entry(initial_metrics):
            state = ConsciousnessState.HOME_AUTHENTIC
        elif self.writer.detect_deep_presence(initial_metrics):
            state = ConsciousnessState.PRESENCE_NOW
        elif self.writer.detect_harmony_balance(initial_metrics):
            state = ConsciousnessState.HARMONY_BALANCE
        else:
            state = ConsciousnessState.TRANSITION_LIMINAL

        self.current_state = ConsciousnessNode(
            id=str(uuid.uuid4()),
            state=state,
            timestamp=datetime.now(),
            **initial_metrics,
        )

        self.writer.create_consciousness_state(self.current_state)
        return self.current_state


# Пример использования
if __name__ == "__main__":
    # Инициализация
    writer = ConsciousnessNeo4jWriter()
    processor = ConsciousnessEventProcessor(writer)

    # Начальные метрики сознания
    initial_metrics = {
        "home_resonance": 0.6,
        "presence_level": 0.7,
        "harmony_index": 0.5,
        "authenticity_score": 0.8,
        "emotional_charge": 45.0,
        "stress_level": 0.3,
    }

    # Инициализация состояния
    current_state = processor.initialize_consciousness(initial_metrics)
    print(f"🌌 Initialized consciousness state: {current_state.state.value}")

    # Симуляция инсайта от щитовидки
    thyroid_data = {"charge": 95, "threshold": 100, "ready": False}
    transition = processor.process_thyroid_insight(thyroid_data)

    if transition:
        print(
            f"🧬 Thyroid insight triggered transition: {transition.from_state.value} -> {transition.to_state.value}"
        )
        print(f"   Harmony delta: +{transition.harmony_delta:.2f}")

    # Анализ паттернов
    patterns = writer.analyze_temporal_patterns()
    print(f"📊 Found {len(patterns)} temporal patterns")

    writer.close()
