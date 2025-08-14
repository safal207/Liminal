"""
🌌 Consciousness Schema for Neo4j Temporal Data Lake
Революционная архитектура состояний сознания
"""

import json
from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional


class ConsciousnessState(Enum):
    """Основные состояния сознания"""

    # Базовые философские состояния
    HOME_AUTHENTIC = "home_authentic"  # Дом - искренность с собой
    RESONANCE_FLOW = "resonance_flow"  # Поток резонанса с другими
    PRESENCE_NOW = "presence_now"  # Присутствие здесь и сейчас
    QUESTION_SPACE = "question_space"  # Пространство правильных вопросов
    HARMONY_BALANCE = "harmony_balance"  # Гармония от понимания себя
    TRANSITION_LIMINAL = "transition_liminal"  # Переходное состояние

    # Эмоциональные состояния и нелинейные пути
    DOUBT_CREATIVE = "doubt_creative"  # Творческое сомнение как ресурс
    CURIOUS_FLOW = "curious_flow"  # Поток любопытства
    ANGER_PROTECTION = "anger_protection"  # Защитный гнев как граница
    BOUNDARY_INTEGRITY = "boundary_integrity"  # Целостность личных границ
    WITHDRAWAL_PROTECTION = "withdrawal_protection"  # Уход для самозащиты
    GRIEF_TRANSFORMATION = "grief_transformation"  # Трансформация через горе
    GRATITUDE_PRESENCE = "gratitude_presence"  # Благодарность в настоящем


class TransitionTrigger(Enum):
    """Триггеры переходов между состояниями"""

    # Базовые триггеры
    THYROID_INSIGHT = "thyroid_insight_release"
    SELF_HONESTY = "moment_of_self_honesty"
    RESONANCE_MATCH = "resonance_with_other"
    QUESTION_EMERGENCE = "right_question_asked"
    HARMONY_RECOGNITION = "harmony_recognized"
    PRESENCE_ANCHOR = "presence_anchored"
    DEEP_BREATH = "deep_breathing_meditation"
    MEDITATION = "meditation_practice"
    AUTHENTICITY_INSIGHT = "authenticity_insight"

    # Эмоциональные триггеры и нелинейные переходы
    CREATIVE_DOUBT = "allowing_creative_doubt"  # Принятие творческого сомнения
    CURIOSITY_SPARK = "curiosity_sparked"  # Вспышка любопытства
    BOUNDARY_VIOLATION = "boundary_violation_detected"  # Обнаружение нарушения границ
    ANGER_RELEASE = "healthy_anger_released"  # Здоровое выражение гнева
    BOUNDARY_SETTING = "boundary_consciously_set"  # Осознанная установка границ
    OVERWHELM_DETECTED = "emotional_overwhelm_detected"  # Обнаружение перегрузки
    GRIEF_ACKNOWLEDGED = "grief_consciously_felt"  # Осознанное проживание горя
    GRATITUDE_PRACTICE = "gratitude_practice_engaged"  # Практика благодарности


@dataclass
class ConsciousnessNode:
    """Узел состояния сознания в Neo4j"""

    id: str
    state: ConsciousnessState
    timestamp: datetime

    # Философские метрики (0.0 - 1.0)
    home_resonance: float  # Насколько "дома" с собой
    presence_level: float  # Уровень присутствия в моменте
    harmony_index: float  # Индекс внутренней гармонии
    authenticity_score: float  # Оценка искренности

    # Эмоциональные данные
    emotional_charge: float  # От щитовидки
    stress_level: float  # Уровень стресса

    # Контекстные данные
    location: Optional[str] = None
    activity: Optional[str] = None
    companions: Optional[List[str]] = None

    def to_neo4j_dict(self) -> Dict[str, Any]:
        """Преобразование в формат для Neo4j"""
        return {
            "id": self.id,
            "state": self.state.value,
            "timestamp": self.timestamp.isoformat(),
            "home_resonance": self.home_resonance,
            "presence_level": self.presence_level,
            "harmony_index": self.harmony_index,
            "authenticity_score": self.authenticity_score,
            "emotional_charge": self.emotional_charge,
            "stress_level": self.stress_level,
            "location": self.location,
            "activity": self.activity,
            "companions": self.companions or [],
        }


@dataclass
class StateTransition:
    """Переход между состояниями сознания"""

    id: str
    from_state: ConsciousnessState
    to_state: ConsciousnessState
    trigger: TransitionTrigger
    timestamp: datetime
    duration_seconds: float

    # Изменения в метриках
    home_resonance_delta: float
    presence_delta: float
    harmony_delta: float
    authenticity_delta: float

    # Контекст перехода
    trigger_data: Dict[str, Any]

    def to_neo4j_dict(self) -> Dict[str, Any]:
        """Преобразование в формат для Neo4j"""
        return {
            "id": self.id,
            "from_state": self.from_state.value,
            "to_state": self.to_state.value,
            "trigger": self.trigger.value,
            "timestamp": self.timestamp.isoformat(),
            "duration_seconds": self.duration_seconds,
            "home_resonance_delta": self.home_resonance_delta,
            "presence_delta": self.presence_delta,
            "harmony_delta": self.harmony_delta,
            "authenticity_delta": self.authenticity_delta,
            "trigger_data": json.dumps(self.trigger_data),
        }


class ConsciousnessQueries:
    """Cypher запросы для анализа сознания"""

    @staticmethod
    def create_consciousness_node() -> str:
        """Создание узла состояния сознания"""
        return """
        CREATE (c:ConsciousnessState {
            id: $id,
            state: $state,
            timestamp: datetime($timestamp),
            home_resonance: $home_resonance,
            presence_level: $presence_level,
            harmony_index: $harmony_index,
            authenticity_score: $authenticity_score,
            emotional_charge: $emotional_charge,
            stress_level: $stress_level,
            location: $location,
            activity: $activity,
            companions: $companions
        })
        RETURN c
        """

    @staticmethod
    def create_state_transition() -> str:
        """Создание перехода между состояниями"""
        return """
        MATCH (from:ConsciousnessState {id: $from_id})
        MATCH (to:ConsciousnessState {id: $to_id})
        CREATE (from)-[t:TRANSITIONS_TO {
            id: $transition_id,
            trigger: $trigger,
            timestamp: datetime($timestamp),
            duration_seconds: $duration_seconds,
            home_resonance_delta: $home_resonance_delta,
            presence_delta: $presence_delta,
            harmony_delta: $harmony_delta,
            authenticity_delta: $authenticity_delta,
            trigger_data: $trigger_data
        }]->(to)
        RETURN t
        """

    @staticmethod
    def find_home_states() -> str:
        """Поиск состояний 'дома' (высокая искренность)"""
        return """
        MATCH (c:ConsciousnessState)
        WHERE c.state = 'home_authentic' 
        AND c.home_resonance > 0.8
        AND c.authenticity_score > 0.8
        RETURN c
        ORDER BY c.timestamp DESC
        LIMIT 10
        """

    @staticmethod
    def analyze_temporal_patterns() -> str:
        """Анализ временных паттернов переходов"""
        return """
        MATCH (from:ConsciousnessState)-[t:TRANSITIONS_TO]->(to:ConsciousnessState)
        WHERE t.timestamp > datetime() - duration('P7D')  // Последние 7 дней
        RETURN 
            from.state as from_state,
            to.state as to_state,
            t.trigger as trigger,
            count(*) as frequency,
            avg(t.duration_seconds) as avg_duration,
            avg(t.harmony_delta) as avg_harmony_change
        ORDER BY frequency DESC
        """

    @staticmethod
    def find_resonance_moments() -> str:
        """Поиск моментов резонанса между пользователями"""
        return """
        MATCH (c1:ConsciousnessState), (c2:ConsciousnessState)
        WHERE c1.id <> c2.id
        AND abs(duration.between(c1.timestamp, c2.timestamp).seconds) < 60  // В пределах минуты
        AND c1.state = c2.state
        AND c1.home_resonance > 0.7 AND c2.home_resonance > 0.7
        RETURN c1, c2, 
               abs(c1.home_resonance - c2.home_resonance) as resonance_similarity
        ORDER BY resonance_similarity ASC
        LIMIT 10
        """

    @staticmethod
    def consciousness_timeline() -> str:
        """Временная линия состояний сознания"""
        return """
        MATCH (c:ConsciousnessState)
        WHERE c.timestamp > datetime() - duration('P1D')  // Последние 24 часа
        OPTIONAL MATCH (c)-[t:TRANSITIONS_TO]->(next:ConsciousnessState)
        RETURN c, t, next
        ORDER BY c.timestamp ASC
        """


# Философские константы
PHILOSOPHICAL_THRESHOLDS = {
    "HOME_RESONANCE_HIGH": 0.8,  # Высокий уровень "дома"
    "PRESENCE_DEEP": 0.9,  # Глубокое присутствие
    "HARMONY_BALANCED": 0.75,  # Сбалансированная гармония
    "AUTHENTICITY_HONEST": 0.85,  # Высокая искренность
    "RESONANCE_SYNC_WINDOW": 60,  # Окно синхронизации резонанса (сек)
}

# Вопросы для Question-Driven Architecture
CONSCIOUSNESS_QUESTIONS = [
    "Кто я есть на самом деле?",
    "Что сейчас происходит внутри меня?",
    "Где я чувствую себя дома?",
    "С кем я резонирую?",
    "Какой вопрос хочет быть заданным?",
    "Что приносит мне гармонию?",
    "Как я могу быть более искренним с собой?",
]
