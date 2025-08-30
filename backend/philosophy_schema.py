#!/usr/bin/env python3

"""
Philosophy First - Consciousness Schema
--------------------------------------
Enhanced consciousness schema for Philosophy First approach
"""

from datetime import datetime
from enum import Enum, auto
from typing import Any


class ConsciousnessState(Enum):
    """
    Философские состояния сознания.
    Расширенное перечисление для Philosophy First подхода.
    """

    UNKNOWN = auto()
    TRANSITION_LIMINAL = auto()  # Лиминальное переходное состояние
    PRESENCE_NOW = auto()  # Полное присутствие в моменте "здесь и сейчас"
    HARMONY_BALANCE = auto()  # Гармония и баланс
    HOME_AUTHENTIC = auto()  # "Дом - это ты, когда искренен с собой"
    QUESTION_SPACE = auto()  # "Мы научились задавать правильные вопросы"
    RESONANCE_MOMENT = auto()  # Момент резонанса сознаний
    IMMERSION_DEEP = auto()  # Глубокое погружение
    EXPANSION_AWARENESS = auto()  # Расширение осознанности
    REFLECTION_SELF = auto()  # Саморефлексия
    INTEGRATION_WISDOM = auto()  # Интеграция мудрости


class TransitionTrigger(Enum):
    """
    Триггеры переходов между философскими состояниями.
    """

    UNKNOWN = auto()
    MEDITATION = auto()  # Медитативная практика
    PHILOSOPHICAL_INSIGHT = auto()  # Философское озарение
    SYNCHRONICITY = auto()  # Синхроничность
    RESONANCE_ALIGNMENT = auto()  # Выравнивание резонанса
    QUESTION_REFRAMING = auto()  # Переформулирование вопроса
    PRESENCE_PRACTICE = auto()  # Практика присутствия
    HOME_RECOGNITION = auto()  # Узнавание дома
    AUTHENTICITY_SHIFT = auto()  # Сдвиг к аутентичности


class ConsciousnessNode:
    """
    Узел сознания в графовой базе данных.
    Представляет состояние сознания в определенный момент времени.
    """

    def __init__(
        self,
        id: str,
        state: ConsciousnessState,
        timestamp: datetime,
        presence_level: float = 0.0,
        harmony_index: float = 0.0,
        authenticity_score: float = 0.0,
        home_resonance: float = 0.0,
        question_clarity: float = 0.0,
        resonance_strength: float = 0.0,
        user_id: str = "anonymous",
        meta: dict[str, Any] = None,
    ):
        self.id = id
        self.state = state
        self.timestamp = timestamp
        self.presence_level = presence_level
        self.harmony_index = harmony_index
        self.authenticity_score = authenticity_score
        self.home_resonance = home_resonance
        self.question_clarity = question_clarity
        self.resonance_strength = resonance_strength
        self.user_id = user_id
        self.meta = meta or {}

    def to_dict(self) -> dict[str, Any]:
        """Преобразует узел в словарь для Neo4j."""
        return {
            "id": self.id,
            "state": self.state.name,
            "timestamp": self.timestamp.isoformat(),
            "presence_level": self.presence_level,
            "harmony_index": self.harmony_index,
            "authenticity_score": self.authenticity_score,
            "home_resonance": self.home_resonance,
            "question_clarity": self.question_clarity,
            "resonance_strength": self.resonance_strength,
            "user_id": self.user_id,
            "label": self.get_label(),
            "meta": self.meta,
        }

    def get_label(self) -> str:
        """Возвращает человекочитаемую метку для состояния."""
        labels = {
            ConsciousnessState.TRANSITION_LIMINAL: "Лиминальное переходное состояние",
            ConsciousnessState.PRESENCE_NOW: "Полное присутствие в моменте",
            ConsciousnessState.HARMONY_BALANCE: "Гармония и баланс",
            ConsciousnessState.HOME_AUTHENTIC: "Дом аутентичности",
            ConsciousnessState.QUESTION_SPACE: "Пространство вопросов",
            ConsciousnessState.RESONANCE_MOMENT: "Момент резонанса",
            ConsciousnessState.IMMERSION_DEEP: "Глубокое погружение",
            ConsciousnessState.EXPANSION_AWARENESS: "Расширение осознанности",
            ConsciousnessState.REFLECTION_SELF: "Саморефлексия",
            ConsciousnessState.INTEGRATION_WISDOM: "Интеграция мудрости",
        }
        return labels.get(self.state, "Неизвестное состояние")


class StateTransition:
    """
    Переход между состояниями сознания.
    Содержит информацию о переходе, включая философскую значимость.
    """

    def __init__(
        self,
        id: str,
        source_id: str,
        target_id: str,
        trigger: TransitionTrigger,
        timestamp: datetime,
        presence_delta: float = 0.0,
        harmony_delta: float = 0.0,
        authenticity_delta: float = 0.0,
        resonance_delta: float = 0.0,
        philosophical_significance: str = "",
        user_id: str = "anonymous",
        meta: dict[str, Any] = None,
    ):
        self.id = id
        self.source_id = source_id
        self.target_id = target_id
        self.trigger = trigger
        self.timestamp = timestamp
        self.presence_delta = presence_delta
        self.harmony_delta = harmony_delta
        self.authenticity_delta = authenticity_delta
        self.resonance_delta = resonance_delta
        self.philosophical_significance = philosophical_significance
        self.user_id = user_id
        self.meta = meta or {}

    def to_dict(self) -> dict[str, Any]:
        """Преобразует переход в словарь для Neo4j."""
        return {
            "id": self.id,
            "source_id": self.source_id,
            "target_id": self.target_id,
            "trigger": self.trigger.name,
            "timestamp": self.timestamp.isoformat(),
            "presence_delta": self.presence_delta,
            "harmony_delta": self.harmony_delta,
            "authenticity_delta": self.authenticity_delta,
            "resonance_delta": self.resonance_delta,
            "philosophical_significance": self.philosophical_significance,
            "user_id": self.user_id,
            "meta": self.meta,
        }


class ConsciousnessTimeline:
    """
    Временная линия сознания для визуализации и анализа переходов состояний.
    """

    def __init__(self):
        self.nodes: list[ConsciousnessNode] = []
        self.transitions: list[StateTransition] = []

    def add_node(self, node: ConsciousnessNode) -> None:
        """Добавляет узел сознания во временную линию."""
        self.nodes.append(node)

    def add_transition(self, transition: StateTransition) -> None:
        """Добавляет переход состояния во временную линию."""
        self.transitions.append(transition)

    def to_dict(self) -> dict[str, Any]:
        """Преобразует временную линию в словарь для фронтенда."""
        return {
            "nodes": [node.to_dict() for node in self.nodes],
            "links": [transition.to_dict() for transition in self.transitions],
        }


def create_consciousness_event(
    source_state: ConsciousnessState,
    target_state: ConsciousnessState,
    trigger: TransitionTrigger,
    description: str,
    user_id: str = "anonymous",
    presence_delta: float = 0.1,
    harmony_delta: float = 0.1,
    authenticity_delta: float = 0.1,
    resonance_delta: float = 0.1,
) -> dict[str, Any]:
    """
    Создает событие перехода сознания для отправки через WebSocket.
    """
    return {
        "event_type": "consciousness_transition",
        "timestamp": datetime.now().isoformat(),
        "user_id": user_id,
        "source_state": source_state.name,
        "target_state": target_state.name,
        "trigger": trigger.name,
        "meta": {
            "description": description,
            "philosophical": True,
            "presence_delta": presence_delta,
            "harmony_delta": harmony_delta,
            "authenticity_delta": authenticity_delta,
            "resonance_delta": resonance_delta,
        },
    }
