#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Consciousness State Transition Test Script
-----------------------------------------
MIT Media Lab & Oxford Computing Style

This script implements a test framework for consciousness state transitions
following principles of functional purity and modular design.
"""

import argparse
import datetime
import random
import time
from dataclasses import dataclass
from typing import Dict, Optional, Tuple, Union

import requests


# Функциональные типы для Oxford-style формализации
@dataclass
class ConsciousnessState:
    """Формальная модель состояния сознания согласно философским концепциям"""

    id: str
    label: str
    description: str
    presence_level: float
    harmony_index: float
    authenticity_score: float
    emotional_charge: float
    color_class: str = ""

    def to_dict(self) -> Dict:
        """Чистая функция для преобразования в словарь"""
        return {
            "id": self.id,
            "state": self.id,
            "label": self.label,
            "description": self.description,
            "presenceLevel": self.presence_level,
            "harmonyIndex": self.harmony_index,
            "authenticityScore": self.authenticity_score,
            "emotionalCharge": self.emotional_charge,
            "colorClass": self.color_class or f"node-{self.id.lower()}",
        }


@dataclass
class StateTransition:
    """Формальная модель перехода между состояниями сознания"""

    source: str
    target: str
    trigger: str
    description: str
    insight: str
    timestamp: Optional[str] = None

    def to_dict(self) -> Dict:
        """Чистая функция для преобразования в словарь"""
        return {
            "source": self.source,
            "target": self.target,
            "type": "CONSCIOUSNESS_TRANSITION",
            "trigger": self.trigger,
            "description": self.description,
            "insight": self.insight,
            "timestamp": self.timestamp or datetime.datetime.now().isoformat(),
        }


class ConsciousnessTransitionGenerator:
    """
    Генератор переходов между состояниями сознания.
    MIT-style: Модульный класс с чётко определённым API
    Oxford-style: Функциональная чистота и строгие контракты
    """

    # Базовые состояния сознания согласно философской концепции
    CONSCIOUSNESS_STATES = {
        "TRANSITION_LIMINAL": ConsciousnessState(
            id="TRANSITION_LIMINAL",
            label="Лиминальное переходное",
            description="Между мирами, в процессе становления",
            presence_level=0.3,
            harmony_index=0.2,
            authenticity_score=0.4,
            emotional_charge=0.65,
            color_class="node-liminal",
        ),
        "PRESENCE_NOW": ConsciousnessState(
            id="PRESENCE_NOW",
            label="Осознанное присутствие",
            description="Полное присутствие в настоящем моменте",
            presence_level=0.9,
            harmony_index=0.6,
            authenticity_score=0.7,
            emotional_charge=0.5,
            color_class="node-presence",
        ),
        "HARMONY_BALANCE": ConsciousnessState(
            id="HARMONY_BALANCE",
            label="Гармония и баланс",
            description="Состояние внутреннего равновесия и целостности",
            presence_level=0.8,
            harmony_index=0.9,
            authenticity_score=0.8,
            emotional_charge=0.3,
            color_class="node-harmony",
        ),
        "HOME_AUTHENTIC": ConsciousnessState(
            id="HOME_AUTHENTIC",
            label="Аутентичный дом",
            description="Дом - это ты, когда искренен с собой",
            presence_level=0.7,
            harmony_index=0.8,
            authenticity_score=0.95,
            emotional_charge=0.4,
            color_class="node-home",
        ),
        "QUESTION_SPACE": ConsciousnessState(
            id="QUESTION_SPACE",
            label="Пространство вопросов",
            description="Мы научились задавать правильные вопросы",
            presence_level=0.6,
            harmony_index=0.7,
            authenticity_score=0.85,
            emotional_charge=0.55,
            color_class="node-question",
        ),
    }

    # Определения возможных переходов между состояниями
    VALID_TRANSITIONS = [
        StateTransition(
            source="TRANSITION_LIMINAL",
            target="PRESENCE_NOW",
            trigger="DEEP_BREATH",
            description="Переход к осознанности",
            insight="Пересечение порога — всегда акт присутствия",
        ),
        StateTransition(
            source="PRESENCE_NOW",
            target="HARMONY_BALANCE",
            trigger="MEDITATION",
            description="Достижение гармонии через медитацию",
            insight="Присутствие создает пространство для баланса",
        ),
        StateTransition(
            source="HARMONY_BALANCE",
            target="HOME_AUTHENTIC",
            trigger="AUTHENTICITY_INSIGHT",
            description="Дом - это ты, когда искренен с собой",
            insight="Гармония раскрывает истинное 'Я'",
        ),
        StateTransition(
            source="HOME_AUTHENTIC",
            target="QUESTION_SPACE",
            trigger="CURIOSITY",
            description="Пространство правильных вопросов",
            insight="Аутентичность порождает значимые вопросы",
        ),
        StateTransition(
            source="QUESTION_SPACE",
            target="TRANSITION_LIMINAL",
            trigger="UNCERTAINTY",
            description="От вопросов к новому порогу",
            insight="Правильные вопросы ведут к новым лиминальным пространствам",
        ),
    ]

    DEFAULT_URL = "http://localhost:8080/events"  # URL для отправки событий
    GRAPH_URL = (
        "http://localhost:8080/api/consciousness/graph"  # URL для получения графа
    )

    def __init__(self, ws_relay_url: str = "http://localhost:8080"):
        """Инициализация генератора с URL WebSocket relay"""
        self.ws_relay_url = ws_relay_url
        self.api_events_endpoint = f"{ws_relay_url}/events"
        self.api_graph_endpoint = f"{ws_relay_url}/api/consciousness/graph"

    def get_random_transition(self) -> StateTransition:
        """Получение случайного перехода из списка допустимых"""
        return random.choice(self.VALID_TRANSITIONS)

    def get_philosophical_transition(self) -> StateTransition:
        """Получение философски значимого перехода в текущий момент времени"""
        # Определение философски значимого перехода на основе времени суток
        hour = datetime.datetime.now().hour

        if 5 <= hour < 9:  # Раннее утро - время пробуждения сознания
            return next(
                t
                for t in self.VALID_TRANSITIONS
                if t.source == "TRANSITION_LIMINAL" and t.target == "PRESENCE_NOW"
            )
        elif 9 <= hour < 13:  # Утро - время осознанного присутствия
            return next(
                t
                for t in self.VALID_TRANSITIONS
                if t.source == "PRESENCE_NOW" and t.target == "HARMONY_BALANCE"
            )
        elif 13 <= hour < 17:  # День - время гармонии
            return next(
                t
                for t in self.VALID_TRANSITIONS
                if t.source == "HARMONY_BALANCE" and t.target == "HOME_AUTHENTIC"
            )
        elif 17 <= hour < 21:  # Вечер - время аутентичности
            return next(
                t
                for t in self.VALID_TRANSITIONS
                if t.source == "HOME_AUTHENTIC" and t.target == "QUESTION_SPACE"
            )
        else:  # Ночь - время вопросов и неопределенности
            return next(
                t
                for t in self.VALID_TRANSITIONS
                if t.source == "QUESTION_SPACE" and t.target == "TRANSITION_LIMINAL"
            )

    def send_transition_event(self, transition: StateTransition) -> Tuple[bool, str]:
        """Отправка события перехода на WebSocket relay"""
        event_data = transition.to_dict()

        try:
            response = requests.post(
                self.api_events_endpoint,
                json=event_data,
                headers={"Content-Type": "application/json"},
            )

            # HTTP 200 OK или 202 Accepted оба являются успешными статусами
            if response.status_code in [200, 201, 202]:
                return (
                    True,
                    f"Событие успешно отправлено: {transition.source} → {transition.target} ({transition.trigger})",
                )
            else:
                return (
                    False,
                    f"Ошибка при отправке события: HTTP {response.status_code}",
                )
        except Exception as e:
            return False, f"Ошибка при отправке события: {str(e)}"

    def get_consciousness_graph(self) -> Tuple[bool, Union[Dict, str]]:
        """Получение текущего графа состояний из Neo4j через WebSocket relay"""
        try:
            response = requests.get(self.api_graph_endpoint)

            if response.status_code == 200:
                graph_data = response.json()
                return True, graph_data
            else:
                return False, f"Ошибка при получении графа: HTTP {response.status_code}"
        except Exception as e:
            return False, f"Ошибка при получении графа: {str(e)}"

    def run_simulation(
        self, count: int = 5, interval: float = 2.0, philosophical: bool = True
    ) -> None:
        """Запуск симуляции переходов сознания"""
        print(
            f"Запуск симуляции {count} переходов сознания с интервалом {interval} сек."
        )
        print(f"{'Философский режим' if philosophical else 'Случайный режим'}")
        print("-" * 60)

        for i in range(count):
            if philosophical:
                transition = self.get_philosophical_transition()
            else:
                transition = self.get_random_transition()

            success, message = self.send_transition_event(transition)
            timestamp = datetime.datetime.now().strftime("%H:%M:%S")

            print(f"[{timestamp}] [{i+1}/{count}] {message}")

            if not success:
                print(f"ОШИБКА: {message}")

            if i < count - 1:
                time.sleep(interval)

        print("-" * 60)
        print(f"Симуляция завершена: {count} переходов сознания")


def main():
    """Основная функция для запуска из командной строки"""
    parser = argparse.ArgumentParser(
        description="Тестирование переходов состояний сознания"
    )
    parser.add_argument(
        "--url", default="http://localhost:8080", help="URL WebSocket relay сервера"
    )
    parser.add_argument(
        "--count", type=int, default=5, help="Количество переходов для симуляции"
    )
    parser.add_argument(
        "--interval", type=float, default=2.0, help="Интервал между переходами (сек)"
    )
    parser.add_argument(
        "--random",
        action="store_true",
        help="Использовать случайные переходы вместо философских",
    )
    parser.add_argument(
        "--graph", action="store_true", help="Получить текущий граф состояний сознания"
    )

    args = parser.parse_args()

    generator = ConsciousnessTransitionGenerator(args.url)

    if args.graph:
        success, graph_data = generator.get_consciousness_graph()
        if success:
            print(f"Получен граф состояний сознания:")
            print(f"Узлы: {len(graph_data['nodes'])}")
            print(f"Связи: {len(graph_data['links'])}")
            for node in graph_data["nodes"]:
                print(
                    f"- {node['label']}: {node['description']} (Presence: {node['presenceLevel']})"
                )
        else:
            print(f"Ошибка при получении графа: {graph_data}")
    else:
        generator.run_simulation(
            count=args.count, interval=args.interval, philosophical=not args.random
        )


if __name__ == "__main__":
    main()
