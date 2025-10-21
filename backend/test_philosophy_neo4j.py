#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Philosophy First - Neo4j Temporal Data Lake Integration Test
-----------------------------------------------------------
Интеграция философских концепций с Neo4j и WebSocket relay
"""

import argparse
import json
import os
import random
import sys
import time
import traceback
import uuid
from datetime import datetime

import requests

try:
    from backend.philosophy_neo4j import PhilosophyNeo4jWriter
    from backend.philosophy_schema import (
        ConsciousnessNode,
        ConsciousnessState,
        StateTransition,
        TransitionTrigger,
        create_consciousness_event,
    )
except ModuleNotFoundError:  # pragma: no cover - support legacy invocation
    from philosophy_neo4j import PhilosophyNeo4jWriter
    from philosophy_schema import (  # type: ignore[no-redef]
        ConsciousnessNode,
        ConsciousnessState,
        StateTransition,
        TransitionTrigger,
        create_consciousness_event,
    )


def log_message(title, content=None):
    """Выводит отформатированное сообщение"""
    print(f"\n=== {title} ===")
    if content:
        if isinstance(content, (dict, list)):
            try:
                print(json.dumps(content, indent=2, ensure_ascii=False))
            except:
                print(str(content))
        else:
            print(str(content))


def create_philosophical_transitions(writer, users=None):
    """
    Создает философские переходы состояний сознания.

    Args:
        writer: Neo4j Writer
        users: Список пользователей для создания переходов
    """
    log_message("СОЗДАНИЕ ФИЛОСОФСКИХ ПЕРЕХОДОВ СОЗНАНИЯ")

    # Philosophy First transition sequence
    transitions_to_create = [
        (
            ConsciousnessState.TRANSITION_LIMINAL,
            ConsciousnessState.PRESENCE_NOW,
            "Переход к полному присутствию в моменте",
            TransitionTrigger.PRESENCE_PRACTICE,
        ),
        (
            ConsciousnessState.PRESENCE_NOW,
            ConsciousnessState.HARMONY_BALANCE,
            "Достижение гармонии через медитацию",
            TransitionTrigger.MEDITATION,
        ),
        (
            ConsciousnessState.HARMONY_BALANCE,
            ConsciousnessState.HOME_AUTHENTIC,
            "Дом - это ты, когда искренен с собой",
            TransitionTrigger.HOME_RECOGNITION,
        ),
        (
            ConsciousnessState.HOME_AUTHENTIC,
            ConsciousnessState.QUESTION_SPACE,
            "Мы научились задавать правильные вопросы",
            TransitionTrigger.QUESTION_REFRAMING,
        ),
        (
            ConsciousnessState.QUESTION_SPACE,
            ConsciousnessState.RESONANCE_MOMENT,
            "Момент резонанса сознания",
            TransitionTrigger.RESONANCE_ALIGNMENT,
        ),
        (
            ConsciousnessState.RESONANCE_MOMENT,
            ConsciousnessState.REFLECTION_SELF,
            "Саморефлексия после момента резонанса",
            TransitionTrigger.PHILOSOPHICAL_INSIGHT,
        ),
        (
            ConsciousnessState.REFLECTION_SELF,
            ConsciousnessState.INTEGRATION_WISDOM,
            "Интеграция полученной мудрости",
            TransitionTrigger.SYNCHRONICITY,
        ),
    ]

    # Create users for testing resonance
    if not users:
        users = [
            "philosopher_1",
            "philosopher_2",
            "consciousness_explorer",
            "authentic_self",
        ]

    created_transitions = []

    for source_state, target_state, description, trigger in transitions_to_create:
        for user_id in users:
            try:
                # Create source node with philosophical metrics
                source_node = ConsciousnessNode(
                    id=str(uuid.uuid4()),
                    state=source_state,
                    timestamp=datetime.now(),
                    presence_level=random.uniform(0.4, 0.6),
                    harmony_index=random.uniform(0.4, 0.6),
                    authenticity_score=random.uniform(0.4, 0.6),
                    home_resonance=random.uniform(0.4, 0.6),
                    question_clarity=random.uniform(0.4, 0.6),
                    resonance_strength=random.uniform(0.4, 0.6),
                    user_id=user_id,
                    meta={
                        "philosophical": True,
                        "description": f"{source_state.name} state",
                    },
                )

                # Create target node with improved philosophical metrics
                target_node = ConsciousnessNode(
                    id=str(uuid.uuid4()),
                    state=target_state,
                    timestamp=datetime.now(),
                    presence_level=random.uniform(0.6, 0.9),
                    harmony_index=random.uniform(0.6, 0.9),
                    authenticity_score=random.uniform(0.6, 0.9),
                    home_resonance=random.uniform(0.6, 0.9),
                    question_clarity=random.uniform(0.6, 0.9),
                    resonance_strength=random.uniform(0.6, 0.9),
                    user_id=user_id,
                    meta={
                        "philosophical": True,
                        "description": f"{target_state.name} state",
                    },
                )

                # Create transition between nodes
                transition = StateTransition(
                    id=str(uuid.uuid4()),
                    source_id=source_node.id,
                    target_id=target_node.id,
                    trigger=trigger,
                    timestamp=datetime.now(),
                    presence_delta=target_node.presence_level
                    - source_node.presence_level,
                    harmony_delta=target_node.harmony_index - source_node.harmony_index,
                    authenticity_delta=target_node.authenticity_score
                    - source_node.authenticity_score,
                    resonance_delta=target_node.resonance_strength
                    - source_node.resonance_strength,
                    philosophical_significance=description,
                    user_id=user_id,
                    meta={"philosophical": True},
                )

                # Save to Neo4j
                writer.create_consciousness_node(source_node)
                writer.create_consciousness_node(target_node)
                writer.create_state_transition(transition)

                log_message(
                    f"Создан переход для {user_id}",
                    f"{source_state.name} → {target_state.name}",
                )
                print(f"  Триггер: {trigger.name}")
                print(f"  Описание: {description}")

                created_transitions.append(transition.id)

                # Небольшая пауза для временной дисперсии
                time.sleep(0.2)

            except Exception as e:
                log_message("Ошибка создания перехода", str(e))
                traceback.print_exc()

    return created_transitions


def broadcast_transitions_to_websocket(writer, transition_ids):
    """
    Отправляет переходы состояний сознания в WebSocket relay.

    Args:
        writer: Neo4j Writer
        transition_ids: Список ID переходов для отправки
    """
    log_message("ОТПРАВКА ПЕРЕХОДОВ В WEBSOCKET RELAY")

    websocket_url = "http://localhost:8080/events"
    log_message("WebSocket URL", websocket_url)

    successful = 0

    for transition_id in transition_ids:
        try:
            # Получаем детали перехода
            transition_details = writer.get_transition_details(transition_id)
            if not transition_details:
                log_message("Переход не найден", f"ID: {transition_id}")
                continue

            # Создаем событие для WebSocket
            event = {
                "event_type": "consciousness_transition",
                "timestamp": datetime.now().isoformat(),
                "user_id": transition_details.get("user_id", "unknown"),
                "source_state": transition_details.get("source_state", "unknown"),
                "target_state": transition_details.get("target_state", "unknown"),
                "trigger": transition_details.get("trigger", "UNKNOWN"),
                "meta": {
                    "description": transition_details.get(
                        "philosophical_significance", ""
                    ),
                    "philosophical": True,
                    "presence_delta": transition_details.get("presence_delta", 0),
                    "harmony_delta": transition_details.get("harmony_delta", 0),
                    "authenticity_delta": transition_details.get(
                        "authenticity_delta", 0
                    ),
                    "resonance_delta": transition_details.get("resonance_delta", 0),
                },
            }

            # Отправляем в WebSocket relay через HTTP
            log_message(f"Отправка события в WebSocket", f"ID: {transition_id}")
            print(json.dumps(event, indent=2, ensure_ascii=False))

            response = requests.post(
                websocket_url, json=event, headers={"Content-Type": "application/json"}
            )

            if response.status_code == 202:  # Accepted
                log_message("Событие отправлено", f"Код ответа: {response.status_code}")
                successful += 1
            else:
                log_message("Ошибка отправки", f"Код ответа: {response.status_code}")
                print(f"Ответ: {response.text}")

            # Пауза между отправками
            time.sleep(1)

        except Exception as e:
            log_message("Ошибка трансляции события", str(e))
            traceback.print_exc()

    log_message("ИТОГО ОТПРАВЛЕНО", f"{successful} из {len(transition_ids)}")


def analyze_philosophy_patterns(writer):
    """
    Анализирует философские паттерны в Temporal Data Lake.

    Args:
        writer: Neo4j Writer
    """
    log_message("АНАЛИЗ ФИЛОСОФСКИХ ПАТТЕРНОВ")

    try:
        # Анализ временных паттернов
        patterns = writer.analyze_temporal_patterns(days=7)
        log_message(f"Найдено {len(patterns)} паттернов")

        for i, pattern in enumerate(patterns[:5], 1):
            log_message(
                f"Паттерн #{i}: {pattern['source']} → {pattern['target']}",
                f"Частота: {pattern.get('frequency')}, Тип: {pattern.get('pattern_type')}",
            )
            print(
                f"  Философская значимость: {pattern.get('philosophical_significance', 'Н/Д')}"
            )
            print(f"  Первое появление: {pattern.get('first_occurrence', 'Н/Д')}")
            print(f"  Последнее появление: {pattern.get('last_occurrence', 'Н/Д')}")

    except Exception as e:
        log_message("Ошибка анализа паттернов", str(e))
        traceback.print_exc()


def find_philosophy_resonance(writer):
    """
    Находит моменты философского резонанса между пользователями.

    Args:
        writer: Neo4j Writer
    """
    log_message("ПОИСК МОМЕНТОВ ФИЛОСОФСКОГО РЕЗОНАНСА")

    try:
        # Поиск моментов резонанса
        resonance_moments = writer.find_resonance_moments(hours=24)
        log_message(f"Найдено {len(resonance_moments)} моментов резонанса")

        for i, moment in enumerate(resonance_moments[:5], 1):
            log_message(
                f"Резонанс #{i}: {moment.get('user1')} и {moment.get('user2')}",
                f"Состояние: {moment.get('to_state')}, Тип: {moment.get('resonance_type')}",
            )
            print(
                f"  Философское значение: {moment.get('philosophical_meaning', 'Н/Д')}"
            )
            print(
                f"  Разница во времени: {moment.get('time_diff_seconds', 'Н/Д')} секунд"
            )
            print(f"  Время 1: {moment.get('timestamp1', 'Н/Д')}")
            print(f"  Время 2: {moment.get('timestamp2', 'Н/Д')}")

    except Exception as e:
        log_message("Ошибка поиска резонансов", str(e))
        traceback.print_exc()


def get_philosophy_timeline(writer):
    """
    Получает временную линию философских состояний сознания.

    Args:
        writer: Neo4j Writer
    """
    log_message("ВРЕМЕННАЯ ЛИНИЯ ФИЛОСОФСКИХ СОСТОЯНИЙ")

    try:
        # Получение временной линии
        timeline = writer.get_consciousness_timeline(hours=24)
        nodes = timeline.get("nodes", [])
        links = timeline.get("links", [])

        log_message(
            f"Временная линия содержит {len(nodes)} состояний и {len(links)} переходов"
        )

        if nodes and links:
            log_message("Примеры переходов:", "")
            for link in links[:3]:
                source = link.get("source", "Н/Д")
                target = link.get("target", "Н/Д")
                source_state = next(
                    (n.get("state", "unknown") for n in nodes if n.get("id") == source),
                    "unknown",
                )
                target_state = next(
                    (n.get("state", "unknown") for n in nodes if n.get("id") == target),
                    "unknown",
                )

                print(f"  {source_state} → {target_state}")
                print(f"    Триггер: {link.get('trigger', 'Н/Д')}")
                print(
                    f"    Философская значимость: {link.get('philosophical_significance', 'Н/Д')}"
                )
                print(f"    Время: {link.get('timestamp', 'Н/Д')}")

            log_message("Примеры состояний:", "")
            for node in nodes[:3]:
                print(f"  {node.get('state', 'Н/Д')} ({node.get('label', 'Н/Д')})")
                print(f"    Присутствие: {node.get('presenceLevel', 0):.2f}")
                print(f"    Гармония: {node.get('harmonyIndex', 0):.2f}")
                print(f"    Аутентичность: {node.get('authenticityScore', 0):.2f}")
                print(f"    Резонанс дома: {node.get('homeResonance', 0):.2f}")
                print(f"    Ясность вопросов: {node.get('questionClarity', 0):.2f}")
        else:
            log_message("Временная линия пуста", "Не найдены состояния и переходы")

    except Exception as e:
        log_message("Ошибка получения временной линии", str(e))
        traceback.print_exc()


def check_websocket_relay():
    """Проверяет доступность WebSocket relay сервера"""
    log_message("ПРОВЕРКА WEBSOCKET RELAY")

    try:
        # Проверяем API endpoint
        response = requests.get("http://localhost:8080/api/consciousness/graph")

        if response.status_code == 200:
            log_message("WebSocket API доступен", "Получен философский граф")
            try:
                data = response.json()
                print(f"  Узлов: {len(data.get('nodes', []))}")
                print(f"  Связей: {len(data.get('links', []))}")
                print(json.dumps(data, indent=2, ensure_ascii=False)[:500] + "...")
            except:
                print("  Ошибка разбора JSON")
                print(response.text[:200])
        else:
            log_message("Ошибка API запроса", f"Код ответа: {response.status_code}")

        # Проверяем endpoint событий
        test_event = {
            "event_type": "test_event",
            "timestamp": datetime.now().isoformat(),
            "message": "Philosophy First WebSocket test",
        }

        response = requests.post(
            "http://localhost:8080/events",
            json=test_event,
            headers={"Content-Type": "application/json"},
        )

        if response.status_code == 202:
            log_message(
                "Endpoint событий доступен", f"Код ответа: {response.status_code}"
            )
        else:
            log_message(
                "Ошибка отправки тестового события",
                f"Код ответа: {response.status_code}",
            )

    except Exception as e:
        log_message("Ошибка подключения к WebSocket relay", str(e))
        print("Убедитесь, что Go WebSocket relay запущен на порту 8080")


def main():
    """Главная функция скрипта"""
    print("=== Philosophy First - Neo4j Temporal Data Lake Integration ===")

    parser = argparse.ArgumentParser(description="Philosophy First Neo4j Integration")
    parser.add_argument(
        "--uri", type=str, default="bolt://localhost:7687", help="Neo4j URI"
    )
    parser.add_argument("--user", type=str, default="neo4j", help="Neo4j user")
    parser.add_argument(
        "--password", type=str, default="NewStrongPass123!", help="Neo4j password"
    )
    parser.add_argument(
        "--create", action="store_true", help="Create philosophical transitions"
    )
    parser.add_argument(
        "--broadcast", action="store_true", help="Broadcast transitions to WebSocket"
    )
    parser.add_argument(
        "--analyze", action="store_true", help="Analyze philosophical patterns"
    )
    parser.add_argument(
        "--resonance", action="store_true", help="Find resonance moments"
    )
    parser.add_argument(
        "--timeline", action="store_true", help="Get consciousness timeline"
    )
    parser.add_argument("--check", action="store_true", help="Check WebSocket relay")
    parser.add_argument("--all", action="store_true", help="Run all operations")
    args = parser.parse_args()

    # Проверяем доступность WebSocket relay если запрошено
    if args.check or args.all:
        check_websocket_relay()

    # Инициализируем Neo4j writer
    try:
        log_message("ПОДКЛЮЧЕНИЕ К NEO4J", f"URI: {args.uri}")
        writer = PhilosophyNeo4jWriter(
            uri=args.uri, user=args.user, password=args.password
        )
        log_message("Neo4j подключен успешно")
    except Exception as e:
        log_message("Ошибка подключения к Neo4j", str(e))
        traceback.print_exc()
        sys.exit(1)

    try:
        transition_ids = []

        # Создаем философские переходы
        if args.create or args.all:
            transition_ids = create_philosophical_transitions(writer)

        # Транслируем переходы через WebSocket
        if transition_ids and (args.broadcast or args.all):
            broadcast_transitions_to_websocket(writer, transition_ids)

        # Анализируем философские паттерны
        if args.analyze or args.all:
            analyze_philosophy_patterns(writer)

        # Ищем моменты резонанса
        if args.resonance or args.all:
            find_philosophy_resonance(writer)

        # Получаем временную линию
        if args.timeline or args.all:
            get_philosophy_timeline(writer)

    finally:
        # Закрываем соединение с Neo4j
        writer.close()
        log_message("Соединение с Neo4j закрыто")


if __name__ == "__main__":
    main()
