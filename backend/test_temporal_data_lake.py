#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Neo4j Temporal Data Lake Демонстрация
------------------------------------
Philosophy First подход к анализу состояний сознания во времени
"""

import argparse
import json
import sys
from datetime import datetime
from typing import Any, Dict

try:
    from backend.consciousness_neo4j import (
        ConsciousnessEventProcessor,
        ConsciousnessNeo4jWriter,
    )
except ModuleNotFoundError:  # pragma: no cover - compatibility with legacy layout
    from consciousness_neo4j import (  # type: ignore[no-redef]
        ConsciousnessEventProcessor,
        ConsciousnessNeo4jWriter,
    )


def print_styled(title, content, style="info"):
    """Стилизованный вывод в консоль"""
    styles = {
        "header": "\033[95m",
        "info": "\033[94m",
        "success": "\033[92m",
        "warning": "\033[93m",
        "error": "\033[91m",
        "bold": "\033[1m",
        "underline": "\033[4m",
        "end": "\033[0m",
    }

    print(
        f"\n{styles.get('bold', '')}{styles.get(style, '')}== {title} =={styles.get('end', '')}"
    )
    if isinstance(content, (dict, list)):
        print(json.dumps(content, indent=2, ensure_ascii=False))
    else:
        print(content)


def analyze_temporal_patterns(writer, days=7):
    """Анализ временных паттернов переходов сознания"""
    print_styled(
        "АНАЛИЗ ВРЕМЕННЫХ ПАТТЕРНОВ СОЗНАНИЯ", f"За последние {days} дней", "header"
    )

    patterns = writer.analyze_temporal_patterns(days=days)

    if not patterns:
        print_styled(
            "Паттерны не найдены",
            "В базе данных недостаточно информации о переходах",
            "warning",
        )
        return

    print_styled(f"Найдено паттернов: {len(patterns)}", "", "success")

    for i, pattern in enumerate(patterns[:5], 1):
        print_styled(
            f"Паттерн #{i}: {pattern['source']} → {pattern['target']}",
            f"Частота: {pattern['frequency']}, Тип: {pattern['pattern_type']}",
            "info",
        )
        print(f"  Философское значение: {pattern['philosophical_significance']}")
        print(f"  Медианное время: {pattern.get('median_hour', 'н/д')} часов")
        print(f"  Первое появление: {pattern.get('first_occurrence', 'н/д')}")
        print(f"  Последнее появление: {pattern.get('last_occurrence', 'н/д')}")
        print("-" * 60)


def find_resonance_moments(writer, hours=24):
    """Поиск моментов резонанса между пользователями"""
    print_styled(
        "ПОИСК МОМЕНТОВ РЕЗОНАНСА СОЗНАНИЯ", f"За последние {hours} часов", "header"
    )

    resonance_moments = writer.find_resonance_moments(hours=hours)

    if not resonance_moments:
        print_styled(
            "Резонансные моменты не найдены",
            "В базе данных недостаточно информации о синхронных переходах",
            "warning",
        )
        return

    print_styled(
        f"Найдено резонансных моментов: {len(resonance_moments)}", "", "success"
    )

    for i, moment in enumerate(resonance_moments[:5], 1):
        print_styled(
            f"Резонанс #{i}: {moment['user1']} и {moment['user2']}",
            f"Состояние: {moment['to_state']}, Тип: {moment['resonance_type']}",
            "info",
        )
        print(f"  Философское значение: {moment['philosophical_meaning']}")
        print(f"  Разница во времени: {moment['time_diff_seconds']} секунд")
        print(f"  Timestamp 1: {moment['timestamp1']}")
        print(f"  Timestamp 2: {moment['timestamp2']}")
        print("-" * 60)


def get_consciousness_timeline(writer, hours=24, user_id=None):
    """Получение и анализ временной линии сознания"""
    print_styled("ВРЕМЕННАЯ ЛИНИЯ СОЗНАНИЯ", f"За последние {hours} часов", "header")

    timeline = writer.get_consciousness_timeline(hours=hours, user_id=user_id)

    if not timeline or not timeline.get("nodes") or not timeline.get("links"):
        print_styled(
            "Временная линия пуста",
            "В базе данных недостаточно информации о переходах",
            "warning",
        )
        return

    meta = timeline.get("meta", {})
    nodes = timeline.get("nodes", [])
    links = timeline.get("links", [])

    print_styled("Мета-информация", "", "bold")
    print(f"  Всего состояний: {meta.get('total_states', 0)}")
    print(f"  Всего переходов: {meta.get('total_transitions', 0)}")
    print(f"  Доминирующее состояние: {meta.get('dominant_state', 'н/д')}")
    print(
        f"  Философская интерпретация: {meta.get('philosophical_interpretation', 'н/д')}"
    )

    print_styled("Тренды:", "", "info")
    print(f"  Присутствие: {meta.get('presence_trend', 0):.2f}")
    print(f"  Гармония: {meta.get('harmony_trend', 0):.2f}")
    print(f"  Аутентичность: {meta.get('authenticity_trend', 0):.2f}")

    print_styled("Распределение состояний:", "", "info")
    for state, count in meta.get("state_distribution", {}).items():
        print(f"  {state}: {count}")

    print_styled("Примеры переходов:", "", "info")
    for link in links[:3]:
        print(f"  {link.get('source', '')} → {link.get('target', '')}")
        print(f"    Триггер: {link.get('trigger', 'н/д')}")
        print(
            f"    Философское значение: {link.get('philosophical_significance', 'н/д')}"
        )
        print(f"    Время: {link.get('timestamp', 'н/д')}")

    print_styled("Примеры состояний:", "", "info")
    for node in nodes[:3]:
        print(f"  {node.get('state', 'н/д')} ({node.get('label', 'н/д')})")
        print(f"    Присутствие: {node.get('presenceLevel', 0):.2f}")
        print(f"    Гармония: {node.get('harmonyIndex', 0):.2f}")
        print(f"    Аутентичность: {node.get('authenticityScore', 0):.2f}")


def simulate_transition_example(writer):
    """Создание примера перехода для тестирования"""
    import uuid

    from consciousness_schema import (
        ConsciousnessNode,
        ConsciousnessState,
        StateTransition,
        TransitionTrigger,
    )

    print_styled("СИМУЛЯЦИЯ ПЕРЕХОДА СОЗНАНИЯ", "", "header")

    # Создаем начальное состояние
    current_state = ConsciousnessNode(
        id=str(uuid.uuid4()),
        state=ConsciousnessState.TRANSITION_LIMINAL,
        timestamp=datetime.now(),
        home_resonance=0.5,
        presence_level=0.3,
        harmony_index=0.2,
        authenticity_score=0.4,
        emotional_charge=65.0,
        stress_level=0.6,
    )

    # Создаем состояние назначения
    target_state = ConsciousnessNode(
        id=str(uuid.uuid4()),
        state=ConsciousnessState.PRESENCE_NOW,
        timestamp=datetime.now(),
        home_resonance=0.5,
        presence_level=0.8,  # Повышение присутствия
        harmony_index=0.3,
        authenticity_score=0.5,  # Небольшое повышение подлинности
        emotional_charge=40.0,  # Снижение эмоционального заряда
        stress_level=0.4,  # Снижение стресса
    )

    # Создаем переход
    transition = StateTransition(
        id=str(uuid.uuid4()),
        from_state=current_state.state,
        to_state=target_state.state,
        trigger=TransitionTrigger.MEDITATION_PRACTICE,
        timestamp=datetime.now(),
        duration_seconds=600.0,  # 10 минут медитации
        home_resonance_delta=0.0,
        presence_delta=target_state.presence_level - current_state.presence_level,
        harmony_delta=target_state.harmony_index - current_state.harmony_index,
        authenticity_delta=target_state.authenticity_score
        - current_state.authenticity_score,
        trigger_data={"practice_type": "mindfulness", "duration_minutes": 10},
    )

    # Сохраняем в Neo4j
    try:
        node1 = writer.create_consciousness_state(current_state)
        node2 = writer.create_consciousness_state(target_state)
        trans = writer.create_state_transition(
            transition, current_state.id, target_state.id
        )

        print_styled(
            "Переход успешно создан",
            f"{current_state.state.value} → {target_state.state.value}",
            "success",
        )
        print(f"  Триггер: {transition.trigger.value}")
        print(f"  Дельта присутствия: +{transition.presence_delta:.2f}")
        print(f"  Дельта гармонии: +{transition.harmony_delta:.2f}")
        print(f"  Дельта аутентичности: +{transition.authenticity_delta:.2f}")

    except Exception as e:
        print_styled("Ошибка создания перехода", str(e), "error")


def main():
    print("Запуск Neo4j Temporal Data Lake демонстрации...")
    parser = argparse.ArgumentParser(
        description="Neo4j Temporal Data Lake демонстрация"
    )
    parser.add_argument("--days", type=int, default=7, help="Период анализа в днях")
    parser.add_argument("--hours", type=int, default=24, help="Период анализа в часах")
    parser.add_argument("--user", type=str, help="ID пользователя для фильтрации")
    parser.add_argument(
        "--simulate", action="store_true", help="Симулировать переход для тестирования"
    )
    parser.add_argument(
        "--analyze", action="store_true", help="Анализировать временные паттерны"
    )
    parser.add_argument(
        "--resonance", action="store_true", help="Искать резонансные моменты"
    )
    parser.add_argument(
        "--timeline", action="store_true", help="Получить временную линию"
    )
    parser.add_argument("--all", action="store_true", help="Запустить все функции")
    parser.add_argument(
        "--docker", action="store_true", default=True, help="Использовать Docker Neo4j"
    )
    parser.add_argument(
        "--uri", type=str, default="bolt://localhost:7687", help="Neo4j URI"
    )
    args = parser.parse_args()
    print(f"Аргументы: {vars(args)}")

    # Инициализация Neo4j Writer
    try:
        print("Попытка подключения к Neo4j...")
        print(f"URI: {args.uri}")

        # Используем явные параметры подключения для Docker
        if args.docker:
            # Настройки для подключения к Docker контейнеру
            writer = ConsciousnessNeo4jWriter(
                uri=args.uri, user="neo4j", password="NewStrongPass123!"
            )
            print("Используется Docker Neo4j контейнер")
        else:
            # Стандартные настройки из переменных окружения
            writer = ConsciousnessNeo4jWriter()
            print("Используются настройки по умолчанию")

        print("Neo4j Writer создан")
        print_styled("Neo4j соединение", "Установлено успешно", "success")
    except Exception as e:
        print(f"ОШИБКА: {e}")
        print_styled("Ошибка подключения к Neo4j", str(e), "error")
        print("\nПроверьте, что Neo4j Docker контейнер запущен:")
        print("docker ps | findstr neo4j")
        import traceback

        traceback.print_exc()
        sys.exit(1)

    try:
        if args.simulate or args.all:
            simulate_transition_example(writer)

        if args.analyze or args.all:
            analyze_temporal_patterns(writer, days=args.days)

        if args.resonance or args.all:
            find_resonance_moments(writer, hours=args.hours)

        if args.timeline or args.all:
            get_consciousness_timeline(writer, hours=args.hours, user_id=args.user)

    finally:
        writer.close()
        print_styled("Neo4j соединение", "Закрыто", "info")


if __name__ == "__main__":
    main()
