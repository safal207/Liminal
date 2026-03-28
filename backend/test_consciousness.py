"""
🧪 Тестирование Consciousness Temporal Data Lake
"""

import json
from datetime import datetime

from consciousness_schema import (
    PHILOSOPHICAL_THRESHOLDS,
    ConsciousnessNode,
    ConsciousnessState,
    TransitionTrigger,
)


def test_consciousness_node():
    """Тест создания узла сознания"""
    node = ConsciousnessNode(
        id="test-node-1",
        state=ConsciousnessState.HOME_AUTHENTIC,
        timestamp=datetime.now(),
        home_resonance=0.85,
        presence_level=0.7,
        harmony_index=0.6,
        authenticity_score=0.9,
        emotional_charge=50.0,
        stress_level=0.3,
        location="home",
        activity="meditation",
        companions=["self"],
    )

    # Проверка преобразования в Neo4j формат
    neo4j_dict = node.to_neo4j_dict()
    print("🧠 ConsciousnessNode создан:")
    print(json.dumps(neo4j_dict, indent=2))

    # Проверка философских метрик
    is_home = (
        neo4j_dict["home_resonance"] > PHILOSOPHICAL_THRESHOLDS["HOME_RESONANCE_HIGH"]
    )
    is_authentic = (
        neo4j_dict["authenticity_score"]
        > PHILOSOPHICAL_THRESHOLDS["AUTHENTICITY_HONEST"]
    )

    print(f"🏠 В состоянии 'дома': {is_home}")
    print(f"✨ Высокая искренность: {is_authentic}")

    return node


def simulate_consciousness_journey():
    """Симуляция путешествия сознания"""
    print("\n🌌 Симуляция путешествия сознания:")

    # Начальное состояние
    states = []
    current = ConsciousnessNode(
        id="journey-1",
        state=ConsciousnessState.TRANSITION_LIMINAL,
        timestamp=datetime.now(),
        home_resonance=0.5,
        presence_level=0.4,
        harmony_index=0.3,
        authenticity_score=0.6,
        emotional_charge=70.0,
        stress_level=0.6,
        location="unknown",
        activity="searching",
        companions=[],
    )
    states.append(current)
    print(
        f"1. {current.state.value}: home_resonance={current.home_resonance}, presence={current.presence_level}"
    )

    # Переход к присутствию
    current = ConsciousnessNode(
        id="journey-2",
        state=ConsciousnessState.PRESENCE_NOW,
        timestamp=datetime.now(),
        home_resonance=0.5,
        presence_level=0.8,  # Рост присутствия
        harmony_index=0.4,
        authenticity_score=0.7,
        emotional_charge=65.0,
        stress_level=0.5,
        location="here",
        activity="breathing",
        companions=[],
    )
    states.append(current)
    print(
        f"2. {current.state.value}: home_resonance={current.home_resonance}, presence={current.presence_level}"
    )

    # Переход к вопросам
    current = ConsciousnessNode(
        id="journey-3",
        state=ConsciousnessState.QUESTION_SPACE,
        timestamp=datetime.now(),
        home_resonance=0.6,
        presence_level=0.8,
        harmony_index=0.5,
        authenticity_score=0.8,  # Рост искренности
        emotional_charge=60.0,
        stress_level=0.4,
        location="mind",
        activity="questioning",
        companions=[],
    )
    states.append(current)
    print(
        f"3. {current.state.value}: home_resonance={current.home_resonance}, authenticity={current.authenticity_score}"
    )

    # Переход к дому
    current = ConsciousnessNode(
        id="journey-4",
        state=ConsciousnessState.HOME_AUTHENTIC,
        timestamp=datetime.now(),
        home_resonance=0.9,  # Высокий резонанс дома
        presence_level=0.8,
        harmony_index=0.7,
        authenticity_score=0.9,  # Высокая искренность
        emotional_charge=40.0,
        stress_level=0.2,
        location="self",
        activity="being",
        companions=["true self"],
    )
    states.append(current)
    print(
        f"4. {current.state.value}: home_resonance={current.home_resonance}, authenticity={current.authenticity_score}"
    )

    # Переход к гармонии
    current = ConsciousnessNode(
        id="journey-5",
        state=ConsciousnessState.HARMONY_BALANCE,
        timestamp=datetime.now(),
        home_resonance=0.9,
        presence_level=0.9,
        harmony_index=0.9,  # Высокая гармония
        authenticity_score=0.9,
        emotional_charge=30.0,
        stress_level=0.1,
        location="everywhere",
        activity="flowing",
        companions=["universe"],
    )
    states.append(current)
    print(
        f"5. {current.state.value}: harmony={current.harmony_index}, stress={current.stress_level}"
    )

    return states


def generate_cypher_examples():
    """Генерация примеров Cypher запросов"""
    print("\n📊 Примеры Cypher запросов для Neo4j:")

    # Создание узла сознания
    print("\n1. Создание узла сознания:")
    print("""
    CREATE (c:ConsciousnessState {
        id: "journey-5",
        state: "harmony_balance",
        timestamp: datetime(),
        home_resonance: 0.9,
        presence_level: 0.9,
        harmony_index: 0.9,
        authenticity_score: 0.9,
        emotional_charge: 30.0,
        stress_level: 0.1,
        location: "everywhere",
        activity: "flowing",
        companions: ["universe"]
    })
    RETURN c
    """)

    # Создание перехода
    print("\n2. Создание перехода между состояниями:")
    print("""
    MATCH (from:ConsciousnessState {id: "journey-4"})
    MATCH (to:ConsciousnessState {id: "journey-5"})
    CREATE (from)-[t:TRANSITIONS_TO {
        id: "transition-4-5",
        trigger: "harmony_recognized",
        timestamp: datetime(),
        duration_seconds: 3.5,
        home_resonance_delta: 0.0,
        presence_delta: 0.1,
        harmony_delta: 0.2,
        authenticity_delta: 0.0,
        trigger_data: {
            source: "meditation",
            intensity: 0.8
        }
    }]->(to)
    RETURN t
    """)

    # Анализ временных паттернов
    print("\n3. Анализ временных паттернов:")
    print("""
    MATCH (from:ConsciousnessState)-[t:TRANSITIONS_TO]->(to:ConsciousnessState)
    WHERE t.timestamp > datetime() - duration('P7D')
    RETURN 
        from.state as from_state,
        to.state as to_state,
        t.trigger as trigger,
        count(*) as frequency,
        avg(t.duration_seconds) as avg_duration,
        avg(t.harmony_delta) as avg_harmony_change
    ORDER BY frequency DESC
    """)

    # Поиск резонанса
    print("\n4. Поиск резонанса между пользователями:")
    print("""
    MATCH (c1:ConsciousnessState), (c2:ConsciousnessState)
    WHERE c1.id <> c2.id
    AND abs(duration.between(c1.timestamp, c2.timestamp).seconds) < 60
    AND c1.state = c2.state
    AND c1.home_resonance > 0.7 AND c2.home_resonance > 0.7
    RETURN c1, c2, 
           abs(c1.home_resonance - c2.home_resonance) as resonance_similarity
    ORDER BY resonance_similarity ASC
    LIMIT 10
    """)


if __name__ == "__main__":
    print("🌌 Тестирование Consciousness Temporal Data Lake")
    print("=" * 50)

    try:
        # Тест узла сознания
        print("Testing consciousness node...")
        node = test_consciousness_node()
        print("Node test completed successfully")

        # Симуляция путешествия
        print("Simulating consciousness journey...")
        states = simulate_consciousness_journey()
        print("Journey simulation completed successfully")

        # Примеры Cypher
        print("Generating Cypher examples...")
        generate_cypher_examples()
        print("Cypher examples generated successfully")

        print("\n✅ Тестирование завершено успешно!")
        print("Когда Neo4j будет готов, можно будет запустить полную интеграцию.")
    except Exception as e:
        print(f"Error during test execution: {e}")
        import traceback

        traceback.print_exc()
