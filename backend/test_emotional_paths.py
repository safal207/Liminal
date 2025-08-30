"""
🌊 Тестирование эмоциональных путей и нелинейных переходов сознания
Интеграция с Neo4j и WebSocket для визуализации сложных эмоциональных паттернов
"""

import time
import uuid
from datetime import datetime

from consciousness_neo4j import ConsciousnessNeo4jWriter
from consciousness_schema import (
    ConsciousnessNode,
    ConsciousnessState,
    StateTransition,
    TransitionTrigger,
)


def create_emotion_transition_path(
    writer, from_state, to_state, trigger, emotion_data=None, delay=1.0
):
    """Создает переход между эмоциональными состояниями с дополнительными данными"""

    # Настройка метаданных для эмоционального перехода
    trigger_data = emotion_data or {}

    # Создание начального состояния
    from_node = ConsciousnessNode(
        id=str(uuid.uuid4()),
        state=from_state,
        timestamp=datetime.now(),
        home_resonance=0.5 + (0.3 * (trigger_data.get("intensity", 0.5))),
        presence_level=0.6,
        harmony_index=0.4,
        authenticity_score=0.7,
        emotional_charge=50.0 + (20.0 * (trigger_data.get("intensity", 0.5))),
        stress_level=0.3 + (0.4 * (trigger_data.get("stress_factor", 0.2))),
        location=trigger_data.get("location"),
        activity=trigger_data.get("activity"),
        companions=trigger_data.get("companions"),
    )

    print(f"🧠 Создаем состояние: {from_state.value}")
    created_from = writer.create_consciousness_state(from_node)
    print(f"✅ Состояние создано в Neo4j: {created_from['id']}")

    # Небольшая пауза для демонстрационных целей
    time.sleep(delay)

    # Создание нового состояния после перехода
    to_node = ConsciousnessNode(
        id=str(uuid.uuid4()),
        state=to_state,
        timestamp=datetime.now(),
        home_resonance=0.5 + (0.35 * (trigger_data.get("intensity", 0.5))),
        presence_level=0.7,
        harmony_index=0.5,
        authenticity_score=0.75,
        emotional_charge=45.0,
        stress_level=0.25,
        location=trigger_data.get("location"),
        activity=trigger_data.get("activity"),
        companions=trigger_data.get("companions"),
    )

    print(f"🧠 Создаем состояние: {to_state.value}")
    created_to = writer.create_consciousness_state(to_node)
    print(f"✅ Состояние создано в Neo4j: {created_to['id']}")

    # Создаем переход между состояниями
    transition = StateTransition(
        id=str(uuid.uuid4()),
        from_state=from_state,
        to_state=to_state,
        trigger=trigger,
        timestamp=datetime.now(),
        duration_seconds=trigger_data.get("duration_seconds", 2.0),
        home_resonance_delta=0.05,
        presence_delta=0.1,
        harmony_delta=0.1,
        authenticity_delta=0.05,
        trigger_data=trigger_data,
    )

    print(
        f"🔄 Создаем эмоциональный переход: {transition.from_state.value} → {transition.to_state.value}"
    )
    print(f"📊 Триггер: {transition.trigger.value}")

    # Создаем переход в Neo4j и отправляем событие
    created_transition = writer.create_state_transition(
        transition, created_from["id"], created_to["id"]
    )

    return created_transition


def test_emotional_consciousness_paths():
    """Тестирует нелинейные эмоциональные пути сознания"""

    print("🌊 Запуск тестов эмоциональных путей сознания...")
    print("🔌 Убедитесь, что Neo4j и Go WebSocket relay запущены")

    # Инициализация Neo4j Writer
    writer = ConsciousnessNeo4jWriter()

    # 1. Путь Сомнение → Любопытство
    print("\n1️⃣ ЭМОЦИОНАЛЬНЫЙ ПУТЬ: Творческое сомнение → Поток любопытства")
    create_emotion_transition_path(
        writer,
        ConsciousnessState.DOUBT_CREATIVE,
        ConsciousnessState.CURIOUS_FLOW,
        TransitionTrigger.CREATIVE_DOUBT,
        emotion_data={
            "doubt_type": "existential_question",
            "curiosity_subject": "consciousness_nature",
            "intensity": 0.8,
            "duration_seconds": 5.0,
            "insight": "Сомнение - это начало мудрости, а не её конец",
            "location": "study_room",
            "activity": "philosophical_reading",
        },
    )
    time.sleep(2)

    # 2. Путь Гнев → Установка границ
    print("\n2️⃣ ЭМОЦИОНАЛЬНЫЙ ПУТЬ: Защитный гнев → Целостность границ")
    create_emotion_transition_path(
        writer,
        ConsciousnessState.ANGER_PROTECTION,
        ConsciousnessState.BOUNDARY_INTEGRITY,
        TransitionTrigger.BOUNDARY_SETTING,
        emotion_data={
            "trigger_event": "boundary_violation",
            "boundary_type": "emotional_space",
            "intensity": 0.7,
            "stress_factor": 0.6,
            "resolution_quality": "clear_communication",
            "insight": "Гнев - сигнал о нарушении личных границ",
            "companions": ["partner", "friend"],
        },
    )
    time.sleep(2)

    # 3. Путь Перегрузка → Уход для защиты
    print("\n3️⃣ ЭМОЦИОНАЛЬНЫЙ ПУТЬ: Перегрузка → Защитное уединение")
    create_emotion_transition_path(
        writer,
        ConsciousnessState.HARMONY_BALANCE,
        ConsciousnessState.WITHDRAWAL_PROTECTION,
        TransitionTrigger.OVERWHELM_DETECTED,
        emotion_data={
            "trigger_event": "social_overwhelm",
            "intensity": 0.9,
            "stress_factor": 0.8,
            "recovery_strategy": "conscious_solitude",
            "insight": "Уход - акт самозаботы, а не избегания",
            "location": "home",
            "duration_seconds": 30.0,
        },
    )
    time.sleep(2)

    # 4. Путь Горе → Трансформация → Благодарность
    print("\n4️⃣ ЭМОЦИОНАЛЬНЫЙ ПУТЬ: Горе → Трансформация → Благодарность")

    # 4.1 Горе → Трансформация
    create_emotion_transition_path(
        writer,
        ConsciousnessState.GRIEF_TRANSFORMATION,
        ConsciousnessState.HOME_AUTHENTIC,
        TransitionTrigger.GRIEF_ACKNOWLEDGED,
        emotion_data={
            "grief_source": "meaningful_loss",
            "intensity": 0.85,
            "stress_factor": 0.7,
            "insight": "В глубине потери находится семя роста",
            "duration_seconds": 15.0,
        },
    )
    time.sleep(2)

    # 4.2 Трансформация → Благодарность
    create_emotion_transition_path(
        writer,
        ConsciousnessState.HOME_AUTHENTIC,
        ConsciousnessState.GRATITUDE_PRESENCE,
        TransitionTrigger.GRATITUDE_PRACTICE,
        emotion_data={
            "gratitude_focus": "life_meaning",
            "intensity": 0.75,
            "stress_factor": 0.2,
            "insight": "Благодарность - проявление полноты, а не отрицание потери",
            "duration_seconds": 10.0,
        },
    )

    print("\n✨ Эмоциональные пути сознания успешно созданы и визуализированы!")
    print("🌿 Исследуйте нелинейные связи между состояниями в Neo4j и HTML-клиенте")
    print("🔄 Интеграция эмоциональных путей с философскими концепциями завершена")


if __name__ == "__main__":
    test_emotional_consciousness_paths()
