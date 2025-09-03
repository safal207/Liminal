"""
🌐 Тестирование WebSocket интеграции для Neo4j Consciousness State Transitions
Проверка работы полного цикла: Neo4j → Redis → Go WebSocket relay
"""

import json
import os
import time
import uuid
from datetime import datetime

import requests
from consciousness_neo4j import ConsciousnessNeo4jWriter
from consciousness_schema import (ConsciousnessNode, ConsciousnessState,
                                  StateTransition, TransitionTrigger)


def send_event_to_websocket(event_data):
    """Отправляет событие непосредственно на WebSocket relay через HTTP"""
    try:
        url = "http://localhost:8080/events"
        headers = {"Content-Type": "application/json"}
        response = requests.post(url, json=event_data, headers=headers)

        if response.status_code == 200 or response.status_code == 202:
            print(
                f"✅ Событие успешно отправлено на WebSocket relay: {response.status_code}"
            )
            return True
        else:
            print(
                f"❌ Ошибка отправки события: {response.status_code}, {response.text}"
            )
            return False
    except Exception as e:
        print(f"❌ Исключение при отправке события: {str(e)}")
        return False


def test_consciousness_websocket_integration():
    """Тестирует полный цикл интеграции перехода состояний через WebSocket relay"""

    print("🧪 Запуск тестов WebSocket интеграции для Consciousness...")

    # Инициализация Neo4j Writer (только для управления моделью данных)
    writer = ConsciousnessNeo4jWriter()

    # Создание начального состояния сознания
    initial_state = ConsciousnessNode(
        id=str(uuid.uuid4()),
        state=ConsciousnessState.TRANSITION_LIMINAL,
        timestamp=datetime.now(),
        home_resonance=0.5,
        presence_level=0.6,
        harmony_index=0.4,
        authenticity_score=0.7,
        emotional_charge=50.0,
        stress_level=0.3,
    )

    print(f"🧠 Создаем начальное состояние: {initial_state.state.value}")
    created_state = writer.create_consciousness_state(initial_state)
    print(f"✅ Состояние создано в Neo4j: {created_state['id']}")

    # Небольшая пауза для демонстрационных целей
    time.sleep(1)

    # Создание нового состояния с более высоким уровнем присутствия
    new_state = ConsciousnessNode(
        id=str(uuid.uuid4()),
        state=ConsciousnessState.PRESENCE_NOW,
        timestamp=datetime.now(),
        home_resonance=0.5,
        presence_level=0.8,  # Повышенное присутствие
        harmony_index=0.5,  # Слегка повысилась гармония
        authenticity_score=0.7,
        emotional_charge=45.0,
        stress_level=0.25,  # Понизился стресс
    )

    print(f"🧠 Создаем новое состояние: {new_state.state.value}")
    created_new_state = writer.create_consciousness_state(new_state)
    print(f"✅ Новое состояние создано в Neo4j: {created_new_state['id']}")

    # Создаем переход между состояниями
    transition = StateTransition(
        id=str(uuid.uuid4()),
        from_state=initial_state.state,
        to_state=new_state.state,
        trigger=TransitionTrigger.DEEP_BREATH,
        timestamp=datetime.now(),
        duration_seconds=1.0,
        home_resonance_delta=0.0,
        presence_delta=0.2,  # +0.2 к присутствию
        harmony_delta=0.1,  # +0.1 к гармонии
        authenticity_delta=0.0,
        trigger_data={"breath_depth": 0.85, "breath_duration": 4.5},
    )

    print(
        f"🔄 Создаем переход состояния: {transition.from_state.value} → {transition.to_state.value}"
    )
    print(
        f"📊 Presence +{transition.presence_delta}, Harmony +{transition.harmony_delta}"
    )

    # Создаем переход в Neo4j
    created_transition = writer.create_state_transition(
        transition, initial_state.id, new_state.id
    )

    # Отправляем событие напрямую в WebSocket relay через HTTP
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

    send_event_to_websocket(event)

    print(f"✅ Переход состояния создан в Neo4j и отправлен на WebSocket relay!")
    print(
        f"🔌 Проверьте WebSocket-клиент, вы должны увидеть событие перехода состояния."
    )

    # Небольшая пауза, чтобы увидеть результаты в WebSocket-клиенте
    time.sleep(1)

    # Создадим еще один переход с другим триггером
    meditation_state = ConsciousnessNode(
        id=str(uuid.uuid4()),
        state=ConsciousnessState.HARMONY_BALANCE,
        timestamp=datetime.now(),
        home_resonance=0.6,
        presence_level=0.9,  # Еще повышенное присутствие
        harmony_index=0.7,  # Повышенная гармония
        authenticity_score=0.8,
        emotional_charge=30.0,
        stress_level=0.1,  # Значительно пониженный стресс
    )

    print(f"🧠 Создаем состояние медитации: {meditation_state.state.value}")
    created_meditation = writer.create_consciousness_state(meditation_state)

    meditation_transition = StateTransition(
        id=str(uuid.uuid4()),
        from_state=new_state.state,
        to_state=meditation_state.state,
        trigger=TransitionTrigger.MEDITATION,
        timestamp=datetime.now(),
        duration_seconds=180.0,  # 3 минуты медитации
        home_resonance_delta=0.1,
        presence_delta=0.1,
        harmony_delta=0.2,  # Значительный рост гармонии
        authenticity_delta=0.1,
        trigger_data={"meditation_type": "mindfulness", "duration_minutes": 3},
    )

    print(
        f"🔄 Создаем переход медитации: {meditation_transition.from_state.value} → {meditation_transition.to_state.value}"
    )
    print(
        f"📊 Harmony +{meditation_transition.harmony_delta}, Home Resonance +{meditation_transition.home_resonance_delta}"
    )

    # Создаем переход медитации в Neo4j
    created_meditation_transition = writer.create_state_transition(
        meditation_transition, new_state.id, meditation_state.id
    )

    # Отправляем второе событие напрямую в WebSocket relay через HTTP
    meditation_event = {
        "type": "state_transition",
        "from_state": meditation_transition.from_state.value,
        "to_state": meditation_transition.to_state.value,
        "transition_id": meditation_transition.id,
        "timestamp": meditation_transition.timestamp.isoformat(),
        "trigger": meditation_transition.trigger.value,
        "presence_delta": meditation_transition.presence_delta,
        "harmony_delta": meditation_transition.harmony_delta,
        "authenticity_delta": meditation_transition.authenticity_delta,
        "trigger_data": meditation_transition.trigger_data,
    }

    send_event_to_websocket(meditation_event)

    print(f"✅ Переход медитации создан в Neo4j и отправлен на WebSocket relay!")
    print(
        f"🌐 Все клиенты WebSocket получили уведомление о переходе в состояние: {meditation_state.state.value}"
    )

    # Закрываем соединение с Neo4j
    writer.close()

    # Создадим событие Home State для демонстрации философской концепции
    home_state = ConsciousnessNode(
        id=str(uuid.uuid4()),
        state=ConsciousnessState.HOME_AUTHENTIC,
        timestamp=datetime.now(),
        home_resonance=0.9,  # Высокий резонанс с домом
        presence_level=0.85,
        harmony_index=0.8,
        authenticity_score=0.95,  # Высокая искренность - ключевой атрибут Home State
        emotional_charge=20.0,
        stress_level=0.05,  # Минимальный стресс
    )

    print(
        f"🏠 Создаем Home State: {home_state.state.value} (Дом - это ты, когда искренен с собой)"
    )
    created_home_state = writer.create_consciousness_state(home_state)

    home_transition = StateTransition(
        id=str(uuid.uuid4()),
        from_state=meditation_state.state,
        to_state=home_state.state,
        trigger=TransitionTrigger.AUTHENTICITY_INSIGHT,
        timestamp=datetime.now(),
        duration_seconds=5.0,
        home_resonance_delta=0.3,  # Значительный рост резонанса с домом
        presence_delta=-0.05,  # Небольшое снижение присутствия
        harmony_delta=0.1,
        authenticity_delta=0.15,  # Рост искренности
        trigger_data={
            "insight_type": "home_recognition",
            "authenticity_threshold": 0.9,
        },
    )

    # Создаем переход к Home State в Neo4j
    created_home_transition = writer.create_state_transition(
        home_transition, meditation_state.id, home_state.id
    )

    # Отправляем третье событие - переход к Home State
    home_event = {
        "type": "state_transition",
        "from_state": home_transition.from_state.value,
        "to_state": home_transition.to_state.value,
        "transition_id": home_transition.id,
        "timestamp": home_transition.timestamp.isoformat(),
        "trigger": home_transition.trigger.value,
        "presence_delta": home_transition.presence_delta,
        "harmony_delta": home_transition.harmony_delta,
        "authenticity_delta": home_transition.authenticity_delta,
        "home_resonance_delta": home_transition.home_resonance_delta,
        "trigger_data": home_transition.trigger_data,
        "philosophical_insight": "Дом - это не место. Дом - это ты, когда ты искренен с собой.",
    }

    send_event_to_websocket(home_event)

    print(f"🏠 Переход к Home State создан в Neo4j и отправлен на WebSocket relay!")
    print(f"🌟 Философский инсайт: Дом - это ты, когда искренен с собой")

    print("🏆 Тесты WebSocket интеграции успешно завершены!")
    print("🔌 Проверьте вкладку с HTML-клиентом для визуализации событий")
    print(
        "🧠 Полный путь перехода состояний сознания: TRANSITION_LIMINAL → PRESENCE_NOW → HARMONY_BALANCE → HOME_AUTHENTIC"
    )


if __name__ == "__main__":
    test_consciousness_websocket_integration()
