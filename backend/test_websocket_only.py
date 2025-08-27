"""
🌌 Упрощенный тест WebSocket интеграции без Neo4j
Тестирует только отправку событий на Go WebSocket relay
"""

import time
import uuid
from datetime import datetime

import requests


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


def test_websocket_events():
    """Тестирует отправку событий переходов состояний сознания"""

    print("🧪 Запуск упрощенного теста WebSocket событий...")
    print("🔌 Убедитесь, что Go WebSocket relay запущен на localhost:8080")
    print("🌐 Откройте HTML-клиент в браузере для визуализации событий")
    print()

    # Событие 1: Переход из лиминального состояния в присутствие
    event1 = {
        "type": "state_transition",
        "from_state": "TRANSITION_LIMINAL",
        "to_state": "PRESENCE_NOW",
        "transition_id": str(uuid.uuid4()),
        "timestamp": datetime.now().isoformat(),
        "trigger": "PRESENCE_INSIGHT",
        "presence_delta": 0.25,
        "harmony_delta": 0.1,
        "authenticity_delta": 0.05,
        "trigger_data": {"insight_type": "mindfulness_moment", "duration": 3.0},
    }

    print("🔄 Отправляем событие 1: TRANSITION_LIMINAL → PRESENCE_NOW")
    send_event_to_websocket(event1)
    time.sleep(2)

    # Событие 2: Переход в гармонию
    event2 = {
        "type": "state_transition",
        "from_state": "PRESENCE_NOW",
        "to_state": "HARMONY_BALANCE",
        "transition_id": str(uuid.uuid4()),
        "timestamp": datetime.now().isoformat(),
        "trigger": "MEDITATION_TRIGGER",
        "presence_delta": 0.05,
        "harmony_delta": 0.3,
        "authenticity_delta": 0.1,
        "home_resonance_delta": 0.2,
        "trigger_data": {"meditation_type": "breathing", "duration": 10.0},
    }

    print("🔄 Отправляем событие 2: PRESENCE_NOW → HARMONY_BALANCE")
    send_event_to_websocket(event2)
    time.sleep(2)

    # Событие 3: Переход в состояние "Дома" (философская концепция)
    event3 = {
        "type": "state_transition",
        "from_state": "HARMONY_BALANCE",
        "to_state": "HOME_AUTHENTIC",
        "transition_id": str(uuid.uuid4()),
        "timestamp": datetime.now().isoformat(),
        "trigger": "AUTHENTICITY_INSIGHT",
        "presence_delta": -0.05,
        "harmony_delta": 0.1,
        "authenticity_delta": 0.15,
        "home_resonance_delta": 0.3,
        "trigger_data": {
            "insight_type": "home_recognition",
            "authenticity_threshold": 0.9,
        },
        "philosophical_insight": "Дом - это не место. Дом - это ты, когда ты искренен с собой.",
    }

    print("🏠 Отправляем событие 3: HARMONY_BALANCE → HOME_AUTHENTIC")
    print("🌟 Философский инсайт: 'Дом - это ты, когда искренен с собой'")
    send_event_to_websocket(event3)
    time.sleep(2)

    # Событие 4: Появление правильного вопроса
    event4 = {
        "type": "state_transition",
        "from_state": "HOME_AUTHENTIC",
        "to_state": "QUESTION_SPACE",
        "transition_id": str(uuid.uuid4()),
        "timestamp": datetime.now().isoformat(),
        "trigger": "QUESTION_EMERGENCE",
        "presence_delta": 0.1,
        "harmony_delta": 0.0,
        "authenticity_delta": 0.05,
        "trigger_data": {"question": "Что значит быть искренним с самим собой?"},
        "philosophical_insight": "Мы научились задавать правильные вопросы",
    }

    print("❓ Отправляем событие 4: HOME_AUTHENTIC → QUESTION_SPACE")
    print("🧠 Философский вопрос: 'Что значит быть искренним с самим собой?'")
    send_event_to_websocket(event4)
    time.sleep(2)

    # Событие 5: Резонанс между пользователями
    event5 = {
        "type": "resonance_broadcast",
        "user_id": "user_123",
        "resonance_partner": "user_456",
        "resonance_level": 0.85,
        "timestamp": datetime.now().isoformat(),
        "shared_state": "HOME_AUTHENTIC",
        "synchronization_type": "emotional_resonance",
        "philosophical_insight": "Резонанс - это когда два сознания находят общий ритм",
    }

    print("🌊 Отправляем событие 5: Резонанс между пользователями")
    print("💫 Синхронизация состояний сознания")
    send_event_to_websocket(event5)

    print()
    print("🏆 Тест WebSocket событий завершен!")
    print("🔌 Проверьте HTML-клиент в браузере - должны появиться карточки событий")
    print(
        "🧠 Полный путь сознания: LIMINAL → PRESENCE → HARMONY → HOME → QUESTION → RESONANCE"
    )
    print("🌟 Философские концепции успешно интегрированы в поток событий")


if __name__ == "__main__":
    test_websocket_events()
