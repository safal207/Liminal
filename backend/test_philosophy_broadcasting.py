#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Philosophy First: Тест Broadcasting состояний сознания через WebSocket
"""

import json
import time
import traceback
from datetime import datetime

import requests

# Конфигурация
WS_RELAY_URL = "http://localhost:8080/events"

# Философские состояния сознания
STATES = {
    "HOME_AUTHENTIC": "Дом - это ты, когда искренен с собой",
    "QUESTION_SPACE": "Мы научились задавать правильные вопросы",
    "PRESENCE_NOW": "Полное присутствие в настоящем моменте",
    "HARMONY_BALANCE": "Гармония как равновесие противоположностей",
    "RESONANCE_MOMENT": "Резонанс - момент синхронизации сознаний",
}


def print_styled(header, message, style=None):
    """Стилизованный вывод в консоль"""
    styles = {
        "success": "\033[92m",  # Зеленый
        "info": "\033[94m",  # Синий
        "warning": "\033[93m",  # Желтый
        "error": "\033[91m",  # Красный
        "bold": "\033[1m",  # Жирный
        "reset": "\033[0m",  # Сброс
    }

    style_code = styles.get(style, styles["info"])
    reset = styles["reset"]
    bold = styles["bold"]

    print(f"{bold}[{header}]{reset} {style_code}{message}{reset}")


def create_consciousness_event(source_state, target_state, user_id="philosopher_1"):
    """Создание события перехода состояния сознания"""
    timestamp = datetime.now().isoformat()

    event = {
        "event_type": "consciousness_transition",
        "timestamp": timestamp,
        "user_id": user_id,
        "source_state": source_state,
        "target_state": target_state,
        "meta": {
            "description": f"Переход из {source_state} в {target_state}",
            "philosophical": True,
            "source_description": STATES.get(source_state, ""),
            "target_description": STATES.get(target_state, ""),
            "intensity": 0.85,
        },
    }

    return event


def send_event(event):
    """Отправка события в Go WebSocket relay"""
    print_styled(
        "Событие", f"Отправка {event['source_state']} → {event['target_state']}"
    )

    try:
        response = requests.post(WS_RELAY_URL, json=event)
        if response.status_code == 202:  # Accepted
            print_styled(
                "Успех",
                f"Событие принято сервером (код {response.status_code})",
                "success",
            )
            return True
        else:
            print_styled("Ошибка", f"Сервер вернул код {response.status_code}", "error")
            return False
    except Exception as e:
        print_styled("Ошибка", f"Не удалось отправить событие: {e}", "error")
        return False


def test_philosophy_transitions():
    """Тест переходов состояний сознания в философской последовательности"""
    # Последовательность переходов из Philosophy First концепции
    transitions = [
        ("TRANSITION_LIMINAL", "PRESENCE_NOW"),
        ("PRESENCE_NOW", "HARMONY_BALANCE"),
        ("HARMONY_BALANCE", "HOME_AUTHENTIC"),
        ("HOME_AUTHENTIC", "QUESTION_SPACE"),
        ("QUESTION_SPACE", "RESONANCE_MOMENT"),
    ]

    print_styled("Начало", "Тестирование Philosophy First переходов сознания", "bold")

    for i, (source, target) in enumerate(transitions):
        print_styled(
            "Переход", f"{i+1}/{len(transitions)}: {source} → {target}", "info"
        )

        # Создание и отправка события
        event = create_consciousness_event(source, target)
        success = send_event(event)

        if success:
            print_styled(
                "Трансляция",
                f"Переход {source} → {target} транслируется через WebSocket",
                "success",
            )
        else:
            print_styled(
                "Ошибка",
                f"Не удалось транслировать переход {source} → {target}",
                "error",
            )

        # Пауза между отправками
        time.sleep(2)

    print_styled("Завершено", "Все переходы Philosophy First протестированы", "bold")


def test_api_endpoints():
    """Тестирование всех API эндпоинтов Go WebSocket relay"""
    endpoints = {
        "consciousness_graph": WS_RELAY_URL.replace(
            "/events", "/api/consciousness/graph"
        ),
        "consciousness_api": WS_RELAY_URL.replace("/events", "/api/consciousness"),
        "events_api": WS_RELAY_URL,
        "root": WS_RELAY_URL.replace("/events", "/"),
    }

    print_styled("API Tests", "Проверка всех API эндпоинтов", "bold")

    for name, url in endpoints.items():
        try:
            print_styled("Запрос", f"GET {url}", "info")
            response = requests.get(url)
            print_styled("Ответ", f"Код: {response.status_code}", "info")

            if response.status_code == 200:
                try:
                    content_type = response.headers.get("Content-Type", "")
                    if "application/json" in content_type:
                        data = response.json()
                        print_styled(
                            "JSON",
                            json.dumps(data, indent=2, ensure_ascii=False)[:500]
                            + "...",
                            "success",
                        )
                    else:
                        print_styled("Текст", response.text[:200] + "...", "success")
                except Exception as e:
                    print_styled(
                        "Парсинг", f"Не удалось обработать ответ: {e}", "warning"
                    )
            else:
                print_styled(
                    "Внимание",
                    f"Неожиданный код ответа: {response.status_code}",
                    "warning",
                )
        except Exception as e:
            print_styled("Ошибка", f"Не удалось выполнить запрос к {url}: {e}", "error")
            traceback.print_exc()

    print_styled("API Tests", "Проверка API эндпоинтов завершена", "bold")


if __name__ == "__main__":
    print("=== Philosophy First: WebSocket Broadcasting Test ===")

    # Проверка доступности Go WebSocket relay через тестирование всех эндпоинтов
    try:
        print_styled(
            "Диагностика", "Тестирование всех API эндпоинтов Go WebSocket relay", "info"
        )
        test_api_endpoints()

        # Прямая отправка запроса с детальным выводом
        print_styled("Диагностика", "Детальный запрос к API графа сознания", "info")
        response = requests.get(
            WS_RELAY_URL.replace("/events", "/api/consciousness/graph")
        )

        print(f"Status Code: {response.status_code}")
        print(f"Headers: {dict(response.headers)}")
        print(f"Content: {response.text}")

        # Если API работает, запускаем тест философских переходов
        if response.status_code == 200:
            print_styled(
                "Готово", "Go WebSocket relay доступен, запускаем тест", "success"
            )
            test_philosophy_transitions()
        else:
            print_styled(
                "Внимание",
                f"Go WebSocket relay вернул код {response.status_code}, но продолжаем тестирование",
                "warning",
            )
            test_philosophy_transitions()

    except Exception as e:
        print_styled("Ошибка", f"Произошла ошибка: {e}", "error")
        traceback.print_exc()

        # Запускаем тест философских переходов в любом случае
        print_styled("Восстановление", "Продолжаем тест несмотря на ошибки", "warning")
        try:
            test_philosophy_transitions()
        except Exception as e2:
            print_styled(
                "Критическая ошибка", f"Не удалось запустить тест: {e2}", "error"
            )
            traceback.print_exc()
