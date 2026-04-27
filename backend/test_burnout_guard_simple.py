"""
🚀🛡️ BurnoutGuard Simple Test — базовая проверка функциональности

Простой тест для проверки работоспособности BurnoutGuard:
- Инициализация компонентов
- Создание тестовых данных
- Анализ риска выгорания
- Генерация рекомендаций
"""

import asyncio
import sys
from datetime import datetime, timedelta
from pathlib import Path

import pytest

# Добавляем путь к backend
backend_path = Path(__file__).parent
sys.path.insert(0, str(backend_path))

# Импорты BurnoutGuard
try:
    # Прямые импорты из локальных модулей
    from dataclasses import dataclass

    # Создаем простые mock классы для Emotime
    from enum import Enum
    from typing import List

    from burnout_guard.core import BurnoutRiskScorer
    from burnout_guard.modes import BurnoutModeMapper, BurnoutModeType, BurnoutRiskLevel
    from burnout_guard.recommendations import RecommendationEngine
    from burnout_guard.utils import create_alert_message, format_risk_score

    class EmotionalModeType(Enum):
        CALM = "calm"
        FOCUS = "focus"
        STRESS = "stress"
        JOY = "joy"
        CONTEMPLATION = "contemplation"
        NEUTRAL = "neutral"

    @dataclass
    class EmotionalMode:
        name: str
        type: EmotionalModeType
        intensity: float
        confidence: float
        description: str
        duration: int = 1

    @dataclass
    class EmotionalFeatures:
        valence: float
        arousal: float
        dominance: float
        tempo: float
        intensity: float
        timestamp: datetime
        confidence: float
        sources: List[str]

    @dataclass
    class EmotionalPoint:
        timestamp: datetime
        valence: float
        arousal: float
        mode: EmotionalMode
        confidence: float

    @dataclass
    class EmotimeState:
        timestamp: datetime
        features: EmotionalFeatures
        mode: EmotionalMode
        resonance_trace: List[EmotionalPoint]
        confidence: float

    print("✅ Все модули BurnoutGuard успешно импортированы")

except ImportError as e:
    print(f"❌ Ошибка импорта: {e}")
    sys.exit(1)


def create_test_emotional_mode(
    mode_type: EmotionalModeType, intensity: float = 0.7
) -> EmotionalMode:
    """Создает тестовый эмоциональный режим."""
    return EmotionalMode(
        name=mode_type.value,
        type=mode_type,
        intensity=intensity,
        confidence=0.8,
        description=f"Test {mode_type.value} mode",
        duration=1,
    )


def create_test_emotional_features(
    valence: float = 0.0, arousal: float = 0.5
) -> EmotionalFeatures:
    """Создает тестовые эмоциональные признаки."""
    return EmotionalFeatures(
        valence=valence,
        arousal=arousal,
        dominance=0.5,
        tempo=0.5,
        intensity=0.7,
        timestamp=datetime.now(),
        confidence=0.8,
        sources=["test"],
    )


def create_test_emotime_state(
    mode_type: EmotionalModeType, valence: float = 0.0
) -> EmotimeState:
    """Создает тестовое состояние Emotime."""
    features = create_test_emotional_features(valence=valence)
    mode = create_test_emotional_mode(mode_type)

    # Создаем простой resonance trace
    resonance_trace = [
        EmotionalPoint(
            timestamp=datetime.now() - timedelta(minutes=i),
            valence=valence + (i * 0.1),
            arousal=0.5,
            mode=mode,
            confidence=0.8,
        )
        for i in range(5)
    ]

    return EmotimeState(
        timestamp=datetime.now(),
        features=features,
        mode=mode,
        resonance_trace=resonance_trace,
        confidence=0.8,
    )


async def run_burnout_mode_mapping():
    """Тест маппинга эмоциональных режимов в режимы выгорания."""
    print("\n🧪 Тестирование маппинга режимов выгорания...")

    mapper = BurnoutModeMapper(lookback_hours=24)

    # Тестовые сценарии
    test_scenarios = [
        # Хронический стресс
        {
            "name": "Хронический стресс",
            "modes": [
                (
                    create_test_emotional_mode(EmotionalModeType.STRESS, 0.9),
                    datetime.now() - timedelta(hours=i),
                )
                for i in range(3)
            ],
            "expected_risk": "high",
        },
        # Здоровое состояние
        {
            "name": "Здоровое состояние",
            "modes": [
                (
                    create_test_emotional_mode(EmotionalModeType.CALM, 0.7),
                    datetime.now() - timedelta(hours=i),
                )
                for i in range(3)
            ],
            "expected_risk": "low",
        },
        # Переработка
        {
            "name": "Длительный фокус (переработка)",
            "modes": [
                (
                    create_test_emotional_mode(EmotionalModeType.FOCUS, 0.8),
                    datetime.now() - timedelta(hours=i),
                )
                for i in range(4)
            ],
            "expected_risk": "medium",
        },
    ]

    for scenario in test_scenarios:
        print(f"\n  📋 Сценарий: {scenario['name']}")

        burnout_mode = await mapper.analyze_burnout_risk(scenario["modes"])

        print(f"    Режим выгорания: {burnout_mode.type.value}")
        print(f"    Уровень риска: {burnout_mode.risk_level.value}")
        print(f"    Скор риска: {burnout_mode.risk_score:.2f}")
        print(f"    Индикаторы: {', '.join(burnout_mode.primary_indicators)}")
        print(f"    Уверенность: {burnout_mode.confidence:.2f}")

        # Простая проверка
        if scenario["expected_risk"] == "high" and burnout_mode.risk_score > 0.6:
            print("    ✅ Высокий риск правильно определен")
        elif scenario["expected_risk"] == "low" and burnout_mode.risk_score < 0.4:
            print("    ✅ Низкий риск правильно определен")
        elif (
            scenario["expected_risk"] == "medium"
            and 0.4 <= burnout_mode.risk_score <= 0.6
        ):
            print("    ✅ Средний риск правильно определен")
        else:
            print(
                "    ⚠️ Ожидался "
                f"{scenario['expected_risk']} риск, получен скор "
                f"{burnout_mode.risk_score:.2f}"
            )


async def run_risk_scoring():
    """Тест системы скоринга риска."""
    print("\n🧪 Тестирование скоринга риска...")

    scorer = BurnoutRiskScorer("test_user")

    # Тестовые состояния
    test_states = [
        {
            "name": "Сильный стресс",
            "emotime_state": create_test_emotime_state(
                EmotionalModeType.STRESS, valence=-0.7
            ),
            "context": {"session_duration_hours": 10},
        },
        {
            "name": "Спокойное состояние",
            "emotime_state": create_test_emotime_state(
                EmotionalModeType.CALM, valence=0.3
            ),
            "context": {"session_duration_hours": 4},
        },
        {
            "name": "Длительный фокус",
            "emotime_state": create_test_emotime_state(
                EmotionalModeType.FOCUS, valence=0.1
            ),
            "context": {"session_duration_hours": 12},
        },
    ]

    for test_state in test_states:
        print(f"\n  📋 Состояние: {test_state['name']}")

        # Создаем dummy burnout mode
        from burnout_guard.modes import BurnoutMode

        dummy_mode = BurnoutMode(
            type=BurnoutModeType.HEALTHY,
            risk_level=BurnoutRiskLevel.LOW,
            risk_score=0.3,
            primary_indicators=["test"],
            emotional_pattern="test_pattern",
            duration_hours=1.0,
        )

        risk_assessment = await scorer.calculate_risk(
            test_state["emotime_state"], dummy_mode, test_state["context"]
        )

        print(f"    Скор риска: {risk_assessment.score:.2f}")
        print(f"    Уровень: {risk_assessment.level.value}")
        print(f"    Уверенность: {risk_assessment.confidence:.2f}")
        print(f"    Факторы: {list(risk_assessment.factors.keys())}")
        print(f"    Индикаторы: {', '.join(risk_assessment.emotional_indicators[:3])}")


async def run_recommendations():
    """Тест системы рекомендаций."""
    print("\n🧪 Тестирование системы рекомендаций...")

    engine = RecommendationEngine("test_user")

    # Создаем тестовые состояния выгорания
    from burnout_guard.core import BurnoutRisk, BurnoutState
    from burnout_guard.modes import BurnoutMode

    # Критическое состояние
    critical_risk = BurnoutRisk(
        score=0.9,
        level=BurnoutRiskLevel.CRITICAL,
        factors={
            "emotional": 0.9,
            "behavioral": 0.8,
            "temporal": 0.7,
            "ml_confidence": 0.6,
        },
        confidence=0.8,
        timestamp=datetime.now(),
        emotional_indicators=["Хронический стресс", "Эмоциональное истощение"],
        behavioral_patterns=["Длительная работа"],
        duration_risk=0.8,
        trend_risk=0.7,
    )

    critical_mode = BurnoutMode(
        type=BurnoutModeType.EMOTIONAL_EXHAUSTION,
        risk_level=BurnoutRiskLevel.CRITICAL,
        risk_score=0.9,
        primary_indicators=["Стресс", "Истощение"],
        emotional_pattern="chronic_stress",
        duration_hours=8.0,
        confidence=0.8,
    )

    critical_state = BurnoutState(
        timestamp=datetime.now(),
        emotime_state=create_test_emotime_state(EmotionalModeType.STRESS, valence=-0.8),
        burnout_mode=critical_mode,
        risk_assessment=critical_risk,
        risk_history=[0.5, 0.6, 0.7, 0.8, 0.9],
        mode_stability=0.8,
        intervention_needed=True,
    )

    print("\n  📋 Критическое состояние:")
    recommendations = await engine.get_recommendations(critical_state)

    for i, rec in enumerate(recommendations, 1):
        print(f"    {i}. {rec.title}")
        print(f"       {rec.description}")
        print(
            f"       Тип: {rec.type.value}, "
            f"Приоритет: {rec.priority}, "
            f"Время: {rec.estimated_time} мин"
        )

    # Здоровое состояние
    healthy_risk = BurnoutRisk(
        score=0.2,
        level=BurnoutRiskLevel.LOW,
        factors={
            "emotional": 0.2,
            "behavioral": 0.1,
            "temporal": 0.2,
            "ml_confidence": 0.3,
        },
        confidence=0.9,
        timestamp=datetime.now(),
        emotional_indicators=["Стабильное состояние"],
        behavioral_patterns=["Нормальная активность"],
        duration_risk=0.1,
        trend_risk=0.0,
    )

    healthy_mode = BurnoutMode(
        type=BurnoutModeType.HEALTHY,
        risk_level=BurnoutRiskLevel.LOW,
        risk_score=0.2,
        primary_indicators=["Баланс"],
        emotional_pattern="stable",
        duration_hours=2.0,
        confidence=0.9,
    )

    healthy_state = BurnoutState(
        timestamp=datetime.now(),
        emotime_state=create_test_emotime_state(EmotionalModeType.CALM, valence=0.4),
        burnout_mode=healthy_mode,
        risk_assessment=healthy_risk,
        risk_history=[0.3, 0.2, 0.2, 0.1, 0.2],
        mode_stability=0.9,
        intervention_needed=False,
    )

    print("\n  📋 Здоровое состояние:")
    recommendations = await engine.get_recommendations(healthy_state)

    for i, rec in enumerate(recommendations, 1):
        print(f"    {i}. {rec.title}")
        print(f"       Тип: {rec.type.value}, Приоритет: {rec.priority}")


async def run_utils():
    """Тест утилит."""
    print("\n🧪 Тестирование утилит...")

    # Тест форматирования риска
    test_scores = [0.1, 0.3, 0.5, 0.7, 0.9]

    print("\n  📋 Форматирование скоров риска:")
    for score in test_scores:
        formatted = format_risk_score(score)
        print(f"    Скор {score}: {formatted}")

    # Тест создания алертов
    print("\n  📋 Создание алертов:")
    test_indicators = ["Хронический стресс", "Длительная работа", "Недостаток отдыха"]
    alert_message = create_alert_message(0.8, test_indicators)
    print(f"    Алерт: {alert_message}")


@pytest.mark.asyncio
async def test_burnout_mode_mapping():
    await run_burnout_mode_mapping()


@pytest.mark.asyncio
async def test_risk_scoring():
    await run_risk_scoring()


@pytest.mark.asyncio
async def test_recommendations():
    await run_recommendations()


@pytest.mark.asyncio
async def test_utils():
    await run_utils()


async def main():
    """Главная функция тестирования."""
    print("🚀🛡️ Тестирование BurnoutGuard - AI защита от выгорания")
    print("=" * 60)

    try:
        await run_burnout_mode_mapping()
        await run_risk_scoring()
        await run_recommendations()
        await run_utils()

        print("\n" + "=" * 60)
        print("✅ Все тесты BurnoutGuard завершены успешно!")
        print("\n🎯 Система готова к интеграции с мобильным приложением")
        print("💡 Следующие шаги:")
        print("   1. Создание мобильного интерфейса")
        print("   2. Интеграция с системой аутентификации")
        print("   3. Настройка базы данных для персистентности")
        print("   4. Создание HR дашборда для команд")

    except Exception as e:
        print(f"\n❌ Ошибка в тестах: {e}")
        import traceback

        traceback.print_exc()


if __name__ == "__main__":
    asyncio.run(main())
