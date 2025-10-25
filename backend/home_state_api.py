"""
Home State API - endpoints для мониторинга состояния "дома в себе".

Философская концепция: "Дом — это ты, когда ты искренен с собой"
"""

from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional

from fastapi import APIRouter, HTTPException, Query, status
from fastapi.responses import JSONResponse
from backend.home_state_detector import HomeStateMetrics, home_state_detector
from backend.logging_config import get_logger
from pydantic import BaseModel


class HomeStateResponse(BaseModel):
    """Ответ с текущим состоянием дома."""

    user_id: str
    authenticity_level: float
    home_state_label: str
    is_home: bool
    confidence: float
    timestamp: datetime

    # Детальные метрики
    consistency_score: float
    presence_index: float
    self_alignment: float
    emotional_congruence: float
    reflection_depth: float


class HomeStateTrendResponse(BaseModel):
    """Ответ с трендом состояния дома."""

    user_id: str
    period_days: int
    measurements_count: int
    avg_authenticity: float
    avg_presence: float
    home_percentage: float
    trend_direction: str  # "improving", "declining", "stable"
    measurements: List[HomeStateResponse]


class HomeStateAnalyticsResponse(BaseModel):
    """Аналитика состояния дома."""

    user_id: str
    total_measurements: int
    days_tracked: int
    best_authenticity_score: float
    avg_authenticity: float
    home_sessions_count: int
    longest_home_streak: int
    authenticity_trend: str
    recommendations: List[str]


router = APIRouter(prefix="/home-state", tags=["home-state"])
logger = get_logger(__name__)


@router.get("/current/{user_id}", response_model=HomeStateResponse)
async def get_current_home_state(user_id: str) -> HomeStateResponse:
    """
    Получить текущее состояние 'дома в себе' для пользователя.

    Args:
        user_id: ID пользователя

    Returns:
        HomeStateResponse: Текущие метрики состояния дома
    """

    current_state = home_state_detector.get_user_home_state(user_id)

    if not current_state:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Состояние дома для пользователя {user_id} не найдено",
        )

    return HomeStateResponse(
        user_id=user_id,
        authenticity_level=current_state.authenticity_level,
        home_state_label=current_state.home_state_label,
        is_home=current_state.is_home,
        confidence=current_state.confidence,
        timestamp=current_state.timestamp,
        consistency_score=current_state.consistency_score,
        presence_index=current_state.presence_index,
        self_alignment=current_state.self_alignment,
        emotional_congruence=current_state.emotional_congruence,
        reflection_depth=current_state.reflection_depth,
    )


@router.get("/trend/{user_id}", response_model=HomeStateTrendResponse)
async def get_home_state_trend(
    user_id: str,
    days: int = Query(
        default=7, ge=1, le=30, description="Количество дней для анализа тренда"
    ),
) -> HomeStateTrendResponse:
    """
    Получить тренд состояния 'дома в себе' за указанный период.

    Args:
        user_id: ID пользователя
        days: Количество дней для анализа (1-30)

    Returns:
        HomeStateTrendResponse: Тренд метрик за период
    """

    trend_data = home_state_detector.get_home_state_trend(user_id, days)

    if not trend_data:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Данные тренда для пользователя {user_id} за последние {days} дней не найдены",
        )

    # Рассчитываем статистики
    avg_authenticity = sum(m.authenticity_level for m in trend_data) / len(trend_data)
    avg_presence = sum(m.presence_index for m in trend_data) / len(trend_data)
    home_percentage = sum(1 for m in trend_data if m.is_home) / len(trend_data) * 100

    # Определяем направление тренда
    if len(trend_data) >= 3:
        recent_avg = sum(m.authenticity_level for m in trend_data[-3:]) / 3
        early_avg = sum(m.authenticity_level for m in trend_data[:3]) / 3

        if recent_avg > early_avg + 0.1:
            trend_direction = "improving"
        elif recent_avg < early_avg - 0.1:
            trend_direction = "declining"
        else:
            trend_direction = "stable"
    else:
        trend_direction = "insufficient_data"

    # Конвертируем метрики в ответы
    measurements = [
        HomeStateResponse(
            user_id=user_id,
            authenticity_level=m.authenticity_level,
            home_state_label=m.home_state_label,
            is_home=m.is_home,
            confidence=m.confidence,
            timestamp=m.timestamp,
            consistency_score=m.consistency_score,
            presence_index=m.presence_index,
            self_alignment=m.self_alignment,
            emotional_congruence=m.emotional_congruence,
            reflection_depth=m.reflection_depth,
        )
        for m in trend_data
    ]

    return HomeStateTrendResponse(
        user_id=user_id,
        period_days=days,
        measurements_count=len(trend_data),
        avg_authenticity=avg_authenticity,
        avg_presence=avg_presence,
        home_percentage=home_percentage,
        trend_direction=trend_direction,
        measurements=measurements,
    )


@router.get("/analytics/{user_id}", response_model=HomeStateAnalyticsResponse)
async def get_home_state_analytics(user_id: str) -> HomeStateAnalyticsResponse:
    """
    Получить аналитику состояния 'дома в себе' для пользователя.

    Args:
        user_id: ID пользователя

    Returns:
        HomeStateAnalyticsResponse: Подробная аналитика
    """

    # Получаем всю историю пользователя
    all_data = home_state_detector.home_state_history.get(user_id, [])

    if not all_data:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Аналитические данные для пользователя {user_id} не найдены",
        )

    # Рассчитываем статистики
    total_measurements = len(all_data)

    # Дни отслеживания
    first_measurement = min(m.timestamp for m in all_data)
    last_measurement = max(m.timestamp for m in all_data)
    days_tracked = (last_measurement - first_measurement).days + 1

    # Лучший и средний уровень аутентичности
    best_authenticity = max(m.authenticity_level for m in all_data)
    avg_authenticity = sum(m.authenticity_level for m in all_data) / total_measurements

    # Сессии "дома"
    home_sessions = sum(1 for m in all_data if m.is_home)

    # Самая длинная серия "дома"
    longest_streak = 0
    current_streak = 0
    for measurement in all_data:
        if measurement.is_home:
            current_streak += 1
            longest_streak = max(longest_streak, current_streak)
        else:
            current_streak = 0

    # Тренд аутентичности
    if len(all_data) >= 6:
        recent_third = all_data[-len(all_data) // 3 :]
        early_third = all_data[: len(all_data) // 3]

        recent_avg = sum(m.authenticity_level for m in recent_third) / len(recent_third)
        early_avg = sum(m.authenticity_level for m in early_third) / len(early_third)

        if recent_avg > early_avg + 0.15:
            authenticity_trend = "Значительное улучшение"
        elif recent_avg > early_avg + 0.05:
            authenticity_trend = "Улучшение"
        elif recent_avg < early_avg - 0.15:
            authenticity_trend = "Значительное снижение"
        elif recent_avg < early_avg - 0.05:
            authenticity_trend = "Снижение"
        else:
            authenticity_trend = "Стабильно"
    else:
        authenticity_trend = "Недостаточно данных"

    # Рекомендации
    recommendations = _generate_recommendations(
        all_data, avg_authenticity, home_sessions, total_measurements
    )

    return HomeStateAnalyticsResponse(
        user_id=user_id,
        total_measurements=total_measurements,
        days_tracked=days_tracked,
        best_authenticity_score=best_authenticity,
        avg_authenticity=avg_authenticity,
        home_sessions_count=home_sessions,
        longest_home_streak=longest_streak,
        authenticity_trend=authenticity_trend,
        recommendations=recommendations,
    )


@router.post("/analyze/{user_id}")
async def analyze_message_for_home_state(
    user_id: str, message_data: Dict[str, Any]
) -> HomeStateResponse:
    """
    Анализирует сообщение пользователя и обновляет состояние 'дома в себе'.

    Args:
        user_id: ID пользователя
        message_data: Данные сообщения для анализа

    Returns:
        HomeStateResponse: Обновленные метрики
    """

    try:
        # Анализируем сообщение
        metrics = await home_state_detector.analyze_user_behavior(user_id, message_data)

        logger.info(
            f"Проанализировано сообщение пользователя {user_id}: {metrics.home_state_label}"
        )

        return HomeStateResponse(
            user_id=user_id,
            authenticity_level=metrics.authenticity_level,
            home_state_label=metrics.home_state_label,
            is_home=metrics.is_home,
            confidence=metrics.confidence,
            timestamp=metrics.timestamp,
            consistency_score=metrics.consistency_score,
            presence_index=metrics.presence_index,
            self_alignment=metrics.self_alignment,
            emotional_congruence=metrics.emotional_congruence,
            reflection_depth=metrics.reflection_depth,
        )

    except Exception as e:
        logger.error(f"Ошибка анализа сообщения для {user_id}: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Ошибка анализа состояния дома: {str(e)}",
        )


@router.get("/export/{user_id}")
async def export_home_state_data(user_id: str) -> Dict[str, Any]:
    """
    Экспортирует все данные о состоянии 'дома в себе' для пользователя.

    Args:
        user_id: ID пользователя

    Returns:
        Dict: Полный экспорт данных
    """

    try:
        export_data = await home_state_detector.export_metrics(user_id)

        if not export_data["current_state"]["authenticity_level"]:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail=f"Данные для экспорта для пользователя {user_id} не найдены",
            )

        logger.info(f"Экспортированы данные состояния дома для пользователя {user_id}")

        return export_data

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Ошибка экспорта данных для {user_id}: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Ошибка экспорта данных: {str(e)}",
        )


def _generate_recommendations(
    measurements: List[HomeStateMetrics],
    avg_authenticity: float,
    home_sessions: int,
    total_measurements: int,
) -> List[str]:
    """Генерирует персональные рекомендации для пользователя."""

    recommendations = []

    # Анализ уровня аутентичности
    if avg_authenticity < 0.4:
        recommendations.append("Попробуйте чаще выражать свои истинные чувства и мысли")
        recommendations.append(
            "Обратите внимание на моменты, когда вы не искренни с собой"
        )
    elif avg_authenticity < 0.6:
        recommendations.append("Вы на правильном пути к большей искренности с собой")
        recommendations.append("Уделите время саморефлексии и самопознанию")
    else:
        recommendations.append("Отличный уровень аутентичности! Продолжайте быть собой")

    # Анализ частоты состояния "дома"
    home_percentage = (
        (home_sessions / total_measurements) * 100 if total_measurements > 0 else 0
    )

    if home_percentage < 30:
        recommendations.append("Найдите время для практик осознанности и медитации")
        recommendations.append(
            "Исследуйте, что помогает вам чувствовать себя 'дома в себе'"
        )
    elif home_percentage < 60:
        recommendations.append(
            "Замечательный прогресс! Продолжайте развивать связь с собой"
        )
    else:
        recommendations.append("Вы часто находитесь 'дома в себе' - это прекрасно!")

    # Анализ присутствия
    avg_presence = sum(m.presence_index for m in measurements) / len(measurements)
    if avg_presence < 0.5:
        recommendations.append(
            "Практикуйте присутствие в моменте - это ключ к аутентичности"
        )
        recommendations.append(
            "Попробуйте техники mindfulness для развития осознанности"
        )

    # Анализ саморефлексии
    avg_reflection = sum(m.reflection_depth for m in measurements) / len(measurements)
    if avg_reflection < 0.5:
        recommendations.append(
            "Задавайте себе больше вопросов о своих мотивах и чувствах"
        )
        recommendations.append("Ведите дневник для углубления саморефлексии")

    return recommendations
