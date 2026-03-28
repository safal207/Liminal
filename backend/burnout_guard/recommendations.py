"""
🚀🛡️ BurnoutGuard Recommendations Engine — персонализированные рекомендации

Система умных рекомендаций для предотвращения выгорания:
- Персонализированные советы на основе эмоционального состояния
- Адаптивные рекомендации по времени и контексту
- Интеграция с ML для улучшения точности
- Отслеживание эффективности рекомендаций

"Превращаем данные о выгорании в действенные советы" ✨
"""

import asyncio
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass
from enum import Enum
import random

from .core import BurnoutState, BurnoutRisk
from .modes import BurnoutModeType, BurnoutRiskLevel
from .utils import safe_logger, get_time_of_day_category, is_working_hours


class RecommendationType(Enum):
    """Типы рекомендаций."""

    IMMEDIATE_ACTION = "immediate"  # немедленные действия
    SHORT_TERM = "short_term"  # краткосрочные (1-4 часа)
    DAILY_ROUTINE = "daily"  # ежедневные привычки
    WEEKLY_PLANNING = "weekly"  # недельное планирование
    LIFESTYLE = "lifestyle"  # изменения образа жизни


@dataclass
class Recommendation:
    """Рекомендация для пользователя."""

    id: str
    type: RecommendationType
    title: str
    description: str

    # Метаданные
    priority: int  # 1-5 (5 = самый высокий)
    estimated_time: int  # время выполнения в минутах
    difficulty: str  # easy, medium, hard

    # Контекст применения
    applicable_risk_levels: List[BurnoutRiskLevel]
    applicable_modes: List[BurnoutModeType]
    time_sensitive: bool = False

    # Персонализация
    user_preferences: List[str] = None
    effectiveness_score: float = 1.0  # 0.0-1.0

    # Отслеживание
    usage_count: int = 0
    success_rate: float = 0.0
    last_used: Optional[datetime] = None


class RecommendationEngine:
    """
    Движок персонализированных рекомендаций.

    Анализирует состояние выгорания и предлагает
    целевые действия для восстановления и профилактики.
    """

    def __init__(self, user_id: str):
        self.user_id = user_id

        # База знаний рекомендаций
        self.recommendations_db = self._initialize_recommendations()

        # Персональные данные
        self.user_preferences = self._load_user_preferences()
        self.effectiveness_history: Dict[str, List[float]] = {}
        self.context_factors: Dict[str, Any] = {}

    def _initialize_recommendations(self) -> List[Recommendation]:
        """Инициализирует базу рекомендаций."""

        recommendations = []

        # Немедленные действия при критическом риске
        recommendations.extend(
            [
                Recommendation(
                    id="critical_break_now",
                    type=RecommendationType.IMMEDIATE_ACTION,
                    title="🚨 Немедленный перерыв",
                    description="Остановите все задачи и сделайте 15-минутный перерыв. Выйдите из-за компьютера.",
                    priority=5,
                    estimated_time=15,
                    difficulty="easy",
                    applicable_risk_levels=[
                        BurnoutRiskLevel.CRITICAL,
                        BurnoutRiskLevel.HIGH,
                    ],
                    applicable_modes=[
                        BurnoutModeType.EMOTIONAL_EXHAUSTION,
                        BurnoutModeType.CRISIS,
                    ],
                    time_sensitive=True,
                ),
                Recommendation(
                    id="breathing_exercise",
                    type=RecommendationType.IMMEDIATE_ACTION,
                    title="🫁 Дыхательная гимнастика",
                    description="4-7-8 дыхание: вдох 4 сек, задержка 7 сек, выдох 8 сек. Повторить 4 раза.",
                    priority=4,
                    estimated_time=5,
                    difficulty="easy",
                    applicable_risk_levels=[
                        BurnoutRiskLevel.HIGH,
                        BurnoutRiskLevel.CRITICAL,
                    ],
                    applicable_modes=list(BurnoutModeType),
                    time_sensitive=True,
                ),
            ]
        )

        # Краткосрочные действия
        recommendations.extend(
            [
                Recommendation(
                    id="hydration_reminder",
                    type=RecommendationType.SHORT_TERM,
                    title="💧 Увлажнение",
                    description="Выпейте стакан воды и съешьте что-то легкое. Обезвоживание усиливает стресс.",
                    priority=3,
                    estimated_time=5,
                    difficulty="easy",
                    applicable_risk_levels=[
                        BurnoutRiskLevel.MEDIUM,
                        BurnoutRiskLevel.HIGH,
                    ],
                    applicable_modes=[
                        BurnoutModeType.OVERWORK,
                        BurnoutModeType.EMOTIONAL_EXHAUSTION,
                    ],
                ),
                Recommendation(
                    id="task_prioritization",
                    type=RecommendationType.SHORT_TERM,
                    title="📝 Приоритизация задач",
                    description="Выберите 3 самые важные задачи на сегодня. Остальные перенесите на завтра.",
                    priority=4,
                    estimated_time=10,
                    difficulty="medium",
                    applicable_risk_levels=[
                        BurnoutRiskLevel.MEDIUM,
                        BurnoutRiskLevel.HIGH,
                    ],
                    applicable_modes=[
                        BurnoutModeType.OVERWORK,
                        BurnoutModeType.INEFFICACY,
                    ],
                ),
            ]
        )

        # Ежедневные привычки
        recommendations.extend(
            [
                Recommendation(
                    id="micro_breaks",
                    type=RecommendationType.DAILY_ROUTINE,
                    title="⏰ Микро-перерывы",
                    description="Каждые 25 минут делайте 5-минутный перерыв. Используйте технику Pomodoro.",
                    priority=3,
                    estimated_time=5,
                    difficulty="medium",
                    applicable_risk_levels=[
                        BurnoutRiskLevel.LOW,
                        BurnoutRiskLevel.MEDIUM,
                    ],
                    applicable_modes=[
                        BurnoutModeType.OVERWORK,
                        BurnoutModeType.HEALTHY,
                    ],
                ),
                Recommendation(
                    id="gratitude_practice",
                    type=RecommendationType.DAILY_ROUTINE,
                    title="🙏 Практика благодарности",
                    description="Перед сном запишите 3 вещи, за которые вы благодарны сегодня.",
                    priority=2,
                    estimated_time=5,
                    difficulty="easy",
                    applicable_risk_levels=[
                        BurnoutRiskLevel.LOW,
                        BurnoutRiskLevel.MEDIUM,
                    ],
                    applicable_modes=[
                        BurnoutModeType.CYNICISM,
                        BurnoutModeType.INEFFICACY,
                    ],
                ),
            ]
        )

        return recommendations

    def _load_user_preferences(self) -> Dict[str, Any]:
        """Загружает пользовательские предпочтения."""
        # В реальной реализации это будет из базы данных
        return {
            "preferred_break_activities": ["walk", "meditation", "tea"],
            "work_style": "focused_sessions",
            "notification_frequency": "moderate",
            "exercise_preference": "light",
            "social_preference": "moderate",
        }

    async def get_recommendations(
        self, burnout_state: BurnoutState, context: Optional[Dict[str, Any]] = None
    ) -> List[Recommendation]:
        """
        Получает персонализированные рекомендации.

        Args:
            burnout_state: Текущее состояние выгорания
            context: Дополнительный контекст (время, активность и т.д.)

        Returns:
            Список персонализированных рекомендаций
        """

        context = context or {}
        self.context_factors.update(context)

        # Фильтруем рекомендации по состоянию
        applicable = self._filter_applicable_recommendations(burnout_state)

        # Персонализируем рекомендации
        personalized = self._personalize_recommendations(applicable, burnout_state)

        # Сортируем по приоритету и эффективности
        sorted_recs = self._sort_recommendations(personalized, burnout_state)

        # Ограничиваем количество (максимум 5)
        final_recommendations = sorted_recs[:5]

        safe_logger.debug(
            f"Generated {len(final_recommendations)} recommendations for user {self.user_id}"
        )
        return final_recommendations

    def _filter_applicable_recommendations(
        self, burnout_state: BurnoutState
    ) -> List[Recommendation]:
        """Фильтрует применимые рекомендации."""

        applicable = []
        risk_level = burnout_state.risk_assessment.level
        burnout_mode = burnout_state.burnout_mode.type

        for rec in self.recommendations_db:
            # Проверяем применимость по уровню риска
            if risk_level in rec.applicable_risk_levels:
                applicable.append(rec)
                continue

            # Проверяем применимость по режиму выгорания
            if burnout_mode in rec.applicable_modes:
                applicable.append(rec)

        return applicable

    def _personalize_recommendations(
        self, recommendations: List[Recommendation], burnout_state: BurnoutState
    ) -> List[Recommendation]:
        """Персонализирует рекомендации под пользователя."""

        personalized = []
        current_hour = datetime.now().hour

        for rec in recommendations:
            # Копируем рекомендацию для модификации
            pers_rec = rec

            # Адаптируем под время дня
            if rec.time_sensitive:
                if (
                    not is_working_hours(current_hour)
                    and rec.type == RecommendationType.IMMEDIATE_ACTION
                ):
                    pers_rec.title = f"🌙 {rec.title} (вечерняя версия)"
                    pers_rec.description = self._adapt_for_evening(rec.description)

            # Учитываем пользовательские предпочтения
            if "meditation" in self.user_preferences.get(
                "preferred_break_activities", []
            ):
                if "breathing" in rec.description.lower():
                    pers_rec.effectiveness_score *= 1.2

            # Учитываем историю эффективности
            if rec.id in self.effectiveness_history:
                avg_effectiveness = sum(self.effectiveness_history[rec.id]) / len(
                    self.effectiveness_history[rec.id]
                )
                pers_rec.effectiveness_score = avg_effectiveness

            personalized.append(pers_rec)

        return personalized

    def _sort_recommendations(
        self, recommendations: List[Recommendation], burnout_state: BurnoutState
    ) -> List[Recommendation]:
        """Сортирует рекомендации по релевантности."""

        def recommendation_score(rec: Recommendation) -> float:
            score = 0.0

            # Базовый приоритет
            score += rec.priority * 20

            # Эффективность
            score += rec.effectiveness_score * 15

            # Временная чувствительность
            if rec.time_sensitive and burnout_state.risk_assessment.level in [
                BurnoutRiskLevel.HIGH,
                BurnoutRiskLevel.CRITICAL,
            ]:
                score += 25

            # Простота выполнения
            difficulty_bonus = {"easy": 10, "medium": 5, "hard": 0}
            score += difficulty_bonus.get(rec.difficulty, 0)

            # Частота использования (разнообразие)
            if rec.usage_count > 3:
                score -= 5  # снижаем приоритет часто используемых

            # Время с последнего использования
            if rec.last_used:
                days_since = (datetime.now() - rec.last_used).days
                score += min(days_since * 2, 10)  # максимум +10 за новизну

            return score

        return sorted(recommendations, key=recommendation_score, reverse=True)

    def _adapt_for_evening(self, description: str) -> str:
        """Адаптирует описание для вечернего времени."""

        adaptations = {
            "Выйдите из-за компьютера": "Отойдите от экранов и подготовьтесь ко сну",
            "сделайте перерыв": "сделайте расслабляющий перерыв",
            "задачи": "завтрашние задачи",
        }

        adapted = description
        for old, new in adaptations.items():
            adapted = adapted.replace(old, new)

        return adapted

    async def track_recommendation_outcome(
        self,
        recommendation_id: str,
        effectiveness_rating: float,
        completed: bool = True,
    ):
        """Отслеживает результат выполнения рекомендации."""

        # Обновляем статистику рекомендации
        for rec in self.recommendations_db:
            if rec.id == recommendation_id:
                rec.usage_count += 1
                rec.last_used = datetime.now()

                if completed:
                    # Обновляем историю эффективности
                    if recommendation_id not in self.effectiveness_history:
                        self.effectiveness_history[recommendation_id] = []

                    self.effectiveness_history[recommendation_id].append(
                        effectiveness_rating
                    )

                    # Ограничиваем историю последними 10 записями
                    if len(self.effectiveness_history[recommendation_id]) > 10:
                        self.effectiveness_history[recommendation_id] = (
                            self.effectiveness_history[recommendation_id][-10:]
                        )

                    # Обновляем общий success rate
                    total_ratings = len(self.effectiveness_history[recommendation_id])
                    successful_ratings = sum(
                        1
                        for r in self.effectiveness_history[recommendation_id]
                        if r >= 0.6
                    )
                    rec.success_rate = successful_ratings / total_ratings

                break

        safe_logger.debug(
            f"Tracked outcome for recommendation {recommendation_id}: effectiveness={effectiveness_rating}, completed={completed}"
        )

    async def get_daily_recommendations(
        self, burnout_state: BurnoutState
    ) -> List[Recommendation]:
        """Получает ежедневные рекомендации."""

        daily_recs = [
            rec
            for rec in self.recommendations_db
            if rec.type == RecommendationType.DAILY_ROUTINE
        ]

        # Персонализируем и сортируем
        personalized = self._personalize_recommendations(daily_recs, burnout_state)
        sorted_recs = self._sort_recommendations(personalized, burnout_state)

        return sorted_recs[:3]  # максимум 3 ежедневные привычки

    async def get_emergency_recommendations(
        self, burnout_state: BurnoutState
    ) -> List[Recommendation]:
        """Получает экстренные рекомендации для критических ситуаций."""

        emergency_recs = [
            rec
            for rec in self.recommendations_db
            if (
                rec.type == RecommendationType.IMMEDIATE_ACTION
                and rec.time_sensitive
                and BurnoutRiskLevel.CRITICAL in rec.applicable_risk_levels
            )
        ]

        # Сортируем по приоритету
        return sorted(emergency_recs, key=lambda x: x.priority, reverse=True)[:3]
