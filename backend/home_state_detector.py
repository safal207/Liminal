"""
Home State Detector - определение состояния "дома в себе".

Философская концепция: "Дом — это ты, когда ты искренен с собой"

Система анализирует поведенческие паттерны пользователя для определения
уровня аутентичности и присутствия в моменте.
"""

import asyncio
import json
import math
import time
from dataclasses import dataclass
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional

from logging_config import get_logger
logger = get_logger(__name__)


@dataclass
class HomeStateMetrics:
    """Метрики состояния 'дома в себе'."""

    authenticity_level: float  # 0.0-1.0 - общий уровень искренности
    consistency_score: float  # 0.0-1.0 - постоянство поведения
    presence_index: float  # 0.0-1.0 - присутствие в моменте
    self_alignment: float  # 0.0-1.0 - соответствие внутренним ценностям
    emotional_congruence: float  # 0.0-1.0 - соответствие эмоций и слов
    reflection_depth: float  # 0.0-1.0 - глубина саморефлексии

    timestamp: datetime
    confidence: float  # 0.0-1.0 - уверенность в оценке

    @property
    def is_home(self) -> bool:
        """Находится ли человек 'дома в себе'."""
        return self.authenticity_level > 0.7

    @property
    def home_state_label(self) -> str:
        """Текстовое описание состояния."""
        if self.authenticity_level >= 0.8:
            return "Глубоко дома в себе"
        elif self.authenticity_level >= 0.6:
            return "На пути домой"
        elif self.authenticity_level >= 0.4:
            return "В поиске себя"
        else:
            return "Вдали от дома"


@dataclass
class UserBehaviorPattern:
    """Паттерн поведения пользователя."""

    user_id: str
    message_frequency: float  # Частота сообщений
    response_delay: float  # Задержка ответов
    message_length_variance: float  # Разброс длины сообщений
    emotional_consistency: float  # Эмоциональная согласованность
    topic_focus: float  # Фокус на теме
    self_reference_ratio: float  # Доля самореференций
    question_to_statement_ratio: float  # Соотношение вопросов к утверждениям
    authenticity_markers: List[str]  # Маркеры аутентичности

    created_at: datetime
    last_updated: datetime


class HomeStateDetector:
    """
    Детектор состояния 'дома в себе'.

    Анализирует поведенческие паттерны для определения уровня аутентичности
    и присутствия пользователя в моменте.
    """

    def __init__(self):
        self.user_patterns: Dict[str, UserBehaviorPattern] = {}
        self.home_state_history: Dict[str, List[HomeStateMetrics]] = {}
        self.authenticity_markers = {
            # Маркеры искренности в тексте
            "honest_expressions": [
                "честно говоря",
                "на самом деле",
                "если быть честным",
            ],
            "uncertainty_markers": ["не уверен", "возможно", "кажется"],
            "self_awareness": ["понимаю", "осознаю", "чувствую"],
            "vulnerability": ["боюсь", "волнуюсь", "сомневаюсь"],
            "present_moment": ["сейчас", "в данный момент", "здесь и сейчас"],
        }

        logger.info(
            "HomeStateDetector инициализирован с философской концепцией 'дома в себе'"
        )

    async def analyze_user_behavior(
        self, user_id: str, message_data: Dict[str, Any]
    ) -> HomeStateMetrics:
        """
        Анализирует поведение пользователя и определяет состояние 'дома в себе'.

        Args:
            user_id: ID пользователя
            message_data: Данные сообщения для анализа

        Returns:
            HomeStateMetrics: Метрики состояния дома
        """

        # Обновляем паттерн поведения
        await self._update_behavior_pattern(user_id, message_data)

        # Получаем текущий паттерн
        pattern = self.user_patterns.get(user_id)
        if not pattern:
            # Для нового пользователя возвращаем нейтральные метрики
            return self._create_default_metrics()

        # Анализируем аутентичность
        authenticity = await self._calculate_authenticity_level(user_id, message_data)
        consistency = await self._calculate_consistency_score(user_id)
        presence = await self._calculate_presence_index(message_data)
        alignment = await self._calculate_self_alignment(user_id, message_data)
        congruence = await self._calculate_emotional_congruence(message_data)
        reflection = await self._calculate_reflection_depth(message_data)

        # Рассчитываем уверенность в оценке
        confidence = await self._calculate_confidence(user_id)

        metrics = HomeStateMetrics(
            authenticity_level=authenticity,
            consistency_score=consistency,
            presence_index=presence,
            self_alignment=alignment,
            emotional_congruence=congruence,
            reflection_depth=reflection,
            timestamp=datetime.now(),
            confidence=confidence,
        )

        # Сохраняем в историю
        if user_id not in self.home_state_history:
            self.home_state_history[user_id] = []

        self.home_state_history[user_id].append(metrics)

        # Ограничиваем историю последними 100 записями
        if len(self.home_state_history[user_id]) > 100:
            self.home_state_history[user_id] = self.home_state_history[user_id][-100:]

        logger.info(
            f"Пользователь {user_id}: {metrics.home_state_label} (аутентичность: {authenticity:.2f})"
        )

        return metrics

    async def _update_behavior_pattern(
        self, user_id: str, message_data: Dict[str, Any]
    ):
        """Обновляет паттерн поведения пользователя."""

        now = datetime.now()
        message_text = message_data.get("text", "")
        message_length = len(message_text)

        if user_id not in self.user_patterns:
            # Создаем новый паттерн
            self.user_patterns[user_id] = UserBehaviorPattern(
                user_id=user_id,
                message_frequency=1.0,
                response_delay=0.0,
                message_length_variance=0.0,
                emotional_consistency=0.5,
                topic_focus=0.5,
                self_reference_ratio=0.0,
                question_to_statement_ratio=0.0,
                authenticity_markers=[],
                created_at=now,
                last_updated=now,
            )
        else:
            # Обновляем существующий паттерн
            pattern = self.user_patterns[user_id]

            # Рассчитываем временную разницу
            time_diff = (now - pattern.last_updated).total_seconds()

            # Обновляем частоту сообщений (экспоненциальное сглаживание)
            alpha = 0.3  # Коэффициент сглаживания
            new_frequency = 1.0 / max(time_diff, 1.0)  # Частота в сообщениях/сек
            pattern.message_frequency = (
                alpha * new_frequency + (1 - alpha) * pattern.message_frequency
            )

            # Обновляем разброс длины сообщений
            avg_length = 100  # Примерная средняя длина
            length_variance = abs(message_length - avg_length) / avg_length
            pattern.message_length_variance = (
                alpha * length_variance + (1 - alpha) * pattern.message_length_variance
            )

            pattern.last_updated = now

    async def _calculate_authenticity_level(
        self, user_id: str, message_data: Dict[str, Any]
    ) -> float:
        """
        Рассчитывает уровень аутентичности (искренности с собой).

        Основано на:
        - Наличии маркеров честности в тексте
        - Эмоциональной открытости
        - Готовности к уязвимости
        - Использовании личных местоимений
        """

        message_text = message_data.get("text", "").lower()
        authenticity_score = 0.5  # Базовый уровень

        # Проверяем маркеры искренности
        for category, markers in self.authenticity_markers.items():
            found_markers = sum(1 for marker in markers if marker in message_text)
            if found_markers > 0:
                if category == "honest_expressions":
                    authenticity_score += 0.15
                elif category == "vulnerability":
                    authenticity_score += 0.2  # Уязвимость = высокая аутентичность
                elif category == "self_awareness":
                    authenticity_score += 0.1
                elif category == "uncertainty_markers":
                    authenticity_score += 0.05  # Признание неуверенности = честность

        # Анализируем личные местоимения (я, мне, мой)
        personal_pronouns = ["я ", "мне", "мой", "моя", "мое", "меня"]
        personal_count = sum(
            message_text.count(pronoun) for pronoun in personal_pronouns
        )
        word_count = len(message_text.split())

        if word_count > 0:
            personal_ratio = personal_count / word_count
            # Умеренное использование личных местоимений указывает на баланс
            if 0.05 <= personal_ratio <= 0.2:
                authenticity_score += 0.1

        # Ограничиваем диапазон
        return max(0.0, min(1.0, authenticity_score))

    async def _calculate_consistency_score(self, user_id: str) -> float:
        """Рассчитывает постоянство поведения."""

        pattern = self.user_patterns.get(user_id)
        if not pattern:
            return 0.5

        # Анализируем стабильность паттернов
        history = self.home_state_history.get(user_id, [])
        if len(history) < 3:
            return 0.5

        # Рассчитываем вариацию последних метрик
        recent_authenticity = [m.authenticity_level for m in history[-5:]]

        if len(recent_authenticity) > 1:
            variance = sum(
                (x - sum(recent_authenticity) / len(recent_authenticity)) ** 2
                for x in recent_authenticity
            ) / len(recent_authenticity)
            consistency = 1.0 - min(
                variance, 1.0
            )  # Меньше вариации = больше постоянства
            return consistency

        return 0.5

    async def _calculate_presence_index(self, message_data: Dict[str, Any]) -> float:
        """Рассчитывает индекс присутствия в моменте."""

        message_text = message_data.get("text", "").lower()
        presence_score = 0.5

        # Проверяем маркеры присутствия
        present_markers = self.authenticity_markers.get("present_moment", [])
        found_present = sum(1 for marker in present_markers if marker in message_text)

        if found_present > 0:
            presence_score += 0.3

        # Анализируем время ответа (быстрый ответ может указывать на присутствие)
        response_time = message_data.get("response_time", 5.0)
        if response_time < 10.0:  # Быстрый ответ
            presence_score += 0.2
        elif response_time > 60.0:  # Очень медленный ответ
            presence_score -= 0.1

        return max(0.0, min(1.0, presence_score))

    async def _calculate_self_alignment(
        self, user_id: str, message_data: Dict[str, Any]
    ) -> float:
        """Рассчитывает соответствие внутренним ценностям."""

        # Пока базовая реализация
        # В будущем можно анализировать соответствие заявленным ценностям
        return 0.6

    async def _calculate_emotional_congruence(
        self, message_data: Dict[str, Any]
    ) -> float:
        """Рассчитывает соответствие эмоций и слов."""

        # Пока базовая реализация
        # В будущем можно анализировать эмоциональный тон сообщений
        return 0.6

    async def _calculate_reflection_depth(self, message_data: Dict[str, Any]) -> float:
        """Рассчитывает глубину саморефлексии."""

        message_text = message_data.get("text", "").lower()
        reflection_score = 0.4

        # Ищем вопросы к себе
        self_questions = ["почему я", "что я", "как я", "зачем я"]
        found_questions = sum(1 for q in self_questions if q in message_text)

        if found_questions > 0:
            reflection_score += 0.3

        # Ищем рефлексивные слова
        reflection_words = ["думаю", "размышляю", "анализирую", "понимаю", "осознаю"]
        found_reflection = sum(1 for word in reflection_words if word in message_text)

        if found_reflection > 0:
            reflection_score += 0.2

        return max(0.0, min(1.0, reflection_score))

    async def _calculate_confidence(self, user_id: str) -> float:
        """Рассчитывает уверенность в оценке."""

        history = self.home_state_history.get(user_id, [])

        # Больше данных = больше уверенности
        if len(history) < 3:
            return 0.3
        elif len(history) < 10:
            return 0.6
        else:
            return 0.9

    def _create_default_metrics(self) -> HomeStateMetrics:
        """Создает метрики по умолчанию для нового пользователя."""

        return HomeStateMetrics(
            authenticity_level=0.5,
            consistency_score=0.5,
            presence_index=0.5,
            self_alignment=0.5,
            emotional_congruence=0.5,
            reflection_depth=0.5,
            timestamp=datetime.now(),
            confidence=0.3,
        )

    def get_user_home_state(self, user_id: str) -> Optional[HomeStateMetrics]:
        """Получает текущее состояние 'дома в себе' для пользователя."""

        history = self.home_state_history.get(user_id, [])
        if history:
            return history[-1]
        return None

    def get_home_state_trend(
        self, user_id: str, days: int = 7
    ) -> List[HomeStateMetrics]:
        """Получает тренд состояния 'дома в себе' за указанный период."""

        history = self.home_state_history.get(user_id, [])
        cutoff_date = datetime.now() - timedelta(days=days)

        return [m for m in history if m.timestamp >= cutoff_date]

    async def export_metrics(self, user_id: str) -> Dict[str, Any]:
        """Экспортирует метрики пользователя для анализа."""

        current_state = self.get_user_home_state(user_id)
        trend = self.get_home_state_trend(user_id)
        pattern = self.user_patterns.get(user_id)

        return {
            "user_id": user_id,
            "current_state": {
                "authenticity_level": (
                    current_state.authenticity_level if current_state else None
                ),
                "home_state_label": (
                    current_state.home_state_label if current_state else "Неизвестно"
                ),
                "is_home": current_state.is_home if current_state else False,
                "confidence": current_state.confidence if current_state else 0.0,
                "timestamp": (
                    current_state.timestamp.isoformat() if current_state else None
                ),
            },
            "trend_stats": {
                "measurements_count": len(trend),
                "avg_authenticity": (
                    sum(m.authenticity_level for m in trend) / len(trend)
                    if trend
                    else 0.0
                ),
                "avg_presence": (
                    sum(m.presence_index for m in trend) / len(trend) if trend else 0.0
                ),
                "home_percentage": (
                    sum(1 for m in trend if m.is_home) / len(trend) * 100
                    if trend
                    else 0.0
                ),
            },
            "behavior_pattern": {
                "message_frequency": pattern.message_frequency if pattern else 0.0,
                "emotional_consistency": (
                    pattern.emotional_consistency if pattern else 0.0
                ),
                "created_at": pattern.created_at.isoformat() if pattern else None,
            },
        }


# Глобальный экземпляр детектора
home_state_detector = HomeStateDetector()
