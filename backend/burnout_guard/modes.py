"""
🚀🛡️ BurnoutGuard Modes — режимы выгорания

Расширение эмоциональных режимов Emotime для детекции выгорания:
- Маппинг 6 базовых режимов в индикаторы выгорания
- Специальные паттерны выгорания
- Скоринг риска на основе комбинаций режимов

Базовые режимы Emotime → Индикаторы выгорания:
• Stress → Высокий риск выгорания
• Calm → Низкий риск (восстановление)
• Focus → Средний риск (если длительный)
• Joy → Очень низкий риск (позитивное состояние)
• Contemplation → Средний риск (рефлексия проблем)
• Neutral → Базовый риск
"""

from dataclasses import dataclass
from datetime import datetime, timedelta
from enum import Enum
from typing import List, Optional, Tuple

import numpy as np

try:
    from ..emotime.modes import EmotionalMode
    from ..emotime.modes import ModeType as EmotionalModeType
except ImportError:  # pragma: no cover - support legacy top-level package imports
    from emotime.modes import EmotionalMode
    from emotime.modes import ModeType as EmotionalModeType


class BurnoutRiskLevel(Enum):
    """Уровни риска выгорания."""

    VERY_LOW = "very_low"  # 0-20%
    LOW = "low"  # 21-40%
    MEDIUM = "medium"  # 41-60%
    HIGH = "high"  # 61-80%
    CRITICAL = "critical"  # 81-100%


class BurnoutModeType(Enum):
    """Типы режимов выгорания."""

    HEALTHY = "healthy"  # Здоровое состояние
    OVERWORK = "overwork"  # Переработка
    EMOTIONAL_EXHAUSTION = "exhaustion"  # Эмоциональное истощение
    CYNICISM = "cynicism"  # Цинизм/отстраненность
    INEFFICACY = "inefficacy"  # Снижение эффективности
    CRISIS = "crisis"  # Кризис выгорания


@dataclass
class BurnoutMode:
    """Режим выгорания."""

    type: BurnoutModeType
    risk_level: BurnoutRiskLevel
    risk_score: float  # 0.0-1.0
    primary_indicators: List[str]  # основные индикаторы
    emotional_pattern: str  # паттерн эмоциональных режимов
    duration_hours: float = 0.0  # длительность в часах
    confidence: float = 0.0  # уверенность в детекции


class BurnoutModeMapper:
    """
    Система маппинга эмоциональных режимов в режимы выгорания.

    Анализирует последовательности и комбинации эмоциональных режимов
    для выявления паттернов выгорания.
    """

    # Маппинг базовых эмоциональных режимов в факторы риска
    EMOTIONAL_TO_RISK_MAPPING = {
        EmotionalModeType.STRESS: {
            "base_risk": 0.8,
            "risk_multiplier": 1.5,  # стресс усиливает риск
            "burnout_indicators": ["high_arousal", "negative_valence", "tension"],
        },
        EmotionalModeType.CALM: {
            "base_risk": 0.1,
            "risk_multiplier": 0.3,  # спокойствие снижает риск
            "burnout_indicators": ["recovery", "balance", "restoration"],
        },
        EmotionalModeType.FOCUS: {
            "base_risk": 0.3,
            "risk_multiplier": 1.2,  # длительный фокус может быть риском
            "burnout_indicators": ["concentration", "potential_overwork"],
        },
        EmotionalModeType.JOY: {
            "base_risk": 0.05,
            "risk_multiplier": 0.2,  # радость защищает от выгорания
            "burnout_indicators": ["positive_engagement", "fulfillment"],
        },
        EmotionalModeType.CONTEMPLATION: {
            "base_risk": 0.4,
            "risk_multiplier": 0.8,  # размышления могут указывать на проблемы
            "burnout_indicators": ["reflection", "potential_rumination"],
        },
        EmotionalModeType.NEUTRAL: {
            "base_risk": 0.25,
            "risk_multiplier": 1.0,  # базовый уровень
            "burnout_indicators": ["baseline"],
        },
    }

    # Опасные паттерны комбинаций режимов
    DANGEROUS_PATTERNS = {
        "chronic_stress": {
            "pattern": [EmotionalModeType.STRESS] * 3,  # 3+ циклов стресса подряд
            "risk_score": 0.9,
            "burnout_mode": BurnoutModeType.EMOTIONAL_EXHAUSTION,
        },
        "stress_contemplation_cycle": {
            "pattern": [EmotionalModeType.STRESS, EmotionalModeType.CONTEMPLATION],
            "risk_score": 0.7,
            "burnout_mode": BurnoutModeType.CYNICISM,
        },
        "prolonged_focus": {
            "pattern": [EmotionalModeType.FOCUS] * 4,  # 4+ циклов фокуса
            "risk_score": 0.6,
            "burnout_mode": BurnoutModeType.OVERWORK,
        },
        "emotional_flatness": {
            "pattern": [EmotionalModeType.NEUTRAL] * 5,  # 5+ нейтральных циклов
            "risk_score": 0.5,
            "burnout_mode": BurnoutModeType.INEFFICACY,
        },
        "joy_deficit": {
            "pattern": "no_joy_24h",  # отсутствие радости 24 часа
            "risk_score": 0.8,
            "burnout_mode": BurnoutModeType.EMOTIONAL_EXHAUSTION,
        },
    }

    def __init__(self, lookback_hours: int = 24):
        self.lookback_hours = lookback_hours
        self.mode_history: List[Tuple[EmotionalMode, datetime]] = []

    async def analyze_burnout_risk(
        self, emotional_modes: List[Tuple[EmotionalMode, datetime]]
    ) -> BurnoutMode:
        """
        Анализирует риск выгорания на основе истории эмоциональных режимов.

        Args:
            emotional_modes: История эмоциональных режимов с timestamps

        Returns:
            Определенный режим выгорания
        """

        # Обновляем историю
        cutoff_time = datetime.now() - timedelta(hours=self.lookback_hours)
        recent_modes = [(mode, ts) for mode, ts in emotional_modes if ts >= cutoff_time]

        if not recent_modes:
            return self._create_default_mode()

        # 1. Анализируем базовый риск
        base_risk = await self._calculate_base_risk(recent_modes)

        # 2. Ищем опасные паттерны
        pattern_risk, detected_pattern = await self._detect_dangerous_patterns(
            recent_modes
        )

        # 3. Анализируем тренды
        trend_risk = await self._analyze_trends(recent_modes)

        # 4. Комбинируем риски
        total_risk = min(1.0, base_risk * 0.4 + pattern_risk * 0.4 + trend_risk * 0.2)

        # 5. Определяем режим выгорания
        burnout_mode = await self._determine_burnout_mode(
            total_risk, detected_pattern, recent_modes
        )

        return burnout_mode

    async def _calculate_base_risk(
        self, recent_modes: List[Tuple[EmotionalMode, datetime]]
    ) -> float:
        """Вычисляет базовый риск на основе отдельных режимов."""

        total_risk = 0.0
        total_duration = 0.0

        for mode, _timestamp in recent_modes:
            mode_mapping = self.EMOTIONAL_TO_RISK_MAPPING.get(mode.type, {})
            base_risk = mode_mapping.get("base_risk", 0.25)

            # Учитываем интенсивность и длительность режима
            duration_weight = mode.duration if hasattr(mode, "duration") else 1.0
            intensity_weight = mode.intensity if hasattr(mode, "intensity") else 1.0

            weighted_risk = base_risk * intensity_weight * duration_weight
            total_risk += weighted_risk
            total_duration += duration_weight

        return total_risk / max(total_duration, 1.0)

    async def _detect_dangerous_patterns(
        self, recent_modes: List[Tuple[EmotionalMode, datetime]]
    ) -> Tuple[float, Optional[str]]:
        """Детектит опасные паттерны в последовательности режимов."""

        if len(recent_modes) < 2:
            return 0.0, None

        mode_sequence = [mode.type for mode, _ in recent_modes]
        max_risk = 0.0
        detected_pattern = None

        # Проверяем каждый опасный паттерн
        for pattern_name, pattern_config in self.DANGEROUS_PATTERNS.items():
            if pattern_name == "joy_deficit":
                # Специальная проверка отсутствия радости
                has_joy = any(mode == EmotionalModeType.JOY for mode in mode_sequence)
                if (
                    not has_joy and len(recent_modes) >= 12
                ):  # 12 циклов = примерно 12 часов
                    risk = pattern_config["risk_score"]
                    if risk > max_risk:
                        max_risk = risk
                        detected_pattern = pattern_name
            else:
                # Проверка последовательных паттернов
                pattern = pattern_config["pattern"]
                if self._sequence_contains_pattern(mode_sequence, pattern):
                    risk = pattern_config["risk_score"]
                    if risk > max_risk:
                        max_risk = risk
                        detected_pattern = pattern_name

        return max_risk, detected_pattern

    def _sequence_contains_pattern(
        self, sequence: List[EmotionalModeType], pattern: List[EmotionalModeType]
    ) -> bool:
        """Проверяет, содержит ли последовательность опасный паттерн."""

        if len(pattern) > len(sequence):
            return False

        # Ищем паттерн в последовательности
        for i in range(len(sequence) - len(pattern) + 1):
            if sequence[i : i + len(pattern)] == pattern:
                return True

        return False

    async def _analyze_trends(
        self, recent_modes: List[Tuple[EmotionalMode, datetime]]
    ) -> float:
        """Анализирует тренды ухудшения эмоционального состояния."""

        if len(recent_modes) < 4:
            return 0.0

        # Разбиваем на временные окна
        window_size = len(recent_modes) // 4
        windows = [
            recent_modes[i : i + window_size]
            for i in range(0, len(recent_modes), window_size)
        ]

        # Вычисляем средний риск для каждого окна
        window_risks = []
        for window in windows:
            if window:
                avg_risk = sum(
                    self.EMOTIONAL_TO_RISK_MAPPING.get(mode.type, {}).get(
                        "base_risk", 0.25
                    )
                    for mode, _ in window
                ) / len(window)
                window_risks.append(avg_risk)

        if len(window_risks) < 2:
            return 0.0

        # Вычисляем тренд (увеличивается ли риск?)
        trend = np.polyfit(range(len(window_risks)), window_risks, 1)[0]

        # Положительный тренд = увеличение риска
        return max(0.0, min(1.0, trend * 2.0))  # нормализуем к [0, 1]

    async def _determine_burnout_mode(
        self,
        total_risk: float,
        detected_pattern: Optional[str],
        recent_modes: List[Tuple[EmotionalMode, datetime]],
    ) -> BurnoutMode:
        """Определяет конкретный режим выгорания."""

        # Определяем уровень риска
        if total_risk <= 0.2:
            risk_level = BurnoutRiskLevel.VERY_LOW
        elif total_risk <= 0.4:
            risk_level = BurnoutRiskLevel.LOW
        elif total_risk <= 0.6:
            risk_level = BurnoutRiskLevel.MEDIUM
        elif total_risk <= 0.8:
            risk_level = BurnoutRiskLevel.HIGH
        else:
            risk_level = BurnoutRiskLevel.CRITICAL

        # Определяем тип выгорания на основе паттерна
        if detected_pattern:
            pattern_config = self.DANGEROUS_PATTERNS[detected_pattern]
            burnout_type = pattern_config.get("burnout_mode", BurnoutModeType.OVERWORK)
        else:
            # Определяем по доминирующему эмоциональному режиму
            mode_counts = {}
            for mode, _ in recent_modes:
                mode_counts[mode.type] = mode_counts.get(mode.type, 0) + 1

            dominant_mode = max(mode_counts, key=mode_counts.get)
            burnout_type = self._map_emotional_to_burnout_type(
                dominant_mode, total_risk
            )

        # Создаем индикаторы
        indicators = self._generate_indicators(recent_modes, detected_pattern)

        # Формируем паттерн описания
        pattern_desc = self._generate_pattern_description(
            recent_modes, detected_pattern
        )

        return BurnoutMode(
            type=burnout_type,
            risk_level=risk_level,
            risk_score=total_risk,
            primary_indicators=indicators,
            emotional_pattern=pattern_desc,
            duration_hours=self.lookback_hours,
            confidence=min(
                0.95, 0.5 + total_risk * 0.5
            ),  # выше риск = выше уверенность
        )

    def _map_emotional_to_burnout_type(
        self, dominant_mode: EmotionalModeType, risk_score: float
    ) -> BurnoutModeType:
        """Маппит доминирующий эмоциональный режим в тип выгорания."""

        if risk_score < 0.3:
            return BurnoutModeType.HEALTHY
        elif dominant_mode == EmotionalModeType.STRESS:
            return BurnoutModeType.EMOTIONAL_EXHAUSTION
        elif dominant_mode == EmotionalModeType.FOCUS:
            return BurnoutModeType.OVERWORK
        elif dominant_mode == EmotionalModeType.CONTEMPLATION:
            return BurnoutModeType.CYNICISM
        elif dominant_mode == EmotionalModeType.NEUTRAL:
            return BurnoutModeType.INEFFICACY
        else:
            return BurnoutModeType.OVERWORK

    def _generate_indicators(
        self,
        recent_modes: List[Tuple[EmotionalMode, datetime]],
        detected_pattern: Optional[str],
    ) -> List[str]:
        """Генерирует список индикаторов выгорания."""

        indicators = []

        # Индикаторы на основе режимов
        mode_counts = {}
        for mode, _ in recent_modes:
            mode_counts[mode.type] = mode_counts.get(mode.type, 0) + 1

        total_modes = len(recent_modes)

        if mode_counts.get(EmotionalModeType.STRESS, 0) / total_modes > 0.3:
            indicators.append("Высокий уровень стресса")

        if mode_counts.get(EmotionalModeType.JOY, 0) / total_modes < 0.1:
            indicators.append("Недостаток позитивных эмоций")

        if mode_counts.get(EmotionalModeType.FOCUS, 0) / total_modes > 0.5:
            indicators.append("Чрезмерная концентрация на работе")

        if mode_counts.get(EmotionalModeType.NEUTRAL, 0) / total_modes > 0.4:
            indicators.append("Эмоциональная притупленность")

        # Индикаторы на основе паттернов
        if detected_pattern:
            pattern_indicators = {
                "chronic_stress": "Хронический стресс",
                "stress_contemplation_cycle": "Циклы стресса и размышлений",
                "prolonged_focus": "Длительная переработка",
                "emotional_flatness": "Эмоциональная уплощенность",
                "joy_deficit": "Отсутствие радости",
            }
            indicator = pattern_indicators.get(detected_pattern)
            if indicator:
                indicators.append(indicator)

        return indicators if indicators else ["Базовые показатели"]

    def _generate_pattern_description(
        self,
        recent_modes: List[Tuple[EmotionalMode, datetime]],
        detected_pattern: Optional[str],
    ) -> str:
        """Генерирует описание эмоционального паттерна."""

        if detected_pattern:
            descriptions = {
                "chronic_stress": "Повторяющиеся циклы стресса",
                "stress_contemplation_cycle": "Чередование стресса и размышлений",
                "prolonged_focus": "Длительные периоды концентрации",
                "emotional_flatness": "Преобладание нейтрального состояния",
                "joy_deficit": "Отсутствие положительных эмоций",
            }
            return descriptions.get(detected_pattern, "Неопределенный паттерн")

        # Генерируем описание на основе доминирующих режимов
        mode_counts = {}
        for mode, _ in recent_modes:
            mode_counts[mode.type] = mode_counts.get(mode.type, 0) + 1

        if not mode_counts:
            return "Недостаточно данных"

        dominant_mode = max(mode_counts, key=mode_counts.get)
        mode_names = {
            EmotionalModeType.CALM: "спокойствие",
            EmotionalModeType.FOCUS: "концентрация",
            EmotionalModeType.STRESS: "стресс",
            EmotionalModeType.JOY: "радость",
            EmotionalModeType.CONTEMPLATION: "размышления",
            EmotionalModeType.NEUTRAL: "нейтральность",
        }

        dominant_name = mode_names.get(dominant_mode, "неизвестно")
        return f"Преобладание: {dominant_name}"

    def _create_default_mode(self) -> BurnoutMode:
        """Создает режим по умолчанию при недостатке данных."""

        return BurnoutMode(
            type=BurnoutModeType.HEALTHY,
            risk_level=BurnoutRiskLevel.LOW,
            risk_score=0.2,
            primary_indicators=["Недостаточно данных для анализа"],
            emotional_pattern="Базовое состояние",
            duration_hours=0.0,
            confidence=0.3,
        )
