from dataclasses import dataclass
from typing import Any, Dict, List

from .modes import BurnoutModeType, BurnoutRiskLevel

try:
    from ..emotime.modes import ModeType as EmotionalModeType
except ImportError:  # pragma: no cover - support legacy top-level package imports
    from emotime.modes import ModeType as EmotionalModeType


@dataclass
class BurnoutPattern:
    name: str
    risk_score: float
    burnout_mode: BurnoutModeType


class BurnoutDetector:
    """Простой детектор выгорания."""

    def __init__(self):
        # Веса риска для разных эмоциональных режимов
        self.mode_risk_weights = {
            EmotionalModeType.STRESS: 0.9,
            EmotionalModeType.CONTEMPLATION: 0.4,
            EmotionalModeType.FOCUS: 0.3,
            EmotionalModeType.NEUTRAL: 0.25,
            EmotionalModeType.CALM: 0.1,
            EmotionalModeType.JOY: 0.05,
        }

    def analyze_burnout_risk(self, emotional_history: List[Any]) -> Dict[str, Any]:
        """Анализирует риск выгорания."""

        if not emotional_history:
            return {
                "risk_score": 0.0,
                "risk_level": BurnoutRiskLevel.LOW,
                "indicators": [],
            }

        # 1. Базовый риск от режимов
        mode_risk = 0.0
        mode_counts = {}

        for data in emotional_history:
            mode = getattr(data, "mode", getattr(data, "type", None))
            intensity = getattr(data, "intensity", 0.5)
            weight = self.mode_risk_weights.get(mode, 0.25)
            mode_risk += weight * intensity
            mode_counts[mode] = mode_counts.get(mode, 0) + 1

        mode_risk /= len(emotional_history)

        # 2. Риск от паттернов
        pattern_risk = self._analyze_patterns(emotional_history)

        # 3. Риск от эмоциональной валентности
        valence_risk = self._analyze_valence_risk(emotional_history)

        # 4. Итоговый скор
        total_risk = mode_risk * 0.5 + pattern_risk * 0.3 + valence_risk * 0.2
        total_risk = max(0.0, min(1.0, total_risk))

        # 5. Определяем уровень риска
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

        return {
            "risk_score": total_risk,
            "risk_level": risk_level,
            "dominant_mode": (
                max(mode_counts, key=mode_counts.get) if mode_counts else None
            ),
            "indicators": self._generate_indicators(
                emotional_history, mode_counts, total_risk
            ),
        }

    def _analyze_patterns(self, history: List[Any]) -> float:
        """Анализирует паттерны в истории."""
        if len(history) < 3:
            return 0.0

        # Проверка на хронический стресс
        recent_modes = [
            getattr(d, "mode", getattr(d, "type", None)) for d in history[-5:]
        ]
        if recent_modes.count(EmotionalModeType.STRESS) >= 3:
            return 0.9

        # Проверка на длительный фокус
        if recent_modes.count(EmotionalModeType.FOCUS) >= 4:
            return 0.6

        return 0.0

    def _analyze_valence_risk(self, history: List[Any]) -> float:
        """Анализирует риск от эмоциональной валентности."""
        if not history:
            return 0.0
        valences = [getattr(d, "valence", 0.0) for d in history]
        avg_valence = sum(valences) / len(history)
        if avg_valence < -0.5:
            return 0.8
        elif avg_valence < -0.2:
            return 0.5
        return 0.0

    def _generate_indicators(
        self, history: List[Any], mode_counts: Dict, risk_score: float
    ) -> List[str]:
        """Генерирует индикаторы риска."""
        indicators = []
        total_count = len(history)
        if mode_counts.get(EmotionalModeType.STRESS, 0) / total_count > 0.3:
            indicators.append("Высокий уровень стресса")
        if mode_counts.get(EmotionalModeType.JOY, 0) / total_count < 0.1:
            indicators.append("Недостаток позитивных эмоций")
        return indicators
