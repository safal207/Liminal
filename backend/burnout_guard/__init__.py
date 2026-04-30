"""
🚀🛡️ BurnoutGuard — AI-защита от выгорания

Система раннего предупреждения и защиты от профессионального выгорания,
построенная на основе Emotime для анализа эмоционального состояния.

Основные компоненты:
- Детектор выгорания (на основе эмоциональных паттернов)
- Система скоринга риска выгорания
- Персонализированные рекомендации
- Командная аналитика для HR
- Мобильный интерфейс

"Защитим от выгорания, сохранив внутренний огонь" ✨
"""

from .analytics import TeamAnalytics, TeamBurnoutTrend
from .core import BurnoutGuardEngine, BurnoutRisk, BurnoutState
from .modes import BurnoutMode, BurnoutModeType
from .recommendations import Recommendation, RecommendationEngine

__version__ = "1.0.0"
__author__ = "Resonance Liminal Team"

__all__ = [
    "BurnoutGuardEngine",
    "BurnoutState",
    "BurnoutRisk",
    "RecommendationEngine",
    "Recommendation",
    "TeamAnalytics",
    "TeamBurnoutTrend",
    "BurnoutMode",
    "BurnoutModeType",
]
