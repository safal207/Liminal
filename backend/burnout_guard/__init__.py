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

from .core import BurnoutGuardEngine, BurnoutState, BurnoutRisk
from .detector import BurnoutDetector, BurnoutPattern
from .recommendations import RecommendationEngine, Recommendation
from .analytics import TeamAnalytics, BurnoutTrend
from .modes import BurnoutMode, BurnoutModeType

__version__ = "1.0.0"
__author__ = "Resonance Liminal Team"

__all__ = [
    "BurnoutGuardEngine",
    "BurnoutState", 
    "BurnoutRisk",
    "BurnoutDetector",
    "BurnoutPattern",
    "RecommendationEngine",
    "Recommendation",
    "TeamAnalytics",
    "BurnoutTrend",
    "BurnoutMode",
    "BurnoutModeType"
]