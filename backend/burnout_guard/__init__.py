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
from .recommendations import RecommendationEngine, Recommendation
from .analytics import TeamAnalytics, TeamBurnoutTrend
from .modes import BurnoutMode, BurnoutModeType

# Детектор может отсутствовать в легковесных окружениях (например, CI)
try:  # pragma: no cover - опциональный импорт
    from .detector import BurnoutDetector, BurnoutPattern  # type: ignore
except ImportError:  # pragma: no cover
    BurnoutDetector = None  # type: ignore
    BurnoutPattern = None  # type: ignore

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
    "BurnoutModeType"
] 

if BurnoutDetector is not None:
    __all__.extend(["BurnoutDetector", "BurnoutPattern"])
