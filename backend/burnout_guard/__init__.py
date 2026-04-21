"""
🚀🛡️ BurnoutGuard — AI-защита от выгорания
"""

from .analytics import TeamAnalytics
from .analytics import TeamBurnoutTrend as BurnoutTrend
from .core import BurnoutGuardEngine, BurnoutRisk, BurnoutState
from .detector import BurnoutDetector, BurnoutPattern
from .modes import BurnoutMode, BurnoutModeType
from .recommendations import Recommendation, RecommendationEngine

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
    "BurnoutModeType",
]
