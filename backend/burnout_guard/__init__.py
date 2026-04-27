"""
🚀🛡️ BurnoutGuard — AI-защита от выгорания
"""

from .analytics import TeamAnalytics, TeamBurnoutTrend
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
    "RecommendationEngine",
    "Recommendation",
    "TeamAnalytics",
    "TeamBurnoutTrend",
    "BurnoutMode",
    "BurnoutModeType",
    "BurnoutDetector",
    "BurnoutPattern",
]
