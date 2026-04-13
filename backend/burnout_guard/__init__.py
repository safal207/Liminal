"""
🚀🛡️ BurnoutGuard — AI-защита от выгорания
"""

from .core import BurnoutGuardEngine, BurnoutState, BurnoutRisk
from .detector import BurnoutDetector, BurnoutPattern
from .recommendations import RecommendationEngine, Recommendation
from .analytics import TeamAnalytics, TeamBurnoutTrend as BurnoutTrend
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
