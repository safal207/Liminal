"""
🧭🧠 Retrosplenial Gateway Layer — Brain's Internal Compass

Inspired by neuroscience discovery: Scientists found that the retrosplenial complex (RSC)
and superior parietal lobule work as the brain's internal compass, maintaining directional
orientation regardless of environmental changes.

This layer serves as LIMINAL's central navigation system:
- Direction Encoding: Maps events to semantic directions (N/E/S/W of meaning)
- Context Stability: Maintains orientation during transitions
- Memory Compass: Links perception to spatial-temporal memory
- Transition Navigation: Guides movement through liminal states

Architecture Integration:
[ Perception Layer ] → [ Retrosplenial Gateway Layer ] → [ Memory Layer ]
                              ↓
                      [ Scripts Layer ] ↔ [ ChronoAnalytics ]

"Like a blade of grass in the wind that, no matter how it bends,
 always points to where the sun is." — The brain's compass never loses direction.
"""

from .alpha_engine import AlphaWaveEngine
from .beta_engine import BetaWaveEngine
from .core import (
    DirectionEncoder,
    MemoryCompass,
    NavigationEvent,
    RetrosplenialGateway,
    TransitionNavigator,
)
from .delta_engine import DeltaWaveEngine
from .directions import DirectionalSpace, NavigationContext, SemanticDirection
from .gamma_engine import GammaSynchronyCompass
from .stability import ContextStabilizer, OrientationMaintainer, TransitionBalance
from .theta_engine import ThetaOscillationEngine

__all__ = [
    "RetrosplenialGateway",
    "DirectionEncoder",
    "MemoryCompass",
    "TransitionNavigator",
    "NavigationEvent",
    "ThetaOscillationEngine",
    "GammaSynchronyCompass",
    "AlphaWaveEngine",
    "BetaWaveEngine",
    "DeltaWaveEngine",
    "SemanticDirection",
    "DirectionalSpace",
    "NavigationContext",
    "ContextStabilizer",
    "TransitionBalance",
    "OrientationMaintainer",
]

__version__ = "1.0.0"
__description__ = "Brain-inspired navigation system for liminal transitions"
