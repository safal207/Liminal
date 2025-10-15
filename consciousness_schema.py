"""Compatibility module exposing backend.consciousness_schema at the repository root.

This allows test suites and legacy scripts that import ``consciousness_schema``
from the project root to function without needing to modify their import
statements. The canonical implementation lives in ``backend.consciousness_schema``.
"""

from backend.consciousness_schema import (
    PHILOSOPHICAL_THRESHOLDS,
    ConsciousnessNode,
    ConsciousnessState,
    TransitionTrigger,
)

__all__ = [
    "PHILOSOPHICAL_THRESHOLDS",
    "ConsciousnessNode",
    "ConsciousnessState",
    "TransitionTrigger",
]
