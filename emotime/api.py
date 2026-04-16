"""Compatibility wrapper for legacy `emotime.api` imports."""

from backend.emotime.api import *  # noqa: F401,F403
from backend.emotime.api import emotime_router

DEMO_SCENARIOS = {
    "calm_morning": {},
    "work_focus": {},
    "stress_peak": {},
    "joy_breakthrough": {},
}

__all__ = ["DEMO_SCENARIOS", "emotime_router"]
