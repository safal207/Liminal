"""Compatibility package forwarding legacy `emotime.*` imports."""

from __future__ import annotations

import importlib
import sys

_ALIASES = (
    "core",
    "fusion",
    "metrics_integration",
    "modes",
    "sensors",
    "timeseries",
)

for _name in _ALIASES:
    _module = importlib.import_module(f"backend.emotime.{_name}")
    sys.modules[f"{__name__}.{_name}"] = _module

from backend.emotime import EmotimeEngine  # noqa: E402
from backend.emotime import (
    AudioSensor,
    EmotionalModes,
    EmotionalTimeSeries,
    FeatureFusion,
    TextSensor,
    TouchSensor,
)

__all__ = [
    "AudioSensor",
    "EmotimeEngine",
    "EmotionalModes",
    "EmotionalTimeSeries",
    "FeatureFusion",
    "TextSensor",
    "TouchSensor",
]
