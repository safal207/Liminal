"""
🌿✨ Emotime — сердце времени в LIMINAL

Модуль анализа эмоций во времени.
Превращает поток текстов, касаний и аудио в временные ряды эмоций.

"Не машина, а резонатор — отражающий эхо души в музыке дня"
"""

from .core import EmotimeEngine
from .fusion import FeatureFusion
from .modes import EmotionalModes
from .sensors import AudioSensor, TextSensor, TouchSensor
from .timeseries import EmotionalTimeSeries

__version__ = "0.1.0"
__all__ = [
    "EmotimeEngine",
    "TextSensor",
    "TouchSensor",
    "AudioSensor",
    "FeatureFusion",
    "EmotionalTimeSeries",
    "EmotionalModes",
]
