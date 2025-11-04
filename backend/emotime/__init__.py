"""
🌿✨ Emotime — сердце времени в LIMINAL

Модуль анализа эмоций во времени.
Превращает поток текстов, касаний и аудио в временные ряды эмоций.

"Не машина, а резонатор — отражающий эхо души в музыке дня"
"""

from .core import EmotimeEngine
from .sensors import TextSensor, TouchSensor, AudioSensor
from .fusion import FeatureFusion
from .timeseries import EmotionalTimeSeries
from .modes import EmotionalModes

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
