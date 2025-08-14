"""
Пакет utils - вспомогательные утилиты для работы с агентами.

Модули:
    event_bus: Реализация шины событий на основе Redis
"""

from .event_bus import EventBus

__all__ = [
    "EventBus",
]
