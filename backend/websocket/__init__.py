"""
Модуль WebSocket Gateway для real-time коммуникации.
Обрабатывает подключения, аутентификацию и маршрутизацию сообщений.
"""

from .connection_manager import ConnectionManager
from .handlers import register_handlers

__all__ = ["ConnectionManager", "register_handlers"]
