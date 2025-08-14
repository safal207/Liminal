"""
Feature Extractor для ML-моделей.
Извлекает фичи из WebSocket метрик и логов для обучения AutoML моделей.
"""

import json
import time
from dataclasses import dataclass
from typing import Any, Dict, List, Optional

from loguru import logger


@dataclass
class WebSocketFeatures:
    """Структура фичей для ML-анализа WebSocket трафика."""

    timestamp: float
    user_id: str
    messages_per_minute: float
    avg_message_size: float
    connection_duration: float
    channels_count: int
    error_rate: float
    rate_limit_violations: int
    ip_address: str

    def to_dict(self) -> Dict[str, Any]:
        """Конвертирует в словарь для ML-обработки."""
        return {
            "timestamp": self.timestamp,
            "user_id": self.user_id,
            "messages_per_minute": self.messages_per_minute,
            "avg_message_size": self.avg_message_size,
            "connection_duration": self.connection_duration,
            "channels_count": self.channels_count,
            "error_rate": self.error_rate,
            "rate_limit_violations": self.rate_limit_violations,
            "ip_address": self.ip_address,
        }


class FeatureExtractor:
    """
    Извлекает фичи из WebSocket активности для ML-анализа.
    Интегрируется с Prometheus метриками и ConnectionManager.
    """

    def __init__(self):
        self.user_sessions: Dict[str, Dict] = {}
        self.feature_buffer: List[WebSocketFeatures] = []
        self.buffer_size = 1000  # Максимальный размер буфера

    def track_user_activity(
        self, user_id: str, message_size: int, channels: List[str], ip_address: str
    ) -> None:
        """
        Отслеживает активность пользователя для извлечения фичей.

        Args:
            user_id: ID пользователя
            message_size: Размер сообщения в байтах
            channels: Список каналов пользователя
            ip_address: IP адрес пользователя
        """
        current_time = time.time()

        if user_id not in self.user_sessions:
            self.user_sessions[user_id] = {
                "start_time": current_time,
                "message_count": 0,
                "total_message_size": 0,
                "channels": set(),
                "errors": 0,
                "rate_limit_violations": 0,
                "ip_address": ip_address,
                "last_activity": current_time,
            }

        session = self.user_sessions[user_id]
        session["message_count"] += 1
        session["total_message_size"] += message_size
        session["channels"].update(channels)
        session["last_activity"] = current_time

    def track_error(self, user_id: str) -> None:
        """Отслеживает ошибки пользователя."""
        if user_id in self.user_sessions:
            self.user_sessions[user_id]["errors"] += 1

    def track_rate_limit_violation(self, user_id: str) -> None:
        """Отслеживает нарушения rate limit."""
        if user_id in self.user_sessions:
            self.user_sessions[user_id]["rate_limit_violations"] += 1

    def extract_features(self, user_id: str) -> Optional[WebSocketFeatures]:
        """
        Извлекает фичи для конкретного пользователя.

        Args:
            user_id: ID пользователя

        Returns:
            WebSocketFeatures или None если недостаточно данных
        """
        if user_id not in self.user_sessions:
            return None

        session = self.user_sessions[user_id]
        current_time = time.time()

        # Вычисляем фичи
        connection_duration = current_time - session["start_time"]
        if connection_duration < 60:  # Минимум 1 минута для стабильных фичей
            return None

        messages_per_minute = session["message_count"] / (connection_duration / 60)
        avg_message_size = session["total_message_size"] / max(
            session["message_count"], 1
        )
        error_rate = session["errors"] / max(session["message_count"], 1)

        features = WebSocketFeatures(
            timestamp=current_time,
            user_id=user_id,
            messages_per_minute=messages_per_minute,
            avg_message_size=avg_message_size,
            connection_duration=connection_duration,
            channels_count=len(session["channels"]),
            error_rate=error_rate,
            rate_limit_violations=session["rate_limit_violations"],
            ip_address=session["ip_address"],
        )

        # Добавляем в буфер для последующего анализа
        self.feature_buffer.append(features)

        # Ограничиваем размер буфера
        if len(self.feature_buffer) > self.buffer_size:
            self.feature_buffer.pop(0)

        logger.debug(f"Извлечены фичи для пользователя {user_id}: {features}")
        return features

    def get_recent_features(self, limit: int = 100) -> List[Dict[str, Any]]:
        """
        Возвращает последние извлеченные фичи для ML-анализа.

        Args:
            limit: Максимальное количество записей

        Returns:
            Список фичей в формате словарей
        """
        recent_features = self.feature_buffer[-limit:] if self.feature_buffer else []
        return [f.to_dict() for f in recent_features]

    def cleanup_old_sessions(self, max_age_hours: int = 24) -> None:
        """
        Очищает старые сессии пользователей.

        Args:
            max_age_hours: Максимальный возраст сессии в часах
        """
        current_time = time.time()
        max_age_seconds = max_age_hours * 3600

        old_users = [
            user_id
            for user_id, session in self.user_sessions.items()
            if current_time - session["last_activity"] > max_age_seconds
        ]

        for user_id in old_users:
            del self.user_sessions[user_id]

        if old_users:
            logger.info(f"Очищены старые сессии для {len(old_users)} пользователей")


# Глобальный экземпляр для использования в приложении
feature_extractor = FeatureExtractor()
