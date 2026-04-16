"""
Anomaly Detector для обнаружения аномалий в WebSocket трафике.
Интегрируется с AutoML моделями и предоставляет real-time анализ.
"""

import time
from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Tuple

from logging_config import get_logger
logger = get_logger(__name__)

from .feature_extractor import WebSocketFeatures, feature_extractor
from .model_manager import model_manager


@dataclass
class AnomalyAlert:
    """Структура алерта об аномалии."""

    timestamp: float
    user_id: str
    anomaly_type: str
    severity: str  # low, medium, high, critical
    confidence: float
    features: Dict[str, Any]
    recommended_action: str


class AnomalyDetector:
    """
    Детектор аномалий для WebSocket трафика.
    Использует ML-модели и эвристические правила.
    """

    def __init__(self):
        self.alerts_buffer: List[AnomalyAlert] = []
        self.user_baselines: Dict[str, Dict[str, float]] = {}
        self.detection_rules = {
            "message_flood": {
                "threshold": 100,  # сообщений в минуту
                "severity": "high",
            },
            "error_spike": {"threshold": 0.5, "severity": "medium"},  # 50% ошибок
            "rate_limit_abuse": {
                "threshold": 10,  # нарушений за сессию
                "severity": "high",
            },
            "suspicious_channels": {
                "threshold": 20,  # каналов одновременно
                "severity": "medium",
            },
        }

    def update_user_baseline(self, user_id: str, features: WebSocketFeatures) -> None:
        """
        Обновляет базовую линию поведения пользователя.

        Args:
            user_id: ID пользователя
            features: Фичи активности
        """
        if user_id not in self.user_baselines:
            self.user_baselines[user_id] = {
                "avg_messages_per_minute": features.messages_per_minute,
                "avg_message_size": features.avg_message_size,
                "typical_channels": features.channels_count,
                "samples_count": 1,
            }
        else:
            baseline = self.user_baselines[user_id]
            samples = baseline["samples_count"]

            # Экспоненциальное скользящее среднее
            alpha = 0.1  # Коэффициент обучения
            baseline["avg_messages_per_minute"] = (
                alpha * features.messages_per_minute
                + (1 - alpha) * baseline["avg_messages_per_minute"]
            )
            baseline["avg_message_size"] = (
                alpha * features.avg_message_size
                + (1 - alpha) * baseline["avg_message_size"]
            )
            baseline["typical_channels"] = (
                alpha * features.channels_count
                + (1 - alpha) * baseline["typical_channels"]
            )
            baseline["samples_count"] = min(samples + 1, 1000)  # Ограничиваем счетчик

    def detect_rule_based_anomalies(
        self, features: WebSocketFeatures
    ) -> List[AnomalyAlert]:
        """
        Обнаруживает аномалии на основе эвристических правил.

        Args:
            features: Фичи для анализа

        Returns:
            Список обнаруженных аномалий
        """
        alerts = []
        current_time = time.time()

        # 1. Проверка на флуд сообщений
        if (
            features.messages_per_minute
            > self.detection_rules["message_flood"]["threshold"]
        ):
            alerts.append(
                AnomalyAlert(
                    timestamp=current_time,
                    user_id=features.user_id,
                    anomaly_type="message_flood",
                    severity=self.detection_rules["message_flood"]["severity"],
                    confidence=min(features.messages_per_minute / 100, 1.0),
                    features=features.to_dict(),
                    recommended_action="Применить временный rate limit или заблокировать пользователя",
                )
            )

        # 2. Проверка на высокий уровень ошибок
        if features.error_rate > self.detection_rules["error_spike"]["threshold"]:
            alerts.append(
                AnomalyAlert(
                    timestamp=current_time,
                    user_id=features.user_id,
                    anomaly_type="error_spike",
                    severity=self.detection_rules["error_spike"]["severity"],
                    confidence=features.error_rate,
                    features=features.to_dict(),
                    recommended_action="Проверить состояние клиента или качество соединения",
                )
            )

        # 3. Проверка на злоупотребление rate limit
        if (
            features.rate_limit_violations
            > self.detection_rules["rate_limit_abuse"]["threshold"]
        ):
            alerts.append(
                AnomalyAlert(
                    timestamp=current_time,
                    user_id=features.user_id,
                    anomaly_type="rate_limit_abuse",
                    severity=self.detection_rules["rate_limit_abuse"]["severity"],
                    confidence=min(features.rate_limit_violations / 20, 1.0),
                    features=features.to_dict(),
                    recommended_action="Увеличить строгость rate limiting для пользователя",
                )
            )

        # 4. Проверка на подозрительное количество каналов
        if (
            features.channels_count
            > self.detection_rules["suspicious_channels"]["threshold"]
        ):
            alerts.append(
                AnomalyAlert(
                    timestamp=current_time,
                    user_id=features.user_id,
                    anomaly_type="suspicious_channels",
                    severity=self.detection_rules["suspicious_channels"]["severity"],
                    confidence=min(features.channels_count / 50, 1.0),
                    features=features.to_dict(),
                    recommended_action="Ограничить количество каналов для пользователя",
                )
            )

        return alerts

    def detect_ml_based_anomalies(
        self, features: WebSocketFeatures
    ) -> List[AnomalyAlert]:
        """
        Обнаруживает аномалии используя ML-модели.

        Args:
            features: Фичи для анализа

        Returns:
            Список обнаруженных аномалий
        """
        alerts = []
        current_time = time.time()

        try:
            # Используем ML-модель для детекции аномалий
            prediction = model_manager.predict("anomaly_detection", features.to_dict())

            if prediction and prediction.get("is_anomaly", False):
                confidence = prediction.get("anomaly_score", 0.5)

                # Определяем серьезность на основе confidence score
                if confidence > 0.8:
                    severity = "critical"
                elif confidence > 0.6:
                    severity = "high"
                elif confidence > 0.4:
                    severity = "medium"
                else:
                    severity = "low"

                alerts.append(
                    AnomalyAlert(
                        timestamp=current_time,
                        user_id=features.user_id,
                        anomaly_type="ml_detected_anomaly",
                        severity=severity,
                        confidence=confidence,
                        features=features.to_dict(),
                        recommended_action="Детальный анализ активности пользователя",
                    )
                )

        except Exception as e:
            logger.error(f"Ошибка ML-детекции аномалий: {e}")

        return alerts

    def detect_baseline_deviations(
        self, features: WebSocketFeatures
    ) -> List[AnomalyAlert]:
        """
        Обнаруживает отклонения от базовой линии пользователя.

        Args:
            features: Фичи для анализа

        Returns:
            Список обнаруженных аномалий
        """
        alerts = []
        current_time = time.time()
        user_id = features.user_id

        if user_id not in self.user_baselines:
            return alerts  # Недостаточно данных для сравнения

        baseline = self.user_baselines[user_id]

        # Проверяем отклонения от нормального поведения
        message_rate_deviation = abs(
            features.messages_per_minute - baseline["avg_messages_per_minute"]
        ) / max(baseline["avg_messages_per_minute"], 1)

        channels_deviation = abs(
            features.channels_count - baseline["typical_channels"]
        ) / max(baseline["typical_channels"], 1)

        # Если отклонение больше 300% от нормы
        if message_rate_deviation > 3.0:
            alerts.append(
                AnomalyAlert(
                    timestamp=current_time,
                    user_id=user_id,
                    anomaly_type="behavior_deviation",
                    severity="medium",
                    confidence=min(message_rate_deviation / 5, 1.0),
                    features=features.to_dict(),
                    recommended_action="Мониторить изменения в поведении пользователя",
                )
            )

        if channels_deviation > 2.0:
            alerts.append(
                AnomalyAlert(
                    timestamp=current_time,
                    user_id=user_id,
                    anomaly_type="channel_pattern_change",
                    severity="low",
                    confidence=min(channels_deviation / 3, 1.0),
                    features=features.to_dict(),
                    recommended_action="Проверить изменения в использовании каналов",
                )
            )

        return alerts

    def analyze_user_activity(self, user_id: str) -> List[AnomalyAlert]:
        """
        Анализирует активность пользователя на предмет аномалий.

        Args:
            user_id: ID пользователя

        Returns:
            Список обнаруженных аномалий
        """
        # Извлекаем фичи для пользователя
        features = feature_extractor.extract_features(user_id)
        if not features:
            return []

        all_alerts = []

        # 1. Эвристические правила
        rule_alerts = self.detect_rule_based_anomalies(features)
        all_alerts.extend(rule_alerts)

        # 2. ML-модели
        ml_alerts = self.detect_ml_based_anomalies(features)
        all_alerts.extend(ml_alerts)

        # 3. Отклонения от базовой линии
        baseline_alerts = self.detect_baseline_deviations(features)
        all_alerts.extend(baseline_alerts)

        # Обновляем базовую линию пользователя
        self.update_user_baseline(user_id, features)

        # Сохраняем алерты в буфер
        self.alerts_buffer.extend(all_alerts)

        # Ограничиваем размер буфера
        if len(self.alerts_buffer) > 1000:
            self.alerts_buffer = self.alerts_buffer[-1000:]

        # Логируем критические алерты
        for alert in all_alerts:
            if alert.severity in ["high", "critical"]:
                logger.warning(
                    f"Аномалия {alert.anomaly_type} для пользователя {user_id}: "
                    f"severity={alert.severity}, confidence={alert.confidence:.2f}"
                )

        return all_alerts

    def get_recent_alerts(
        self, limit: int = 50, min_severity: str = "low"
    ) -> List[Dict[str, Any]]:
        """
        Возвращает последние алерты.

        Args:
            limit: Максимальное количество алертов
            min_severity: Минимальная серьезность алертов

        Returns:
            Список алертов в формате словарей
        """
        severity_levels = {"low": 0, "medium": 1, "high": 2, "critical": 3}
        min_level = severity_levels.get(min_severity, 0)

        filtered_alerts = [
            alert
            for alert in self.alerts_buffer
            if severity_levels.get(alert.severity, 0) >= min_level
        ]

        # Сортируем по времени (новые первыми) и ограничиваем количество
        recent_alerts = sorted(
            filtered_alerts, key=lambda x: x.timestamp, reverse=True
        )[:limit]

        return [
            {
                "timestamp": alert.timestamp,
                "user_id": alert.user_id,
                "anomaly_type": alert.anomaly_type,
                "severity": alert.severity,
                "confidence": alert.confidence,
                "recommended_action": alert.recommended_action,
            }
            for alert in recent_alerts
        ]


# Глобальный экземпляр для использования в приложении
anomaly_detector = AnomalyDetector()
