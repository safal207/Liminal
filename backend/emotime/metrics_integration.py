"""
🌿✨ Emotime Metrics Integration — интеграция с системой метрик LIMINAL

Класс для интеграции Emotime с Prometheus метриками LIMINAL.
Отслеживает все аспекты работы эмоциональных временных рядов.
"""

import time
from datetime import datetime
from typing import Dict, List, Optional, Any

try:
    from metrics.collectors import (
        emotime_sensor_data_total,
        emotime_emotional_features,
        emotime_mode_duration_seconds,
        emotime_mode_transitions_total,
        emotime_fusion_confidence,
        emotime_heartbeat_total,
        emotime_timeseries_points,
        emotime_peak_detection_total,
    )

    METRICS_AVAILABLE = True
except ImportError:
    try:
        from .utils import safe_logger

        safe_logger.warning("Prometheus metrics not available")
    except:
        print("WARNING: Emotime - Prometheus metrics not available")
    METRICS_AVAILABLE = False

from .sensors import SensorData, SensorType
from .fusion import EmotionalFeatures
from .modes import EmotionalMode, ModeType
from .timeseries import EmotionalPoint


class EmotimeMetrics:
    """
    Система метрик для Emotime.

    Интегрируется с Prometheus для отслеживания:
    - Данных с сенсоров
    - Эмоциональных признаков
    - Режимов и переходов
    - Производительности системы
    """

    def __init__(self):
        self.metrics_enabled = METRICS_AVAILABLE
        self.last_mode: Optional[str] = None
        self.mode_start_time: Optional[float] = None

        try:
            from .utils import safe_logger

            if not self.metrics_enabled:
                safe_logger.warning("Metrics disabled - Prometheus not available")
            else:
                safe_logger.info("Metrics enabled")
        except:
            if not self.metrics_enabled:
                print("Emotime metrics disabled")
            else:
                print("Emotime metrics enabled")

    def record_sensor_data(self, sensor_data: SensorData):
        """Записывает метрики данных с сенсоров."""
        if not self.metrics_enabled:
            return

        sensor_type = sensor_data.sensor_type.value
        emotime_sensor_data_total.labels(sensor_type=sensor_type).inc()

    def record_emotional_features(self, features: EmotionalFeatures):
        """Записывает метрики эмоциональных признаков."""
        if not self.metrics_enabled:
            return

        # Записываем распределения всех признаков
        emotime_emotional_features.labels(feature_type="valence").observe(
            features.valence
        )
        emotime_emotional_features.labels(feature_type="arousal").observe(
            features.arousal
        )
        emotime_emotional_features.labels(feature_type="dominance").observe(
            features.dominance
        )
        emotime_emotional_features.labels(feature_type="tempo").observe(features.tempo)
        emotime_emotional_features.labels(feature_type="intensity").observe(
            features.intensity
        )

        # Записываем уверенность фьюжн системы
        if features.confidence:
            emotime_fusion_confidence.observe(features.confidence)

    def record_mode_transition(self, new_mode: EmotionalMode):
        """Записывает переход между эмоциональными режимами."""
        if not self.metrics_enabled:
            return

        current_time = time.time()
        current_mode = new_mode.type.value

        # Если это переход от предыдущего режима
        if self.last_mode and self.last_mode != current_mode:
            # Записываем продолжительность предыдущего режима
            if self.mode_start_time:
                duration = current_time - self.mode_start_time
                emotime_mode_duration_seconds.labels(mode_type=self.last_mode).observe(
                    duration
                )

            # Записываем переход
            emotime_mode_transitions_total.labels(
                from_mode=self.last_mode, to_mode=current_mode
            ).inc()

        # Обновляем текущий режим
        if self.last_mode != current_mode:
            self.last_mode = current_mode
            self.mode_start_time = current_time

    def record_heartbeat(self, status: str = "success"):
        """Записывает сердцебиение Emotime."""
        if not self.metrics_enabled:
            return

        emotime_heartbeat_total.labels(status=status).inc()

    def record_timeseries_points(self, point_count: int):
        """Записывает количество точек в временных рядах."""
        if not self.metrics_enabled:
            return

        emotime_timeseries_points.set(point_count)

    def record_peak_detection(self, point: EmotionalPoint):
        """Записывает обнаружение эмоционального пика."""
        if not self.metrics_enabled or not point.is_peak:
            return

        # Определяем тип пика на основе валентности
        peak_type = "positive" if point.valence > 0 else "negative"
        emotime_peak_detection_total.labels(peak_type=peak_type).inc()

    def record_batch_processing(
        self, sensor_data_list: List[SensorData], features: Optional[EmotionalFeatures]
    ):
        """Записывает метрики батчевой обработки."""
        if not self.metrics_enabled:
            return

        # Записываем данные со всех сенсоров в батче
        for sensor_data in sensor_data_list:
            self.record_sensor_data(sensor_data)

        # Записываем результирующие признаки
        if features:
            self.record_emotional_features(features)
            self.record_heartbeat("success")
        else:
            self.record_heartbeat("no_data")

    def get_metrics_summary(self) -> Dict[str, Any]:
        """Возвращает сводку по метрикам Emotime."""
        if not self.metrics_enabled:
            return {"status": "metrics_disabled"}

        return {
            "status": "active",
            "metrics_enabled": True,
            "current_mode": self.last_mode,
            "mode_duration": (
                time.time() - self.mode_start_time if self.mode_start_time else 0
            ),
            "available_metrics": [
                "sensor_data_total",
                "emotional_features",
                "mode_duration_seconds",
                "mode_transitions_total",
                "fusion_confidence",
                "heartbeat_total",
                "timeseries_points",
                "peak_detection_total",
            ],
        }


# Глобальный экземпляр метрик
emotime_metrics = EmotimeMetrics()
