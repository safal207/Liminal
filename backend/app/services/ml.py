"""ML integration helpers for FastAPI routes."""
from __future__ import annotations

from typing import Any, Dict


class MLService:
    """Expose ML helper functions with safe fallbacks when ML is disabled."""

    def __init__(self) -> None:
        try:
            from backend.ml_features import (  # type: ignore
                ML_ENABLED,
                extract_traffic_features,
                extract_user_patterns,
                get_current_anomaly_scores,
                get_prediction_features,
                register_auth_event,
                register_channel_activity,
                register_ip_address,
            )

            self.enabled = ML_ENABLED
            self._extract_user_patterns = extract_user_patterns
            self._extract_traffic_features = extract_traffic_features
            self._get_current_anomaly_scores = get_current_anomaly_scores
            self._get_prediction_features = get_prediction_features
            self._register_auth_event = register_auth_event
            self._register_channel_activity = register_channel_activity
            self._register_ip_address = register_ip_address
        except Exception:
            self.enabled = False
            self._extract_user_patterns = lambda: {"enabled": False}
            self._extract_traffic_features = lambda: {"enabled": False}
            self._get_current_anomaly_scores = lambda: {"enabled": False}
            self._get_prediction_features = lambda: {"enabled": False}
            self._register_auth_event = lambda *args, **kwargs: None
            self._register_channel_activity = lambda *args, **kwargs: None
            self._register_ip_address = lambda *args, **kwargs: None

    def collect_metrics(self) -> Dict[str, Any]:
        return {
            "user_patterns": self._extract_user_patterns(),
            "traffic_features": self._extract_traffic_features(),
            "anomaly_scores": self._get_current_anomaly_scores(),
            "prediction_features": self._get_prediction_features(),
        }

    def register_ip_address(self, ip: str) -> None:
        self._register_ip_address(ip)

    def register_channel_activity(self, channel: str) -> None:
        self._register_channel_activity(channel)

    def register_auth_event(self, user_id: str, success: bool) -> None:
        self._register_auth_event(user_id, success)
