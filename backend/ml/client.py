"""Client wrapper for interacting with the ML inference microservice."""
from __future__ import annotations

import logging
import os
from typing import Any, Dict, Optional

import requests

logger = logging.getLogger(__name__)


class MLInferenceError(RuntimeError):
    """Raised when the ML inference service cannot be reached."""


class LocalMLInferenceAdapter:
    """Fallback adapter that proxies to the legacy in-process ML module."""

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
        except Exception as exc:  # pragma: no cover - defensive fallback
            logger.warning("Falling back to disabled ML adapter: %s", exc)
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

    def register_ip(self, ip: str) -> None:
        self._register_ip_address(ip)

    def register_channel(self, channel: str) -> None:
        self._register_channel_activity(channel)

    def register_auth(self, user_id: str, success: bool) -> None:
        self._register_auth_event(user_id, success)


class MLInferenceClient:
    """HTTP client with graceful fallback to the local ML adapter."""

    def __init__(
        self,
        base_url: Optional[str] = None,
        timeout: float = 5.0,
        fallback: Optional[LocalMLInferenceAdapter] = None,
    ) -> None:
        self.base_url = base_url or os.getenv("ML_SERVICE_URL")
        self.timeout = timeout
        self._session = requests.Session() if self.base_url else None
        self._fallback = fallback or LocalMLInferenceAdapter()

    @property
    def enabled(self) -> bool:
        return bool(self.base_url) or self._fallback.enabled

    def _request(self, method: str, path: str, payload: Optional[Dict[str, Any]] = None) -> Any:
        if not self._session or not self.base_url:
            raise MLInferenceError("ML service URL is not configured")

        url = f"{self.base_url.rstrip('/')}/{path.lstrip('/')}"
        response = self._session.request(method, url, json=payload, timeout=self.timeout)
        response.raise_for_status()
        if response.headers.get("content-type", "").startswith("application/json"):
            return response.json()
        return {}

    def collect_metrics(self) -> Dict[str, Any]:
        if self.base_url:
            try:
                data = self._request("GET", "/ml/metrics/summary")
                if isinstance(data, dict):
                    return data
            except Exception as exc:
                logger.warning("Falling back to local ML metrics: %s", exc)
        return self._fallback.collect_metrics()

    def register_ip(self, ip: str) -> None:
        if self.base_url:
            try:
                self._request("POST", "/ml/events/ip", {"ip": ip})
                return
            except Exception as exc:
                logger.debug("ML service IP registration failed: %s", exc)
        self._fallback.register_ip(ip)

    def register_channel(self, channel: str) -> None:
        if self.base_url:
            try:
                self._request("POST", "/ml/events/channel", {"channel": channel})
                return
            except Exception as exc:
                logger.debug("ML service channel registration failed: %s", exc)
        self._fallback.register_channel(channel)

    def register_auth(self, user_id: str, success: bool) -> None:
        if self.base_url:
            try:
                self._request(
                    "POST",
                    "/ml/events/auth",
                    {"user_id": user_id, "success": success},
                )
                return
            except Exception as exc:
                logger.debug("ML service auth registration failed: %s", exc)
        self._fallback.register_auth(user_id, success)
