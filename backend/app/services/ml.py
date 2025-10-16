"""ML integration helpers for FastAPI routes."""
from __future__ import annotations

from typing import Any, Dict, Optional

from backend.ml.client import MLInferenceClient


class MLService:
    """Expose ML helper functions backed by the ML inference client."""

    def __init__(self, client: Optional[MLInferenceClient] = None) -> None:
        self._client = client or MLInferenceClient()
        self.enabled = self._client.enabled

    def collect_metrics(self) -> Dict[str, Any]:
        return self._client.collect_metrics()

    def register_ip_address(self, ip: str) -> None:
        self._client.register_ip(ip)

    def register_channel_activity(self, channel: str) -> None:
        self._client.register_channel(channel)

    def register_auth_event(self, user_id: str, success: bool) -> None:
        self._client.register_auth(user_id, success)
