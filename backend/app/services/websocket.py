"""WebSocket connection manager helpers."""
from __future__ import annotations

import os
from typing import Optional

from backend.redis_client import RedisClient
from backend.websocket.connection_manager import ConnectionManager


class ConnectionManagerService:
    """Factory for the appropriate connection manager implementation."""

    def __init__(self) -> None:
        self._manager: Optional[ConnectionManager] = None

    def _create_manager(self) -> ConnectionManager:
        max_connections = int(os.getenv("WS_MAX_CONNECTIONS", "100"))
        max_connections_per_ip = int(os.getenv("WS_MAX_CONNECTIONS_PER_IP", "10"))
        rate_limit_messages_per_second = int(
            os.getenv("WS_RATE_LIMIT_PER_SECOND", "10")
        )
        rate_limit_burst = int(os.getenv("WS_RATE_LIMIT_BURST", "20"))

        use_redis = os.getenv("USE_REDIS", "false").lower() == "true"
        if use_redis:
            from backend.websocket.redis_connection_manager import (  # type: ignore
                RedisConnectionManager,
            )

            redis_url = os.getenv("REDIS_URL", "redis://localhost:6379/0")
            return RedisConnectionManager(
                redis_url=redis_url,
                max_connections=max_connections,
                max_connections_per_ip=max_connections_per_ip,
            )

        redis_client = None
        if os.getenv("USE_REDIS_CLIENT", "false").lower() == "true":
            redis_client = RedisClient()

        return ConnectionManager(
            redis_client=redis_client,
            max_connections=max_connections,
            max_connections_per_ip=max_connections_per_ip,
            rate_limit_messages_per_second=rate_limit_messages_per_second,
            rate_limit_burst=rate_limit_burst,
        )

    def get_manager(self) -> ConnectionManager:
        if self._manager is None:
            self._manager = self._create_manager()
        return self._manager
