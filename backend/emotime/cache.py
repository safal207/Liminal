"""Simple in-memory cache used by the Emotime API."""

from __future__ import annotations

import hashlib
from typing import Any, Dict, Optional


class EmotimeCache:
    """Tiny in-memory cache shim for local runs and tests."""

    def __init__(self) -> None:
        self._items: Dict[str, Any] = {}

    def get_cache_key(self, text: str, user_id: str) -> str:
        payload = f"{user_id}:{text}".encode("utf-8")
        return hashlib.sha256(payload).hexdigest()

    def get(self, key: str) -> Optional[Any]:
        return self._items.get(key)

    def set(self, key: str, value: Any, ttl: int | None = None) -> None:
        _ = ttl
        self._items[key] = value


emotime_cache = EmotimeCache()
