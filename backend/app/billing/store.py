"""JSON-backed persistence for billing state (dev/small deploy)."""

from __future__ import annotations

import json
import threading
from pathlib import Path
from typing import Optional, Set

from .models import BillingStoreData, UserBillingState

_lock = threading.Lock()
_processed_ids: Set[str] = set()


def _default_store_path() -> Path:
    return Path(__file__).resolve().parent / "data" / "billing_store.json"


def get_store_path() -> Path:
    from backend.core.settings import get_settings

    s = get_settings()
    raw = getattr(s, "billing", None)
    path_str = getattr(raw, "store_path", None) if raw else None
    if path_str:
        return Path(path_str)
    return _default_store_path()


def _ensure_parent(path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)


def _load_raw() -> BillingStoreData:
    path = get_store_path()
    if not path.is_file():
        return BillingStoreData()
    try:
        with path.open("r", encoding="utf-8") as f:
            data = json.load(f)
    except (json.JSONDecodeError, OSError):
        return BillingStoreData()
    users = data.get("users") or {}
    ev_ids = data.get("processed_event_ids") or []
    if not isinstance(users, dict):
        users = {}
    if not isinstance(ev_ids, list):
        ev_ids = []
    return BillingStoreData(users=users, processed_event_ids=ev_ids)


def _save_raw(data: BillingStoreData) -> None:
    path = get_store_path()
    _ensure_parent(path)
    payload = {
        "users": data.users,
        "processed_event_ids": data.processed_event_ids[-500:],
    }
    with path.open("w", encoding="utf-8") as f:
        json.dump(payload, f, indent=2, ensure_ascii=False)


def get_user_state(user_id: str) -> UserBillingState:
    with _lock:
        raw = _load_raw()
        u = raw.users.get(user_id)
        if not u:
            return UserBillingState(user_id=user_id)
        return UserBillingState.from_dict({**u, "user_id": user_id})


def upsert_user_state(state: UserBillingState) -> None:
    with _lock:
        raw = _load_raw()
        raw.users[state.user_id] = state.to_dict()
        _save_raw(raw)


def is_event_processed(event_id: str) -> bool:
    with _lock:
        raw = _load_raw()
        return event_id in raw.processed_event_ids or event_id in _processed_ids


def mark_event_processed(event_id: str) -> None:
    with _lock:
        _processed_ids.add(event_id)
        raw = _load_raw()
        if event_id not in raw.processed_event_ids:
            raw.processed_event_ids.append(event_id)
            _save_raw(raw)


def find_user_id_by_stripe_customer(customer_id: str) -> Optional[str]:
    with _lock:
        raw = _load_raw()
        for uid, u in raw.users.items():
            if u.get("stripe_customer_id") == customer_id:
                return uid
    return None
