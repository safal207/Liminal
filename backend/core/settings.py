"""Central application configuration loaded deterministically from environment variables."""

from __future__ import annotations

import os
import secrets
from functools import lru_cache
from typing import Any, Sequence

from dotenv import load_dotenv
from pydantic import BaseModel, Field

load_dotenv(override=False)

DEFAULT_SECRET = ""
DEFAULT_ALGORITHM = "HS256"
DEFAULT_EXPIRE_MINUTES = 30
DEFAULT_MEMORY_TIMELINE_INITIAL_LIMIT = 100
DEFAULT_MEMORY_TIMELINE_MAX_ITEMS = 1000
_EPHEMERAL_DEV_SECRET = secrets.token_urlsafe(48)


def _first_env(names: Sequence[str], default: str = "") -> str:
    for name in names:
        value = os.getenv(name)
        if value is not None:
            return value
    return default


def _env_bool(names: Sequence[str], default: bool = False) -> bool:
    raw = _first_env(names)
    if not raw:
        return default
    return raw.strip().lower() in {"1", "true", "yes", "on"}


def _env_int(names: Sequence[str], default: int) -> int:
    raw = _first_env(names)
    if not raw:
        return default
    try:
        return int(raw)
    except ValueError:
        return default


def _jwt_secret() -> str:
    configured = _first_env(("JWT__SECRET_KEY", "JWT_SECRET_KEY")).strip()
    if configured:
        return configured
    environment = _first_env(("ENV",), "development").strip().lower()
    if environment == "production":
        return DEFAULT_SECRET
    return _EPHEMERAL_DEV_SECRET


class JWTSettings(BaseModel):
    """JWT generation and validation settings."""

    secret_key: str = Field(default_factory=_jwt_secret)
    algorithm: str = DEFAULT_ALGORITHM
    access_token_expire_minutes: int = DEFAULT_EXPIRE_MINUTES


class MemoryTimelineSettings(BaseModel):
    """In-memory timeline retention settings."""

    initial_state_limit: int = DEFAULT_MEMORY_TIMELINE_INITIAL_LIMIT
    max_retained_events: int = DEFAULT_MEMORY_TIMELINE_MAX_ITEMS


class BillingSettings(BaseModel):
    """Stripe and local billing store settings."""

    stripe_secret_key: str = ""
    stripe_webhook_secret: str = ""
    stripe_price_pro_monthly: str = ""
    stripe_success_url: str = "http://127.0.0.1:8000/"
    stripe_cancel_url: str = "http://127.0.0.1:8000/"
    store_path: str = ""


class IntegrationSettings(BaseModel):
    """External service and infrastructure settings."""

    use_redis: bool = False
    redis_url: str = "redis://localhost:6379/0"
    neo4j_uri: str = "bolt://localhost:7687"
    neo4j_user: str = "neo4j"
    neo4j_password: str = ""
    ml_enabled: bool = False


class Settings(BaseModel):
    """Authoritative settings object used by all runtime components."""

    jwt: JWTSettings = Field(default_factory=JWTSettings)
    memory_timeline: MemoryTimelineSettings = Field(
        default_factory=MemoryTimelineSettings
    )
    integrations: IntegrationSettings = Field(default_factory=IntegrationSettings)
    billing: BillingSettings = Field(default_factory=BillingSettings)

    def __init__(self, **values: Any) -> None:
        values = dict(values)
        values.setdefault(
            "jwt",
            JWTSettings(
                secret_key=_jwt_secret(),
                algorithm=_first_env(
                    ("JWT__ALGORITHM", "JWT_ALGORITHM"), DEFAULT_ALGORITHM
                ),
                access_token_expire_minutes=_env_int(
                    (
                        "JWT__ACCESS_TOKEN_EXPIRE_MINUTES",
                        "JWT_ACCESS_TOKEN_EXPIRE_MINUTES",
                    ),
                    DEFAULT_EXPIRE_MINUTES,
                ),
            ),
        )
        values.setdefault(
            "memory_timeline",
            MemoryTimelineSettings(
                initial_state_limit=_env_int(
                    (
                        "MEMORY_TIMELINE__INITIAL_STATE_LIMIT",
                        "MEMORY_TIMELINE_INITIAL_STATE_LIMIT",
                    ),
                    DEFAULT_MEMORY_TIMELINE_INITIAL_LIMIT,
                ),
                max_retained_events=_env_int(
                    (
                        "MEMORY_TIMELINE__MAX_RETAINED_EVENTS",
                        "MEMORY_TIMELINE_MAX_RETAINED_EVENTS",
                    ),
                    DEFAULT_MEMORY_TIMELINE_MAX_ITEMS,
                ),
            ),
        )
        values.setdefault(
            "integrations",
            IntegrationSettings(
                use_redis=_env_bool(("INTEGRATIONS__USE_REDIS", "USE_REDIS"), False),
                redis_url=_first_env(
                    ("INTEGRATIONS__REDIS_URL", "REDIS_URL"),
                    "redis://localhost:6379/0",
                ),
                neo4j_uri=_first_env(
                    ("INTEGRATIONS__NEO4J_URI", "NEO4J_URI"),
                    "bolt://localhost:7687",
                ),
                neo4j_user=_first_env(
                    ("INTEGRATIONS__NEO4J_USER", "NEO4J_USER"), "neo4j"
                ),
                neo4j_password=_first_env(
                    ("INTEGRATIONS__NEO4J_PASSWORD", "NEO4J_PASSWORD")
                ),
                ml_enabled=_env_bool(("INTEGRATIONS__ML_ENABLED", "ML_ENABLED"), False),
            ),
        )
        values.setdefault(
            "billing",
            BillingSettings(
                stripe_secret_key=_first_env(
                    ("BILLING__STRIPE_SECRET_KEY", "STRIPE_SECRET_KEY")
                ),
                stripe_webhook_secret=_first_env(
                    (
                        "BILLING__STRIPE_WEBHOOK_SECRET",
                        "STRIPE_WEBHOOK_SECRET",
                    )
                ),
                stripe_price_pro_monthly=_first_env(
                    (
                        "BILLING__STRIPE_PRICE_PRO_MONTHLY",
                        "STRIPE_PRICE_PRO_MONTHLY",
                    )
                ),
                stripe_success_url=_first_env(
                    ("BILLING__STRIPE_SUCCESS_URL", "STRIPE_SUCCESS_URL"),
                    "http://127.0.0.1:8000/",
                ),
                stripe_cancel_url=_first_env(
                    ("BILLING__STRIPE_CANCEL_URL", "STRIPE_CANCEL_URL"),
                    "http://127.0.0.1:8000/",
                ),
                store_path=_first_env(("BILLING__STORE_PATH", "BILLING_STORE_PATH")),
            ),
        )
        super().__init__(**values)


@lru_cache()
def get_settings() -> Settings:
    return Settings()


settings = get_settings()

__all__ = [
    "DEFAULT_SECRET",
    "Settings",
    "JWTSettings",
    "MemoryTimelineSettings",
    "IntegrationSettings",
    "BillingSettings",
    "settings",
    "get_settings",
]
