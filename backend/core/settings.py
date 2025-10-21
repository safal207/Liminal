"""Application-wide configuration loaded from environment variables."""

from __future__ import annotations

from functools import lru_cache
from typing import Any, Dict, Iterable, Tuple, Type

from pydantic import AliasChoices, BaseModel, Field

try:  # pragma: no cover - prefer the dedicated package when available
    from pydantic_settings import BaseSettings, SettingsConfigDict
    USING_PYDANTIC_SETTINGS = True
except Exception:  # pragma: no cover - fallback for environments without pydantic-settings
    from pydantic import BaseModel

    USING_PYDANTIC_SETTINGS = False

    class BaseSettings(BaseModel):  # type: ignore
        """Minimal fallback that mimics pydantic-settings behaviour."""

        model_config: Dict[str, Any] = {}

    SettingsConfigDict = Dict[str, Any]  # type: ignore[misc,assignment]

DEFAULT_SECRET = "resonance-liminal-secret-key-change-in-production"
DEFAULT_ALGORITHM = "HS256"
DEFAULT_EXPIRE_MINUTES = 30
DEFAULT_MEMORY_TIMELINE_INITIAL_LIMIT = 100
DEFAULT_MEMORY_TIMELINE_MAX_ITEMS = 1000


def _cast_bool(raw_value: str) -> bool:
    return raw_value.strip().lower() in {"1", "true", "yes", "on"}


class JWTSettings(BaseModel):
    """Configuration related to JWT token generation and validation."""

    secret_key: str = Field(
        DEFAULT_SECRET,
        validation_alias=AliasChoices("JWT__SECRET_KEY", "JWT_SECRET_KEY"),
        json_schema_extra={"env": ["JWT__SECRET_KEY", "JWT_SECRET_KEY"]},
    )
    algorithm: str = Field(
        DEFAULT_ALGORITHM,
        validation_alias=AliasChoices("JWT__ALGORITHM", "JWT_ALGORITHM"),
        json_schema_extra={"env": ["JWT__ALGORITHM", "JWT_ALGORITHM"]},
    )
    access_token_expire_minutes: int = Field(
        DEFAULT_EXPIRE_MINUTES,
        validation_alias=AliasChoices(
            "JWT__ACCESS_TOKEN_EXPIRE_MINUTES", "JWT_ACCESS_TOKEN_EXPIRE_MINUTES"
        ),
        json_schema_extra={
            "env": [
                "JWT__ACCESS_TOKEN_EXPIRE_MINUTES",
                "JWT_ACCESS_TOKEN_EXPIRE_MINUTES",
            ]
        },
    )


class MemoryTimelineSettings(BaseModel):
    """Settings that control behaviour of the in-memory timeline."""

    initial_state_limit: int = Field(
        DEFAULT_MEMORY_TIMELINE_INITIAL_LIMIT,
        validation_alias=AliasChoices(
            "MEMORY_TIMELINE__INITIAL_STATE_LIMIT", "MEMORY_TIMELINE_INITIAL_STATE_LIMIT"
        ),
        json_schema_extra={
            "env": [
                "MEMORY_TIMELINE__INITIAL_STATE_LIMIT",
                "MEMORY_TIMELINE_INITIAL_STATE_LIMIT",
            ]
        },
    )
    max_retained_events: int = Field(
        DEFAULT_MEMORY_TIMELINE_MAX_ITEMS,
        validation_alias=AliasChoices(
            "MEMORY_TIMELINE__MAX_RETAINED_EVENTS",
            "MEMORY_TIMELINE_MAX_RETAINED_EVENTS",
        ),
        json_schema_extra={
            "env": [
                "MEMORY_TIMELINE__MAX_RETAINED_EVENTS",
                "MEMORY_TIMELINE_MAX_RETAINED_EVENTS",
            ]
        },
    )


class IntegrationSettings(BaseModel):
    """Settings for external integrations and infrastructure dependencies."""

    use_redis: bool = Field(
        False,
        validation_alias=AliasChoices(
            "INTEGRATIONS__USE_REDIS", "USE_REDIS"
        ),
        json_schema_extra={"env": ["INTEGRATIONS__USE_REDIS", "USE_REDIS"]},
    )
    redis_url: str = Field(
        "redis://localhost:6379/0",
        validation_alias=AliasChoices(
            "INTEGRATIONS__REDIS_URL", "REDIS_URL"
        ),
        json_schema_extra={"env": ["INTEGRATIONS__REDIS_URL", "REDIS_URL"]},
    )
    neo4j_uri: str = Field(
        "bolt://localhost:7687",
        validation_alias=AliasChoices(
            "INTEGRATIONS__NEO4J_URI", "NEO4J_URI"
        ),
        json_schema_extra={"env": ["INTEGRATIONS__NEO4J_URI", "NEO4J_URI"]},
    )
    neo4j_user: str = Field(
        "neo4j",
        validation_alias=AliasChoices(
            "INTEGRATIONS__NEO4J_USER", "NEO4J_USER"
        ),
        json_schema_extra={"env": ["INTEGRATIONS__NEO4J_USER", "NEO4J_USER"]},
    )
    neo4j_password: str = Field(
        "password",
        validation_alias=AliasChoices(
            "INTEGRATIONS__NEO4J_PASSWORD", "NEO4J_PASSWORD"
        ),
        json_schema_extra={
            "env": ["INTEGRATIONS__NEO4J_PASSWORD", "NEO4J_PASSWORD"]
        },
    )
    ml_enabled: bool = Field(
        False,
        validation_alias=AliasChoices(
            "INTEGRATIONS__ML_ENABLED", "ML_ENABLED"
        ),
        json_schema_extra={"env": ["INTEGRATIONS__ML_ENABLED", "ML_ENABLED"]},
    )


class Settings(BaseSettings):
    """Centralised application settings loaded via Pydantic."""

    jwt: JWTSettings = Field(default_factory=JWTSettings)
    memory_timeline: MemoryTimelineSettings = Field(default_factory=MemoryTimelineSettings)
    integrations: IntegrationSettings = Field(default_factory=IntegrationSettings)

    if USING_PYDANTIC_SETTINGS:
        model_config = SettingsConfigDict(
            env_file=".env",
            env_file_encoding="utf-8",
            case_sensitive=False,
            env_nested_delimiter="__",
        )
    else:  # pragma: no cover - fallback for environments without pydantic-settings
        model_config: Dict[str, Any] = {"case_sensitive": False}

    def __init__(self, **values: Any):
        if not USING_PYDANTIC_SETTINGS:
            values = self._load_env_overrides(values)
        super().__init__(**values)

    @classmethod
    def _load_env_overrides(cls, values: Dict[str, Any]) -> Dict[str, Any]:
        """Populate values from environment variables for the fallback path."""

        import os

        merged = dict(values)
        section_models: Dict[str, Type[BaseModel]] = {
            "jwt": JWTSettings,
            "memory_timeline": MemoryTimelineSettings,
            "integrations": IntegrationSettings,
        }
        env_mapping: Dict[str, Dict[str, Tuple[Iterable[str], Type[Any], Any]]] = {
            "jwt": {
                "secret_key": (("JWT__SECRET_KEY", "JWT_SECRET_KEY"), str, DEFAULT_SECRET),
                "algorithm": (("JWT__ALGORITHM", "JWT_ALGORITHM"), str, DEFAULT_ALGORITHM),
                "access_token_expire_minutes": (
                    ("JWT__ACCESS_TOKEN_EXPIRE_MINUTES", "JWT_ACCESS_TOKEN_EXPIRE_MINUTES"),
                    int,
                    DEFAULT_EXPIRE_MINUTES,
                ),
            },
            "memory_timeline": {
                "initial_state_limit": (
                    (
                        "MEMORY_TIMELINE__INITIAL_STATE_LIMIT",
                        "MEMORY_TIMELINE_INITIAL_STATE_LIMIT",
                    ),
                    int,
                    DEFAULT_MEMORY_TIMELINE_INITIAL_LIMIT,
                ),
                "max_retained_events": (
                    (
                        "MEMORY_TIMELINE__MAX_RETAINED_EVENTS",
                        "MEMORY_TIMELINE_MAX_RETAINED_EVENTS",
                    ),
                    int,
                    DEFAULT_MEMORY_TIMELINE_MAX_ITEMS,
                ),
            },
            "integrations": {
                "use_redis": (
                    ("INTEGRATIONS__USE_REDIS", "USE_REDIS"),
                    _cast_bool,
                    False,
                ),
                "redis_url": (
                    ("INTEGRATIONS__REDIS_URL", "REDIS_URL"),
                    str,
                    "redis://localhost:6379/0",
                ),
                "neo4j_uri": (
                    ("INTEGRATIONS__NEO4J_URI", "NEO4J_URI"),
                    str,
                    "bolt://localhost:7687",
                ),
                "neo4j_user": (
                    ("INTEGRATIONS__NEO4J_USER", "NEO4J_USER"),
                    str,
                    "neo4j",
                ),
                "neo4j_password": (
                    ("INTEGRATIONS__NEO4J_PASSWORD", "NEO4J_PASSWORD"),
                    str,
                    "password",
                ),
                "ml_enabled": (
                    ("INTEGRATIONS__ML_ENABLED", "ML_ENABLED"),
                    _cast_bool,
                    False,
                ),
            },
        }

        for section, model_cls in section_models.items():
            current = merged.get(section)
            if isinstance(current, BaseModel):
                section_data: Dict[str, Any] = current.model_dump()
            else:
                section_data = dict(current or {})

            for field_name, (env_names, caster, default) in env_mapping[section].items():
                if field_name in section_data and section_data[field_name] is not None:
                    continue

                def _read_env(names: Iterable[str]) -> Any:
                    for env_name in names:
                        raw_value = os.getenv(env_name)
                        if raw_value is None:
                            continue
                        try:
                            return caster(raw_value)
                        except (TypeError, ValueError):
                            return default
                    return default

                section_data[field_name] = _read_env(env_names)

            merged[section] = model_cls(**section_data)

        return merged


@lru_cache()
def get_settings() -> "Settings":
    """Return a cached Settings instance."""

    return Settings()


settings = get_settings()

__all__ = ["Settings", "settings", "get_settings"]
