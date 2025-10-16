"""Application-wide configuration loaded from environment variables."""

from functools import lru_cache
from typing import Any, Dict, Tuple, Type

from pydantic import Field

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


class Settings(BaseSettings):
    """Centralised application settings loaded via Pydantic."""

    jwt_secret_key: str = Field(DEFAULT_SECRET, json_schema_extra={"env": "JWT_SECRET_KEY"})
    jwt_algorithm: str = Field(DEFAULT_ALGORITHM, json_schema_extra={"env": "JWT_ALGORITHM"})
    jwt_access_token_expire_minutes: int = Field(
        DEFAULT_EXPIRE_MINUTES,
        json_schema_extra={"env": "JWT_ACCESS_TOKEN_EXPIRE_MINUTES"},
    )

    if USING_PYDANTIC_SETTINGS:
        model_config = SettingsConfigDict(
            env_file=".env", env_file_encoding="utf-8", case_sensitive=False
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
        env_mapping: Dict[str, Tuple[str, Type[Any], Any]] = {
            "jwt_secret_key": ("JWT_SECRET_KEY", str, DEFAULT_SECRET),
            "jwt_algorithm": ("JWT_ALGORITHM", str, DEFAULT_ALGORITHM),
            "jwt_access_token_expire_minutes": (
                "JWT_ACCESS_TOKEN_EXPIRE_MINUTES",
                int,
                DEFAULT_EXPIRE_MINUTES,
            ),
        }

        for field_name, (env_name, caster, default) in env_mapping.items():
            if field_name in merged and merged[field_name] is not None:
                continue
            raw_value = os.getenv(env_name)
            if raw_value is None:
                merged.setdefault(field_name, default)
                continue
            try:
                merged[field_name] = caster(raw_value)
            except (TypeError, ValueError):
                merged[field_name] = default

        return merged


@lru_cache()
def get_settings() -> "Settings":
    """Return a cached Settings instance."""

    return Settings()


settings = get_settings()

__all__ = ["Settings", "settings", "get_settings"]
