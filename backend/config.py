#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""Compatibility configuration facade backed by the central settings model."""

from __future__ import annotations

import os
from functools import lru_cache
from typing import Optional

from pydantic import BaseModel

from backend.core.settings import DEFAULT_SECRET, get_settings as get_core_settings

MIN_PRODUCTION_SECRET_LENGTH = 32
DEFAULT_NEO4J_PASSWORD = ""


class Settings(BaseModel):
    """Application settings with one authoritative security source."""

    environment: str = "development"
    debug: bool = False
    host: str = "127.0.0.1"
    port: int = 8000

    neo4j_uri: str = "bolt://localhost:7687"
    neo4j_user: str = "neo4j"
    neo4j_password: str = DEFAULT_NEO4J_PASSWORD

    redis_url: str = "redis://localhost:6379/0"
    use_redis: bool = False

    jwt_secret_key: str = DEFAULT_SECRET
    jwt_algorithm: str = "HS256"

    ml_enabled: bool = False
    openai_api_key: Optional[str] = None
    metrics_enabled: bool = True

    def __init__(self, **kwargs):
        core = get_core_settings()
        environment = os.getenv("ENV", "development").strip().lower()
        debug = os.getenv("DEBUG", "").strip().lower()

        values = {
            "environment": environment,
            "debug": debug in {"1", "true", "yes", "on"}
            if debug
            else environment == "development",
            "host": os.getenv("HOST", "127.0.0.1"),
            "port": int(os.getenv("PORT", "8000")),
            "neo4j_uri": core.integrations.neo4j_uri,
            "neo4j_user": core.integrations.neo4j_user,
            "neo4j_password": core.integrations.neo4j_password,
            "redis_url": core.integrations.redis_url,
            "use_redis": core.integrations.use_redis,
            "jwt_secret_key": core.jwt.secret_key,
            "jwt_algorithm": core.jwt.algorithm,
            "ml_enabled": core.integrations.ml_enabled,
            "openai_api_key": os.getenv("OPENAI_API_KEY"),
            "metrics_enabled": os.getenv("PROMETHEUS_ENABLED", "true").lower()
            == "true",
        }
        values.update(kwargs)
        self._validate_production(values)
        super().__init__(**values)

    @staticmethod
    def _validate_production(values: dict) -> None:
        if values["environment"] != "production":
            return

        secret = str(values["jwt_secret_key"]).strip()
        if (
            not secret
            or secret == DEFAULT_SECRET
            or len(secret) < MIN_PRODUCTION_SECRET_LENGTH
        ):
            raise RuntimeError(
                "JWT_SECRET_KEY must be a non-default secret of at least 32 characters "
                "when ENV=production"
            )

        neo4j_password = str(values["neo4j_password"]).strip()
        if not neo4j_password or neo4j_password == DEFAULT_NEO4J_PASSWORD:
            raise RuntimeError(
                "NEO4J_PASSWORD must be set to a non-default value when ENV=production"
            )


@lru_cache()
def get_settings() -> Settings:
    return Settings()


def get_database_settings():
    settings = get_settings()
    return type(
        "DatabaseSettings",
        (),
        {
            "neo4j_uri": settings.neo4j_uri,
            "neo4j_user": settings.neo4j_user,
            "neo4j_password": settings.neo4j_password,
            "redis_url": settings.redis_url,
            "redis_enabled": settings.use_redis,
        },
    )()


def get_security_settings():
    settings = get_settings()
    return type(
        "SecuritySettings",
        (),
        {
            "jwt_secret_key": settings.jwt_secret_key,
            "jwt_algorithm": settings.jwt_algorithm,
        },
    )()


def get_ml_settings():
    settings = get_settings()
    return type(
        "MLSettings",
        (),
        {
            "ml_enabled": settings.ml_enabled,
            "openai_api_key": settings.openai_api_key,
        },
    )()


def get_websocket_settings():
    settings = get_settings()
    return type(
        "WebSocketSettings",
        (),
        {
            "max_connections": int(os.getenv("WS_MAX_CONNECTIONS", "100")),
            "max_queue_size": int(os.getenv("WS_MAX_QUEUE_SIZE", "10000")),
            "redis_enabled": settings.use_redis,
            "redis_url": settings.redis_url,
            "redis_max_connections": int(
                os.getenv("REDIS_MAX_CONNECTIONS", "100")
            ),
        },
    )()


def get_monitoring_settings():
    settings = get_settings()
    return type(
        "MonitoringSettings",
        (),
        {
            "metrics_enabled": settings.metrics_enabled,
            "prometheus_port": int(os.getenv("PROMETHEUS_PORT", "9090")),
        },
    )()


def get_app_settings():
    settings = get_settings()
    return type(
        "AppSettings",
        (),
        {
            "environment": settings.environment,
            "debug": settings.debug,
            "host": settings.host,
            "port": settings.port,
        },
    )()
