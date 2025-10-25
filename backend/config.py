#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Simplified configuration management for LIMINAL local testing.
"""

import os
from functools import lru_cache
from typing import Optional

from pydantic import BaseModel, Field


class Settings(BaseModel):
    """Application settings for local testing."""
    
    # Environment
    environment: str = "development"
    debug: bool = True
    
    # Server
    host: str = "0.0.0.0"
    port: int = 8000
    
    # Database
    neo4j_uri: str = "bolt://localhost:7687"
    neo4j_user: str = "neo4j"
    neo4j_password: str = "NewStrongPass123!"
    
    # Redis (optional)
    redis_url: str = "redis://localhost:6379"
    use_redis: bool = False
    
    # JWT
    jwt_secret_key: str = "test-jwt-secret-key-for-local-development-only"
    jwt_algorithm: str = "HS256"
    
    # ML
    ml_enabled: bool = True
    openai_api_key: Optional[str] = None
    
    # Monitoring
    metrics_enabled: bool = True
    
    def __init__(self, **kwargs):
        # Load from environment variables
        env_values = {
            'environment': os.getenv('ENV', 'development'),
            'neo4j_uri': os.getenv('NEO4J_URI', 'bolt://localhost:7687'),
            'neo4j_user': os.getenv('NEO4J_USER', 'neo4j'),
            'neo4j_password': os.getenv('NEO4J_PASSWORD', 'NewStrongPass123!'),
            'redis_url': os.getenv('REDIS_URL', 'redis://localhost:6379'),
            'use_redis': os.getenv('USE_REDIS', 'false').lower() == 'true',
            'jwt_secret_key': os.getenv('JWT_SECRET_KEY', 'test-jwt-secret-key-for-local-development-only'),
            'ml_enabled': os.getenv('ML_ENABLED', 'true').lower() == 'true',
            'openai_api_key': os.getenv('OPENAI_API_KEY'),
            'metrics_enabled': os.getenv('PROMETHEUS_ENABLED', 'true').lower() == 'true',
        }
        # Override with environment values
        kwargs.update({k: v for k, v in env_values.items() if v is not None})
        super().__init__(**kwargs)


@lru_cache()
def get_settings() -> Settings:
    """Get cached application settings."""
    return Settings()


# Compatibility functions for existing code
def get_database_settings():
    """Get database settings for compatibility."""
    settings = get_settings()
    return type('DatabaseSettings', (), {
        'neo4j_uri': settings.neo4j_uri,
        'neo4j_user': settings.neo4j_user,
        'neo4j_password': settings.neo4j_password,
        'redis_url': settings.redis_url,
        'redis_enabled': settings.use_redis,
    })()


def get_security_settings():
    """Get security settings for compatibility."""
    settings = get_settings()
    return type('SecuritySettings', (), {
        'jwt_secret_key': settings.jwt_secret_key,
        'jwt_algorithm': settings.jwt_algorithm,
    })()


def get_ml_settings():
    """Get ML settings for compatibility."""
    settings = get_settings()
    return type('MLSettings', (), {
        'ml_enabled': settings.ml_enabled,
        'openai_api_key': settings.openai_api_key,
    })()


def get_websocket_settings():
    """Get WebSocket settings for compatibility."""
    return type('WebSocketSettings', (), {
        'max_connections': 1000,
        'max_queue_size': 10000,
        'redis_enabled': get_settings().use_redis,
        'redis_url': get_settings().redis_url,
        'redis_max_connections': 100,
    })()


def get_monitoring_settings():
    """Get monitoring settings for compatibility."""
    settings = get_settings()
    return type('MonitoringSettings', (), {
        'metrics_enabled': settings.metrics_enabled,
        'prometheus_port': 9090,
    })()


def get_app_settings():
    """Get application settings for compatibility."""
    settings = get_settings()
    return type('AppSettings', (), {
        'environment': settings.environment,
        'debug': settings.debug,
        'host': settings.host,
        'port': settings.port,
    })()