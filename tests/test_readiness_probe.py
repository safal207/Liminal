import os
import sys
import importlib
from unittest.mock import patch

from fastapi.testclient import TestClient


def get_app_with_env(use_redis: bool):
    """
    Helper to load the FastAPI app with a specific USE_REDIS environment.
    This reloads necessary modules to ensure the environment variable is read.
    """
    os.environ["USE_REDIS"] = "true" if use_redis else "false"
    os.environ["REDIS_URL"] = "redis://mock-redis:6379"

    # List of modules that depend on the environment variable at import time
    modules_to_reload = [
        "backend.websocket.connection_manager",
        "backend.websocket.redis_connection_manager",
        "backend.api",
    ]

    for module_name in modules_to_reload:
        if module_name in sys.modules:
            importlib.reload(sys.modules[module_name])

    from backend.api import app
    return app


def test_readiness_probe_when_redis_is_off():
    """
    Test that when Redis is not configured, the app is ready.
    """
    app_no_redis = get_app_with_env(use_redis=False)
    with TestClient(app_no_redis) as client:
        response = client.get("/ready")
        json_data = response.json()
        assert response.status_code == 200
        assert json_data["ready"] is True
        assert json_data["checks"]["redis_configured"] is False


def test_readiness_probe_when_redis_is_on_and_connected():
    """
    Test that when Redis is configured and connected, the app is ready.
    """
    app_with_redis = get_app_with_env(use_redis=True)
    from backend.api import connection_manager

    with TestClient(app_with_redis) as client:
        with patch.object(connection_manager, "_is_connected", True):
            response = client.get("/ready")
            json_data = response.json()
            assert response.status_code == 200
            assert json_data["ready"] is True
            assert json_data["checks"]["redis_configured"] is True
            assert json_data["checks"]["redis_connected"] is True


def test_readiness_probe_when_redis_is_on_and_disconnected():
    """
    Test that when Redis is configured but NOT connected, the app is NOT ready.
    """
    app_with_redis = get_app_with_env(use_redis=True)
    from backend.api import connection_manager

    with TestClient(app_with_redis) as client:
        with patch.object(connection_manager, "_is_connected", False):
            response = client.get("/ready")
            json_data = response.json()
            assert response.status_code == 200
            assert json_data["ready"] is False
            assert json_data["checks"]["redis_configured"] is True
            assert json_data["checks"]["redis_connected"] is False
