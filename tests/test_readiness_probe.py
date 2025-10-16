import importlib
import os
import sys
from unittest.mock import patch

from fastapi.testclient import TestClient


def get_app_with_env(use_redis: bool):
    """Load the FastAPI app with a specific USE_REDIS environment configuration."""
    os.environ["USE_REDIS"] = "true" if use_redis else "false"
    os.environ["REDIS_URL"] = "redis://mock-redis:6379"

    modules_to_reload = [
        "backend.app.services.websocket",
        "backend.app.dependencies",
        "backend.app.main",
    ]

    for module_name in modules_to_reload:
        if module_name in sys.modules:
            importlib.reload(sys.modules[module_name])

    from backend.app import main as app_module

    return app_module.app


def test_readiness_probe_when_redis_is_off():
    app_no_redis = get_app_with_env(use_redis=False)
    with TestClient(app_no_redis) as client:
        response = client.get("/ready")
        json_data = response.json()
        assert response.status_code == 200
        assert json_data["ready"] is True
        assert json_data["checks"]["redis_configured"] is False


def test_readiness_probe_when_redis_is_on_and_connected():
    app_with_redis = get_app_with_env(use_redis=True)
    from backend.app.dependencies import get_connection_manager

    manager = get_connection_manager()

    with TestClient(app_with_redis) as client:
        with patch.object(manager, "_is_connected", True):
            response = client.get("/ready")
            json_data = response.json()
            assert response.status_code == 200
            assert json_data["ready"] is True
            assert json_data["checks"]["redis_configured"] is True
            assert json_data["checks"]["redis_connected"] is True


def test_readiness_probe_when_redis_is_on_and_disconnected():
    app_with_redis = get_app_with_env(use_redis=True)
    from backend.app.dependencies import get_connection_manager

    manager = get_connection_manager()

    with TestClient(app_with_redis) as client:
        with patch.object(manager, "_is_connected", False):
            response = client.get("/ready")
            json_data = response.json()
            assert response.status_code == 200
            assert json_data["ready"] is False
            assert json_data["checks"]["redis_configured"] is True
            assert json_data["checks"]["redis_connected"] is False
