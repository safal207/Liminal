import os

# Ensure test-friendly environment
os.environ.setdefault("TEST_MODE", "true")

from backend.app.main import app  # noqa: E402
from backend.redis_client import DummyRedis  # noqa: E402

def test_health_live_ok(client):
    r = client.get("/health/live")
    assert r.status_code == 200
    assert r.json().get("status") == "alive"


def test_health_startup_flag(client):
    # TestClient triggers startup events automatically
    r = client.get("/health/startup")
    assert r.status_code == 200
    assert r.json().get("status") == "started"


def test_health_ready_dep_checks(client):
    # Readiness should be 503 if Redis runs in Dummy mode, otherwise 200
    is_dummy = isinstance(getattr(app.state.redis_client, "client", None), DummyRedis)
    r = client.get("/health/ready")
    if is_dummy:
        assert r.status_code == 503
        assert r.json()["detail"]["status"] == "not_ready"
        assert r.json()["detail"]["components"]["redis"]["status"] == "error"
    else:
        assert r.status_code == 200
        assert r.json()["status"] == "ready"
        assert r.json()["components"]["redis"]["status"] == "ok"
