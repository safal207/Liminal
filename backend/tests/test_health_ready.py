def test_health_endpoint(client):
    resp = client.get("/health")
    assert resp.status_code == 200
    data = resp.json()
    assert data.get("status") == "ok"
    assert "timestamp" in data
    assert "ml_enabled" in data
    assert "redis_connected" in data


def test_ready_endpoint(client):
    resp = client.get("/ready")
    assert resp.status_code == 200
    data = resp.json()
    assert isinstance(data.get("ready"), bool)
    assert isinstance(data.get("checks"), dict)

    checks = data["checks"]
    for key in [
        "app_loaded",
        "event_loop",
        "ml_enabled",
        "redis_configured",
        "redis_connected",
    ]:
        assert key in checks

    # В тестовом контексте event loop активен, ожидаем готовность
    assert data["ready"] is True
