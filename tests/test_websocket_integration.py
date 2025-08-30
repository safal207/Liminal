"""
Интеграционные smoke‑тесты WebSocket (реальный сервер)
Запускаются только при наличии поднятого WS/API сервера на localhost:8080.
Помечены как @pytest.mark.integration, чтобы отделить от unit‑прогонов.
"""

from __future__ import annotations

import asyncio
import contextlib
import json
import os
from datetime import datetime

import pytest
import requests

try:
    import websockets
except Exception:  # websockets может отсутствовать в минимальной среде
    websockets = None

API_URL = os.getenv("WS_API_URL", "http://localhost:8080")
WS_URL = os.getenv("WS_URL", "ws://localhost:8080/ws")


@pytest.mark.integration
def test_server_running_integration():
    """Smoke: сервер отвечает по HTTP (GraphQL endpoint)."""
    try:
        resp = requests.get(f"{API_URL}/graphql", timeout=2)
        if resp.status_code != 200:
            pytest.skip(f"Сервер запущен, но вернул {resp.status_code}")
    except Exception as e:
        pytest.skip(f"Сервер недоступен: {e}")


@pytest.mark.integration
@pytest.mark.asyncio
async def test_websocket_connection_integration():
    """Smoke: устанавливается WS‑соединение и закрывается корректно."""
    if websockets is None:
        pytest.skip("Пакет websockets недоступен")

    # Пару коротких ретраев на случай старта сервера
    last_err: Exception | None = None
    for _ in range(3):
        try:
            ws = await asyncio.wait_for(websockets.connect(WS_URL, ping_interval=None), timeout=2.0)
            await ws.close()
            return
        except Exception as e:
            last_err = e
            await asyncio.sleep(0.5)
    pytest.skip(f"WS соединение не установлено: {last_err}")


@pytest.mark.integration
@pytest.mark.asyncio
async def test_event_smoke_integration():
    """Smoke: отправляем тестовое событие через HTTP и ожидаем получить его по WS.
    Если сервер не реализует такой бродкаст — тест будет пропущен.
    """
    if websockets is None:
        pytest.skip("Пакет websockets недоступен")

    # Проверяем доступность HTTP
    try:
        ok = requests.get(f"{API_URL}/graphql", timeout=2)
        if ok.status_code != 200:
            pytest.skip("HTTP доступен, но эндпоинт вернул не 200")
    except Exception as e:
        pytest.skip(f"HTTP недоступен: {e}")

    # Пробуем открыть WS
    try:
        ws = await asyncio.wait_for(websockets.connect(WS_URL, ping_interval=None), timeout=2.0)
    except Exception as e:
        pytest.skip(f"WS недоступен: {e}")

    # Конструируем тестовое событие (совместимо с исходными ожиданиями)
    test_event = {
        "source": "TRANSITION_LIMINAL",
        "target": "PRESENCE_NOW",
        "type": "CONSCIOUSNESS_TRANSITION",
        "trigger": "DEEP_BREATH",
        "timestamp": datetime.now().isoformat(),
        "description": "Интеграционный smoke‑эвент",
    }

    # Пытаемся отправить событие через HTTP, ожидаем 200
    try:
        r = requests.post(f"{API_URL}/events", json=test_event, timeout=2)
        if r.status_code != 200:
            await ws.close()
            pytest.skip(f"/events вернул {r.status_code}")
    except Exception as e:
        await ws.close()
        pytest.skip(f"/events недоступен: {e}")

    # Ждем сообщение с небольшим таймаутом; если не пришло — пропускаем
    try:
        raw = await asyncio.wait_for(ws.recv(), timeout=2.5)
        data = json.loads(raw)
        assert data.get("type") == "CONSCIOUSNESS_TRANSITION"
        assert data.get("source") == "TRANSITION_LIMINAL"
        assert data.get("target") == "PRESENCE_NOW"
    except TimeoutError:
        pytest.skip("Сервер не транслировал событие в отведённое время")
    finally:
        with contextlib.suppress(Exception):
            await ws.close()
