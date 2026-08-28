"""Authenticated WebSocket integration tests against a running backend."""

from __future__ import annotations

import asyncio
import json
import os

import pytest
import requests

websockets = pytest.importorskip("websockets")
websocket_exceptions = pytest.importorskip("websockets.exceptions")
ConnectionClosed = websocket_exceptions.ConnectionClosed

API_URL = os.getenv("WS_API_URL", "http://localhost:8080")
WS_URL = os.getenv("WS_URL", "ws://localhost:8080/ws/timeline")
USERNAME = os.getenv("WS_TEST_USERNAME", "testuser")
PASSWORD = os.getenv("LIMINAL_TEST_USER_PASS", "testpass")


def _get_access_token() -> str:
    response = requests.post(
        f"{API_URL}/auth/login",
        json={"username": USERNAME, "password": PASSWORD},
        timeout=5,
    )
    assert response.status_code == 200, response.text
    token = response.json().get("access_token")
    assert isinstance(token, str) and token
    return token


@pytest.mark.integration
def test_server_ready_integration() -> None:
    response = requests.get(f"{API_URL}/ready", timeout=5)

    assert response.status_code == 200, response.text
    payload = response.json()
    assert payload.get("ready") is True


@pytest.mark.integration
@pytest.mark.asyncio
async def test_authenticated_websocket_subscription_integration() -> None:
    token = _get_access_token()

    async with websockets.connect(
        WS_URL,
        ping_interval=None,
        open_timeout=5,
    ) as websocket:
        auth_required = json.loads(await asyncio.wait_for(websocket.recv(), timeout=5))
        assert auth_required["type"] == "auth_required"

        await websocket.send(json.dumps({"type": "auth", "token": token}))
        auth_response = json.loads(await asyncio.wait_for(websocket.recv(), timeout=5))
        assert auth_response["type"] == "auth_success"

        await websocket.send('{"type":"subscribe","channel":"timeline"}')
        initial_state = json.loads(await asyncio.wait_for(websocket.recv(), timeout=5))
        assert initial_state["event"] == "initial_state"

        subscribed = json.loads(await asyncio.wait_for(websocket.recv(), timeout=5))
        assert subscribed == {"type": "subscribed", "channel": "timeline"}

        await websocket.send('{"type":"unsubscribe","channel":"timeline"}')
        unsubscribed = json.loads(await asyncio.wait_for(websocket.recv(), timeout=5))
        assert unsubscribed == {"type": "unsubscribed", "channel": "timeline"}


@pytest.mark.integration
@pytest.mark.asyncio
async def test_invalid_websocket_token_is_rejected_integration() -> None:
    async with websockets.connect(
        WS_URL,
        ping_interval=None,
        open_timeout=5,
    ) as websocket:
        auth_required = json.loads(await asyncio.wait_for(websocket.recv(), timeout=5))
        assert auth_required["type"] == "auth_required"

        await websocket.send('{"type":"auth","token":"invalid-token"}')
        with pytest.raises(ConnectionClosed) as exc_info:
            await asyncio.wait_for(websocket.recv(), timeout=5)

        assert exc_info.value.code == 1008
