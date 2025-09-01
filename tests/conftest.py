"""
SOMA & Liminal Project - Common test fixtures and utilities
"""

import os
import subprocess
import sys
import time
from datetime import datetime, timedelta
from pathlib import Path
from unittest.mock import patch

import pytest
import requests

# HACK: Add project root and scripts to sys.path.
# This is an anti-pattern and should be removed once the `scripts` directory
# is refactored into a proper, installable package (`src/soma` or similar).
# The project is installed in editable mode, so `src` is already on the path.
sys.path.append(str(Path(__file__).parent.parent))
sys.path.append(str(Path(__file__).parent.parent / "scripts"))

# Import SOMA modules
from scripts.consciousness_maturation import (
    ConsciousnessMaturationSystem,
    InMemoryHistoryStorage,
    LearningEvent,
    MaturationStage,
)


@pytest.fixture(scope="session", autouse=True)
def live_server():
    """
    Session-scoped fixture to automatically run the Uvicorn server in a
    separate process for all integration tests.
    """
    port = os.getenv("PORT", "8080")
    url = f"http://127.0.0.1:{port}/at-risk"

    # Use the mock DB backend for all tests
    env = os.environ.copy()
    env["LIMINAL_DB_BACKEND"] = "mock"

    cmd = [sys.executable, "-m", "uvicorn", "liminal.at_risk_server_neo4j:app", "--port", port]
    # Use a null device for stdout/stderr to keep test output clean
    with open(os.devnull, 'w') as devnull:
        proc = subprocess.Popen(cmd, stdout=devnull, stderr=devnull, env=env)

    # Wait for the server to be ready
    timeout = 60.0
    start_time = time.time()
    server_ready = False
    while time.time() - start_time < timeout:
        try:
            with requests.get(url, timeout=1) as resp:
                if resp.status_code in (200, 304):
                    server_ready = True
                    break
        except requests.ConnectionError:
            time.sleep(0.5)

    if not server_ready:
        proc.terminate()
        pytest.fail(f"Server did not start at {url} within {timeout}s.")

    yield url

    proc.terminate()
    try:
        proc.wait(timeout=5)
    except subprocess.TimeoutExpired:
        proc.kill()


@pytest.fixture
def test_dir():
    """Return a test directory path that can be used for temporary files"""
    test_dir_path = Path(__file__).parent / "test_data"
    test_dir_path.mkdir(exist_ok=True)
    return test_dir_path


@pytest.fixture
def mock_maturation_system(test_dir):
    """Create a deterministic maturation system with fixed time and in-memory storage"""
    # Fixed point in time for all calculations
    fixed_now = datetime(2025, 1, 1, 12, 0, 0)
    fixed_now_ts = fixed_now.timestamp()

    def now_fn():
        return fixed_now_ts

    # In-memory storage to avoid filesystem flakiness
    storage = InMemoryHistoryStorage()

    system = ConsciousnessMaturationSystem(
        str(test_dir),
        now_fn=now_fn,
        history_storage=storage,
        record_initial_milestone=False,  # avoid off-by-one event counts in tests
    )

    # Set a fixed birth time (1 day old) and recompute stage deterministically -> INFANT
    system.birth_time = fixed_now - timedelta(days=1)
    system.current_stage = system.calculate_current_stage()

    return system


@pytest.fixture
def mock_elder_maturation_system(mock_maturation_system):
    """Create a mock maturation system at ELDER stage"""
    with patch.object(mock_maturation_system, "current_stage", MaturationStage.ELDER):
        yield mock_maturation_system


@pytest.fixture
def sample_learning_event():
    """Create a sample learning event for testing"""
    return LearningEvent(
        timestamp=datetime.now(),
        event_type="test",
        description="Test description",
        source_module="test_module",
        context={"key": "value"},
        conclusions=["conclusion1", "conclusion2"],
        related_events=["event1", "event2"],
    )
