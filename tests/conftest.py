"""
SOMA Project - Common test fixtures and utilities
"""

import sys
from datetime import datetime, timedelta
from pathlib import Path
from unittest.mock import patch

import pytest

# Add project root to sys.path to ensure imports work correctly
sys.path.append(str(Path(__file__).parent.parent))
sys.path.append(str(Path(__file__).parent.parent / "scripts"))
# Add src/ for packages like rince
sys.path.append(str(Path(__file__).parent.parent / "src"))

# Import SOMA modules
from scripts.consciousness_maturation import (
    ConsciousnessMaturationSystem,
    InMemoryHistoryStorage,
    LearningEvent,
    MaturationStage,
)


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
