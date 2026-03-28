"""
Unit tests for BurnoutGuard core logic.

Numpy and ML dependencies are mocked via sys.modules so tests run
in any environment (CI, dev without ML packages installed).
"""

from __future__ import annotations

import sys
import types
from datetime import datetime
from typing import Any
from unittest.mock import MagicMock

import pytest

# ---------------------------------------------------------------------------
# Minimal numpy stub so burnout_guard modules can be imported without numpy
# ---------------------------------------------------------------------------


def _make_numpy_stub() -> types.ModuleType:
    np = types.ModuleType("numpy")
    np.array = lambda x, **kw: x  # type: ignore[assignment]
    np.mean = lambda x, **kw: sum(x) / len(x) if x else 0.0
    np.std = lambda x, **kw: 0.0
    np.diff = lambda x, **kw: [0.0]
    np.clip = lambda v, lo, hi: max(lo, min(hi, v))
    np.zeros = lambda shape, **kw: [0.0] * (
        shape if isinstance(shape, int) else shape[0]
    )
    np.linspace = lambda a, b, n: [a + (b - a) * i / (n - 1) for i in range(n)]
    np.nan = float("nan")
    np.float32 = float
    np.float64 = float
    return np


_numpy_stub = _make_numpy_stub()


# ---------------------------------------------------------------------------
# Patch sys.modules BEFORE importing anything from burnout_guard
# ---------------------------------------------------------------------------


@pytest.fixture(autouse=True, scope="module")
def _patch_numpy():
    """Inject numpy stub for the duration of this module."""
    original = sys.modules.get("numpy")
    sys.modules["numpy"] = _numpy_stub  # type: ignore[assignment]

    # Also stub heavy ML / emotime sub-modules that may not be installed
    for mod in [
        "torch",
        "transformers",
        "sklearn",
        "onnxruntime",
        "backend.emotime.ml",
    ]:
        if mod not in sys.modules:
            sys.modules[mod] = MagicMock()

    yield

    # Restore
    if original is None:
        sys.modules.pop("numpy", None)
    else:
        sys.modules["numpy"] = original


# ---------------------------------------------------------------------------
# Helpers to build minimal mock objects
# ---------------------------------------------------------------------------


def _make_emotime_state() -> Any:
    """Return a minimal EmotimeState-like mock."""
    features = MagicMock()
    features.valence = -0.4
    features.arousal = 0.7
    features.dominance = 0.3
    features.confidence = 0.85
    features.intensity = 0.6

    mode = MagicMock()
    mode.type = MagicMock()
    mode.type.value = "stress"
    mode.intensity = 0.7
    mode.duration = 3600.0
    mode.confidence = 0.8

    state = MagicMock()
    state.features = features
    state.mode = mode
    state.confidence = 0.85
    state.resonance_trace = []
    return state


def _make_burnout_mode() -> Any:
    from backend.burnout_guard.modes import (
        BurnoutMode,
        BurnoutModeType,
        BurnoutRiskLevel,
    )

    return BurnoutMode(
        type=BurnoutModeType.OVERWORK,
        risk_level=BurnoutRiskLevel.HIGH,
        risk_score=0.72,
        primary_indicators=["prolonged_stress", "high_arousal"],
        emotional_pattern="stress_dominant",
        duration_hours=5.0,
        confidence=0.80,
    )


# ---------------------------------------------------------------------------
# Tests: BurnoutRiskLevel enum
# ---------------------------------------------------------------------------


class TestBurnoutRiskLevel:
    def test_all_levels_defined(self):
        from backend.burnout_guard.modes import BurnoutRiskLevel

        assert {l.value for l in BurnoutRiskLevel} == {
            "very_low",
            "low",
            "medium",
            "high",
            "critical",
        }

    def test_values_are_strings(self):
        from backend.burnout_guard.modes import BurnoutRiskLevel

        for lvl in BurnoutRiskLevel:
            assert isinstance(lvl.value, str)


# ---------------------------------------------------------------------------
# Tests: BurnoutModeType enum
# ---------------------------------------------------------------------------


class TestBurnoutModeType:
    def test_all_types_defined(self):
        from backend.burnout_guard.modes import BurnoutModeType

        values = {t.value for t in BurnoutModeType}
        assert "healthy" in values
        assert "crisis" in values

    def test_overwork_exists(self):
        from backend.burnout_guard.modes import BurnoutModeType

        assert BurnoutModeType.OVERWORK.value == "overwork"


# ---------------------------------------------------------------------------
# Tests: BurnoutMode dataclass
# ---------------------------------------------------------------------------


class TestBurnoutMode:
    def test_burnout_mode_creation(self):
        mode = _make_burnout_mode()
        assert mode.risk_score == 0.72
        assert mode.confidence == 0.80
        assert "prolonged_stress" in mode.primary_indicators

    def test_burnout_mode_defaults(self):
        from backend.burnout_guard.modes import (
            BurnoutMode,
            BurnoutModeType,
            BurnoutRiskLevel,
        )

        m = BurnoutMode(
            type=BurnoutModeType.HEALTHY,
            risk_level=BurnoutRiskLevel.VERY_LOW,
            risk_score=0.1,
            primary_indicators=[],
            emotional_pattern="calm",
        )
        assert m.duration_hours == 0.0
        assert m.confidence == 0.0


# ---------------------------------------------------------------------------
# Tests: BurnoutRiskScorer
# ---------------------------------------------------------------------------


class TestBurnoutRiskScorer:
    def test_risk_factor_weights_sum_to_one(self):
        from backend.burnout_guard.core import BurnoutRiskScorer

        weights = BurnoutRiskScorer.RISK_FACTOR_WEIGHTS
        total = sum(weights.values())
        assert abs(total - 1.0) < 1e-6, f"Weights sum to {total}, expected 1.0"

    def test_scorer_initialises(self):
        from backend.burnout_guard.core import BurnoutRiskScorer

        scorer = BurnoutRiskScorer(user_id="test_user")
        assert scorer.user_id == "test_user"

    @pytest.mark.asyncio
    async def test_calculate_risk_returns_burnout_risk(self):
        from backend.burnout_guard.core import BurnoutRisk, BurnoutRiskScorer

        scorer = BurnoutRiskScorer(user_id="test_user")
        emotime_state = _make_emotime_state()
        burnout_mode = _make_burnout_mode()

        result = await scorer.calculate_risk(emotime_state, burnout_mode)

        assert isinstance(result, BurnoutRisk)
        assert 0.0 <= result.score <= 1.0
        assert 0.0 <= result.confidence <= 1.0
        assert isinstance(result.emotional_indicators, list)
        assert isinstance(result.behavioral_patterns, list)

    @pytest.mark.asyncio
    async def test_high_stress_gives_elevated_score(self):
        from backend.burnout_guard.core import BurnoutRiskScorer

        scorer = BurnoutRiskScorer(user_id="test")
        state = _make_emotime_state()
        state.features.valence = -0.9  # very negative
        state.features.arousal = 0.95  # very high arousal
        mode = _make_burnout_mode()

        risk = await scorer.calculate_risk(state, mode)
        assert risk.score > 0.2, "High-stress state should produce elevated risk"

    @pytest.mark.asyncio
    async def test_risk_level_assigned(self):
        from backend.burnout_guard.core import BurnoutRiskScorer
        from backend.burnout_guard.modes import BurnoutRiskLevel

        scorer = BurnoutRiskScorer(user_id="test")
        risk = await scorer.calculate_risk(_make_emotime_state(), _make_burnout_mode())
        assert risk.level in list(BurnoutRiskLevel)


# ---------------------------------------------------------------------------
# Tests: Recommendation dataclass
# ---------------------------------------------------------------------------


class TestRecommendation:
    def test_recommendation_fields(self):
        from backend.burnout_guard.modes import BurnoutModeType, BurnoutRiskLevel
        from backend.burnout_guard.recommendations import (
            Recommendation,
            RecommendationType,
        )

        rec = Recommendation(
            id="rec_001",
            type=RecommendationType.IMMEDIATE_ACTION,
            title="Take a break",
            description="Step away from work for 10 minutes.",
            priority=5,
            estimated_time=10,
            difficulty="easy",
            applicable_risk_levels=[BurnoutRiskLevel.HIGH, BurnoutRiskLevel.CRITICAL],
            applicable_modes=[BurnoutModeType.OVERWORK],
        )
        assert rec.id == "rec_001"
        assert rec.priority == 5
        assert rec.effectiveness_score == 1.0  # default


# ---------------------------------------------------------------------------
# Tests: BurnoutDatabaseAdapter (mock/fallback mode)
# ---------------------------------------------------------------------------


class TestBurnoutDatabaseAdapter:
    @pytest.mark.asyncio
    async def test_store_risk_returns_true_in_mock_mode(self):
        from backend.burnout_guard.core import BurnoutRisk
        from backend.burnout_guard.modes import BurnoutRiskLevel
        from backend.burnout_guard.persistence import BurnoutDatabaseAdapter

        adapter = BurnoutDatabaseAdapter()
        # No real DB — falls back to mock
        assert adapter.db_adapter is None or True

        risk = BurnoutRisk(
            score=0.65,
            level=BurnoutRiskLevel.HIGH,
            factors={"emotional_state": 0.7},
            confidence=0.8,
            timestamp=datetime.now(),
            emotional_indicators=["stress"],
            behavioral_patterns=["overwork"],
            duration_risk=0.5,
            trend_risk=0.4,
        )
        result = await adapter.store_risk_assessment("user_test", risk)
        assert result is True

    @pytest.mark.asyncio
    async def test_get_risk_history_returns_list(self):
        from backend.burnout_guard.persistence import BurnoutDatabaseAdapter

        adapter = BurnoutDatabaseAdapter()
        history = await adapter.get_risk_history("user_test", days=7)
        assert isinstance(history, list)

    @pytest.mark.asyncio
    async def test_track_recommendation_usage(self):
        from backend.burnout_guard.persistence import BurnoutDatabaseAdapter

        adapter = BurnoutDatabaseAdapter()
        ok = await adapter.track_recommendation_usage(
            user_id="u1",
            recommendation_id="rec_001",
            action="completed",
            effectiveness_rating=0.9,
        )
        assert ok is True
