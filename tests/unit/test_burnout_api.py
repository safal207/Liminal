"""
API endpoint tests for BurnoutGuard.

All external dependencies (EmotimeEngine, BurnoutGuardEngine, DB adapter)
are mocked so tests run without numpy, a database, or any ML packages.
"""

from __future__ import annotations

import sys
import types
from datetime import datetime
from typing import Any
from unittest.mock import AsyncMock, MagicMock, patch

import pytest
from fastapi import FastAPI
from fastapi.testclient import TestClient


# ---------------------------------------------------------------------------
# Minimal numpy stub (same technique as test_burnout_guard.py)
# ---------------------------------------------------------------------------

def _numpy_stub() -> types.ModuleType:
    np = types.ModuleType("numpy")
    np.array   = lambda x, **kw: x          # type: ignore[assignment]
    np.mean    = lambda x, **kw: 0.5
    np.std     = lambda x, **kw: 0.0
    np.diff    = lambda x, **kw: [0.0]
    np.clip    = lambda v, lo, hi: max(lo, min(hi, float(v)))
    np.zeros   = lambda shape, **kw: [0.0]
    np.float32 = float
    np.float64 = float
    np.nan     = float("nan")
    return np


def _make_auth_stub() -> types.ModuleType:
    """Minimal auth_integration stub that satisfies api.py imports."""
    mod = types.ModuleType("backend.burnout_guard.auth_integration")
    mod.BurnoutUserContext = MagicMock                            # type: ignore[assignment]
    mod.BurnoutPermission = MagicMock()                          # type: ignore[assignment]
    mod.BurnoutPermissionChecker = MagicMock                     # type: ignore[assignment]
    mod.BurnoutAuthService = MagicMock                           # type: ignore[assignment]
    mod.BurnoutAuditLogger = MagicMock                           # type: ignore[assignment]
    mod.require_burnout_permission = lambda *a, **kw: (lambda: None)
    mod.require_hr_access  = AsyncMock(return_value={"user_id": "test"})
    mod.require_team_access = AsyncMock(return_value={"user_id": "test"})
    mod.require_user_consent = AsyncMock(return_value=True)
    return mod


@pytest.fixture(autouse=True, scope="module")
def _patch_deps():
    """Stub out numpy, jose, and heavy ML modules for the duration of this module."""
    jose_stub = types.ModuleType("jose")
    jose_stub.JWTError = Exception              # type: ignore[assignment]
    jose_stub.jwt = MagicMock()                 # type: ignore[assignment]

    stubs = {
        "numpy":                                    _numpy_stub(),
        "torch":                                    MagicMock(),
        "transformers":                             MagicMock(),
        "sklearn":                                  MagicMock(),
        "onnxruntime":                              MagicMock(),
        "backend.emotime.ml":                       MagicMock(),
        "jose":                                     jose_stub,
        "backend.burnout_guard.auth_integration":   _make_auth_stub(),
    }
    originals = {}
    for name, stub in stubs.items():
        originals[name] = sys.modules.get(name)
        sys.modules[name] = stub  # type: ignore[assignment]
    yield
    for name, orig in originals.items():
        if orig is None:
            sys.modules.pop(name, None)
        else:
            sys.modules[name] = orig


# ---------------------------------------------------------------------------
# Build a minimal BurnoutState + BurnoutRisk for mocked engine output
# ---------------------------------------------------------------------------

def _mock_burnout_state() -> Any:
    from backend.burnout_guard.modes import (
        BurnoutMode, BurnoutModeType, BurnoutRiskLevel,
    )
    from backend.burnout_guard.core import BurnoutRisk, BurnoutState

    risk = BurnoutRisk(
        score=0.55,
        level=BurnoutRiskLevel.MEDIUM,
        factors={"emotional_state": 0.6, "behavioral_patterns": 0.5},
        confidence=0.82,
        timestamp=datetime.now(),
        emotional_indicators=["moderate_stress"],
        behavioral_patterns=["long_sessions"],
        duration_risk=0.4,
        trend_risk=0.3,
    )
    mode = BurnoutMode(
        type=BurnoutModeType.OVERWORK,
        risk_level=BurnoutRiskLevel.MEDIUM,
        risk_score=0.55,
        primary_indicators=["prolonged_focus"],
        emotional_pattern="focus_dominant",
        duration_hours=4.0,
        confidence=0.78,
    )

    emotime_state = MagicMock()
    emotime_state.confidence = 0.82

    return BurnoutState(
        timestamp=datetime.now(),
        emotime_state=emotime_state,
        burnout_mode=mode,
        risk_assessment=risk,
        risk_history=[0.5, 0.52, 0.55],
        mode_stability=0.7,
        intervention_needed=False,
    )


# ---------------------------------------------------------------------------
# Build isolated FastAPI test app with burnout router
# ---------------------------------------------------------------------------

@pytest.fixture(scope="module")
def test_app() -> FastAPI:
    """
    Create a standalone FastAPI app with only the burnout router mounted.
    Avoids the complexity of booting the full LIMINAL app in unit tests.
    """
    from backend.burnout_guard.api import router as burnout_router

    app = FastAPI()
    app.include_router(burnout_router)
    return app


@pytest.fixture(scope="module")
def client(test_app: FastAPI) -> TestClient:
    return TestClient(test_app, raise_server_exceptions=False)


# ---------------------------------------------------------------------------
# Helper: patch all IO in the assess endpoint
# ---------------------------------------------------------------------------

ASSESS_PATCHES = [
    "backend.burnout_guard.api.EMOTIME_AVAILABLE",
    "backend.burnout_guard.api.EmotimeEngine",
    "backend.burnout_guard.api.BurnoutGuardEngine",
    "backend.burnout_guard.api.RecommendationEngine",
    "backend.burnout_guard.api.BurnoutDatabaseAdapter",
]


def _setup_assess_mocks(patches: dict, state: Any) -> None:
    """Configure mock objects so assess endpoint returns a full assessment."""
    from backend.burnout_guard.recommendations import Recommendation, RecommendationType
    from backend.burnout_guard.modes import BurnoutRiskLevel, BurnoutModeType

    patches["backend.burnout_guard.api.EMOTIME_AVAILABLE"].return_value = True

    engine_instance = MagicMock()
    engine_instance.analyze_now = AsyncMock(return_value=state)
    patches["backend.burnout_guard.api.BurnoutGuardEngine"].return_value = engine_instance

    rec_instance = MagicMock()
    rec_instance.get_recommendations = AsyncMock(return_value=[
        Recommendation(
            id="r1",
            type=RecommendationType.IMMEDIATE_ACTION,
            title="Сделайте перерыв",
            description="Отдохните 10 минут.",
            priority=5,
            estimated_time=10,
            difficulty="easy",
            applicable_risk_levels=[BurnoutRiskLevel.MEDIUM],
            applicable_modes=[BurnoutModeType.OVERWORK],
        )
    ])
    patches["backend.burnout_guard.api.RecommendationEngine"].return_value = rec_instance

    db_instance = MagicMock()
    db_instance.store_risk_assessment = AsyncMock(return_value=True)
    patches["backend.burnout_guard.api.BurnoutDatabaseAdapter"].return_value = db_instance


# ---------------------------------------------------------------------------
# POST /api/v1/burnout/user/{user_id}/assess
# ---------------------------------------------------------------------------

class TestAssessEndpoint:
    def test_assess_returns_200_with_valid_state(self, client):
        state = _mock_burnout_state()

        with patch("backend.burnout_guard.api.EMOTIME_AVAILABLE", True), \
             patch("backend.burnout_guard.api.EmotimeEngine"), \
             patch("backend.burnout_guard.api.BurnoutGuardEngine") as MockEngine, \
             patch("backend.burnout_guard.api.RecommendationEngine") as MockRec, \
             patch("backend.burnout_guard.api.BurnoutDatabaseAdapter") as MockDB:

            # Wire mocks
            engine_inst = MagicMock()
            engine_inst.analyze_now = AsyncMock(return_value=state)
            MockEngine.return_value = engine_inst

            from backend.burnout_guard.recommendations import Recommendation, RecommendationType
            from backend.burnout_guard.modes import BurnoutRiskLevel, BurnoutModeType
            rec_inst = MagicMock()
            rec_inst.get_recommendations = AsyncMock(return_value=[])
            MockRec.return_value = rec_inst

            db_inst = MagicMock()
            db_inst.store_risk_assessment = AsyncMock(return_value=True)
            MockDB.return_value = db_inst

            resp = client.post("/api/v1/burnout/user/test_user/assess", json={})

        assert resp.status_code == 200
        body = resp.json()
        assert body["status"] == "assessed"
        assert body["user_id"] == "test_user"

    def test_assess_returns_risk_assessment_fields(self, client):
        state = _mock_burnout_state()

        with patch("backend.burnout_guard.api.EMOTIME_AVAILABLE", True), \
             patch("backend.burnout_guard.api.EmotimeEngine"), \
             patch("backend.burnout_guard.api.BurnoutGuardEngine") as MockEngine, \
             patch("backend.burnout_guard.api.RecommendationEngine") as MockRec, \
             patch("backend.burnout_guard.api.BurnoutDatabaseAdapter") as MockDB:

            MockEngine.return_value.analyze_now = AsyncMock(return_value=state)
            MockRec.return_value.get_recommendations = AsyncMock(return_value=[])
            MockDB.return_value.store_risk_assessment = AsyncMock(return_value=True)

            resp = client.post("/api/v1/burnout/user/u1/assess", json={})

        body = resp.json()
        risk = body.get("risk_assessment", {})
        assert "score" in risk
        assert "level" in risk
        assert "confidence" in risk
        assert "factors" in risk
        assert 0.0 <= risk["score"] <= 1.0

    def test_assess_no_data_when_engine_returns_none(self, client):
        with patch("backend.burnout_guard.api.EMOTIME_AVAILABLE", True), \
             patch("backend.burnout_guard.api.EmotimeEngine"), \
             patch("backend.burnout_guard.api.BurnoutGuardEngine") as MockEngine, \
             patch("backend.burnout_guard.api.BurnoutDatabaseAdapter"):

            MockEngine.return_value.analyze_now = AsyncMock(return_value=None)

            resp = client.post("/api/v1/burnout/user/u1/assess", json={})

        assert resp.status_code == 200
        assert resp.json()["status"] == "no_data"

    def test_assess_503_when_emotime_unavailable(self, client):
        with patch("backend.burnout_guard.api.EMOTIME_AVAILABLE", False):
            resp = client.post("/api/v1/burnout/user/u1/assess", json={})
        assert resp.status_code == 503

    def test_assess_includes_recommendations(self, client):
        from backend.burnout_guard.recommendations import Recommendation, RecommendationType
        from backend.burnout_guard.modes import BurnoutRiskLevel, BurnoutModeType

        state = _mock_burnout_state()
        rec = Recommendation(
            id="r_test",
            type=RecommendationType.DAILY_ROUTINE,
            title="Медитация",
            description="10 минут утром.",
            priority=3,
            estimated_time=10,
            difficulty="easy",
            applicable_risk_levels=[BurnoutRiskLevel.MEDIUM],
            applicable_modes=[BurnoutModeType.OVERWORK],
        )

        with patch("backend.burnout_guard.api.EMOTIME_AVAILABLE", True), \
             patch("backend.burnout_guard.api.EmotimeEngine"), \
             patch("backend.burnout_guard.api.BurnoutGuardEngine") as MockEngine, \
             patch("backend.burnout_guard.api.RecommendationEngine") as MockRec, \
             patch("backend.burnout_guard.api.BurnoutDatabaseAdapter") as MockDB:

            MockEngine.return_value.analyze_now = AsyncMock(return_value=state)
            MockRec.return_value.get_recommendations = AsyncMock(return_value=[rec])
            MockDB.return_value.store_risk_assessment = AsyncMock(return_value=True)

            resp = client.post("/api/v1/burnout/user/u1/assess", json={})

        recs = resp.json().get("recommendations", [])
        assert len(recs) == 1
        assert recs[0]["id"] == "r_test"
        assert recs[0]["title"] == "Медитация"

    def test_assess_next_actions_for_high_risk(self, client):
        """High-risk state should produce next_actions list."""
        state = _mock_burnout_state()
        from backend.burnout_guard.modes import BurnoutRiskLevel
        state.risk_assessment.level = BurnoutRiskLevel.HIGH
        state.risk_assessment.score = 0.75

        with patch("backend.burnout_guard.api.EMOTIME_AVAILABLE", True), \
             patch("backend.burnout_guard.api.EmotimeEngine"), \
             patch("backend.burnout_guard.api.BurnoutGuardEngine") as MockEngine, \
             patch("backend.burnout_guard.api.RecommendationEngine") as MockRec, \
             patch("backend.burnout_guard.api.BurnoutDatabaseAdapter") as MockDB:

            MockEngine.return_value.analyze_now = AsyncMock(return_value=state)
            MockRec.return_value.get_recommendations = AsyncMock(return_value=[])
            MockDB.return_value.store_risk_assessment = AsyncMock(return_value=True)

            resp = client.post("/api/v1/burnout/user/u1/assess", json={})

        actions = resp.json().get("next_actions", [])
        assert len(actions) > 0
        urgencies = {a["urgency"] for a in actions}
        assert "immediate" in urgencies


# ---------------------------------------------------------------------------
# GET /api/v1/burnout/user/{user_id}/progress
# ---------------------------------------------------------------------------

class TestProgressEndpoint:
    def _mock_history(self):
        return [
            {"timestamp": "2026-03-01T10:00:00", "risk_score": 0.4, "risk_level": "medium", "confidence": 0.8},
            {"timestamp": "2026-03-08T10:00:00", "risk_score": 0.55, "risk_level": "medium", "confidence": 0.85},
            {"timestamp": "2026-03-15T10:00:00", "risk_score": 0.35, "risk_level": "low", "confidence": 0.9},
        ]

    def test_progress_returns_200(self, client):
        with patch("backend.burnout_guard.api.BurnoutDatabaseAdapter") as MockDB:
            db = MagicMock()
            db.get_risk_history     = AsyncMock(return_value=self._mock_history())
            db.get_latest_burnout_state = AsyncMock(return_value={"risk_level": "low", "risk_score": 0.35})
            MockDB.return_value = db

            resp = client.get("/api/v1/burnout/user/u1/progress")

        assert resp.status_code == 200

    def test_progress_response_structure(self, client):
        with patch("backend.burnout_guard.api.BurnoutDatabaseAdapter") as MockDB:
            db = MagicMock()
            db.get_risk_history         = AsyncMock(return_value=self._mock_history())
            db.get_latest_burnout_state = AsyncMock(return_value=None)
            MockDB.return_value = db

            resp = client.get("/api/v1/burnout/user/u1/progress?days=30")

        body = resp.json()
        assert body["user_id"] == "u1"
        assert body["period_days"] == 30
        assert "summary" in body
        assert "risk_history" in body
        assert body["summary"]["data_points"] == 3

    def test_progress_trend_calculation(self, client):
        """With fewer than 14 points, trend should be 'stable'."""
        with patch("backend.burnout_guard.api.BurnoutDatabaseAdapter") as MockDB:
            db = MagicMock()
            db.get_risk_history         = AsyncMock(return_value=self._mock_history())
            db.get_latest_burnout_state = AsyncMock(return_value=None)
            MockDB.return_value = db

            resp = client.get("/api/v1/burnout/user/u1/progress")

        assert resp.json()["summary"]["trend"] in ("stable", "improving", "worsening")

    def test_progress_improving_trend(self, client):
        """14 points where recent avg is much lower → improving."""
        old = [{"timestamp": f"2026-01-{i:02d}T00:00:00", "risk_score": 0.75, "risk_level": "high", "confidence": 0.8} for i in range(1, 8)]
        recent = [{"timestamp": f"2026-02-{i:02d}T00:00:00", "risk_score": 0.25, "risk_level": "low", "confidence": 0.85} for i in range(1, 8)]

        with patch("backend.burnout_guard.api.BurnoutDatabaseAdapter") as MockDB:
            db = MagicMock()
            db.get_risk_history         = AsyncMock(return_value=old + recent)
            db.get_latest_burnout_state = AsyncMock(return_value=None)
            MockDB.return_value = db

            resp = client.get("/api/v1/burnout/user/u1/progress?days=30")

        assert resp.json()["summary"]["trend"] == "improving"

    def test_progress_days_query_param(self, client):
        with patch("backend.burnout_guard.api.BurnoutDatabaseAdapter") as MockDB:
            db = MagicMock()
            db.get_risk_history         = AsyncMock(return_value=[])
            db.get_latest_burnout_state = AsyncMock(return_value=None)
            MockDB.return_value = db

            resp = client.get("/api/v1/burnout/user/u1/progress?days=7")

        assert resp.status_code == 200
        assert resp.json()["period_days"] == 7

    def test_progress_days_validation(self, client):
        """days > 90 should return 422."""
        resp = client.get("/api/v1/burnout/user/u1/progress?days=999")
        assert resp.status_code == 422


# ---------------------------------------------------------------------------
# POST /api/v1/burnout/user/{user_id}/feedback
# ---------------------------------------------------------------------------

class TestFeedbackEndpoint:
    def test_feedback_completed_action(self, client):
        with patch("backend.burnout_guard.api.BurnoutDatabaseAdapter") as MockDB:
            db = MagicMock()
            db.track_recommendation_usage = AsyncMock(return_value=True)
            MockDB.return_value = db

            resp = client.post(
                "/api/v1/burnout/user/u1/feedback",
                json={"recommendation_id": "r1", "action": "completed", "effectiveness_rating": 0.9},
            )

        assert resp.status_code == 200
        body = resp.json()
        assert body["stored"] is True
        assert body["action"] == "completed"

    def test_feedback_dismissed_action(self, client):
        with patch("backend.burnout_guard.api.BurnoutDatabaseAdapter") as MockDB:
            db = MagicMock()
            db.track_recommendation_usage = AsyncMock(return_value=True)
            MockDB.return_value = db

            resp = client.post(
                "/api/v1/burnout/user/u1/feedback",
                json={"recommendation_id": "r1", "action": "dismissed"},
            )

        assert resp.status_code == 200

    def test_feedback_invalid_action_returns_422(self, client):
        resp = client.post(
            "/api/v1/burnout/user/u1/feedback",
            json={"recommendation_id": "r1", "action": "invalid_action"},
        )
        assert resp.status_code == 422

    def test_feedback_missing_recommendation_id(self, client):
        resp = client.post(
            "/api/v1/burnout/user/u1/feedback",
            json={"action": "completed"},
        )
        assert resp.status_code == 422

    def test_feedback_all_valid_actions(self, client):
        for action in ("viewed", "accepted", "dismissed", "completed"):
            with patch("backend.burnout_guard.api.BurnoutDatabaseAdapter") as MockDB:
                MockDB.return_value.track_recommendation_usage = AsyncMock(return_value=True)

                resp = client.post(
                    "/api/v1/burnout/user/u1/feedback",
                    json={"recommendation_id": "r1", "action": action},
                )
            assert resp.status_code == 200, f"action={action} failed with {resp.status_code}"

    def test_feedback_effectiveness_is_optional(self, client):
        with patch("backend.burnout_guard.api.BurnoutDatabaseAdapter") as MockDB:
            MockDB.return_value.track_recommendation_usage = AsyncMock(return_value=True)

            resp = client.post(
                "/api/v1/burnout/user/u1/feedback",
                json={"recommendation_id": "r1", "action": "viewed"},
            )
        assert resp.status_code == 200
