import pytest
from unittest.mock import AsyncMock, patch
from datetime import datetime
from backend.burnout_guard.persistence import BurnoutDatabaseAdapter, BurnoutRisk
from backend.burnout_guard.modes import BurnoutRiskLevel
from backend.database_adapter import DatabaseAdapter

class TestBurnoutDatabaseAdapter:
    @pytest.mark.asyncio
    async def test_store_risk_returns_true_in_mock_mode(self):
        # Create adapter in mock mode
        adapter = BurnoutDatabaseAdapter(mock_mode=True)
        await adapter.initialize()

        # Mock risk assessment data
        risk_assessment = BurnoutRisk(
            score=0.7,
            level=BurnoutRiskLevel.HIGH,
            factors={"emotional": 0.8, "behavioral": 0.6},
            confidence=0.85,
            timestamp=datetime.now(),
            emotional_indicators=["high_stress"],
            behavioral_patterns=["late_work"],
            duration_risk=0.5,
            trend_risk=0.2
        )

        # Test store_risk (which is an alias for store_risk_assessment)
        result = await adapter.store_risk(
            user_id="test-user",
            risk_assessment=risk_assessment
        )

        assert result is True

    @pytest.mark.asyncio
    async def test_track_recommendation_usage(self):
        # Create adapter in mock mode
        adapter = BurnoutDatabaseAdapter(mock_mode=True)
        await adapter.initialize()

        # Test track_recommendation_usage
        result = await adapter.track_recommendation_usage(
            user_id="test-user",
            recommendation_id="rec-123",
            action="accepted"
        )

        assert result is True
