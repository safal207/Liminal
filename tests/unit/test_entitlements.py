"""Unit tests for subscription entitlements mapping."""

from backend.app.billing.entitlements import entitlements_for_tier
from backend.app.billing.models import SubscriptionTier


def test_free_tier_has_no_premium_flags():
    ent = entitlements_for_tier(SubscriptionTier.FREE)
    assert ent.burnout_pro is False
    assert ent.team_dashboard is False
    assert ent.api_access is False


def test_pro_tier_has_burnout_pro():
    ent = entitlements_for_tier(SubscriptionTier.PRO)
    assert ent.burnout_pro is True
    assert ent.team_dashboard is False


def test_team_tier_has_all_flags():
    ent = entitlements_for_tier(SubscriptionTier.TEAM)
    assert ent.burnout_pro is True
    assert ent.team_dashboard is True
    assert ent.api_access is True
