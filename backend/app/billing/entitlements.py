"""Feature flags derived from subscription tier."""

from __future__ import annotations

from dataclasses import dataclass

from .models import SubscriptionTier


@dataclass(frozen=True)
class Entitlements:
    tier: SubscriptionTier
    burnout_pro: bool
    team_dashboard: bool
    api_access: bool


def entitlements_for_tier(tier: SubscriptionTier) -> Entitlements:
    if tier == SubscriptionTier.TEAM:
        return Entitlements(
            tier=tier,
            burnout_pro=True,
            team_dashboard=True,
            api_access=True,
        )
    if tier == SubscriptionTier.PRO:
        return Entitlements(
            tier=tier,
            burnout_pro=True,
            team_dashboard=False,
            api_access=False,
        )
    return Entitlements(
        tier=SubscriptionTier.FREE,
        burnout_pro=False,
        team_dashboard=False,
        api_access=False,
    )
