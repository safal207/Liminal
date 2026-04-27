"""Billing domain models."""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional


class SubscriptionTier(str, Enum):
    FREE = "free"
    PRO = "pro"
    TEAM = "team"


@dataclass
class UserBillingState:
    """Persisted billing snapshot per application user (JWT sub)."""

    user_id: str
    tier: SubscriptionTier = SubscriptionTier.FREE
    stripe_customer_id: Optional[str] = None
    stripe_subscription_id: Optional[str] = None
    status: str = "none"  # active, canceled, past_due, none
    current_period_end: Optional[datetime] = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "user_id": self.user_id,
            "tier": self.tier.value,
            "stripe_customer_id": self.stripe_customer_id,
            "stripe_subscription_id": self.stripe_subscription_id,
            "status": self.status,
            "current_period_end": (
                self.current_period_end.isoformat() if self.current_period_end else None
            ),
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "UserBillingState":
        cpe = data.get("current_period_end")
        dt: Optional[datetime] = None
        if cpe:
            try:
                dt = datetime.fromisoformat(cpe.replace("Z", "+00:00"))
            except ValueError:
                dt = None
        tier_raw = data.get("tier", "free")
        try:
            tier = SubscriptionTier(tier_raw)
        except ValueError:
            tier = SubscriptionTier.FREE
        return cls(
            user_id=data["user_id"],
            tier=tier,
            stripe_customer_id=data.get("stripe_customer_id"),
            stripe_subscription_id=data.get("stripe_subscription_id"),
            status=data.get("status", "none"),
            current_period_end=dt,
        )


@dataclass
class BillingStoreData:
    users: Dict[str, Dict[str, Any]] = field(default_factory=dict)
    processed_event_ids: List[str] = field(default_factory=list)
