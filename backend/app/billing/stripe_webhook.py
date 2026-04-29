"""Stripe webhook event handling."""

from __future__ import annotations

from datetime import datetime, timezone
from typing import Any, Dict, Optional

from backend.core.settings import get_settings

from .models import SubscriptionTier, UserBillingState
from .store import (
    find_user_id_by_stripe_customer,
    is_event_processed,
    mark_event_processed,
    upsert_user_state,
)


def _tier_from_price_id(price_id: str) -> SubscriptionTier:
    s = get_settings().billing
    if price_id and price_id == s.stripe_price_pro_monthly:
        return SubscriptionTier.PRO
    return SubscriptionTier.FREE


def _period_end_from_subscription(sub: Dict[str, Any]) -> Optional[datetime]:
    raw = sub.get("current_period_end")
    if raw is None:
        return None
    try:
        return datetime.fromtimestamp(int(raw), tz=timezone.utc)
    except (TypeError, ValueError, OSError):
        return None


def handle_stripe_event(event: Dict[str, Any]) -> None:
    event_id = event.get("id")
    if not event_id or is_event_processed(event_id):
        return

    et = event.get("type")
    obj = event.get("data", {}).get("object") or {}

    if et == "checkout.session.completed":
        _on_checkout_completed(obj)
    elif et == "customer.subscription.updated":
        _on_subscription_updated(obj)
    elif et == "customer.subscription.deleted":
        _on_subscription_deleted(obj)

    mark_event_processed(event_id)


def _on_checkout_completed(session: Dict[str, Any]) -> None:
    user_id = session.get("client_reference_id") or (
        (session.get("metadata") or {}).get("user_id")
    )
    if not user_id:
        return

    customer_id = session.get("customer")
    sub_id = session.get("subscription")
    mode = session.get("mode")

    state = UserBillingState(user_id=str(user_id))
    if customer_id:
        state.stripe_customer_id = str(customer_id)
    if sub_id:
        state.stripe_subscription_id = str(sub_id)

    if mode == "subscription" and sub_id:
        state.tier = SubscriptionTier.PRO
        state.status = "active"
    upsert_user_state(state)


def _on_subscription_updated(subscription: Dict[str, Any]) -> None:
    customer_id = subscription.get("customer")
    if not customer_id:
        return

    user_id = find_user_id_by_stripe_customer(str(customer_id))
    if not user_id:
        return

    status = subscription.get("status") or "none"
    items = subscription.get("items", {}).get("data") or []
    price_id = ""
    if items:
        price_id = (items[0].get("price") or {}).get("id") or ""

    tier = _tier_from_price_id(price_id)
    if status in ("canceled", "unpaid", "incomplete_expired"):
        tier = SubscriptionTier.FREE

    state = UserBillingState(
        user_id=user_id,
        tier=tier,
        stripe_customer_id=str(customer_id),
        stripe_subscription_id=str(subscription.get("id") or ""),
        status=str(status),
        current_period_end=_period_end_from_subscription(subscription),
    )
    upsert_user_state(state)


def _on_subscription_deleted(subscription: Dict[str, Any]) -> None:
    customer_id = subscription.get("customer")
    if not customer_id:
        return
    user_id = find_user_id_by_stripe_customer(str(customer_id))
    if not user_id:
        return

    state = UserBillingState(
        user_id=user_id,
        tier=SubscriptionTier.FREE,
        stripe_customer_id=str(customer_id),
        stripe_subscription_id=None,
        status="canceled",
        current_period_end=None,
    )
    upsert_user_state(state)
