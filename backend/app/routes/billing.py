"""Stripe Checkout, Customer Portal, webhooks, and subscription status."""

from __future__ import annotations

from typing import Dict, Optional

from fastapi import APIRouter, Depends, HTTPException, Request, status
from pydantic import BaseModel

from backend.auth.dependencies import token_verifier
from backend.core.settings import get_settings

from ..billing.entitlements import entitlements_for_tier
from ..billing.store import get_user_state
from ..billing.stripe_webhook import handle_stripe_event

router = APIRouter(prefix="/billing", tags=["billing"])


class CheckoutSessionResponse(BaseModel):
    url: str


class PortalSessionResponse(BaseModel):
    url: str


class BillingMeResponse(BaseModel):
    tier: str
    entitlements: Dict[str, bool]
    stripe_customer_id: Optional[str] = None
    subscription_status: str = "none"


@router.get("/me", response_model=BillingMeResponse)
async def billing_me(
    payload: dict = Depends(token_verifier),
) -> BillingMeResponse:
    uid = str(payload.get("sub") or "")
    state = get_user_state(uid)
    ent = entitlements_for_tier(state.tier)
    return BillingMeResponse(
        tier=state.tier.value,
        entitlements={
            "burnout_pro": ent.burnout_pro,
            "team_dashboard": ent.team_dashboard,
            "api_access": ent.api_access,
        },
        stripe_customer_id=state.stripe_customer_id,
        subscription_status=state.status,
    )


@router.post("/checkout-session", response_model=CheckoutSessionResponse)
async def create_checkout_session(
    payload: dict = Depends(token_verifier),
) -> CheckoutSessionResponse:
    s = get_settings().billing
    if not s.stripe_secret_key or not s.stripe_price_pro_monthly:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="Stripe billing is not configured",
        )

    import stripe

    stripe.api_key = s.stripe_secret_key
    user_id = str(payload.get("sub") or "")
    if not user_id:
        raise HTTPException(status_code=400, detail="Missing user id in token")

    session = stripe.checkout.Session.create(
        mode="subscription",
        line_items=[{"price": s.stripe_price_pro_monthly, "quantity": 1}],
        success_url=s.stripe_success_url,
        cancel_url=s.stripe_cancel_url,
        client_reference_id=user_id,
        metadata={"user_id": user_id},
    )
    url = session.url
    if not url:
        raise HTTPException(500, detail="Stripe did not return a checkout URL")
    return CheckoutSessionResponse(url=url)


@router.post("/portal-session", response_model=PortalSessionResponse)
async def create_portal_session(
    payload: dict = Depends(token_verifier),
) -> PortalSessionResponse:
    s = get_settings().billing
    if not s.stripe_secret_key:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="Stripe billing is not configured",
        )

    import stripe

    stripe.api_key = s.stripe_secret_key
    user_id = str(payload.get("sub") or "")
    state = get_user_state(user_id)
    if not state.stripe_customer_id:
        raise HTTPException(
            status_code=400,
            detail="No Stripe customer linked; complete checkout first",
        )

    session = stripe.billing_portal.Session.create(
        customer=state.stripe_customer_id,
        return_url=s.stripe_success_url,
    )
    url = session.url
    if not url:
        raise HTTPException(500, detail="Stripe did not return a portal URL")
    return PortalSessionResponse(url=url)


@router.post("/webhook")
async def stripe_webhook(request: Request) -> Dict[str, str]:
    s = get_settings().billing
    if not s.stripe_webhook_secret:
        raise HTTPException(503, detail="Webhook secret not configured")

    import stripe
    from stripe.error import SignatureVerificationError

    payload = await request.body()
    sig = request.headers.get("stripe-signature")
    if not sig:
        raise HTTPException(400, detail="Missing stripe-signature")

    try:
        event = stripe.Webhook.construct_event(payload, sig, s.stripe_webhook_secret)
    except ValueError as exc:
        raise HTTPException(400, detail="Invalid payload") from exc
    except SignatureVerificationError as exc:
        raise HTTPException(400, detail="Invalid signature") from exc

    handle_stripe_event(event)
    return {"status": "ok"}
