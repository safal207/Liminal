"""FastAPI dependencies for billing and entitlements."""

from __future__ import annotations

from typing import Any, Dict

from fastapi import Depends, HTTPException, status

from backend.auth.dependencies import token_verifier

from .entitlements import Entitlements, entitlements_for_tier
from .store import get_user_state


async def get_entitlements(
    payload: Dict[str, Any] = Depends(token_verifier),
) -> Entitlements:
    uid = payload.get("sub")
    if not uid:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Invalid token payload",
        )
    state = get_user_state(str(uid))
    return entitlements_for_tier(state.tier)


async def require_burnout_pro(
    payload: Dict[str, Any] = Depends(token_verifier),
    ent: Entitlements = Depends(get_entitlements),
) -> Dict[str, Any]:
    if not ent.burnout_pro:
        raise HTTPException(
            status_code=status.HTTP_402_PAYMENT_REQUIRED,
            detail=("Pro or Team subscription required for this BurnoutGuard feature"),
        )
    return payload


async def require_team_dashboard(
    payload: Dict[str, Any] = Depends(token_verifier),
    ent: Entitlements = Depends(get_entitlements),
) -> Dict[str, Any]:
    if not ent.team_dashboard:
        raise HTTPException(
            status_code=status.HTTP_402_PAYMENT_REQUIRED,
            detail="Team subscription required",
        )
    return payload
