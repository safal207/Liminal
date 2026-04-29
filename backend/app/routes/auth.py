"""Authentication API routes."""

from __future__ import annotations

from fastapi import APIRouter, Depends, HTTPException, status

from backend.auth.dependencies import token_verifier
from backend.auth.models import Token, UserLogin

from ..billing.entitlements import entitlements_for_tier
from ..billing.store import get_user_state
from ..dependencies import get_auth_service

router = APIRouter(tags=["auth"])


@router.post("/auth/login", response_model=Token)
async def login(user_data: UserLogin, service=Depends(get_auth_service)) -> Token:
    try:
        return service.login(user_data)
    except PermissionError:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Неверные учетные данные",
            headers={"WWW-Authenticate": "Bearer"},
        ) from None


@router.get("/auth/me")
async def get_current_user(payload: dict = Depends(token_verifier)):
    uid = str(payload.get("sub") or "")
    state = get_user_state(uid)
    ent = entitlements_for_tier(state.tier)
    return {
        "user_id": payload.get("sub"),
        "username": payload.get("username"),
        "message": "Токен действителен",
        "subscription_tier": state.tier.value,
        "entitlements": {
            "burnout_pro": ent.burnout_pro,
            "team_dashboard": ent.team_dashboard,
            "api_access": ent.api_access,
        },
    }


@router.post("/token", response_model=Token)
async def login_for_access_token(
    form_data: UserLogin, service=Depends(get_auth_service)
) -> Token:
    try:
        return service.issue_token(form_data)
    except PermissionError:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Incorrect username or password",
            headers={"WWW-Authenticate": "Bearer"},
        ) from None
