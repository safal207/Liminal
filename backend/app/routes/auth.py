"""Authentication API routes."""
from __future__ import annotations

from fastapi import APIRouter, Depends, HTTPException, status

from backend.auth.dependencies import token_verifier
from backend.auth.jwt_utils import authenticate_user, create_access_token_for_user
from backend.auth.models import Token, UserLogin

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
    return {
        "user_id": payload.get("sub"),
        "username": payload.get("username"),
        "message": "Токен действителен",
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
