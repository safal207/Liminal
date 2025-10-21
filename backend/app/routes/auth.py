"""Authentication API routes."""
from __future__ import annotations

from fastapi import APIRouter, Depends, HTTPException, status

from backend.auth.dependencies import token_verifier
from backend.auth.jwt_utils import authenticate_user, create_access_token_for_user
from backend.auth.models import Token, UserLogin

router = APIRouter(tags=["auth"])


@router.post("/auth/login", response_model=Token)
async def login(user_data: UserLogin) -> Token:
    user = authenticate_user(user_data.username, user_data.password)
    if not user:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Неверные учетные данные",
            headers={"WWW-Authenticate": "Bearer"},
        )
    access_token = create_access_token_for_user(user)
    return Token(access_token=access_token, token_type="bearer")


@router.get("/auth/me")
async def get_current_user(payload: dict = Depends(token_verifier)):
    return {
        "user_id": payload.get("sub"),
        "username": payload.get("username"),
        "message": "Токен действителен",
    }


@router.post("/token", response_model=Token)
async def login_for_access_token(form_data: UserLogin) -> Token:
    user = authenticate_user(form_data.username, form_data.password)
    if not user:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Incorrect username or password",
            headers={"WWW-Authenticate": "Bearer"},
        )
    access_token = create_access_token_for_user(user)
    return Token(access_token=access_token, token_type="bearer")
