"""Authenticated debug and diagnostics routes."""

from __future__ import annotations

import os

from fastapi import APIRouter, Depends, HTTPException, status

from backend.auth.dependencies import token_verifier
from backend.config import get_settings

from ..dependencies import (
    get_connection_manager_service,
    get_memory_service,
    get_ml_service,
)


def require_debug_access(payload: dict = Depends(token_verifier)) -> dict:
    """Require access auth and explicit production opt-in for diagnostics."""
    settings = get_settings()
    enabled_in_production = os.getenv("ENABLE_DEBUG_ROUTES", "false").lower() == "true"
    if settings.environment == "production" and not enabled_in_production:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Not found")
    return payload


router = APIRouter(
    tags=["debug"],
    dependencies=[Depends(require_debug_access)],
)


@router.get("/debug/subscribers/count")
async def get_subscribers_count(service=Depends(get_memory_service)):
    return {"count": service.subscriber_count()}


@router.get("/debug/connections/stats")
async def get_connection_stats(service=Depends(get_connection_manager_service)):
    return service.get_connection_stats()


@router.get("/ml_metrics")
async def get_ml_metrics(service=Depends(get_ml_service)):
    return service.collect_metrics()
