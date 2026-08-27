"""Authenticated debug and diagnostics routes."""

from __future__ import annotations

import os
from typing import Annotated, Any

from fastapi import APIRouter, Depends, HTTPException, status

from backend.auth.dependencies import token_verifier
from backend.config import get_settings

from ..dependencies import (
    get_connection_manager_service,
    get_memory_service,
    get_ml_service,
)


def require_debug_routes_enabled() -> None:
    """Hide diagnostics before evaluating authentication in production."""
    settings = get_settings()
    enabled_in_production = os.getenv("ENABLE_DEBUG_ROUTES", "false").lower() == "true"
    if settings.environment == "production" and not enabled_in_production:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Not found")


def require_debug_access(
    payload: Annotated[dict, Depends(token_verifier)],
) -> dict:
    """Require a valid access token for enabled diagnostics routes."""
    return payload


router = APIRouter(
    tags=["debug"],
    dependencies=[Depends(require_debug_routes_enabled), Depends(require_debug_access)],
)


@router.get("/debug/subscribers/count")
async def get_subscribers_count(
    service: Annotated[Any, Depends(get_memory_service)],
):
    return {"count": service.subscriber_count()}


@router.get("/debug/connections/stats")
async def get_connection_stats(
    service: Annotated[Any, Depends(get_connection_manager_service)],
):
    return service.get_connection_stats()


@router.get("/ml_metrics")
async def get_ml_metrics(service: Annotated[Any, Depends(get_ml_service)]):
    return service.collect_metrics()
