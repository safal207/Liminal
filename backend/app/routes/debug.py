"""Authenticated debug and diagnostics routes."""

from __future__ import annotations

import os
import secrets
from typing import Annotated, Any

from fastapi import APIRouter, Depends, Header, HTTPException, status

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


def require_ml_metrics_access(
    token: Annotated[
        str | None,
        Header(alias="X-Liminal-ML-Token"),
    ] = None,
) -> None:
    """Authenticate the internal metrics consumer without enabling debug routes."""
    settings = get_settings()
    configured = os.getenv("ML_METRICS_SERVICE_TOKEN", "").strip()
    if settings.environment != "production" and not configured:
        return
    if len(configured) < 32:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="ML metrics service unavailable",
        )
    if token is None or not secrets.compare_digest(
        token.encode("utf-8"),
        configured.encode("utf-8"),
    ):
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Invalid ML metrics service token",
        )


router = APIRouter(
    tags=["debug"],
    dependencies=[Depends(require_debug_routes_enabled), Depends(require_debug_access)],
)

ml_metrics_router = APIRouter(
    tags=["ml-internal"],
    dependencies=[Depends(require_ml_metrics_access)],
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


@ml_metrics_router.get("/ml_metrics")
async def get_ml_metrics(service: Annotated[Any, Depends(get_ml_service)]):
    return service.collect_metrics()
