"""Authenticated debug and diagnostics routes."""

from __future__ import annotations

from fastapi import APIRouter, Depends

from backend.auth.dependencies import token_verifier

from ..dependencies import (
    get_connection_manager_service,
    get_memory_service,
    get_ml_service,
)

router = APIRouter(
    tags=["debug"],
    dependencies=[Depends(token_verifier)],
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
