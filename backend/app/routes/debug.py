"""Debug and diagnostics routes."""
from __future__ import annotations

from fastapi import APIRouter, Depends

from ..dependencies import (
    get_connection_manager,
    get_memory_timeline,
    get_ml_service,
)

router = APIRouter(tags=["debug"])


@router.get("/debug/subscribers/count")
async def get_subscribers_count(timeline=Depends(get_memory_timeline)):
    if hasattr(timeline, "subscribers"):
        count = len(timeline.subscribers)
    elif hasattr(timeline, "_subscribers"):
        count = len(timeline._subscribers)
    else:
        count = 0
    return {"count": count}


@router.get("/debug/connections/stats")
async def get_connection_stats(manager=Depends(get_connection_manager)):
    return manager.get_connection_stats()


@router.get("/ml_metrics")
async def get_ml_metrics(service=Depends(get_ml_service)):
    return service.collect_metrics()
