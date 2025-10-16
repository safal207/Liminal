"""Routes for memory fragments and the memory timeline."""
from __future__ import annotations

from datetime import datetime
from typing import Dict, List, Optional

from fastapi import APIRouter, Depends, HTTPException, status

from ..dependencies import get_memory_timeline, get_neo4j_service
from ..schemas import MemoryCreate, MemoryFragmentCreate

router = APIRouter(tags=["fragments"])


@router.post("/fragments/", response_model=dict)
async def create_fragment(
    fragment: MemoryFragmentCreate, service=Depends(get_neo4j_service)
):
    fragment_data = fragment.dict()
    fragment_data["id"] = f"mem_{int(datetime.utcnow().timestamp())}"
    fragment_data["timestamp"] = datetime.utcnow().isoformat()
    node = service.create_fragment(fragment_data)
    if not node:
        raise HTTPException(status_code=500, detail="Не удалось создать MemoryFragment")
    return {"status": "success", "id": node.get("id", fragment_data["id"]) }


@router.get("/fragments/", response_model=List[dict])
async def get_fragments(limit: int = 10, service=Depends(get_neo4j_service)):
    try:
        return service.list_fragments(limit)
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc)) from exc


@router.post(
    "/timeline/memories/",
    response_model=Dict,
    status_code=status.HTTP_201_CREATED,
    responses={
        201: {"description": "Воспоминание успешно добавлено"},
        422: {"description": "Ошибка валидации входных данных"},
    },
)
async def add_memory(memory: MemoryCreate, timeline=Depends(get_memory_timeline)):
    try:
        return await timeline.add_memory(
            content=memory.content,
            memory_type=memory.memory_type,
            metadata=memory.metadata or {},
        )
    except Exception as exc:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Ошибка при добавлении воспоминания: {exc}",
        ) from exc


@router.get("/timeline/memories/", response_model=List[Dict])
async def get_memories(
    start_time: Optional[datetime] = None,
    end_time: Optional[datetime] = None,
    memory_type: Optional[str] = None,
    limit: int = 100,
    timeline=Depends(get_memory_timeline),
):
    return timeline.get_timeline(start_time, end_time, memory_type, limit)
