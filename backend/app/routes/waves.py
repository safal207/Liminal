"""Routes for working with dune waves and relationships."""
from __future__ import annotations

from datetime import datetime
from typing import List

from fastapi import APIRouter, Depends, HTTPException

from ..dependencies import get_neo4j_service
from ..schemas import DuneWaveCreate, MentorshipCreate

router = APIRouter(tags=["waves"])


@router.post("/waves/", response_model=dict)
async def create_wave(wave: DuneWaveCreate, service=Depends(get_neo4j_service)):
    wave_data = wave.dict()
    wave_data["id"] = f"wave_{int(datetime.utcnow().timestamp())}"
    wave_data["timestamp"] = datetime.utcnow().isoformat()
    node = service.create_wave(wave_data)
    if not node:
        raise HTTPException(status_code=500, detail="Не удалось создать DuneWave")
    return {"status": "success", "id": node.get("id", wave_data["id"]) }


@router.get("/waves/", response_model=List[dict])
async def get_waves(limit: int = 10, service=Depends(get_neo4j_service)):
    try:
        return service.list_waves(limit)
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc)) from exc


@router.post("/relationships/link-wave-to-memory/", response_model=dict)
async def link_wave_to_memory(wave_id: str, memory_id: str, service=Depends(get_neo4j_service)):
    if not service.link_wave_to_memory(wave_id, memory_id):
        raise HTTPException(status_code=400, detail="Не удалось создать связь")
    return {"status": "success"}


@router.post("/relationships/mentorship/", response_model=dict)
async def create_mentorship(mentorship: MentorshipCreate, service=Depends(get_neo4j_service)):
    if not service.create_mentorship(mentorship.younger_id, mentorship.mentor_id):
        raise HTTPException(
            status_code=400, detail="Не удалось создать связь наставничества"
        )
    return {"status": "success"}
