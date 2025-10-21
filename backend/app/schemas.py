"""Pydantic schemas used by the modular FastAPI application."""
from __future__ import annotations

from typing import Any, Dict, Optional

from pydantic import BaseModel, Field


class DuneWaveCreate(BaseModel):
    phase: str = Field(..., description="Phase of the dune wave")
    emotion: str = Field(..., description="Emotion associated with the wave")
    intensity: float = Field(..., description="Intensity value")
    context: str = Field(..., description="Context of the wave")
    source: str = Field(..., description="Source identifier")


class MemoryFragmentCreate(BaseModel):
    content: str = Field(..., description="Content of the memory fragment")
    type: str = Field(..., description="Type of fragment")
    growth_stage: str = Field(..., description="Growth stage metadata")
    metadata: Optional[Dict[str, Any]] = Field(
        default_factory=dict, description="Additional metadata for the fragment"
    )


class MentorshipCreate(BaseModel):
    younger_id: str = Field(..., description="ID of the mentee fragment")
    mentor_id: str = Field(..., description="ID of the mentor fragment")


class MemoryCreate(BaseModel):
    content: str = Field(..., description="Content of the memory entry")
    memory_type: str = Field(..., description="Type of the memory entry")
    metadata: Optional[Dict[str, Any]] = Field(
        default_factory=dict, description="Additional metadata"
    )
