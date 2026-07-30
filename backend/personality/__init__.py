"""Personality integration with optional research dependencies."""

from __future__ import annotations

import os
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from .adapter import PersonalityAdapter

router = None

if os.getenv("LIMINAL_ENABLE_PERSONALITY_ROUTER", "0") == "1":
    try:
        from .router import router as _router
    except ImportError:
        router = None
    else:
        router = _router


def __getattr__(name: str) -> Any:
    """Load the research-heavy adapter only when explicitly requested."""
    if name == "PersonalityAdapter":
        from .adapter import PersonalityAdapter

        return PersonalityAdapter
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")


__all__ = ["PersonalityAdapter", "router"]
