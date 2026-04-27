"""Liminal backend application package.

`app` is loaded lazily so submodules (e.g. billing) can be imported without
pulling the full FastAPI graph (dependency_injector, etc.) during unit tests.
"""

from __future__ import annotations

from typing import Any


def __getattr__(name: str) -> Any:
    if name == "app":
        from .main import app

        return app
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")


__all__ = ["app"]
