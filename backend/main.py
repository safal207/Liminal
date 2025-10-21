"""Compat entrypoint that exposes the refactored FastAPI application."""
from __future__ import annotations

from backend.app.main import app

__all__ = ["app"]


if __name__ == "__main__":
    import uvicorn

    uvicorn.run("backend.app.main:app", host="0.0.0.0", port=8000)
