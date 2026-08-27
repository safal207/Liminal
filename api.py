"""Compatibility wrapper exposing the FastAPI application at the legacy module path.

Historically the project exposed ``app`` from a top-level ``api`` module.  The
backend refactor moved the canonical application into :mod:`backend.app.main`.
Some third-party integrations and ad-hoc scripts, however, still ``import api``
directly.

To avoid breaking those environments we keep this tiny facade that simply
re-exports the hardened application object and lifespan context.
"""

from __future__ import annotations

from backend.app.main import app, lifespan

__all__ = ["app", "lifespan"]
