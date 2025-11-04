"""Compatibility shim exposing the backend logging configuration helpers.

Historically modules imported ``logging_config`` from the repository root while
newer code keeps the implementation under ``backend.logging_config``.  The test
suite still expects the legacy location, so we simply re-export everything from
the backend module here.
"""

from backend.logging_config import *  # noqa: F401,F403

try:  # pragma: no cover - optional attribute
    from backend.logging_config import __all__  # type: ignore
except ImportError:  # pragma: no cover
    import backend.logging_config as _backend_logging_config

    __all__ = [
        name
        for name in dir(_backend_logging_config)
        if not name.startswith("_")
    ]
