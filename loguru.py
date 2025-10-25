# Minimal compatibility shim for `from loguru import logger`
# Replaces loguru with project logger (structlog-based) without external dependency.
from typing import Any, Optional

try:
    # Primary: project-level logging_config
    from logging_config import get_logger  # type: ignore
except Exception:
    try:
        # Alternate legacy location
        from backend.app_logging import get_logger  # type: ignore
    except Exception:  # fallback to stdlib logging if our config isn't available yet
        import logging
        def get_logger(name: str):
            return logging.getLogger(name)


class _CompatLogger:
    def __init__(self) -> None:
        self._logger = get_logger(__name__)

    # Common logging levels
    def debug(self, msg: str, *args: Any, **kwargs: Any) -> None:
        self._logger.debug(msg, *args, **kwargs)

    def info(self, msg: str, *args: Any, **kwargs: Any) -> None:
        self._logger.info(msg, *args, **kwargs)

    def warning(self, msg: str, *args: Any, **kwargs: Any) -> None:
        # structlog usually maps to .warning as well
        if hasattr(self._logger, "warning"):
            self._logger.warning(msg, *args, **kwargs)
        else:
            self._logger.warn(msg, *args, **kwargs)  # type: ignore[attr-defined]

    def error(self, msg: str, *args: Any, **kwargs: Any) -> None:
        self._logger.error(msg, *args, **kwargs)

    def exception(self, msg: str, *args: Any, **kwargs: Any) -> None:
        # Some callers expect stack traces; structlog .exception does that
        if hasattr(self._logger, "exception"):
            self._logger.exception(msg, *args, **kwargs)
        else:
            self._logger.error(msg, *args, **kwargs)

    # No-op compatibility stubs for loguru-specific API used in code
    def add(self, *args: Any, **kwargs: Any) -> Optional[int]:  # loguru returns handler id
        return 0

    def remove(self, *args: Any, **kwargs: Any) -> None:
        return None

    def bind(self, **kwargs: Any):  # loguru returns a bound logger; we return self
        return self

    # Sometimes code may call .trace; map to debug
    def trace(self, msg: str, *args: Any, **kwargs: Any) -> None:
        self._logger.debug(msg, *args, **kwargs)

    # Allow attribute access passthrough if underlying logger supports more
    def __getattr__(self, item: str):
        return getattr(self._logger, item)


logger = _CompatLogger()
