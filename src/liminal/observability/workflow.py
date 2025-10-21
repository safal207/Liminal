"""Workflow execution auditing helpers."""
from __future__ import annotations

from datetime import datetime
from typing import Any, Mapping

import structlog


class WorkflowAuditLogger:
    """Structured audit logger for workflow execution."""

    def __init__(self, logger: structlog.BoundLogger | None = None) -> None:
        self._logger = logger or structlog.get_logger("workflow.audit")

    def _ts(self) -> str:
        return datetime.utcnow().isoformat() + "Z"

    def log_start(self, workflow: str, run_id: str, parameters: Mapping[str, Any]) -> None:
        self._logger.info(
            "workflow.start",
            workflow=workflow,
            run_id=run_id,
            timestamp=self._ts(),
            parameters=dict(parameters),
        )

    def log_event(
        self, workflow: str, run_id: str, message: str, **fields: Any
    ) -> None:
        payload = {"workflow": workflow, "run_id": run_id, **fields}
        self._logger.info("workflow.event", message=message, timestamp=self._ts(), **payload)

    def log_failure(
        self,
        workflow: str,
        run_id: str,
        exc: BaseException,
        **fields: Any,
    ) -> None:
        payload = {
            "workflow": workflow,
            "run_id": run_id,
            "error": repr(exc),
            **fields,
        }
        self._logger.error("workflow.failure", timestamp=self._ts(), **payload)

    def log_completion(
        self,
        workflow: str,
        run_id: str,
        result: Any,
        duration_seconds: float,
    ) -> None:
        self._logger.info(
            "workflow.complete",
            workflow=workflow,
            run_id=run_id,
            timestamp=self._ts(),
            duration_seconds=duration_seconds,
            result_summary=self._summarise(result),
        )

    @staticmethod
    def _summarise(result: Any) -> Any:
        if isinstance(result, Mapping):
            return {k: WorkflowAuditLogger._summarise(v) for k, v in list(result.items())[:5]}
        if isinstance(result, (list, tuple)):
            return [WorkflowAuditLogger._summarise(v) for v in result[:5]]
        if hasattr(result, "__dict__"):
            return {k: WorkflowAuditLogger._summarise(v) for k, v in vars(result).items()}
        return result


__all__ = ["WorkflowAuditLogger"]
