"""Global dependency injection container for Liminal core components."""
from __future__ import annotations

from dependency_injector import containers, providers
import structlog

from .observability.workflow import WorkflowAuditLogger
from .reality_web import RealityWebInMemory, SystemBreath


class LiminalContainer(containers.DeclarativeContainer):
    """Declare providers for in-memory systems and observability."""

    config = providers.Configuration()

    reality_web = providers.Singleton(RealityWebInMemory)
    system_breath = providers.Singleton(SystemBreath)

    _structlog_logger = providers.Factory(structlog.get_logger, "workflow.audit")
    workflow_audit_logger = providers.Singleton(
        WorkflowAuditLogger, logger=_structlog_logger
    )


_container: LiminalContainer | None = None


def get_container() -> LiminalContainer:
    """Return the lazily constructed global container."""
    global _container
    if _container is None:
        _container = LiminalContainer()
    return _container


def set_container(container: LiminalContainer) -> None:
    """Override the default container, e.g. inside tests."""
    global _container
    _container = container
