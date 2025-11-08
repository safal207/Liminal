"""Dependency injection container for backend services."""
from __future__ import annotations

from dependency_injector import containers, providers

from .services.auth import AuthService
from .services.memory import MemoryTimelineService
from .services.ml import MLService
from .services.neo4j import Neo4jService
from .services.websocket import ConnectionManagerService, TimelineWebSocketService


class AppContainer(containers.DeclarativeContainer):
    """Declare backend service providers."""

    config = providers.Configuration()

    neo4j_service = providers.Singleton(Neo4jService)
    memory_timeline_service = providers.Singleton(MemoryTimelineService)
    ml_service = providers.Singleton(MLService)
    connection_manager_service = providers.Singleton(ConnectionManagerService)
    auth_service = providers.Singleton(AuthService)
    websocket_service = providers.Singleton(TimelineWebSocketService)


_container: AppContainer | None = None


def get_container() -> AppContainer:
    """Return the global backend container instance."""
    global _container
    if _container is None:
        _container = AppContainer()
    return _container


def set_container(container: AppContainer) -> None:
    """Override the global backend container (used by tests)."""
    global _container
    _container = container
