"""Dependency injection container for backend services."""
from __future__ import annotations

try:  # pragma: no cover - exercised indirectly during import
    from dependency_injector import containers, providers
except ModuleNotFoundError:  # pragma: no cover - optional dependency in CI
    class _DeclarativeContainer:
        """Lightweight stand-in for dependency_injector base class."""

        def __call__(self, *args, **kwargs):
            return self

    class _SingletonProvider:
        """Simplistic singleton provider used when dependency_injector is missing."""

        def __init__(self, factory, *args, **kwargs):
            self._factory = factory
            self._args = args
            self._kwargs = kwargs
            self._instance = None

        def __call__(self):
            if self._instance is None:
                self._instance = self._factory(*self._args, **self._kwargs)
            return self._instance

    class _Configuration(dict):
        """Minimal configuration provider stub."""

        def __call__(self, **kwargs):
            self.update(kwargs)
            return self

    class _ContainersModule:
        DeclarativeContainer = _DeclarativeContainer

    class _ProvidersModule:
        Configuration = _Configuration
        Singleton = _SingletonProvider

    containers = _ContainersModule()  # type: ignore[assignment]
    providers = _ProvidersModule()  # type: ignore[assignment]

from .services.memory import MemoryTimelineService
from .services.ml import MLService
from .services.neo4j import Neo4jService
from .services.websocket import ConnectionManagerService


class AppContainer(containers.DeclarativeContainer):
    """Declare backend service providers."""

    config = providers.Configuration()

    neo4j_service = providers.Singleton(Neo4jService)
    memory_timeline_service = providers.Singleton(MemoryTimelineService)
    ml_service = providers.Singleton(MLService)
    connection_manager_service = providers.Singleton(ConnectionManagerService)


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
