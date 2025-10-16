"""Shared Prometheus registry utilities.

The refactored backend exposes a dedicated :class:`CollectorRegistry` instead
of relying on the global default registry.  This avoids duplicate metric
registration during test execution (where multiple modules may define metrics
with overlapping names) while still giving the application a single place to
export Prometheus data from.
"""

from __future__ import annotations

from typing import Iterable, TypeVar

from prometheus_client import CollectorRegistry
from prometheus_client.metrics import MetricWrapperBase

MetricT = TypeVar("MetricT", bound=MetricWrapperBase)


# A dedicated registry keeps our application metrics isolated from any custom
# collectors that may be instantiated inside tests or auxiliary scripts.  This
# prevents ``ValueError: Duplicated timeseries`` errors when a test defines its
# own Gauge/Counter with the same name as the application's collector.
REGISTRY = CollectorRegistry(auto_describe=True)


def _iter_collectors(name: str) -> Iterable[MetricWrapperBase]:
    """Yield collectors registered under the provided ``name``."""

    # ``CollectorRegistry`` keeps a private mapping of metric names to the
    # collectors that own them.  Accessing the private attribute is acceptable
    # here because prometheus_client does not expose a public lookup API.
    collectors = REGISTRY._names_to_collectors.get(name, ())  # type: ignore[attr-defined]
    return tuple(collectors)


def get_or_create_metric(
    metric_cls: type[MetricT],
    name: str,
    documentation: str,
    *args,
    **kwargs,
) -> MetricT:
    """Return an existing collector or create a new one within ``REGISTRY``."""

    existing = next(iter(_iter_collectors(name)), None)
    if existing is not None:
        return existing  # type: ignore[return-value]

    kwargs.setdefault("registry", REGISTRY)
    return metric_cls(name, documentation, *args, **kwargs)


__all__ = ["REGISTRY", "get_or_create_metric"]
