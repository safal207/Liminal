"""Adapter from existing LIMINAL Prometheus observations to runtime telemetry inputs.

This module is intentionally conservative. It maps only metrics that are actually
observable in the existing ML observability surface. Signals that are not yet
measured (for example goal drift or causal drift) remain explicitly unavailable
instead of being fabricated.
"""

from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True)
class PrometheusSnapshot:
    pipeline_runs_total: int
    pipeline_failures_total: int
    run_duration_seconds: float
    duration_budget_seconds: float
    queue_depth: int
    queue_depth_budget: int
    seconds_since_last_success: float
    stale_success_budget_seconds: float


@dataclass(frozen=True)
class ObservableTelemetry:
    tool_failure_rate: float
    latency_pressure: float
    queue_pressure: float
    freshness_pressure: float


def _ratio(value: float, budget: float) -> float:
    if budget <= 0:
        raise ValueError("budget_must_be_positive")
    return max(0.0, min(1.0, value / budget))


def from_prometheus_snapshot(snapshot: PrometheusSnapshot) -> ObservableTelemetry:
    """Normalize existing Prometheus observations into deterministic pressures."""

    if snapshot.pipeline_runs_total < 0:
        raise ValueError("pipeline_runs_total_must_be_non_negative")
    if snapshot.pipeline_failures_total < 0:
        raise ValueError("pipeline_failures_total_must_be_non_negative")
    if snapshot.pipeline_failures_total > snapshot.pipeline_runs_total:
        raise ValueError("pipeline_failures_cannot_exceed_runs")
    if snapshot.queue_depth < 0:
        raise ValueError("queue_depth_must_be_non_negative")
    if snapshot.run_duration_seconds < 0:
        raise ValueError("run_duration_seconds_must_be_non_negative")
    if snapshot.seconds_since_last_success < 0:
        raise ValueError("seconds_since_last_success_must_be_non_negative")

    failure_rate = (
        snapshot.pipeline_failures_total / snapshot.pipeline_runs_total
        if snapshot.pipeline_runs_total
        else 0.0
    )

    return ObservableTelemetry(
        tool_failure_rate=round(failure_rate, 6),
        latency_pressure=round(
            _ratio(snapshot.run_duration_seconds, snapshot.duration_budget_seconds), 6
        ),
        queue_pressure=round(
            _ratio(float(snapshot.queue_depth), float(snapshot.queue_depth_budget)), 6
        ),
        freshness_pressure=round(
            _ratio(
                snapshot.seconds_since_last_success,
                snapshot.stale_success_budget_seconds,
            ),
            6,
        ),
    )
