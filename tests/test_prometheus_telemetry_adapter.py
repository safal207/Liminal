import pytest

from liminal.prometheus_telemetry_adapter import (
    PrometheusSnapshot,
    from_prometheus_snapshot,
)


def test_maps_existing_prometheus_metrics_without_inventing_hidden_signals() -> None:
    telemetry = from_prometheus_snapshot(
        PrometheusSnapshot(
            pipeline_runs_total=20,
            pipeline_failures_total=2,
            run_duration_seconds=3.0,
            duration_budget_seconds=10.0,
            queue_depth=2,
            queue_depth_budget=10,
            seconds_since_last_success=30.0,
            stale_success_budget_seconds=120.0,
        )
    )

    assert telemetry.tool_failure_rate == 0.1
    assert telemetry.latency_pressure == 0.3
    assert telemetry.queue_pressure == 0.2
    assert telemetry.freshness_pressure == 0.25


def test_pressures_are_clamped_at_one() -> None:
    telemetry = from_prometheus_snapshot(
        PrometheusSnapshot(
            pipeline_runs_total=1,
            pipeline_failures_total=1,
            run_duration_seconds=30.0,
            duration_budget_seconds=10.0,
            queue_depth=50,
            queue_depth_budget=10,
            seconds_since_last_success=900.0,
            stale_success_budget_seconds=120.0,
        )
    )

    assert telemetry.tool_failure_rate == 1.0
    assert telemetry.latency_pressure == 1.0
    assert telemetry.queue_pressure == 1.0
    assert telemetry.freshness_pressure == 1.0


def test_rejects_impossible_failure_count() -> None:
    with pytest.raises(ValueError, match="pipeline_failures_cannot_exceed_runs"):
        from_prometheus_snapshot(
            PrometheusSnapshot(
                pipeline_runs_total=2,
                pipeline_failures_total=3,
                run_duration_seconds=1.0,
                duration_budget_seconds=10.0,
                queue_depth=0,
                queue_depth_budget=10,
                seconds_since_last_success=0.0,
                stale_success_budget_seconds=120.0,
            )
        )


def test_rejects_zero_budget() -> None:
    with pytest.raises(ValueError, match="budget_must_be_positive"):
        from_prometheus_snapshot(
            PrometheusSnapshot(
                pipeline_runs_total=1,
                pipeline_failures_total=0,
                run_duration_seconds=1.0,
                duration_budget_seconds=0.0,
                queue_depth=0,
                queue_depth_budget=10,
                seconds_since_last_success=0.0,
                stale_success_budget_seconds=120.0,
            )
        )
