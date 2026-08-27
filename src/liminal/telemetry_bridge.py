"""Experimental bridge from observable agent telemetry to flow/recovery signals.

The bridge maps runtime counters and ratios into the normalized, model-agnostic
inputs consumed by the Flow Regulator and Recovery Router. The mapping is
explicit, deterministic, and intentionally conservative: it does not infer
human emotions or hidden cognitive states.
"""

from __future__ import annotations

from dataclasses import dataclass, replace

from liminal.flow_regulator import FlowSignals
from liminal.recovery_evidence import FieldReliabilityEvidence
from liminal.recovery_policy import RecoverySignals


def _clamp01(value: float) -> float:
    return max(0.0, min(1.0, value))


@dataclass(frozen=True)
class RuntimeTelemetry:
    """Observable metrics collected over one agent-control window."""

    token_utilization: float
    retry_rate: float
    tool_failure_rate: float
    latency_pressure: float
    context_pressure: float
    goal_drift: float
    causal_drift: float
    verified_progress_rate: float
    feedback_success_rate: float
    interruption_rate: float
    recent_recovery_rate: float
    task_difficulty: float
    available_capability: float
    replay_steps_estimate: int
    field_candidate_count: int
    best_anchor_score: float
    field_uncertainty: float
    verified_candidate_available: bool = True
    require_verified: bool = False
    measured_field_cost: int | None = None
    field_verification_success_rate: float | None = None
    field_completion_pressure: float | None = None
    field_observation_count: int = 0


def _validate_unit_fields(telemetry: RuntimeTelemetry) -> None:
    unit_fields = (
        "token_utilization",
        "retry_rate",
        "tool_failure_rate",
        "latency_pressure",
        "context_pressure",
        "goal_drift",
        "causal_drift",
        "verified_progress_rate",
        "feedback_success_rate",
        "interruption_rate",
        "recent_recovery_rate",
        "task_difficulty",
        "available_capability",
        "best_anchor_score",
        "field_uncertainty",
    )
    for field in unit_fields:
        value = getattr(telemetry, field)
        if not 0.0 <= value <= 1.0:
            raise ValueError(f"{field}_must_be_between_0_and_1")
    optional_unit_fields = (
        "field_verification_success_rate",
        "field_completion_pressure",
    )
    for field in optional_unit_fields:
        value = getattr(telemetry, field)
        if value is not None and not 0.0 <= value <= 1.0:
            raise ValueError(f"{field}_must_be_between_0_and_1")
    if telemetry.replay_steps_estimate < 0:
        raise ValueError("replay_steps_estimate_must_be_non_negative")
    if telemetry.field_candidate_count < 0:
        raise ValueError("field_candidate_count_must_be_non_negative")
    if telemetry.measured_field_cost is not None and telemetry.measured_field_cost < 0:
        raise ValueError("measured_field_cost_must_be_non_negative")
    if telemetry.field_observation_count < 0:
        raise ValueError("field_observation_count_must_be_non_negative")


def with_field_reliability(
    telemetry: RuntimeTelemetry,
    evidence: FieldReliabilityEvidence,
) -> RuntimeTelemetry:
    """Return telemetry enriched with explicitly aggregated field evidence."""

    return replace(
        telemetry,
        field_verification_success_rate=evidence.verification_success_rate,
        field_completion_pressure=evidence.completion_pressure,
        field_observation_count=evidence.observation_count,
    )


def to_flow_signals(telemetry: RuntimeTelemetry) -> FlowSignals:
    """Map observable telemetry to the Flow Regulator's normalized signals."""

    _validate_unit_fields(telemetry)

    goal_clarity = _clamp01(1.0 - telemetry.goal_drift)
    feedback_quality = _clamp01(
        telemetry.feedback_success_rate * (1.0 - telemetry.tool_failure_rate)
    )
    interruption_pressure = _clamp01(
        0.65 * telemetry.interruption_rate + 0.35 * telemetry.retry_rate
    )
    recovery_load = _clamp01(
        0.55 * telemetry.recent_recovery_rate
        + 0.25 * telemetry.context_pressure
        + 0.20 * telemetry.causal_drift
    )
    compute_pressure = _clamp01(
        0.45 * telemetry.token_utilization
        + 0.30 * telemetry.latency_pressure
        + 0.25 * telemetry.context_pressure
    )

    return FlowSignals(
        challenge=telemetry.task_difficulty,
        capability=telemetry.available_capability,
        goal_clarity=goal_clarity,
        feedback_quality=feedback_quality,
        progress_rate=telemetry.verified_progress_rate,
        interruption_pressure=interruption_pressure,
        recovery_load=recovery_load,
        compute_pressure=compute_pressure,
    )


def to_recovery_signals(telemetry: RuntimeTelemetry) -> RecoverySignals:
    """Map observable telemetry to the Recovery Router's compact inputs."""

    _validate_unit_fields(telemetry)
    return RecoverySignals(
        replay_steps=telemetry.replay_steps_estimate,
        candidate_count=telemetry.field_candidate_count,
        best_anchor_score=telemetry.best_anchor_score,
        uncertainty=telemetry.field_uncertainty,
        verified_candidate_available=telemetry.verified_candidate_available,
        require_verified=telemetry.require_verified,
        field_scan_cost=telemetry.measured_field_cost,
        field_verification_success_rate=telemetry.field_verification_success_rate,
        field_completion_pressure=telemetry.field_completion_pressure,
        field_observation_count=telemetry.field_observation_count,
    )
