from liminal.flow_regulator import FlowState, regulate_flow
from liminal.recovery_policy import RecoveryMode, choose_recovery_mode
from liminal.telemetry_bridge import (
    RuntimeTelemetry,
    to_flow_signals,
    to_recovery_signals,
)


def _healthy() -> RuntimeTelemetry:
    return RuntimeTelemetry(
        token_utilization=0.35,
        retry_rate=0.05,
        tool_failure_rate=0.02,
        latency_pressure=0.20,
        context_pressure=0.20,
        goal_drift=0.05,
        causal_drift=0.05,
        verified_progress_rate=0.75,
        feedback_success_rate=0.95,
        interruption_rate=0.05,
        recent_recovery_rate=0.05,
        task_difficulty=0.70,
        available_capability=0.72,
        replay_steps_estimate=12,
        field_candidate_count=2,
        best_anchor_score=0.82,
        field_uncertainty=0.12,
        verified_candidate_available=True,
        require_verified=True,
        measured_field_cost=2,
    )


def test_healthy_telemetry_maps_into_flow_corridor() -> None:
    signals = to_flow_signals(_healthy())
    decision = regulate_flow(signals)
    assert decision.state is FlowState.FLOW


def test_healthy_deep_recovery_prefers_field() -> None:
    signals = to_recovery_signals(_healthy())
    decision = choose_recovery_mode(signals)
    assert decision.mode is RecoveryMode.FOCUS_FIELD


def test_goal_drift_triggers_recovery() -> None:
    telemetry = RuntimeTelemetry(**{**_healthy().__dict__, "goal_drift": 0.55})
    decision = regulate_flow(to_flow_signals(telemetry))
    assert decision.state is FlowState.RECOVERY
    assert decision.reason == "goal_clarity_below_flow_corridor"


def test_compute_pressure_can_trigger_overload() -> None:
    telemetry = RuntimeTelemetry(
        **{
            **_healthy().__dict__,
            "token_utilization": 1.0,
            "latency_pressure": 1.0,
            "context_pressure": 1.0,
        }
    )
    decision = regulate_flow(to_flow_signals(telemetry))
    assert decision.state is FlowState.OVERLOADED
    assert decision.reason == "compute_pressure_above_flow_corridor"


def test_missing_verified_candidate_defers_when_required() -> None:
    telemetry = RuntimeTelemetry(
        **{
            **_healthy().__dict__,
            "verified_candidate_available": False,
        }
    )
    decision = choose_recovery_mode(to_recovery_signals(telemetry))
    assert decision.mode is RecoveryMode.DEFER
    assert decision.reason == "verified_anchor_required"


def test_field_reliability_metrics_reach_recovery_router() -> None:
    telemetry = RuntimeTelemetry(
        **{
            **_healthy().__dict__,
            "field_verification_success_rate": 1 / 3,
            "field_completion_pressure": 1 / 3,
            "field_observation_count": 3,
        }
    )

    signals = to_recovery_signals(telemetry)
    assert signals.field_verification_success_rate == 1 / 3
    assert signals.field_completion_pressure == 1 / 3
    assert signals.field_observation_count == 3

    decision = choose_recovery_mode(signals)
    assert decision.mode is RecoveryMode.SEQUENTIAL
    assert decision.reason == "field_observed_verification_rate_too_low"


def test_under_sampled_field_reliability_is_not_treated_as_health_signal() -> None:
    telemetry = RuntimeTelemetry(
        **{
            **_healthy().__dict__,
            "field_verification_success_rate": 0.0,
            "field_completion_pressure": 1.0,
            "field_observation_count": 2,
        }
    )

    decision = choose_recovery_mode(to_recovery_signals(telemetry))
    assert decision.mode is RecoveryMode.FOCUS_FIELD


def test_bridge_rejects_invalid_unit_metric() -> None:
    telemetry = RuntimeTelemetry(**{**_healthy().__dict__, "retry_rate": 1.2})
    try:
        to_flow_signals(telemetry)
    except ValueError as exc:
        assert str(exc) == "retry_rate_must_be_between_0_and_1"
    else:
        raise AssertionError("expected ValueError")


def test_bridge_rejects_invalid_optional_field_metric() -> None:
    telemetry = RuntimeTelemetry(
        **{
            **_healthy().__dict__,
            "field_completion_pressure": 1.2,
            "field_observation_count": 3,
        }
    )
    try:
        to_recovery_signals(telemetry)
    except ValueError as exc:
        assert str(exc) == "field_completion_pressure_must_be_between_0_and_1"
    else:
        raise AssertionError("expected ValueError")
