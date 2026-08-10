from liminal.adaptive_flow_monitor import (
    MonitoringLevel,
    MonitoringSignals,
    choose_monitoring_cadence,
)


def test_stable_flow_uses_sparse_monitoring() -> None:
    decision = choose_monitoring_cadence(
        MonitoringSignals(
            flow_score=0.82,
            goal_clarity=0.90,
            feedback_quality=0.85,
            progress_rate=0.75,
            interruption_pressure=0.10,
            recovery_load=0.05,
            compute_pressure=0.20,
        )
    )
    assert decision.level is MonitoringLevel.SPARSE
    assert decision.inspect_every_steps == 8


def test_mixed_state_uses_normal_monitoring() -> None:
    decision = choose_monitoring_cadence(
        MonitoringSignals(
            flow_score=0.64,
            goal_clarity=0.72,
            feedback_quality=0.68,
            progress_rate=0.58,
            interruption_pressure=0.30,
            recovery_load=0.20,
            compute_pressure=0.30,
        )
    )
    assert decision.level is MonitoringLevel.NORMAL
    assert decision.inspect_every_steps == 4


def test_degraded_progress_uses_dense_monitoring() -> None:
    decision = choose_monitoring_cadence(
        MonitoringSignals(
            flow_score=0.55,
            goal_clarity=0.70,
            feedback_quality=0.65,
            progress_rate=0.30,
        )
    )
    assert decision.level is MonitoringLevel.DENSE
    assert decision.inspect_every_steps == 2


def test_high_recovery_load_uses_continuous_monitoring() -> None:
    decision = choose_monitoring_cadence(
        MonitoringSignals(
            flow_score=0.48,
            goal_clarity=0.60,
            feedback_quality=0.60,
            progress_rate=0.40,
            recovery_load=0.75,
        )
    )
    assert decision.level is MonitoringLevel.CONTINUOUS
    assert decision.inspect_every_steps == 1


def test_mode_switch_instability_forces_continuous_monitoring() -> None:
    decision = choose_monitoring_cadence(
        MonitoringSignals(
            flow_score=0.72,
            goal_clarity=0.80,
            feedback_quality=0.80,
            progress_rate=0.65,
            recent_mode_switches=3,
        )
    )
    assert decision.level is MonitoringLevel.CONTINUOUS


def test_invalid_signal_is_rejected() -> None:
    try:
        choose_monitoring_cadence(
            MonitoringSignals(
                flow_score=1.2,
                goal_clarity=0.8,
                feedback_quality=0.8,
                progress_rate=0.8,
            )
        )
    except ValueError as exc:
        assert str(exc) == "flow_score_must_be_between_0_and_1"
    else:
        raise AssertionError("expected ValueError")
