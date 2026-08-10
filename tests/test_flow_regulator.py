from liminal.flow_regulator import FlowSignals, FlowState, regulate_flow


def test_balanced_work_enters_flow_corridor() -> None:
    decision = regulate_flow(
        FlowSignals(
            challenge=0.72,
            capability=0.75,
            goal_clarity=0.90,
            feedback_quality=0.85,
            progress_rate=0.70,
            interruption_pressure=0.10,
            recovery_load=0.10,
            compute_pressure=0.30,
        )
    )

    assert decision.state is FlowState.FLOW
    assert decision.flow_score > 0.65
    assert decision.adjustment == "continue_without_unnecessary_mode_switch"


def test_overchallenge_decomposes_task() -> None:
    decision = regulate_flow(
        FlowSignals(
            challenge=0.90,
            capability=0.55,
            goal_clarity=0.90,
            feedback_quality=0.80,
            progress_rate=0.45,
        )
    )

    assert decision.state is FlowState.OVERLOADED
    assert decision.reason == "challenge_exceeds_available_capability"


def test_underchallenge_increases_granularity() -> None:
    decision = regulate_flow(
        FlowSignals(
            challenge=0.35,
            capability=0.80,
            goal_clarity=0.90,
            feedback_quality=0.80,
            progress_rate=0.80,
        )
    )

    assert decision.state is FlowState.UNDERLOADED
    assert decision.adjustment == "increase_task_granularity_or_batch_safe_work"


def test_goal_loss_returns_to_value_anchor() -> None:
    decision = regulate_flow(
        FlowSignals(
            challenge=0.65,
            capability=0.65,
            goal_clarity=0.30,
            feedback_quality=0.80,
            progress_rate=0.50,
        )
    )

    assert decision.state is FlowState.RECOVERY
    assert decision.adjustment == "restore_value_and_goal_anchor"


def test_stalled_progress_uses_observe_field_path() -> None:
    decision = regulate_flow(
        FlowSignals(
            challenge=0.70,
            capability=0.72,
            goal_clarity=0.90,
            feedback_quality=0.85,
            progress_rate=0.10,
        )
    )

    assert decision.state is FlowState.RECOVERY
    assert decision.adjustment == "observe_field_and_select_new_continuation"


def test_high_recovery_load_routes_recovery_before_more_work() -> None:
    decision = regulate_flow(
        FlowSignals(
            challenge=0.70,
            capability=0.70,
            goal_clarity=0.90,
            feedback_quality=0.85,
            progress_rate=0.60,
            recovery_load=0.80,
        )
    )

    assert decision.state is FlowState.RECOVERY
    assert decision.adjustment == "observe_then_route_recovery"


def test_high_compute_pressure_breaks_flow_corridor() -> None:
    decision = regulate_flow(
        FlowSignals(
            challenge=0.70,
            capability=0.72,
            goal_clarity=0.90,
            feedback_quality=0.85,
            progress_rate=0.60,
            compute_pressure=0.90,
        )
    )

    assert decision.state is FlowState.OVERLOADED
    assert decision.adjustment == "reduce_context_or_split_task"
