from benchmarks.experimental.flow_recovery_three_way import run_demo


def test_recovery_routing_reduces_explicit_recovery_work() -> None:
    result = run_demo()

    assert result.fixed_loop.work_units == 28
    assert result.recovery_routing.work_units == 13
    assert result.fixed_loop.sequential_replays == 2
    assert result.recovery_routing.field_reanchors == 2


def test_flow_regulation_stops_blind_continuation_outside_corridor() -> None:
    result = run_demo()

    assert result.fixed_loop.continuations == 8
    assert result.fixed_loop.unnecessary_continuations == 4
    assert result.recovery_routing.unnecessary_continuations == 4

    assert result.flow_regulated.continuations == 4
    assert result.flow_regulated.unnecessary_continuations == 0
    assert result.flow_regulated.flow_preserved_steps == 4


def test_flow_regulated_path_routes_recoverable_breaks_through_field() -> None:
    result = run_demo()

    assert result.flow_regulated.recovery_events == 2
    assert result.flow_regulated.field_reanchors == 2
    assert result.flow_regulated.sequential_replays == 0
    assert result.flow_regulated.deferrals == 0


def test_flow_regulation_does_not_claim_universal_cost_superiority() -> None:
    result = run_demo()

    # Recovery-only routing is cheaper on this particular scripted trace because
    # flow regulation pays explicit inspection/adjustment overhead. The value of
    # the regulator here is fewer blind continuations, not a fabricated claim
    # that it wins every cost metric.
    assert result.recovery_routing.work_units == 13
    assert result.flow_regulated.work_units == 15
    assert result.flow_regulated.work_units < result.fixed_loop.work_units
