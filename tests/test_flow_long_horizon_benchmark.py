from benchmarks.experimental.flow_long_horizon import benchmark_matrix, compare


def test_long_horizon_flow_eliminates_blind_continuations() -> None:
    for result in benchmark_matrix():
        assert result.flow_regulated.blind_continuations == 0
        assert result.fixed_loop.blind_continuations > 0
        assert result.routing_only.blind_continuations > 0


def test_routing_reduces_recovery_work_vs_fixed_loop() -> None:
    for result in benchmark_matrix():
        assert result.routing_only.recovery_work < result.fixed_loop.recovery_work


def test_flow_regulation_trades_monitoring_overhead_for_control() -> None:
    for result in benchmark_matrix():
        assert result.flow_regulated.monitoring_work == result.steps
        assert result.flow_regulated.recovery_events > 0


def test_break_even_is_reported_without_forcing_a_win() -> None:
    # The benchmark is allowed to show that full per-step monitoring costs more
    # than routing-only. The invariant is better control, not universal cost win.
    for result in benchmark_matrix():
        assert isinstance(result.flow_vs_routing_delta, int)
        assert isinstance(result.flow_vs_fixed_savings, int)


def test_1000_step_trace_is_deterministic() -> None:
    first = compare(1000)
    second = compare(1000)
    assert first == second
