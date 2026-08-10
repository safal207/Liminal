from benchmarks.experimental.adaptive_flow_monitoring import run


def test_adaptive_monitoring_reduces_inspections_on_long_traces() -> None:
    for steps in (100, 500, 1000):
        result = run(steps)
        assert result.adaptive_inspections < result.fixed_inspections
        assert result.savings_ratio > 0.40


def test_adaptive_monitoring_preserves_high_risk_coverage() -> None:
    for steps in (100, 500, 1000):
        result = run(steps)
        assert result.high_risk_steps > 0
        assert result.high_risk_coverage == 1.0


def test_monitoring_savings_scale_with_trace_length() -> None:
    small = run(100)
    large = run(1000)

    assert large.inspections_saved > small.inspections_saved
    assert large.savings_ratio >= small.savings_ratio - 0.05
