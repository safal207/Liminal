"""Deterministic three-way benchmark for sustained agent control.

This is a synthetic contract benchmark, not evidence that flow regulation improves
real LLM performance. It compares control policies on the same scripted trace:

1. fixed loop: always continue until an explicit interruption forces replay;
2. recovery routing: continue normally, but route explicit recovery efficiently;
3. flow-regulated routing: inspect the operating corridor before continuing and
   route recovery when the corridor breaks.

The benchmark measures abstract work units so CI can test invariants without
network calls, model APIs, or wall-clock noise.
"""

from __future__ import annotations

from dataclasses import dataclass

from liminal.flow_regulator import FlowSignals, FlowState, regulate_flow
from liminal.recovery_policy import RecoveryMode, RecoverySignals, choose_recovery_mode


@dataclass(frozen=True)
class TraceStep:
    step_id: str
    flow: FlowSignals
    recovery: RecoverySignals | None = None


@dataclass(frozen=True)
class StrategyMetrics:
    strategy: str
    work_units: int
    continuations: int
    unnecessary_continuations: int
    recovery_events: int
    sequential_replays: int
    field_reanchors: int
    deferrals: int
    flow_preserved_steps: int


@dataclass(frozen=True)
class BenchmarkResult:
    fixed_loop: StrategyMetrics
    recovery_routing: StrategyMetrics
    flow_regulated: StrategyMetrics


def demo_trace() -> tuple[TraceStep, ...]:
    """Scripted long-horizon trace with healthy work and recoverable drift."""

    return (
        TraceStep(
            "warm_start",
            FlowSignals(0.55, 0.60, 0.90, 0.85, 0.70, 0.10, 0.05, 0.20),
        ),
        TraceStep(
            "deep_work",
            FlowSignals(0.72, 0.70, 0.92, 0.88, 0.78, 0.08, 0.05, 0.30),
        ),
        TraceStep(
            "challenge_spike",
            FlowSignals(0.95, 0.62, 0.90, 0.82, 0.42, 0.12, 0.10, 0.45),
        ),
        TraceStep(
            "goal_drift",
            FlowSignals(0.68, 0.70, 0.42, 0.80, 0.40, 0.15, 0.18, 0.35),
            RecoverySignals(
                replay_steps=12,
                candidate_count=3,
                best_anchor_score=0.82,
                uncertainty=0.18,
                verified_candidate_available=True,
                require_verified=True,
                field_scan_cost=3,
            ),
        ),
        TraceStep(
            "restored_flow",
            FlowSignals(0.70, 0.72, 0.90, 0.88, 0.74, 0.10, 0.12, 0.30),
        ),
        TraceStep(
            "interruption_burst",
            FlowSignals(0.66, 0.70, 0.86, 0.80, 0.50, 0.72, 0.20, 0.35),
            RecoverySignals(
                replay_steps=8,
                candidate_count=2,
                best_anchor_score=0.74,
                uncertainty=0.22,
                verified_candidate_available=True,
                field_scan_cost=2,
            ),
        ),
        TraceStep(
            "compute_pressure",
            FlowSignals(0.74, 0.72, 0.88, 0.82, 0.52, 0.20, 0.25, 0.90),
        ),
        TraceStep(
            "final_flow",
            FlowSignals(0.62, 0.68, 0.92, 0.90, 0.82, 0.08, 0.08, 0.24),
        ),
    )


def _recovery_cost(signals: RecoverySignals, routed: bool) -> tuple[int, RecoveryMode]:
    if not routed:
        return signals.replay_steps, RecoveryMode.SEQUENTIAL
    decision = choose_recovery_mode(signals)
    if decision.mode is RecoveryMode.FOCUS_FIELD:
        return decision.estimated_field_cost, decision.mode
    if decision.mode is RecoveryMode.SEQUENTIAL:
        return signals.replay_steps, decision.mode
    return 1, decision.mode  # deterministic bookkeeping cost for a safe defer


def _run_fixed_loop(trace: tuple[TraceStep, ...]) -> StrategyMetrics:
    work = 0
    continuations = 0
    unnecessary = 0
    recovery_events = 0
    sequential = 0

    for step in trace:
        decision = regulate_flow(step.flow)
        work += 1
        continuations += 1
        if decision.state is not FlowState.FLOW:
            unnecessary += 1
        if step.recovery is not None:
            recovery_events += 1
            cost, _ = _recovery_cost(step.recovery, routed=False)
            work += cost
            sequential += 1

    return StrategyMetrics(
        "fixed_loop", work, continuations, unnecessary, recovery_events,
        sequential, 0, 0, 0,
    )


def _run_recovery_routing(trace: tuple[TraceStep, ...]) -> StrategyMetrics:
    work = 0
    continuations = 0
    unnecessary = 0
    recovery_events = 0
    sequential = 0
    field = 0
    deferred = 0

    for step in trace:
        decision = regulate_flow(step.flow)
        work += 1
        continuations += 1
        if decision.state is not FlowState.FLOW:
            unnecessary += 1
        if step.recovery is not None:
            recovery_events += 1
            cost, mode = _recovery_cost(step.recovery, routed=True)
            work += cost
            sequential += int(mode is RecoveryMode.SEQUENTIAL)
            field += int(mode is RecoveryMode.FOCUS_FIELD)
            deferred += int(mode is RecoveryMode.DEFER)

    return StrategyMetrics(
        "recovery_routing", work, continuations, unnecessary, recovery_events,
        sequential, field, deferred, 0,
    )


def _run_flow_regulated(trace: tuple[TraceStep, ...]) -> StrategyMetrics:
    work = 0
    continuations = 0
    unnecessary = 0
    recovery_events = 0
    sequential = 0
    field = 0
    deferred = 0
    preserved = 0

    for step in trace:
        decision = regulate_flow(step.flow)
        work += 1  # inspect current corridor

        if decision.state is FlowState.FLOW:
            continuations += 1
            preserved += 1
            continue

        # The regulator does not blindly continue outside the corridor.
        if decision.state in (FlowState.OVERLOADED, FlowState.UNDERLOADED):
            work += 1  # bounded adjustment such as decomposition or batching
            continue

        recovery_events += 1
        if step.recovery is None:
            work += 1  # checkpoint / restore goal or feedback before continuing
            continue

        cost, mode = _recovery_cost(step.recovery, routed=True)
        work += cost
        sequential += int(mode is RecoveryMode.SEQUENTIAL)
        field += int(mode is RecoveryMode.FOCUS_FIELD)
        deferred += int(mode is RecoveryMode.DEFER)

    return StrategyMetrics(
        "flow_regulated", work, continuations, unnecessary, recovery_events,
        sequential, field, deferred, preserved,
    )


def run_demo() -> BenchmarkResult:
    trace = demo_trace()
    return BenchmarkResult(
        fixed_loop=_run_fixed_loop(trace),
        recovery_routing=_run_recovery_routing(trace),
        flow_regulated=_run_flow_regulated(trace),
    )


if __name__ == "__main__":
    result = run_demo()
    for metrics in (result.fixed_loop, result.recovery_routing, result.flow_regulated):
        print(metrics)
