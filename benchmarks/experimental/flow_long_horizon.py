"""Deterministic long-horizon benchmark for flow-regulated agent execution.

The benchmark is intentionally synthetic. It compares three runtime strategies
across 100, 500, and 1000 step traces with deterministic interruption,
overload, goal-loss, and context-loss events.

It does not model human happiness or fatigue. It measures operational proxies:
blind continuations outside the flow corridor, recovery work, regulator
inspection overhead, and total abstract work units.
"""

from __future__ import annotations

from dataclasses import dataclass

from liminal.flow_regulator import FlowSignals, FlowState, regulate_flow
from liminal.recovery_policy import RecoveryMode, RecoverySignals, choose_recovery_mode


@dataclass(frozen=True)
class BenchmarkResult:
    steps: int
    strategy: str
    base_work: int
    monitoring_work: int
    recovery_work: int
    blind_continuations: int
    recovery_events: int
    field_recoveries: int
    sequential_recoveries: int

    @property
    def total_work(self) -> int:
        return self.base_work + self.monitoring_work + self.recovery_work


@dataclass(frozen=True)
class BreakEvenResult:
    steps: int
    fixed_loop: BenchmarkResult
    routing_only: BenchmarkResult
    flow_regulated: BenchmarkResult

    @property
    def flow_vs_fixed_savings(self) -> int:
        return self.fixed_loop.total_work - self.flow_regulated.total_work

    @property
    def flow_vs_routing_delta(self) -> int:
        return self.routing_only.total_work - self.flow_regulated.total_work


def _event_kind(step: int) -> str | None:
    # Deterministic, overlapping-free event schedule.
    if step % 47 == 0:
        return "context_loss"
    if step % 31 == 0:
        return "goal_loss"
    if step % 23 == 0:
        return "overload"
    if step % 19 == 0:
        return "interruption"
    return None


def _flow_signals(kind: str | None) -> FlowSignals:
    if kind == "context_loss":
        return FlowSignals(0.72, 0.70, 0.52, 0.50, 0.24, 0.35, 0.78, 0.42)
    if kind == "goal_loss":
        return FlowSignals(0.68, 0.70, 0.42, 0.70, 0.46, 0.18, 0.22, 0.30)
    if kind == "overload":
        return FlowSignals(0.92, 0.62, 0.82, 0.72, 0.40, 0.30, 0.20, 0.62)
    if kind == "interruption":
        return FlowSignals(0.70, 0.72, 0.80, 0.70, 0.48, 0.72, 0.30, 0.35)
    return FlowSignals(0.70, 0.72, 0.86, 0.82, 0.62, 0.12, 0.10, 0.28)


def _recovery_signals(kind: str) -> RecoverySignals:
    if kind == "context_loss":
        return RecoverySignals(
            replay_steps=17,
            candidate_count=3,
            best_anchor_score=0.78,
            uncertainty=0.16,
            verified_candidate_available=True,
            require_verified=True,
            field_scan_cost=3,
        )
    if kind == "goal_loss":
        return RecoverySignals(
            replay_steps=8,
            candidate_count=2,
            best_anchor_score=0.72,
            uncertainty=0.18,
            field_scan_cost=2,
        )
    if kind == "interruption":
        return RecoverySignals(
            replay_steps=5,
            candidate_count=2,
            best_anchor_score=0.60,
            uncertainty=0.28,
            field_scan_cost=2,
        )
    return RecoverySignals(
        replay_steps=4,
        candidate_count=2,
        best_anchor_score=0.58,
        uncertainty=0.25,
        field_scan_cost=2,
    )


def _recovery_cost(kind: str, use_router: bool) -> tuple[int, RecoveryMode]:
    signals = _recovery_signals(kind)
    if not use_router:
        return signals.replay_steps, RecoveryMode.SEQUENTIAL
    decision = choose_recovery_mode(signals)
    if decision.mode == RecoveryMode.FOCUS_FIELD:
        return decision.estimated_field_cost, decision.mode
    if decision.mode == RecoveryMode.SEQUENTIAL:
        return signals.replay_steps, decision.mode
    # DEFER consumes one inspection unit here; no action is forced.
    return 1, decision.mode


def simulate(steps: int, strategy: str) -> BenchmarkResult:
    if steps <= 0:
        raise ValueError("steps_must_be_positive")
    if strategy not in {"fixed_loop", "routing_only", "flow_regulated"}:
        raise ValueError("unknown_strategy")

    base_work = steps
    monitoring_work = 0
    recovery_work = 0
    blind = 0
    recovery_events = 0
    field = 0
    sequential = 0

    for step in range(1, steps + 1):
        kind = _event_kind(step)
        if kind is None:
            if strategy == "flow_regulated":
                monitoring_work += 1
            continue

        if strategy == "fixed_loop":
            blind += 1
            recovery_events += 1
            cost, mode = _recovery_cost(kind, use_router=False)
            recovery_work += cost
            sequential += int(mode == RecoveryMode.SEQUENTIAL)
            continue

        if strategy == "routing_only":
            blind += 1
            recovery_events += 1
            cost, mode = _recovery_cost(kind, use_router=True)
            recovery_work += cost
            field += int(mode == RecoveryMode.FOCUS_FIELD)
            sequential += int(mode == RecoveryMode.SEQUENTIAL)
            continue

        monitoring_work += 1
        decision = regulate_flow(_flow_signals(kind))
        if decision.state == FlowState.FLOW:
            continue

        # Regulator prevents one blind continuation by detecting the broken
        # corridor before another task transition is attempted.
        if decision.state == FlowState.UNDERLOADED:
            continue

        recovery_events += 1
        if decision.state in {FlowState.RECOVERY, FlowState.OVERLOADED}:
            cost, mode = _recovery_cost(kind, use_router=True)
            recovery_work += cost
            field += int(mode == RecoveryMode.FOCUS_FIELD)
            sequential += int(mode == RecoveryMode.SEQUENTIAL)

    return BenchmarkResult(
        steps=steps,
        strategy=strategy,
        base_work=base_work,
        monitoring_work=monitoring_work,
        recovery_work=recovery_work,
        blind_continuations=blind,
        recovery_events=recovery_events,
        field_recoveries=field,
        sequential_recoveries=sequential,
    )


def compare(steps: int) -> BreakEvenResult:
    return BreakEvenResult(
        steps=steps,
        fixed_loop=simulate(steps, "fixed_loop"),
        routing_only=simulate(steps, "routing_only"),
        flow_regulated=simulate(steps, "flow_regulated"),
    )


def benchmark_matrix() -> tuple[BreakEvenResult, ...]:
    return tuple(compare(steps) for steps in (100, 500, 1000))


if __name__ == "__main__":
    for result in benchmark_matrix():
        print(
            result.steps,
            f"fixed={result.fixed_loop.total_work}",
            f"routing={result.routing_only.total_work}",
            f"flow={result.flow_regulated.total_work}",
            f"flow_blind={result.flow_regulated.blind_continuations}",
            f"fixed_blind={result.fixed_loop.blind_continuations}",
        )
