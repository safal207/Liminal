"""Synthetic break-even benchmark for adaptive flow monitoring.

Compares monitoring every step with risk-adaptive cadence over deterministic
traces. This benchmark measures inspection work only; it does not claim real
model-token or wall-clock savings.
"""

from __future__ import annotations

from dataclasses import dataclass

from liminal.adaptive_flow_monitor import (
    MonitoringSignals,
    choose_monitoring_cadence,
)


@dataclass(frozen=True)
class MonitoringResult:
    steps: int
    fixed_inspections: int
    adaptive_inspections: int
    high_risk_steps: int
    high_risk_inspected: int

    @property
    def inspections_saved(self) -> int:
        return self.fixed_inspections - self.adaptive_inspections

    @property
    def savings_ratio(self) -> float:
        if self.fixed_inspections == 0:
            return 0.0
        return round(self.inspections_saved / self.fixed_inspections, 6)

    @property
    def high_risk_coverage(self) -> float:
        if self.high_risk_steps == 0:
            return 1.0
        return round(self.high_risk_inspected / self.high_risk_steps, 6)


def signal_for_step(step: int) -> MonitoringSignals:
    # Stable windows dominate, with deterministic local degradation bursts.
    phase = step % 50
    if 20 <= phase <= 23:  # interruption/recovery burst
        return MonitoringSignals(
            flow_score=0.42,
            goal_clarity=0.62,
            feedback_quality=0.58,
            progress_rate=0.34,
            interruption_pressure=0.72,
            recovery_load=0.66,
            compute_pressure=0.45,
            recent_mode_switches=2,
        )
    if 38 <= phase <= 41:  # progress degradation
        return MonitoringSignals(
            flow_score=0.52,
            goal_clarity=0.66,
            feedback_quality=0.60,
            progress_rate=0.32,
            interruption_pressure=0.30,
            recovery_load=0.22,
            compute_pressure=0.35,
        )
    return MonitoringSignals(
        flow_score=0.84,
        goal_clarity=0.90,
        feedback_quality=0.86,
        progress_rate=0.76,
        interruption_pressure=0.08,
        recovery_load=0.06,
        compute_pressure=0.18,
        recent_mode_switches=0,
    )


def is_high_risk(signals: MonitoringSignals) -> bool:
    return (
        signals.flow_score <= 0.50
        or signals.interruption_pressure >= 0.60
        or signals.recovery_load >= 0.60
        or signals.progress_rate <= 0.35
    )


def run(steps: int) -> MonitoringResult:
    if steps <= 0:
        raise ValueError("steps_must_be_positive")

    adaptive_inspections = 0
    high_risk_steps = 0
    high_risk_inspected = 0
    next_inspection = 1

    for step in range(1, steps + 1):
        signals = signal_for_step(step)
        risk = is_high_risk(signals)
        if risk:
            high_risk_steps += 1

        # Escalation signals are assumed available from cheap runtime counters;
        # they can pull the next full inspection forward.
        urgent = (
            signals.interruption_pressure >= 0.60
            or signals.recovery_load >= 0.60
            or signals.progress_rate <= 0.35
        )
        should_inspect = step >= next_inspection or urgent

        if should_inspect:
            adaptive_inspections += 1
            if risk:
                high_risk_inspected += 1
            decision = choose_monitoring_cadence(signals)
            next_inspection = step + decision.inspect_every_steps

    return MonitoringResult(
        steps=steps,
        fixed_inspections=steps,
        adaptive_inspections=adaptive_inspections,
        high_risk_steps=high_risk_steps,
        high_risk_inspected=high_risk_inspected,
    )


if __name__ == "__main__":
    for size in (100, 500, 1000):
        result = run(size)
        print(
            size,
            f"fixed={result.fixed_inspections}",
            f"adaptive={result.adaptive_inspections}",
            f"saved={result.inspections_saved}",
            f"savings_ratio={result.savings_ratio}",
            f"high_risk_coverage={result.high_risk_coverage}",
        )
