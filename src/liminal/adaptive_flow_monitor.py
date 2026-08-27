"""Adaptive monitoring cadence for the experimental agent flow regulator.

Stable flow is sampled sparsely. Rising risk is sampled more frequently. The
policy is deterministic and does not grant action authority.
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum


class MonitoringLevel(str, Enum):
    SPARSE = "sparse"
    NORMAL = "normal"
    DENSE = "dense"
    CONTINUOUS = "continuous"


@dataclass(frozen=True)
class MonitoringSignals:
    flow_score: float
    goal_clarity: float
    feedback_quality: float
    progress_rate: float
    interruption_pressure: float = 0.0
    recovery_load: float = 0.0
    compute_pressure: float = 0.0
    recent_mode_switches: int = 0


@dataclass(frozen=True)
class MonitoringPolicy:
    sparse_interval: int = 8
    normal_interval: int = 4
    dense_interval: int = 2
    stable_flow_score: float = 0.70
    risk_flow_score: float = 0.50
    high_pressure: float = 0.60
    weak_signal: float = 0.45
    max_recent_mode_switches: int = 2


@dataclass(frozen=True)
class MonitoringDecision:
    level: MonitoringLevel
    inspect_every_steps: int
    risk_score: float
    reason: str


def _unit(name: str, value: float) -> None:
    if not 0.0 <= value <= 1.0:
        raise ValueError(f"{name}_must_be_between_0_and_1")


def _validate(signals: MonitoringSignals, policy: MonitoringPolicy) -> None:
    for name in (
        "flow_score", "goal_clarity", "feedback_quality", "progress_rate",
        "interruption_pressure", "recovery_load", "compute_pressure",
    ):
        _unit(name, getattr(signals, name))
    if signals.recent_mode_switches < 0:
        raise ValueError("recent_mode_switches_must_be_non_negative")
    for name in ("stable_flow_score", "risk_flow_score", "high_pressure", "weak_signal"):
        _unit(name, getattr(policy, name))
    for name in ("sparse_interval", "normal_interval", "dense_interval"):
        if getattr(policy, name) <= 0:
            raise ValueError(f"{name}_must_be_positive")
    if policy.max_recent_mode_switches < 0:
        raise ValueError("max_recent_mode_switches_must_be_non_negative")


def _risk(signals: MonitoringSignals) -> float:
    weakness = max(
        1.0 - signals.goal_clarity,
        1.0 - signals.feedback_quality,
        1.0 - signals.progress_rate,
    )
    pressure = max(
        signals.interruption_pressure,
        signals.recovery_load,
        signals.compute_pressure,
    )
    instability = min(1.0, signals.recent_mode_switches / 4.0)
    risk = (
        0.35 * (1.0 - signals.flow_score)
        + 0.25 * weakness
        + 0.25 * pressure
        + 0.15 * instability
    )
    return round(max(0.0, min(1.0, risk)), 6)


def choose_monitoring_cadence(
    signals: MonitoringSignals,
    policy: MonitoringPolicy | None = None,
) -> MonitoringDecision:
    policy = policy or MonitoringPolicy()
    _validate(signals, policy)
    risk = _risk(signals)

    if (
        signals.recovery_load >= policy.high_pressure
        or signals.interruption_pressure >= policy.high_pressure
        or signals.compute_pressure >= policy.high_pressure
        or signals.recent_mode_switches > policy.max_recent_mode_switches
    ):
        return MonitoringDecision(
            MonitoringLevel.CONTINUOUS,
            1,
            risk,
            "high_runtime_instability",
        )

    if (
        signals.flow_score <= policy.risk_flow_score
        or signals.goal_clarity <= policy.weak_signal
        or signals.feedback_quality <= policy.weak_signal
        or signals.progress_rate <= policy.weak_signal
    ):
        return MonitoringDecision(
            MonitoringLevel.DENSE,
            policy.dense_interval,
            risk,
            "flow_or_support_signal_degraded",
        )

    if (
        signals.flow_score >= policy.stable_flow_score
        and signals.goal_clarity >= 0.70
        and signals.feedback_quality >= 0.70
        and signals.progress_rate >= 0.60
        and max(
            signals.interruption_pressure,
            signals.recovery_load,
            signals.compute_pressure,
        ) <= 0.25
        and signals.recent_mode_switches == 0
    ):
        return MonitoringDecision(
            MonitoringLevel.SPARSE,
            policy.sparse_interval,
            risk,
            "stable_flow_allows_sparse_sampling",
        )

    return MonitoringDecision(
        MonitoringLevel.NORMAL,
        policy.normal_interval,
        risk,
        "mixed_signals_keep_normal_sampling",
    )
