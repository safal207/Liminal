"""Experimental flow-regulation primitive for sustained agent work.

The regulator treats flow as an engineering operating corridor rather than an
emotion: task challenge, available capability, goal clarity, feedback quality,
progress, interruption pressure, recovery load, and compute cost are combined
into an inspectable score and a small set of deterministic adjustments.
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum


class FlowState(str, Enum):
    UNDERLOADED = "underloaded"
    FLOW = "flow"
    OVERLOADED = "overloaded"
    RECOVERY = "recovery"


@dataclass(frozen=True)
class FlowSignals:
    challenge: float
    capability: float
    goal_clarity: float
    feedback_quality: float
    progress_rate: float
    interruption_pressure: float = 0.0
    recovery_load: float = 0.0
    compute_pressure: float = 0.0


@dataclass(frozen=True)
class FlowPolicy:
    balance_tolerance: float = 0.20
    min_goal_clarity: float = 0.60
    min_feedback_quality: float = 0.55
    min_progress_rate: float = 0.35
    max_interruption_pressure: float = 0.45
    max_recovery_load: float = 0.45
    max_compute_pressure: float = 0.75


@dataclass(frozen=True)
class FlowDecision:
    state: FlowState
    flow_score: float
    reason: str
    adjustment: str


def _unit(name: str, value: float) -> None:
    if not 0.0 <= value <= 1.0:
        raise ValueError(f"{name}_must_be_between_0_and_1")


def _validate(signals: FlowSignals, policy: FlowPolicy) -> None:
    for name, value in signals.__dict__.items():
        _unit(name, value)
    for name, value in policy.__dict__.items():
        _unit(name, value)


def _balance(challenge: float, capability: float) -> float:
    return 1.0 - abs(challenge - capability)


def flow_score(signals: FlowSignals) -> float:
    """Return an inspectable 0..1 score for the current operating corridor."""

    balance = _balance(signals.challenge, signals.capability)
    positive = (
        0.30 * balance
        + 0.18 * signals.goal_clarity
        + 0.16 * signals.feedback_quality
        + 0.16 * signals.progress_rate
    )
    friction = (
        0.08 * signals.interruption_pressure
        + 0.07 * signals.recovery_load
        + 0.05 * signals.compute_pressure
    )
    return round(max(0.0, min(1.0, positive - friction)), 6)


def regulate_flow(
    signals: FlowSignals,
    policy: FlowPolicy | None = None,
) -> FlowDecision:
    """Classify the current regime and recommend the smallest useful adjustment."""

    policy = policy or FlowPolicy()
    _validate(signals, policy)
    score = flow_score(signals)
    gap = signals.challenge - signals.capability

    if signals.recovery_load > policy.max_recovery_load:
        return FlowDecision(
            state=FlowState.RECOVERY,
            flow_score=score,
            reason="recovery_load_above_flow_corridor",
            adjustment="observe_then_route_recovery",
        )

    if signals.compute_pressure > policy.max_compute_pressure:
        return FlowDecision(
            state=FlowState.OVERLOADED,
            flow_score=score,
            reason="compute_pressure_above_flow_corridor",
            adjustment="reduce_context_or_split_task",
        )

    if gap > policy.balance_tolerance:
        return FlowDecision(
            state=FlowState.OVERLOADED,
            flow_score=score,
            reason="challenge_exceeds_available_capability",
            adjustment="decompose_or_retrieve_supporting_context",
        )

    if gap < -policy.balance_tolerance:
        return FlowDecision(
            state=FlowState.UNDERLOADED,
            flow_score=score,
            reason="capability_substantially_exceeds_challenge",
            adjustment="increase_task_granularity_or_batch_safe_work",
        )

    if signals.goal_clarity < policy.min_goal_clarity:
        return FlowDecision(
            state=FlowState.RECOVERY,
            flow_score=score,
            reason="goal_clarity_below_flow_corridor",
            adjustment="restore_value_and_goal_anchor",
        )

    if signals.feedback_quality < policy.min_feedback_quality:
        return FlowDecision(
            state=FlowState.RECOVERY,
            flow_score=score,
            reason="feedback_quality_below_flow_corridor",
            adjustment="request_or_generate_fast_verifiable_feedback",
        )

    if signals.interruption_pressure > policy.max_interruption_pressure:
        return FlowDecision(
            state=FlowState.RECOVERY,
            flow_score=score,
            reason="interruption_pressure_above_flow_corridor",
            adjustment="checkpoint_then_reduce_switching",
        )

    if signals.progress_rate < policy.min_progress_rate:
        return FlowDecision(
            state=FlowState.RECOVERY,
            flow_score=score,
            reason="progress_stalled_inside_balanced_work",
            adjustment="observe_field_and_select_new_continuation",
        )

    return FlowDecision(
        state=FlowState.FLOW,
        flow_score=score,
        reason="challenge_capability_feedback_progress_in_corridor",
        adjustment="continue_without_unnecessary_mode_switch",
    )
