"""Experimental runtime routing policy for agent context recovery.

The policy decides *how* a suspended or interrupted workflow should recover:
sequential replay, field-mediated re-anchoring, or deferral when confidence is
insufficient. It is deterministic and model-agnostic so it can sit above CML,
ContinuationToken, or another memory/retrieval implementation.

v0.2 adds optional observed reliability signals for Focus–Field recovery. They
are intentionally ignored until a minimum evidence count is available, so the
router does not overfit to one or two stochastic provider responses.
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum


class RecoveryMode(str, Enum):
    SEQUENTIAL = "sequential"
    FOCUS_FIELD = "focus_field"
    DEFER = "defer"


@dataclass(frozen=True)
class RecoverySignals:
    """Runtime observations used to choose a recovery strategy."""

    replay_steps: int
    candidate_count: int
    best_anchor_score: float
    uncertainty: float = 0.0
    verified_candidate_available: bool = True
    require_verified: bool = False
    field_scan_cost: int | None = None
    field_verification_success_rate: float | None = None
    field_completion_pressure: float | None = None
    field_observation_count: int = 0


@dataclass(frozen=True)
class RecoveryPolicy:
    """Thresholds for deterministic recovery routing."""

    max_sequential_steps: int = 4
    min_field_anchor_score: float = 0.35
    max_field_uncertainty: float = 0.45
    min_field_savings_ratio: float = 0.25
    max_field_candidates: int = 32
    min_field_observations: int = 3
    min_field_verification_success_rate: float = 0.67
    max_field_completion_pressure: float = 0.50


@dataclass(frozen=True)
class RecoveryDecision:
    mode: RecoveryMode
    reason: str
    replay_steps: int
    estimated_field_cost: int
    estimated_savings_ratio: float


def _validate_unit_interval(name: str, value: float) -> None:
    if not 0.0 <= value <= 1.0:
        raise ValueError(f"{name}_must_be_between_0_and_1")


def _validate(signals: RecoverySignals, policy: RecoveryPolicy) -> None:
    if signals.replay_steps < 0:
        raise ValueError("replay_steps_must_be_non_negative")
    if signals.candidate_count < 0:
        raise ValueError("candidate_count_must_be_non_negative")
    if signals.field_scan_cost is not None and signals.field_scan_cost < 0:
        raise ValueError("field_scan_cost_must_be_non_negative")
    if signals.field_observation_count < 0:
        raise ValueError("field_observation_count_must_be_non_negative")
    _validate_unit_interval("best_anchor_score", signals.best_anchor_score)
    _validate_unit_interval("uncertainty", signals.uncertainty)
    if signals.field_verification_success_rate is not None:
        _validate_unit_interval(
            "field_verification_success_rate", signals.field_verification_success_rate
        )
    if signals.field_completion_pressure is not None:
        _validate_unit_interval("field_completion_pressure", signals.field_completion_pressure)
    if policy.max_sequential_steps < 0:
        raise ValueError("max_sequential_steps_must_be_non_negative")
    if policy.max_field_candidates <= 0:
        raise ValueError("max_field_candidates_must_be_positive")
    if policy.min_field_observations < 0:
        raise ValueError("min_field_observations_must_be_non_negative")
    _validate_unit_interval("min_field_anchor_score", policy.min_field_anchor_score)
    _validate_unit_interval("max_field_uncertainty", policy.max_field_uncertainty)
    _validate_unit_interval("min_field_savings_ratio", policy.min_field_savings_ratio)
    _validate_unit_interval(
        "min_field_verification_success_rate",
        policy.min_field_verification_success_rate,
    )
    _validate_unit_interval(
        "max_field_completion_pressure", policy.max_field_completion_pressure
    )


def _field_cost(signals: RecoverySignals) -> int:
    if signals.field_scan_cost is not None:
        return signals.field_scan_cost
    # v0.1 approximation: one unit per bounded candidate scored.
    return signals.candidate_count


def _savings_ratio(replay_steps: int, field_cost: int) -> float:
    if replay_steps <= 0:
        return 0.0
    return max(0.0, min(1.0, (replay_steps - field_cost) / replay_steps))


def _has_field_reliability_evidence(
    signals: RecoverySignals, policy: RecoveryPolicy
) -> bool:
    return signals.field_observation_count >= policy.min_field_observations


def choose_recovery_mode(
    signals: RecoverySignals,
    policy: RecoveryPolicy | None = None,
) -> RecoveryDecision:
    """Choose sequential replay, Focus–Field recovery, or defer.

    Ordering is intentional:
    1. preserve verification requirements;
    2. prefer simple sequential recovery for shallow interruptions;
    3. refuse field re-entry when confidence is too low;
    4. use observed field reliability only when enough samples exist;
    5. use Focus–Field only when it is both credible and economically useful.

    ``field_verification_success_rate`` and ``field_completion_pressure`` are
    optional evidence from prior comparable field attempts. Missing or
    under-sampled evidence is not treated as healthy or unhealthy.
    """

    policy = policy or RecoveryPolicy()
    _validate(signals, policy)
    field_cost = _field_cost(signals)
    savings = _savings_ratio(signals.replay_steps, field_cost)

    if signals.require_verified and not signals.verified_candidate_available:
        return RecoveryDecision(
            mode=RecoveryMode.DEFER,
            reason="verified_anchor_required",
            replay_steps=signals.replay_steps,
            estimated_field_cost=field_cost,
            estimated_savings_ratio=savings,
        )

    if signals.replay_steps <= policy.max_sequential_steps:
        return RecoveryDecision(
            mode=RecoveryMode.SEQUENTIAL,
            reason="shallow_recovery_is_cheaper_or_simpler",
            replay_steps=signals.replay_steps,
            estimated_field_cost=field_cost,
            estimated_savings_ratio=savings,
        )

    if signals.candidate_count == 0:
        return RecoveryDecision(
            mode=RecoveryMode.SEQUENTIAL,
            reason="no_field_candidates",
            replay_steps=signals.replay_steps,
            estimated_field_cost=field_cost,
            estimated_savings_ratio=savings,
        )

    if signals.candidate_count > policy.max_field_candidates:
        return RecoveryDecision(
            mode=RecoveryMode.DEFER,
            reason="field_candidate_bound_exceeded",
            replay_steps=signals.replay_steps,
            estimated_field_cost=field_cost,
            estimated_savings_ratio=savings,
        )

    if signals.best_anchor_score < policy.min_field_anchor_score:
        return RecoveryDecision(
            mode=RecoveryMode.SEQUENTIAL,
            reason="field_anchor_below_confidence_threshold",
            replay_steps=signals.replay_steps,
            estimated_field_cost=field_cost,
            estimated_savings_ratio=savings,
        )

    if signals.uncertainty > policy.max_field_uncertainty:
        return RecoveryDecision(
            mode=RecoveryMode.DEFER,
            reason="field_uncertainty_too_high",
            replay_steps=signals.replay_steps,
            estimated_field_cost=field_cost,
            estimated_savings_ratio=savings,
        )

    if _has_field_reliability_evidence(signals, policy):
        if (
            signals.field_verification_success_rate is not None
            and signals.field_verification_success_rate
            < policy.min_field_verification_success_rate
        ):
            return RecoveryDecision(
                mode=RecoveryMode.SEQUENTIAL,
                reason="field_observed_verification_rate_too_low",
                replay_steps=signals.replay_steps,
                estimated_field_cost=field_cost,
                estimated_savings_ratio=round(savings, 6),
            )

        if (
            signals.field_completion_pressure is not None
            and signals.field_completion_pressure > policy.max_field_completion_pressure
        ):
            return RecoveryDecision(
                mode=RecoveryMode.SEQUENTIAL,
                reason="field_completion_pressure_too_high",
                replay_steps=signals.replay_steps,
                estimated_field_cost=field_cost,
                estimated_savings_ratio=round(savings, 6),
            )

    if savings < policy.min_field_savings_ratio:
        return RecoveryDecision(
            mode=RecoveryMode.SEQUENTIAL,
            reason="field_savings_below_break_even",
            replay_steps=signals.replay_steps,
            estimated_field_cost=field_cost,
            estimated_savings_ratio=round(savings, 6),
        )

    return RecoveryDecision(
        mode=RecoveryMode.FOCUS_FIELD,
        reason="deep_recovery_with_credible_economic_reanchor",
        replay_steps=signals.replay_steps,
        estimated_field_cost=field_cost,
        estimated_savings_ratio=round(savings, 6),
    )
