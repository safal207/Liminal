"""Aggregate observed recovery attempts into reliability evidence.

The window is deliberately scoped by ``recovery_class`` so evidence from one
recovery geometry is not silently generalized to unrelated tasks. Only
Focus–Field attempts contribute to the Focus–Field reliability summary.
"""

from __future__ import annotations

from dataclasses import dataclass

from liminal.recovery_policy import RecoveryMode


@dataclass(frozen=True)
class RecoveryAttemptEvidence:
    recovery_class: str
    mode: RecoveryMode
    verification_passed: bool
    finish_reason: str | None


@dataclass(frozen=True)
class FieldReliabilityEvidence:
    recovery_class: str
    observation_count: int
    verification_success_rate: float | None
    completion_pressure: float | None


def summarize_field_reliability(
    attempts: tuple[RecoveryAttemptEvidence, ...],
    *,
    recovery_class: str,
) -> FieldReliabilityEvidence:
    """Summarize comparable observed Focus–Field attempts.

    ``completion_pressure`` is the observed fraction of comparable field
    attempts that exhausted the provider completion budget (``finish_reason``
    equals ``"length"``). It is intentionally based on an explicit provider
    outcome rather than inferred reasoning difficulty.
    """

    if not recovery_class:
        raise ValueError("recovery_class_required")

    comparable = tuple(
        attempt
        for attempt in attempts
        if attempt.recovery_class == recovery_class
        and attempt.mode is RecoveryMode.FOCUS_FIELD
    )
    count = len(comparable)
    if count == 0:
        return FieldReliabilityEvidence(
            recovery_class=recovery_class,
            observation_count=0,
            verification_success_rate=None,
            completion_pressure=None,
        )

    verified = sum(attempt.verification_passed for attempt in comparable)
    exhausted = sum(attempt.finish_reason == "length" for attempt in comparable)
    return FieldReliabilityEvidence(
        recovery_class=recovery_class,
        observation_count=count,
        verification_success_rate=verified / count,
        completion_pressure=exhausted / count,
    )
