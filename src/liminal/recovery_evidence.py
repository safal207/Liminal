"""Aggregate observed recovery attempts into bounded reliability evidence.

The evidence window is deliberately scoped by ``recovery_class`` so evidence
from one recovery geometry is not silently generalized to unrelated tasks.
Only Focus–Field attempts contribute to the Focus–Field reliability summary.

The runtime window stores observations in memory only. Persistence, cross-run
sharing, and durable provenance belong to a higher evidence layer; keeping this
primitive local avoids creating hidden global learning state.
"""

from __future__ import annotations

from collections import deque
from dataclasses import dataclass
from typing import Iterable

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


class RecoveryEvidenceWindow:
    """Bounded in-memory window of completed recovery attempts.

    Attempts are retained independently per ``recovery_class``. Recording an
    outcome is explicit; the window never infers success from model text or
    tool state. The newest ``max_attempts_per_class`` observations are kept.
    """

    def __init__(self, *, max_attempts_per_class: int = 20) -> None:
        if max_attempts_per_class <= 0:
            raise ValueError("max_attempts_per_class_must_be_positive")
        self.max_attempts_per_class = max_attempts_per_class
        self._attempts_by_class: dict[str, deque[RecoveryAttemptEvidence]] = {}

    def record(self, attempt: RecoveryAttemptEvidence) -> None:
        if not attempt.recovery_class:
            raise ValueError("recovery_class_required")
        bucket = self._attempts_by_class.setdefault(
            attempt.recovery_class,
            deque(maxlen=self.max_attempts_per_class),
        )
        bucket.append(attempt)

    def record_outcome(
        self,
        *,
        recovery_class: str,
        mode: RecoveryMode,
        verification_passed: bool,
        finish_reason: str | None,
    ) -> RecoveryAttemptEvidence:
        """Create and store one evidence record from an explicit runtime outcome."""

        attempt = RecoveryAttemptEvidence(
            recovery_class=recovery_class,
            mode=mode,
            verification_passed=verification_passed,
            finish_reason=finish_reason,
        )
        self.record(attempt)
        return attempt

    def extend(self, attempts: Iterable[RecoveryAttemptEvidence]) -> None:
        for attempt in attempts:
            self.record(attempt)

    def attempts(self, *, recovery_class: str) -> tuple[RecoveryAttemptEvidence, ...]:
        if not recovery_class:
            raise ValueError("recovery_class_required")
        return tuple(self._attempts_by_class.get(recovery_class, ()))

    def summarize_field(self, *, recovery_class: str) -> FieldReliabilityEvidence:
        return summarize_field_reliability(
            self.attempts(recovery_class=recovery_class),
            recovery_class=recovery_class,
        )
