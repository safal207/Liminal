from liminal.recovery_evidence import (
    RecoveryAttemptEvidence,
    summarize_field_reliability,
)
from liminal.recovery_policy import RecoveryMode


def test_field_reliability_uses_only_same_class_focus_field_attempts() -> None:
    attempts = (
        RecoveryAttemptEvidence(
            recovery_class="deep-ledger-recovery",
            mode=RecoveryMode.FOCUS_FIELD,
            verification_passed=True,
            finish_reason="stop",
        ),
        RecoveryAttemptEvidence(
            recovery_class="deep-ledger-recovery",
            mode=RecoveryMode.FOCUS_FIELD,
            verification_passed=False,
            finish_reason="length",
        ),
        RecoveryAttemptEvidence(
            recovery_class="deep-ledger-recovery",
            mode=RecoveryMode.SEQUENTIAL,
            verification_passed=True,
            finish_reason="stop",
        ),
        RecoveryAttemptEvidence(
            recovery_class="other-recovery",
            mode=RecoveryMode.FOCUS_FIELD,
            verification_passed=False,
            finish_reason="length",
        ),
    )

    evidence = summarize_field_reliability(
        attempts, recovery_class="deep-ledger-recovery"
    )

    assert evidence.observation_count == 2
    assert evidence.verification_success_rate == 0.5
    assert evidence.completion_pressure == 0.5


def test_missing_comparable_field_evidence_stays_missing() -> None:
    attempts = (
        RecoveryAttemptEvidence(
            recovery_class="deep-ledger-recovery",
            mode=RecoveryMode.SEQUENTIAL,
            verification_passed=True,
            finish_reason="stop",
        ),
    )

    evidence = summarize_field_reliability(
        attempts, recovery_class="deep-ledger-recovery"
    )

    assert evidence.observation_count == 0
    assert evidence.verification_success_rate is None
    assert evidence.completion_pressure is None


def test_completion_pressure_counts_explicit_length_finish_reason_only() -> None:
    attempts = (
        RecoveryAttemptEvidence(
            recovery_class="deep-ledger-recovery",
            mode=RecoveryMode.FOCUS_FIELD,
            verification_passed=True,
            finish_reason="stop",
        ),
        RecoveryAttemptEvidence(
            recovery_class="deep-ledger-recovery",
            mode=RecoveryMode.FOCUS_FIELD,
            verification_passed=True,
            finish_reason=None,
        ),
        RecoveryAttemptEvidence(
            recovery_class="deep-ledger-recovery",
            mode=RecoveryMode.FOCUS_FIELD,
            verification_passed=False,
            finish_reason="length",
        ),
    )

    evidence = summarize_field_reliability(
        attempts, recovery_class="deep-ledger-recovery"
    )

    assert evidence.completion_pressure == 1 / 3


def test_recovery_class_is_required() -> None:
    try:
        summarize_field_reliability((), recovery_class="")
    except ValueError as exc:
        assert str(exc) == "recovery_class_required"
    else:
        raise AssertionError("expected ValueError")
