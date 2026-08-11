from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PrivateKey

from liminal.recovery_decision_receipt import verify_decision_receipt
from liminal.recovery_evidence_ledger import RecoveryEvidenceLedger
from liminal.recovery_policy import RecoveryMode
from liminal.recovery_runtime import EvidenceAwareRecoveryRuntime
from liminal.telemetry_bridge import RuntimeTelemetry


def _deep_recovery() -> RuntimeTelemetry:
    return RuntimeTelemetry(
        token_utilization=0.35,
        retry_rate=0.05,
        tool_failure_rate=0.02,
        latency_pressure=0.20,
        context_pressure=0.20,
        goal_drift=0.05,
        causal_drift=0.05,
        verified_progress_rate=0.75,
        feedback_success_rate=0.95,
        interruption_rate=0.05,
        recent_recovery_rate=0.05,
        task_difficulty=0.70,
        available_capability=0.72,
        replay_steps_estimate=12,
        field_candidate_count=3,
        best_anchor_score=0.82,
        field_uncertainty=0.12,
        verified_candidate_available=True,
        require_verified=True,
        measured_field_cost=3,
    )


def test_runtime_uses_observed_failures_for_next_comparable_route() -> None:
    runtime = EvidenceAwareRecoveryRuntime()
    for finish_reason in ("stop", "length", "stop"):
        runtime.record_attempt(
            recovery_class="deep-ledger-recovery",
            mode=RecoveryMode.FOCUS_FIELD,
            verification_passed=False,
            finish_reason=finish_reason,
        )

    evidence = runtime.field_evidence(recovery_class="deep-ledger-recovery")
    decision = runtime.decide(
        _deep_recovery(), recovery_class="deep-ledger-recovery"
    )

    assert evidence.observation_count == 3
    assert evidence.verification_success_rate == 0.0
    assert evidence.completion_pressure == 1 / 3
    assert decision.mode is RecoveryMode.SEQUENTIAL
    assert decision.reason == "field_observed_verification_rate_too_low"


def test_runtime_does_not_apply_under_sampled_history() -> None:
    runtime = EvidenceAwareRecoveryRuntime()
    for _ in range(2):
        runtime.record_attempt(
            recovery_class="deep-ledger-recovery",
            mode=RecoveryMode.FOCUS_FIELD,
            verification_passed=False,
            finish_reason="length",
        )

    decision = runtime.decide(
        _deep_recovery(), recovery_class="deep-ledger-recovery"
    )

    assert decision.mode is RecoveryMode.FOCUS_FIELD
    assert decision.reason == "deep_recovery_with_credible_economic_reanchor"


def test_runtime_uses_only_same_recovery_class_history() -> None:
    runtime = EvidenceAwareRecoveryRuntime()
    for _ in range(3):
        runtime.record_attempt(
            recovery_class="other-recovery",
            mode=RecoveryMode.FOCUS_FIELD,
            verification_passed=False,
            finish_reason="length",
        )

    decision = runtime.decide(
        _deep_recovery(), recovery_class="deep-ledger-recovery"
    )

    assert decision.mode is RecoveryMode.FOCUS_FIELD
    assert runtime.field_evidence(
        recovery_class="deep-ledger-recovery"
    ).observation_count == 0


def test_runtime_routes_away_from_high_observed_completion_pressure() -> None:
    runtime = EvidenceAwareRecoveryRuntime()
    outcomes = (
        (True, "length"),
        (True, "length"),
        (True, "stop"),
    )
    for passed, finish_reason in outcomes:
        runtime.record_attempt(
            recovery_class="deep-ledger-recovery",
            mode=RecoveryMode.FOCUS_FIELD,
            verification_passed=passed,
            finish_reason=finish_reason,
        )

    evidence = runtime.field_evidence(recovery_class="deep-ledger-recovery")
    decision = runtime.decide(
        _deep_recovery(), recovery_class="deep-ledger-recovery"
    )

    assert evidence.verification_success_rate == 1.0
    assert evidence.completion_pressure == 2 / 3
    assert decision.mode is RecoveryMode.SEQUENTIAL
    assert decision.reason == "field_completion_pressure_too_high"


def test_runtime_emits_receipt_bound_to_durable_evidence(tmp_path) -> None:
    ledger = RecoveryEvidenceLedger(tmp_path / "recovery.jsonl")
    runtime = EvidenceAwareRecoveryRuntime(evidence_ledger=ledger)
    for finish_reason in ("stop", "length", "stop"):
        runtime.record_attempt(
            recovery_class="deep-ledger-recovery",
            mode=RecoveryMode.FOCUS_FIELD,
            verification_passed=False,
            finish_reason=finish_reason,
        )

    private_key = Ed25519PrivateKey.generate()
    result = runtime.decide_with_receipt(
        _deep_recovery(),
        recovery_class="deep-ledger-recovery",
        private_key=private_key,
        key_id="liminal-test-key-1",
    )

    assert result.decision.mode is RecoveryMode.SEQUENTIAL
    assert verify_decision_receipt(
        result.receipt,
        public_key=private_key.public_key(),
        expected_key_id="liminal-test-key-1",
        ledger=ledger,
    )


def test_runtime_refuses_receipt_without_durable_ledger() -> None:
    runtime = EvidenceAwareRecoveryRuntime()
    private_key = Ed25519PrivateKey.generate()

    try:
        runtime.decide_with_receipt(
            _deep_recovery(),
            recovery_class="deep-ledger-recovery",
            private_key=private_key,
            key_id="liminal-test-key-1",
        )
    except ValueError as exc:
        assert str(exc) == "recovery_decision_receipt_requires_durable_ledger"
    else:
        raise AssertionError("expected durable ledger requirement")
