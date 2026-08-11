import json

from liminal.recovery_evidence import RecoveryAttemptEvidence
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


def test_ledger_round_trips_compact_attempts(tmp_path) -> None:
    path = tmp_path / "recovery-evidence.jsonl"
    ledger = RecoveryEvidenceLedger(path)
    attempt = RecoveryAttemptEvidence(
        recovery_class="deep-ledger-recovery",
        mode=RecoveryMode.FOCUS_FIELD,
        verification_passed=True,
        finish_reason="stop",
    )

    record = ledger.append(attempt)

    assert ledger.attempts() == (attempt,)
    assert len(record.record_sha256) == 64
    persisted = json.loads(path.read_text(encoding="utf-8"))
    assert set(persisted) == {
        "schema_version",
        "previous_sha256",
        "recovery_class",
        "mode",
        "verification_passed",
        "finish_reason",
        "record_sha256",
    }
    assert "content" not in persisted
    assert "prompt" not in persisted


def test_runtime_recovers_evidence_after_restart(tmp_path) -> None:
    path = tmp_path / "recovery-evidence.jsonl"
    first = EvidenceAwareRecoveryRuntime(evidence_ledger=RecoveryEvidenceLedger(path))
    for finish_reason in ("stop", "length", "stop"):
        first.record_attempt(
            recovery_class="deep-ledger-recovery",
            mode=RecoveryMode.FOCUS_FIELD,
            verification_passed=False,
            finish_reason=finish_reason,
        )

    restarted = EvidenceAwareRecoveryRuntime(evidence_ledger=RecoveryEvidenceLedger(path))
    evidence = restarted.field_evidence(recovery_class="deep-ledger-recovery")
    decision = restarted.decide(
        _deep_recovery(), recovery_class="deep-ledger-recovery"
    )

    assert evidence.observation_count == 3
    assert evidence.verification_success_rate == 0.0
    assert decision.mode is RecoveryMode.SEQUENTIAL
    assert decision.reason == "field_observed_verification_rate_too_low"


def test_ledger_detects_tampered_record(tmp_path) -> None:
    path = tmp_path / "recovery-evidence.jsonl"
    ledger = RecoveryEvidenceLedger(path)
    ledger.append(
        RecoveryAttemptEvidence(
            recovery_class="deep-ledger-recovery",
            mode=RecoveryMode.FOCUS_FIELD,
            verification_passed=True,
            finish_reason="stop",
        )
    )

    payload = json.loads(path.read_text(encoding="utf-8"))
    payload["verification_passed"] = False
    path.write_text(json.dumps(payload) + "\n", encoding="utf-8")

    try:
        ledger.attempts()
    except ValueError as exc:
        assert str(exc) == "recovery_evidence_ledger_hash_mismatch:1"
    else:
        raise AssertionError("expected tamper detection")


def test_ledger_detects_broken_chain(tmp_path) -> None:
    path = tmp_path / "recovery-evidence.jsonl"
    ledger = RecoveryEvidenceLedger(path)
    for passed in (True, False):
        ledger.append(
            RecoveryAttemptEvidence(
                recovery_class="deep-ledger-recovery",
                mode=RecoveryMode.FOCUS_FIELD,
                verification_passed=passed,
                finish_reason="stop",
            )
        )

    lines = path.read_text(encoding="utf-8").splitlines()
    first = json.loads(lines[0])
    first["record_sha256"] = "f" * 64
    path.write_text(json.dumps(first) + "\n" + lines[1] + "\n", encoding="utf-8")

    try:
        ledger.attempts()
    except ValueError as exc:
        assert str(exc) == "recovery_evidence_ledger_hash_mismatch:1"
    else:
        raise AssertionError("expected chain verification failure")
