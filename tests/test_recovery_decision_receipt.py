import base64
import json

from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PrivateKey

from liminal.recovery_decision_receipt import (
    RecoveryDecisionReceipt,
    parse_decision_receipt,
    sign_decision_receipt,
    verify_decision_receipt,
)
from liminal.recovery_evidence import RecoveryAttemptEvidence
from liminal.recovery_evidence_attestation import sign_ledger_head
from liminal.recovery_evidence_ledger import RecoveryEvidenceLedger
from liminal.recovery_policy import (
    RecoveryMode,
    RecoveryPolicy,
    RecoverySignals,
    choose_recovery_mode,
)


def _ledger(tmp_path) -> RecoveryEvidenceLedger:
    ledger = RecoveryEvidenceLedger(tmp_path / "recovery.jsonl")
    for passed, finish_reason in (
        (False, "stop"),
        (False, "length"),
        (False, "stop"),
    ):
        ledger.append(
            RecoveryAttemptEvidence(
                recovery_class="deep-ledger-recovery",
                mode=RecoveryMode.FOCUS_FIELD,
                verification_passed=passed,
                finish_reason=finish_reason,
            )
        )
    return ledger


def _signals() -> RecoverySignals:
    return RecoverySignals(
        replay_steps=12,
        candidate_count=3,
        best_anchor_score=0.82,
        uncertainty=0.12,
        verified_candidate_available=True,
        require_verified=True,
        field_scan_cost=3,
        field_verification_success_rate=0.0,
        field_completion_pressure=1 / 3,
        field_observation_count=3,
    )


def _signed_receipt(tmp_path):
    ledger = _ledger(tmp_path)
    private_key = Ed25519PrivateKey.generate()
    policy = RecoveryPolicy()
    signals = _signals()
    decision = choose_recovery_mode(signals, policy)
    attestation = sign_ledger_head(
        ledger,
        private_key=private_key,
        key_id="liminal-test-key-1",
    )
    receipt = sign_decision_receipt(
        recovery_class="deep-ledger-recovery",
        signals=signals,
        policy=policy,
        decision=decision,
        ledger_attestation=attestation,
        private_key=private_key,
        key_id="liminal-test-key-1",
    )
    return ledger, private_key, receipt


def test_signed_decision_receipt_replays_router_offline(tmp_path) -> None:
    ledger, private_key, receipt = _signed_receipt(tmp_path)

    assert receipt.statement["recovery_decision"]["mode"] == "sequential"
    assert receipt.statement["recovery_decision"]["reason"] == (
        "field_observed_verification_rate_too_low"
    )
    assert verify_decision_receipt(
        receipt,
        public_key=private_key.public_key(),
        expected_key_id="liminal-test-key-1",
        ledger=ledger,
    )


def test_decision_receipt_round_trips_through_json(tmp_path) -> None:
    ledger, private_key, original = _signed_receipt(tmp_path)
    payload = json.loads(json.dumps(original.as_dict()))
    parsed = parse_decision_receipt(payload)

    assert parsed == original
    assert verify_decision_receipt(
        parsed,
        public_key=private_key.public_key(),
        ledger=ledger,
    )


def test_receipt_rejects_wrong_public_key(tmp_path) -> None:
    ledger, _private_key, receipt = _signed_receipt(tmp_path)
    other = Ed25519PrivateKey.generate()

    assert not verify_decision_receipt(
        receipt,
        public_key=other.public_key(),
        ledger=ledger,
    )


def test_receipt_no_longer_matches_after_ledger_advances(tmp_path) -> None:
    ledger, private_key, receipt = _signed_receipt(tmp_path)
    ledger.append(
        RecoveryAttemptEvidence(
            recovery_class="deep-ledger-recovery",
            mode=RecoveryMode.FOCUS_FIELD,
            verification_passed=True,
            finish_reason="stop",
        )
    )

    # The historical receipt signature is still valid in isolation, but it is
    # not a receipt for the new ledger head.
    assert verify_decision_receipt(receipt, public_key=private_key.public_key())
    assert not verify_decision_receipt(
        receipt,
        public_key=private_key.public_key(),
        ledger=ledger,
    )


def test_even_validly_signed_contradictory_decision_fails_replay(tmp_path) -> None:
    ledger, private_key, receipt = _signed_receipt(tmp_path)
    statement = json.loads(json.dumps(receipt.statement))
    statement["recovery_decision"]["mode"] = "focus_field"
    statement["recovery_decision"]["reason"] = (
        "deep_recovery_with_credible_economic_reanchor"
    )

    canonical = json.dumps(
        statement,
        sort_keys=True,
        separators=(",", ":"),
        ensure_ascii=True,
    ).encode("utf-8")
    contradictory = RecoveryDecisionReceipt(
        statement=statement,
        algorithm="ed25519",
        key_id="liminal-test-key-1",
        signature_base64=base64.b64encode(private_key.sign(canonical)).decode("ascii"),
    )

    assert not verify_decision_receipt(
        contradictory,
        public_key=private_key.public_key(),
        ledger=ledger,
    )


def test_receipt_contains_no_raw_provider_payload_fields(tmp_path) -> None:
    _ledger_value, _private_key, receipt = _signed_receipt(tmp_path)
    serialized = json.dumps(receipt.as_dict(), sort_keys=True)

    assert "prompt" not in serialized
    assert "raw_response" not in serialized
    assert "api_key" not in serialized
    assert "private_key" not in serialized
