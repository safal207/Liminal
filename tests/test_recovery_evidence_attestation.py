import json

from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PrivateKey

from liminal.recovery_evidence import RecoveryAttemptEvidence
from liminal.recovery_evidence_attestation import (
    parse_attestation,
    sign_ledger_head,
    verify_attestation,
    verify_attestation_matches_ledger,
)
from liminal.recovery_evidence_ledger import RecoveryEvidenceLedger
from liminal.recovery_policy import RecoveryMode


def _append(ledger: RecoveryEvidenceLedger, *, passed: bool = True) -> None:
    ledger.append(
        RecoveryAttemptEvidence(
            recovery_class="deep-ledger-recovery",
            mode=RecoveryMode.FOCUS_FIELD,
            verification_passed=passed,
            finish_reason="stop" if passed else "length",
        )
    )


def test_signed_ledger_head_verifies_offline(tmp_path) -> None:
    ledger = RecoveryEvidenceLedger(tmp_path / "recovery.jsonl")
    _append(ledger)
    _append(ledger, passed=False)
    private_key = Ed25519PrivateKey.generate()

    attestation = sign_ledger_head(
        ledger,
        private_key=private_key,
        key_id="liminal-test-key-1",
    )

    assert attestation.statement.record_count == 2
    assert verify_attestation(
        attestation,
        public_key=private_key.public_key(),
        expected_key_id="liminal-test-key-1",
    )
    assert verify_attestation_matches_ledger(
        attestation,
        ledger,
        public_key=private_key.public_key(),
        expected_key_id="liminal-test-key-1",
    )


def test_attestation_round_trips_through_json_payload(tmp_path) -> None:
    ledger = RecoveryEvidenceLedger(tmp_path / "recovery.jsonl")
    _append(ledger)
    private_key = Ed25519PrivateKey.generate()
    original = sign_ledger_head(
        ledger,
        private_key=private_key,
        key_id="liminal-test-key-1",
    )

    payload = json.loads(json.dumps(original.as_dict()))
    parsed = parse_attestation(payload)

    assert parsed == original
    assert verify_attestation(parsed, public_key=private_key.public_key())


def test_attestation_rejects_wrong_public_key(tmp_path) -> None:
    ledger = RecoveryEvidenceLedger(tmp_path / "recovery.jsonl")
    _append(ledger)
    signer = Ed25519PrivateKey.generate()
    other = Ed25519PrivateKey.generate()
    attestation = sign_ledger_head(
        ledger,
        private_key=signer,
        key_id="liminal-test-key-1",
    )

    assert not verify_attestation(attestation, public_key=other.public_key())


def test_attestation_rejects_wrong_expected_key_id(tmp_path) -> None:
    ledger = RecoveryEvidenceLedger(tmp_path / "recovery.jsonl")
    _append(ledger)
    private_key = Ed25519PrivateKey.generate()
    attestation = sign_ledger_head(
        ledger,
        private_key=private_key,
        key_id="liminal-test-key-1",
    )

    assert not verify_attestation(
        attestation,
        public_key=private_key.public_key(),
        expected_key_id="different-key",
    )


def test_attestation_no_longer_matches_after_ledger_advances(tmp_path) -> None:
    ledger = RecoveryEvidenceLedger(tmp_path / "recovery.jsonl")
    _append(ledger)
    private_key = Ed25519PrivateKey.generate()
    attestation = sign_ledger_head(
        ledger,
        private_key=private_key,
        key_id="liminal-test-key-1",
    )

    _append(ledger, passed=False)

    assert verify_attestation(attestation, public_key=private_key.public_key())
    assert not verify_attestation_matches_ledger(
        attestation,
        ledger,
        public_key=private_key.public_key(),
    )


def test_signing_fails_closed_when_ledger_chain_is_tampered(tmp_path) -> None:
    path = tmp_path / "recovery.jsonl"
    ledger = RecoveryEvidenceLedger(path)
    _append(ledger)
    payload = json.loads(path.read_text(encoding="utf-8"))
    payload["verification_passed"] = False
    path.write_text(json.dumps(payload, sort_keys=True) + "\n", encoding="utf-8")

    private_key = Ed25519PrivateKey.generate()
    try:
        sign_ledger_head(
            ledger,
            private_key=private_key,
            key_id="liminal-test-key-1",
        )
    except ValueError as exc:
        assert "recovery_evidence_ledger_hash_mismatch" in str(exc)
    else:
        raise AssertionError("expected tampered ledger to fail closed")
