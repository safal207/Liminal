from dataclasses import replace

from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PrivateKey

from liminal.recovery_decision_receipt import (
    RecoveryDecisionReceipt,
    sign_decision_receipt,
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
from liminal.recovery_receipt_trust import (
    RecoveryReceiptTrustPolicy,
    TrustedSignerRule,
    public_key_sha256,
    verify_trusted_decision_receipt,
)


RECOVERY_CLASS = "deep-ledger-recovery"
KEY_ID = "liminal-release-key-1"


def _ledger(tmp_path) -> RecoveryEvidenceLedger:
    ledger = RecoveryEvidenceLedger(tmp_path / "recovery.jsonl")
    ledger.append(
        RecoveryAttemptEvidence(
            recovery_class=RECOVERY_CLASS,
            mode=RecoveryMode.FOCUS_FIELD,
            verification_passed=True,
            finish_reason="stop",
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
        field_verification_success_rate=1.0,
        field_completion_pressure=0.0,
        field_observation_count=3,
    )


def _receipt(
    ledger: RecoveryEvidenceLedger,
    private_key: Ed25519PrivateKey,
    *,
    recovery_class: str = RECOVERY_CLASS,
    key_id: str = KEY_ID,
) -> RecoveryDecisionReceipt:
    signals = _signals()
    policy = RecoveryPolicy()
    decision = choose_recovery_mode(signals, policy)
    attestation = sign_ledger_head(ledger, private_key=private_key, key_id=key_id)
    return sign_decision_receipt(
        recovery_class=recovery_class,
        signals=signals,
        policy=policy,
        decision=decision,
        ledger_attestation=attestation,
        private_key=private_key,
        key_id=key_id,
    )


def _trust_policy(private_key: Ed25519PrivateKey) -> RecoveryReceiptTrustPolicy:
    return RecoveryReceiptTrustPolicy(
        rules=(
            TrustedSignerRule(
                key_id=KEY_ID,
                public_key_sha256=public_key_sha256(private_key.public_key()),
                allowed_recovery_classes=(RECOVERY_CLASS,),
            ),
        )
    )


def test_trusted_pinned_signer_receipt_passes(tmp_path) -> None:
    ledger = _ledger(tmp_path)
    signer = Ed25519PrivateKey.generate()
    receipt = _receipt(ledger, signer)

    result = verify_trusted_decision_receipt(
        receipt,
        public_key=signer.public_key(),
        trust_policy=_trust_policy(signer),
        ledger=ledger,
    )

    assert result.trusted
    assert result.reason == "trusted_signer_and_receipt_verified"
    assert result.key_id == KEY_ID
    assert result.recovery_class == RECOVERY_CLASS


def test_valid_ephemeral_signer_is_untrusted_when_not_pinned(tmp_path) -> None:
    ledger = _ledger(tmp_path)
    ephemeral = Ed25519PrivateKey.generate()
    trusted = Ed25519PrivateKey.generate()
    receipt = _receipt(ledger, ephemeral, key_id="ci-ephemeral:123")

    result = verify_trusted_decision_receipt(
        receipt,
        public_key=ephemeral.public_key(),
        trust_policy=_trust_policy(trusted),
        ledger=ledger,
    )

    assert not result.trusted
    assert result.reason == "signer_key_id_not_trusted"


def test_same_key_id_with_different_public_key_fails(tmp_path) -> None:
    ledger = _ledger(tmp_path)
    signer = Ed25519PrivateKey.generate()
    impostor = Ed25519PrivateKey.generate()
    receipt = _receipt(ledger, signer)

    result = verify_trusted_decision_receipt(
        receipt,
        public_key=impostor.public_key(),
        trust_policy=_trust_policy(signer),
        ledger=ledger,
    )

    assert not result.trusted
    assert result.reason == "signer_public_key_fingerprint_mismatch"


def test_signer_cannot_attest_outside_allowed_recovery_class(tmp_path) -> None:
    ledger = _ledger(tmp_path)
    signer = Ed25519PrivateKey.generate()
    receipt = _receipt(ledger, signer, recovery_class="other-recovery-class")

    result = verify_trusted_decision_receipt(
        receipt,
        public_key=signer.public_key(),
        trust_policy=_trust_policy(signer),
        ledger=ledger,
    )

    assert not result.trusted
    assert result.reason == "recovery_class_not_authorized_for_signer"


def test_post_signature_receipt_mutation_fails_trust_verification(tmp_path) -> None:
    ledger = _ledger(tmp_path)
    signer = Ed25519PrivateKey.generate()
    receipt = _receipt(ledger, signer)
    statement = dict(receipt.statement)
    decision = dict(statement["recovery_decision"])
    decision["reason"] = "tampered-after-signing"
    statement["recovery_decision"] = decision
    tampered = replace(receipt, statement=statement)

    result = verify_trusted_decision_receipt(
        tampered,
        public_key=signer.public_key(),
        trust_policy=_trust_policy(signer),
        ledger=ledger,
    )

    assert not result.trusted
    assert result.reason == "receipt_cryptographic_or_replay_verification_failed"


def test_trust_policy_rejects_duplicate_key_ids() -> None:
    signer = Ed25519PrivateKey.generate()
    rule = TrustedSignerRule(
        key_id=KEY_ID,
        public_key_sha256=public_key_sha256(signer.public_key()),
        allowed_recovery_classes=(RECOVERY_CLASS,),
    )

    try:
        RecoveryReceiptTrustPolicy(rules=(rule, rule))
    except ValueError as exc:
        assert str(exc) == "duplicate_trusted_signer_key_id"
    else:
        raise AssertionError("expected duplicate key ids to fail closed")
