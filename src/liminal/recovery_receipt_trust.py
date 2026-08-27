"""Pinned signer trust policy for recovery decision receipts.

Cryptographic validity and signer trust are intentionally separate concerns.
A mathematically valid Ed25519 receipt is not treated as trusted unless the
caller's explicit policy pins the signer's key id, raw-public-key SHA-256, and
allowed recovery scope.

v0.1 is deliberately small and offline:
- no network key discovery;
- no implicit trust-on-first-use;
- no wildcard signer identities;
- no raw prompts, model outputs, credentials, or private keys.
"""

from __future__ import annotations

import hashlib
from dataclasses import dataclass

from cryptography.hazmat.primitives import serialization
from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PublicKey

from liminal.recovery_decision_receipt import (
    RECEIPT_SCHEMA_VERSION,
    RecoveryDecisionReceipt,
    verify_decision_receipt,
)
from liminal.recovery_evidence_ledger import RecoveryEvidenceLedger


TRUST_POLICY_SCHEMA_VERSION = "liminal.recovery-receipt-trust-policy.v0.1"


def public_key_sha256(public_key: Ed25519PublicKey) -> str:
    """Return the lowercase SHA-256 fingerprint of the raw Ed25519 public key."""

    raw = public_key.public_bytes(
        encoding=serialization.Encoding.Raw,
        format=serialization.PublicFormat.Raw,
    )
    return hashlib.sha256(raw).hexdigest()


def _is_sha256_hex(value: str) -> bool:
    if len(value) != 64:
        return False
    try:
        bytes.fromhex(value)
    except ValueError:
        return False
    return value == value.lower()


@dataclass(frozen=True)
class TrustedSignerRule:
    """One explicitly pinned signer and the receipt scopes it may attest."""

    key_id: str
    public_key_sha256: str
    allowed_recovery_classes: tuple[str, ...]
    allowed_receipt_schemas: tuple[str, ...] = (RECEIPT_SCHEMA_VERSION,)

    def __post_init__(self) -> None:
        if not self.key_id:
            raise ValueError("trusted_signer_key_id_required")
        if not _is_sha256_hex(self.public_key_sha256):
            raise ValueError("trusted_signer_invalid_public_key_sha256")
        if not self.allowed_recovery_classes:
            raise ValueError("trusted_signer_recovery_scope_required")
        if any(not value for value in self.allowed_recovery_classes):
            raise ValueError("trusted_signer_invalid_recovery_scope")
        if not self.allowed_receipt_schemas:
            raise ValueError("trusted_signer_receipt_schema_required")
        if any(not value for value in self.allowed_receipt_schemas):
            raise ValueError("trusted_signer_invalid_receipt_schema")


@dataclass(frozen=True)
class RecoveryReceiptTrustPolicy:
    """Explicit offline allow-list of receipt signers."""

    rules: tuple[TrustedSignerRule, ...]
    schema_version: str = TRUST_POLICY_SCHEMA_VERSION

    def __post_init__(self) -> None:
        if self.schema_version != TRUST_POLICY_SCHEMA_VERSION:
            raise ValueError("unsupported_recovery_receipt_trust_policy_schema")
        key_ids = [rule.key_id for rule in self.rules]
        if len(key_ids) != len(set(key_ids)):
            raise ValueError("duplicate_trusted_signer_key_id")

    def rule_for(self, key_id: str) -> TrustedSignerRule | None:
        return next((rule for rule in self.rules if rule.key_id == key_id), None)


@dataclass(frozen=True)
class TrustedReceiptVerification:
    trusted: bool
    reason: str
    key_id: str
    public_key_sha256: str
    recovery_class: str | None


def verify_trusted_decision_receipt(
    receipt: RecoveryDecisionReceipt,
    *,
    public_key: Ed25519PublicKey,
    trust_policy: RecoveryReceiptTrustPolicy,
    ledger: RecoveryEvidenceLedger | None = None,
) -> TrustedReceiptVerification:
    """Verify receipt integrity and then enforce explicit signer trust policy."""

    fingerprint = public_key_sha256(public_key)
    recovery_class = receipt.statement.get("recovery_class")
    if not isinstance(recovery_class, str) or not recovery_class:
        return TrustedReceiptVerification(
            trusted=False,
            reason="receipt_recovery_class_invalid",
            key_id=receipt.key_id,
            public_key_sha256=fingerprint,
            recovery_class=None,
        )

    rule = trust_policy.rule_for(receipt.key_id)
    if rule is None:
        return TrustedReceiptVerification(
            trusted=False,
            reason="signer_key_id_not_trusted",
            key_id=receipt.key_id,
            public_key_sha256=fingerprint,
            recovery_class=recovery_class,
        )
    if fingerprint != rule.public_key_sha256:
        return TrustedReceiptVerification(
            trusted=False,
            reason="signer_public_key_fingerprint_mismatch",
            key_id=receipt.key_id,
            public_key_sha256=fingerprint,
            recovery_class=recovery_class,
        )

    schema_version = receipt.statement.get("schema_version")
    if schema_version not in rule.allowed_receipt_schemas:
        return TrustedReceiptVerification(
            trusted=False,
            reason="receipt_schema_not_authorized_for_signer",
            key_id=receipt.key_id,
            public_key_sha256=fingerprint,
            recovery_class=recovery_class,
        )
    if recovery_class not in rule.allowed_recovery_classes:
        return TrustedReceiptVerification(
            trusted=False,
            reason="recovery_class_not_authorized_for_signer",
            key_id=receipt.key_id,
            public_key_sha256=fingerprint,
            recovery_class=recovery_class,
        )

    if not verify_decision_receipt(
        receipt,
        public_key=public_key,
        expected_key_id=rule.key_id,
        ledger=ledger,
    ):
        return TrustedReceiptVerification(
            trusted=False,
            reason="receipt_cryptographic_or_replay_verification_failed",
            key_id=receipt.key_id,
            public_key_sha256=fingerprint,
            recovery_class=recovery_class,
        )

    return TrustedReceiptVerification(
        trusted=True,
        reason="trusted_signer_and_receipt_verified",
        key_id=receipt.key_id,
        public_key_sha256=fingerprint,
        recovery_class=recovery_class,
    )
