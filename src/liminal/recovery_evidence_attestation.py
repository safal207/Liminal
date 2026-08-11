"""Detached Ed25519 attestations for verified recovery-evidence ledger heads.

The ledger itself is already hash-chained and fail-closed on local tampering.
This module adds a portable provenance statement that can be verified offline
against a separately trusted Ed25519 public key.

The attestation signs only compact ledger metadata: schema versions, record
count, and the verified ledger-head SHA-256. Raw prompts, model responses,
credentials, and private keys are never included.
"""

from __future__ import annotations

import base64
import json
from dataclasses import dataclass
from typing import Any

from cryptography.exceptions import InvalidSignature
from cryptography.hazmat.primitives.asymmetric.ed25519 import (
    Ed25519PrivateKey,
    Ed25519PublicKey,
)

from liminal.recovery_evidence_ledger import (
    GENESIS_SHA256,
    SCHEMA_VERSION as LEDGER_SCHEMA_VERSION,
    RecoveryEvidenceLedger,
)


ATTESTATION_SCHEMA_VERSION = "liminal.recovery-evidence-attestation.v0.1"
ALGORITHM = "ed25519"


def _canonical_json(payload: dict[str, Any]) -> bytes:
    return json.dumps(
        payload,
        sort_keys=True,
        separators=(",", ":"),
        ensure_ascii=True,
    ).encode("utf-8")


@dataclass(frozen=True)
class RecoveryLedgerHeadStatement:
    attestation_schema_version: str
    ledger_schema_version: str
    record_count: int
    ledger_head_sha256: str

    def as_dict(self) -> dict[str, Any]:
        return {
            "attestation_schema_version": self.attestation_schema_version,
            "ledger_schema_version": self.ledger_schema_version,
            "record_count": self.record_count,
            "ledger_head_sha256": self.ledger_head_sha256,
        }


@dataclass(frozen=True)
class RecoveryLedgerAttestation:
    statement: RecoveryLedgerHeadStatement
    algorithm: str
    key_id: str
    signature_base64: str

    def as_dict(self) -> dict[str, Any]:
        return {
            "statement": self.statement.as_dict(),
            "algorithm": self.algorithm,
            "key_id": self.key_id,
            "signature_base64": self.signature_base64,
        }


def ledger_head_statement(ledger: RecoveryEvidenceLedger) -> RecoveryLedgerHeadStatement:
    """Build a statement only after verifying the complete local hash chain."""

    records = ledger.records()
    head = records[-1].record_sha256 if records else GENESIS_SHA256
    return RecoveryLedgerHeadStatement(
        attestation_schema_version=ATTESTATION_SCHEMA_VERSION,
        ledger_schema_version=LEDGER_SCHEMA_VERSION,
        record_count=len(records),
        ledger_head_sha256=head,
    )


def sign_ledger_head(
    ledger: RecoveryEvidenceLedger,
    *,
    private_key: Ed25519PrivateKey,
    key_id: str,
) -> RecoveryLedgerAttestation:
    """Sign the verified ledger head with a caller-supplied private key."""

    if not key_id:
        raise ValueError("key_id_required")
    statement = ledger_head_statement(ledger)
    signature = private_key.sign(_canonical_json(statement.as_dict()))
    return RecoveryLedgerAttestation(
        statement=statement,
        algorithm=ALGORITHM,
        key_id=key_id,
        signature_base64=base64.b64encode(signature).decode("ascii"),
    )


def parse_attestation(payload: dict[str, Any]) -> RecoveryLedgerAttestation:
    """Parse a strict JSON-compatible attestation payload."""

    required = {"statement", "algorithm", "key_id", "signature_base64"}
    if set(payload) != required:
        raise ValueError("recovery_attestation_invalid_keys")
    if payload["algorithm"] != ALGORITHM:
        raise ValueError("recovery_attestation_unsupported_algorithm")
    key_id = payload["key_id"]
    signature_base64 = payload["signature_base64"]
    statement_payload = payload["statement"]
    if not isinstance(key_id, str) or not key_id:
        raise ValueError("recovery_attestation_invalid_key_id")
    if not isinstance(signature_base64, str) or not signature_base64:
        raise ValueError("recovery_attestation_invalid_signature")
    if not isinstance(statement_payload, dict):
        raise ValueError("recovery_attestation_invalid_statement")

    statement_keys = {
        "attestation_schema_version",
        "ledger_schema_version",
        "record_count",
        "ledger_head_sha256",
    }
    if set(statement_payload) != statement_keys:
        raise ValueError("recovery_attestation_invalid_statement_keys")
    if statement_payload["attestation_schema_version"] != ATTESTATION_SCHEMA_VERSION:
        raise ValueError("recovery_attestation_unsupported_schema")
    if statement_payload["ledger_schema_version"] != LEDGER_SCHEMA_VERSION:
        raise ValueError("recovery_attestation_unsupported_ledger_schema")
    record_count = statement_payload["record_count"]
    head = statement_payload["ledger_head_sha256"]
    if not isinstance(record_count, int) or isinstance(record_count, bool) or record_count < 0:
        raise ValueError("recovery_attestation_invalid_record_count")
    if not isinstance(head, str) or len(head) != 64:
        raise ValueError("recovery_attestation_invalid_ledger_head")

    return RecoveryLedgerAttestation(
        statement=RecoveryLedgerHeadStatement(
            attestation_schema_version=ATTESTATION_SCHEMA_VERSION,
            ledger_schema_version=LEDGER_SCHEMA_VERSION,
            record_count=record_count,
            ledger_head_sha256=head,
        ),
        algorithm=ALGORITHM,
        key_id=key_id,
        signature_base64=signature_base64,
    )


def verify_attestation(
    attestation: RecoveryLedgerAttestation,
    *,
    public_key: Ed25519PublicKey,
    expected_key_id: str | None = None,
) -> bool:
    """Verify signature integrity against an externally trusted public key."""

    if attestation.algorithm != ALGORITHM:
        return False
    if expected_key_id is not None and attestation.key_id != expected_key_id:
        return False
    try:
        signature = base64.b64decode(attestation.signature_base64, validate=True)
    except (ValueError, TypeError):
        return False
    try:
        public_key.verify(signature, _canonical_json(attestation.statement.as_dict()))
    except InvalidSignature:
        return False
    return True


def verify_attestation_matches_ledger(
    attestation: RecoveryLedgerAttestation,
    ledger: RecoveryEvidenceLedger,
    *,
    public_key: Ed25519PublicKey,
    expected_key_id: str | None = None,
) -> bool:
    """Verify both signer integrity and exact binding to the current ledger head."""

    if not verify_attestation(
        attestation,
        public_key=public_key,
        expected_key_id=expected_key_id,
    ):
        return False
    return attestation.statement == ledger_head_statement(ledger)
