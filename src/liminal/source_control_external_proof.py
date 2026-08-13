"""Cryptographic verification for the experimental external source/control proof bundle."""

from __future__ import annotations

import base64
import hashlib
import json
from dataclasses import dataclass
from typing import Any

from cryptography.hazmat.primitives import serialization
from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PublicKey

_BUNDLE_SCHEMA = "liminal-source-control-external-proof-bundle/v0.1"
_PRODUCER_CONTRACT_SCHEMA = "liminal-portable-checkpoint-producer-contract/v0.1"
_AUTHORIZATION_CONTRACT_SCHEMA = "liminal-portable-checkpoint-authorization-contract/v0.1"
_PRODUCER_CLAIM_SCHEMA = "liminal-external-checkpoint-producer-claim/v0.1"
_MIGRATION_SCHEMA = "liminal-witness-authority-migration/v0.1"
_PROOF_MANIFEST_SCHEMA = "liminal-external-source-control-proof/v0.1"


@dataclass(frozen=True)
class VerifiedExternalSourceControlProof:
    verified: bool
    producer_root_id: str
    control_plane_root_id: str
    subject_sha256: str
    logical_producer_id: str
    producer_contract_sha256: str
    authorization_contract_sha256: str
    evidence_type: str
    generation: int
    predecessor_witness_sha256: str


def canonical_json_bytes(payload: object) -> bytes:
    return (json.dumps(payload, sort_keys=True, separators=(",", ":")) + "\n").encode()


def _sha256(payload: bytes) -> str:
    return hashlib.sha256(payload).hexdigest()


def _require_dict(bundle: dict[str, Any], key: str) -> dict[str, Any]:
    value = bundle.get(key)
    if not isinstance(value, dict):
        raise ValueError(f"{key}_must_be_object")
    return value


def _load_ed25519_public_key(pem_text: object, *, field: str) -> tuple[Ed25519PublicKey, bytes]:
    if not isinstance(pem_text, str) or not pem_text:
        raise ValueError(f"{field}_must_be_non_empty_pem")
    pem = pem_text.encode()
    key = serialization.load_pem_public_key(pem)
    if not isinstance(key, Ed25519PublicKey):
        raise ValueError(f"{field}_must_be_ed25519")
    return key, pem


def _verify_b64_signature(
    public_key: Ed25519PublicKey,
    signature_b64: object,
    payload: bytes,
    *,
    field: str,
) -> None:
    if not isinstance(signature_b64, str) or not signature_b64:
        raise ValueError(f"{field}_must_be_non_empty_base64")
    try:
        signature = base64.b64decode(signature_b64, validate=True)
    except ValueError as exc:
        raise ValueError(f"{field}_must_be_valid_base64") from exc
    try:
        public_key.verify(signature, payload)
    except Exception as exc:  # cryptography raises InvalidSignature; normalize fail-closed API.
        raise ValueError(f"{field}_verification_failed") from exc


def verify_external_source_control_bundle(
    bundle: object,
) -> VerifiedExternalSourceControlProof:
    if not isinstance(bundle, dict):
        raise ValueError("bundle_must_be_object")
    if bundle.get("schema") != _BUNDLE_SCHEMA:
        raise ValueError("unsupported_external_source_control_bundle_schema")

    producer_contract = _require_dict(bundle, "producer_contract")
    authorization_contract = _require_dict(bundle, "authorization_contract")
    checkpoint = _require_dict(bundle, "checkpoint_generation_1")
    producer_claim = _require_dict(bundle, "producer_claim")
    migration = _require_dict(bundle, "authority_migration")
    proof_manifest = _require_dict(bundle, "external_proof_manifest")

    if producer_contract.get("schema") != _PRODUCER_CONTRACT_SCHEMA:
        raise ValueError("producer_contract_schema_invalid")
    if authorization_contract.get("schema") != _AUTHORIZATION_CONTRACT_SCHEMA:
        raise ValueError("authorization_contract_schema_invalid")
    if producer_claim.get("schema") != _PRODUCER_CLAIM_SCHEMA:
        raise ValueError("producer_claim_schema_invalid")
    if migration.get("schema") != _MIGRATION_SCHEMA:
        raise ValueError("authority_migration_schema_invalid")
    if proof_manifest.get("schema") != _PROOF_MANIFEST_SCHEMA:
        raise ValueError("external_proof_manifest_schema_invalid")

    producer_key, producer_pem = _load_ed25519_public_key(
        bundle.get("producer_public_key_pem"), field="producer_public_key"
    )
    control_key, control_pem = _load_ed25519_public_key(
        bundle.get("control_plane_public_key_pem"), field="control_plane_public_key"
    )
    producer_root_id = f"ed25519-sha256:{_sha256(producer_pem)}"
    control_root_id = f"ed25519-sha256:{_sha256(control_pem)}"
    if bundle.get("producer_root_id") != producer_root_id:
        raise ValueError("producer_root_fingerprint_mismatch")
    if bundle.get("control_plane_root_id") != control_root_id:
        raise ValueError("control_plane_root_fingerprint_mismatch")

    producer_contract_sha = _sha256(canonical_json_bytes(producer_contract))
    authorization_contract_sha = _sha256(canonical_json_bytes(authorization_contract))
    checkpoint_sha = _sha256(canonical_json_bytes(checkpoint))
    producer_claim_sha = _sha256(canonical_json_bytes(producer_claim))
    migration_sha = _sha256(canonical_json_bytes(migration))

    _verify_b64_signature(
        producer_key,
        bundle.get("producer_claim_signature_b64"),
        canonical_json_bytes(producer_claim),
        field="producer_claim_signature",
    )
    _verify_b64_signature(
        control_key,
        bundle.get("authorization_contract_signature_b64"),
        canonical_json_bytes(authorization_contract),
        field="authorization_contract_signature",
    )
    _verify_b64_signature(
        control_key,
        bundle.get("authority_migration_signature_b64"),
        canonical_json_bytes(migration),
        field="authority_migration_signature",
    )

    logical_producer_id = producer_contract.get("logical_producer_id")
    evidence_type = producer_contract.get("output_evidence_type")
    if not isinstance(logical_producer_id, str) or not logical_producer_id:
        raise ValueError("logical_producer_id_invalid")
    if not isinstance(evidence_type, str) or not evidence_type:
        raise ValueError("evidence_type_invalid")

    if authorization_contract.get("logical_producer_id") != logical_producer_id:
        raise ValueError("authorization_logical_producer_mismatch")
    if authorization_contract.get("producer_contract_sha256") != producer_contract_sha:
        raise ValueError("authorization_producer_contract_mismatch")
    if authorization_contract.get("evidence_type") != evidence_type:
        raise ValueError("authorization_evidence_type_mismatch")

    expected_claims = {
        "subject_sha256": checkpoint_sha,
        "logical_producer_id": logical_producer_id,
        "producer_contract_sha256": producer_contract_sha,
        "authorization_contract_sha256": authorization_contract_sha,
        "evidence_type": evidence_type,
    }
    for key, expected in expected_claims.items():
        if producer_claim.get(key) != expected:
            raise ValueError(f"producer_claim_{key}_mismatch")
    generation = producer_claim.get("generation")
    if not isinstance(generation, int) or isinstance(generation, bool) or generation < 0:
        raise ValueError("producer_claim_generation_invalid")
    if checkpoint.get("accepted_generation") != generation:
        raise ValueError("checkpoint_generation_mismatch")
    if producer_claim.get("predecessor_checkpoint_sha256") != checkpoint.get(
        "previous_checkpoint_sha256"
    ):
        raise ValueError("producer_claim_predecessor_checkpoint_mismatch")

    authority = migration.get("checkpoint_authority")
    if not isinstance(authority, dict):
        raise ValueError("authority_migration_checkpoint_authority_invalid")
    expected_authority = {
        "logical_producer_id": logical_producer_id,
        "producer_contract_sha256": producer_contract_sha,
        "authorization_contract_sha256": authorization_contract_sha,
        "evidence_type": evidence_type,
    }
    if authority != expected_authority:
        raise ValueError("authority_migration_contract_mismatch")
    predecessor_witness_sha = migration.get("from_witness_sha256")
    if not isinstance(predecessor_witness_sha, str):
        raise ValueError("authority_migration_predecessor_invalid")

    manifest_expectations = {
        "producer_root_id": producer_root_id,
        "control_plane_root_id": control_root_id,
        "checkpoint_subject_sha256": checkpoint_sha,
        "producer_claim_sha256": producer_claim_sha,
        "producer_contract_sha256": producer_contract_sha,
        "authorization_contract_sha256": authorization_contract_sha,
        "migration_record_sha256": migration_sha,
        "predecessor_witness_sha256": predecessor_witness_sha,
    }
    for key, expected in manifest_expectations.items():
        if proof_manifest.get(key) != expected:
            raise ValueError(f"external_proof_manifest_{key}_mismatch")

    return VerifiedExternalSourceControlProof(
        verified=True,
        producer_root_id=producer_root_id,
        control_plane_root_id=control_root_id,
        subject_sha256=checkpoint_sha,
        logical_producer_id=logical_producer_id,
        producer_contract_sha256=producer_contract_sha,
        authorization_contract_sha256=authorization_contract_sha,
        evidence_type=evidence_type,
        generation=generation,
        predecessor_witness_sha256=predecessor_witness_sha,
    )
