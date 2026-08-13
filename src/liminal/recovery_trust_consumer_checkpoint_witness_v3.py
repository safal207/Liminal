"""Provider-neutral checkpoint witness authority v0.3.

v0.1/v0.2 witnesses bind checkpoint authority to a concrete GitHub workflow path
and commit SHA. That is intentionally preserved as historical evidence, but it cannot
serve as the portable authority identity for producer/control-plane portability.

v0.3 replaces the concrete signer identity with a logical producer authority contract:

    logical_producer_id
    + producer_contract_sha256
    + authorization_contract_sha256
    + evidence_type

Concrete signers/providers remain evidence *about* that authority and are verified
outside this module. Migration from a v0.2 witness is fail-closed and requires an
explicit verified migration record bound to the exact predecessor witness digest.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Any

from liminal.recovery_trust_consumer_checkpoint_attested import (
    checkpoint_sha256,
    validate_checkpoint,
)
from liminal.recovery_trust_consumer_checkpoint_witness import (
    WITNESS_SCHEMA_VERSION_V2,
    validate_witness,
    witness_sha256 as legacy_witness_sha256,
)
from liminal.recovery_trust_root_registry import canonical_json_bytes, sha256_hex

WITNESS_SCHEMA_VERSION_V3 = "liminal.recovery-trust-consumer-checkpoint-witness.v0.3"
_AUTHORITY_MIGRATION_REASON = "concrete_signer_to_logical_producer_authority"
_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")


@dataclass(frozen=True)
class PortableCheckpointAuthority:
    logical_producer_id: str
    producer_contract_sha256: str
    authorization_contract_sha256: str
    evidence_type: str


@dataclass(frozen=True)
class VerifiedAuthorityMigrationEvidence:
    verified: bool
    previous_witness_sha256: str
    authority: PortableCheckpointAuthority
    reason: str = _AUTHORITY_MIGRATION_REASON


@dataclass(frozen=True)
class VerifiedPortableCheckpointEvidence:
    verified: bool
    subject_sha256: str
    logical_producer_id: str
    producer_contract_sha256: str
    authorization_contract_sha256: str
    evidence_type: str


@dataclass(frozen=True)
class PortableCheckpointWitnessDecision:
    authorized: bool
    reason: str
    next_witness: dict[str, Any] | None = None


def _valid_sha256(value: object) -> bool:
    return isinstance(value, str) and _SHA256_RE.fullmatch(value) is not None


def validate_portable_checkpoint_authority(authority: object) -> bool:
    if not isinstance(authority, PortableCheckpointAuthority):
        return False
    return (
        bool(authority.logical_producer_id)
        and _valid_sha256(authority.producer_contract_sha256)
        and _valid_sha256(authority.authorization_contract_sha256)
        and bool(authority.evidence_type)
    )


def _authority_dict(authority: PortableCheckpointAuthority) -> dict[str, str]:
    return {
        "logical_producer_id": authority.logical_producer_id,
        "producer_contract_sha256": authority.producer_contract_sha256,
        "authorization_contract_sha256": authority.authorization_contract_sha256,
        "evidence_type": authority.evidence_type,
    }


def _valid_authority_dict(value: object) -> bool:
    if not isinstance(value, dict) or set(value) != {
        "logical_producer_id",
        "producer_contract_sha256",
        "authorization_contract_sha256",
        "evidence_type",
    }:
        return False
    return (
        isinstance(value.get("logical_producer_id"), str)
        and bool(value["logical_producer_id"])
        and _valid_sha256(value.get("producer_contract_sha256"))
        and _valid_sha256(value.get("authorization_contract_sha256"))
        and isinstance(value.get("evidence_type"), str)
        and bool(value["evidence_type"])
    )


def witness_v3_sha256(payload: object) -> str:
    return sha256_hex(canonical_json_bytes(payload))


def validate_witness_v3(payload: object, previous_witness: object | None = None) -> bool:
    if not isinstance(payload, dict):
        return False
    expected = {
        "schema_version",
        "trust_domain",
        "observed_generation",
        "checkpoint_sha256",
        "accepted_registry_sha256",
        "accepted_manifest_sha256",
        "previous_witness_sha256",
        "checkpoint_authority",
        "authority_migration",
    }
    if set(payload) != expected or payload.get("schema_version") != WITNESS_SCHEMA_VERSION_V3:
        return False
    if not isinstance(payload.get("trust_domain"), str) or not payload["trust_domain"]:
        return False
    generation = payload.get("observed_generation")
    if not isinstance(generation, int) or isinstance(generation, bool) or generation < 0:
        return False
    for key in ("checkpoint_sha256", "accepted_registry_sha256", "accepted_manifest_sha256"):
        if not _valid_sha256(payload.get(key)):
            return False
    if not _valid_authority_dict(payload.get("checkpoint_authority")):
        return False
    migration = payload.get("authority_migration")
    if not isinstance(migration, dict) or set(migration) != {
        "from_schema",
        "from_witness_sha256",
        "reason",
    }:
        return False
    if migration.get("from_schema") != WITNESS_SCHEMA_VERSION_V2:
        return False
    if not _valid_sha256(migration.get("from_witness_sha256")):
        return False
    if migration.get("reason") != _AUTHORITY_MIGRATION_REASON:
        return False

    previous_digest = payload.get("previous_witness_sha256")
    if generation == 0:
        return previous_digest is None and previous_witness is None
    if not _valid_sha256(previous_digest):
        return False
    if previous_witness is None:
        return False
    if isinstance(previous_witness, dict) and previous_witness.get("schema_version") == WITNESS_SCHEMA_VERSION_V3:
        if not validate_witness_v3(previous_witness):
            return False
        if previous_witness["observed_generation"] != generation - 1:
            return False
        return previous_digest == witness_v3_sha256(previous_witness)
    return False


def migrate_witness_v2_to_v3(
    trusted_witness_v2: object,
    previous_witness_v2: object | None,
    *,
    trust_domain: str,
    migration_evidence: VerifiedAuthorityMigrationEvidence,
) -> PortableCheckpointWitnessDecision:
    """Create a v0.3 witness only from a valid v0.2 witness plus verified migration evidence."""

    if not validate_witness(trusted_witness_v2, previous_witness_v2):
        return PortableCheckpointWitnessDecision(False, "trusted_witness_v2_invalid")
    if not trust_domain:
        return PortableCheckpointWitnessDecision(False, "trust_domain_invalid")
    if not migration_evidence.verified:
        return PortableCheckpointWitnessDecision(False, "authority_migration_unverified")
    if migration_evidence.reason != _AUTHORITY_MIGRATION_REASON:
        return PortableCheckpointWitnessDecision(False, "authority_migration_reason_invalid")
    if not validate_portable_checkpoint_authority(migration_evidence.authority):
        return PortableCheckpointWitnessDecision(False, "checkpoint_authority_invalid")
    assert isinstance(trusted_witness_v2, dict)
    predecessor_digest = legacy_witness_sha256(trusted_witness_v2)
    if migration_evidence.previous_witness_sha256 != predecessor_digest:
        return PortableCheckpointWitnessDecision(False, "authority_migration_predecessor_mismatch")

    migrated = {
        "schema_version": WITNESS_SCHEMA_VERSION_V3,
        "trust_domain": trust_domain,
        "observed_generation": trusted_witness_v2["observed_generation"],
        "checkpoint_sha256": trusted_witness_v2["checkpoint_sha256"],
        "accepted_registry_sha256": trusted_witness_v2["accepted_registry_sha256"],
        "accepted_manifest_sha256": trusted_witness_v2["accepted_manifest_sha256"],
        "previous_witness_sha256": None,
        "checkpoint_authority": _authority_dict(migration_evidence.authority),
        "authority_migration": {
            "from_schema": WITNESS_SCHEMA_VERSION_V2,
            "from_witness_sha256": predecessor_digest,
            "reason": _AUTHORITY_MIGRATION_REASON,
        },
    }
    if not validate_witness_v3(migrated):
        return PortableCheckpointWitnessDecision(False, "migrated_witness_invalid")
    return PortableCheckpointWitnessDecision(True, "witness_authority_migrated", migrated)


def evaluate_portable_checkpoint_candidate(
    trusted_witness: object,
    candidate_checkpoint: object,
    *,
    previous_checkpoint: object | None,
    checkpoint_evidence: VerifiedPortableCheckpointEvidence | None,
) -> PortableCheckpointWitnessDecision:
    """Advance a v0.3 witness using logical producer authority rather than concrete signer identity."""

    if not validate_witness_v3(trusted_witness):
        return PortableCheckpointWitnessDecision(False, "trusted_witness_invalid")
    assert isinstance(trusted_witness, dict)
    if not isinstance(candidate_checkpoint, dict):
        return PortableCheckpointWitnessDecision(False, "candidate_checkpoint_invalid")
    generation = candidate_checkpoint.get("accepted_generation")
    if not isinstance(generation, int) or isinstance(generation, bool):
        return PortableCheckpointWitnessDecision(False, "candidate_checkpoint_invalid")

    witnessed_generation = trusted_witness["observed_generation"]
    candidate_digest = checkpoint_sha256(candidate_checkpoint)
    if generation < witnessed_generation:
        return PortableCheckpointWitnessDecision(False, "stale_checkpoint")
    if generation == witnessed_generation:
        if (
            candidate_digest == trusted_witness["checkpoint_sha256"]
            and candidate_checkpoint.get("accepted_registry_sha256")
            == trusted_witness["accepted_registry_sha256"]
            and candidate_checkpoint.get("accepted_manifest_sha256")
            == trusted_witness["accepted_manifest_sha256"]
        ):
            return PortableCheckpointWitnessDecision(True, "checkpoint_already_witnessed", trusted_witness)
        return PortableCheckpointWitnessDecision(False, "same_generation_checkpoint_conflict")
    if generation != witnessed_generation + 1:
        return PortableCheckpointWitnessDecision(False, "checkpoint_generation_gap")
    if not validate_checkpoint(candidate_checkpoint, previous_checkpoint):
        return PortableCheckpointWitnessDecision(False, "candidate_checkpoint_invalid")
    if not isinstance(previous_checkpoint, dict):
        return PortableCheckpointWitnessDecision(False, "previous_checkpoint_missing")
    if checkpoint_sha256(previous_checkpoint) != trusted_witness["checkpoint_sha256"]:
        return PortableCheckpointWitnessDecision(False, "witness_checkpoint_prefix_mismatch")
    if checkpoint_evidence is None or not checkpoint_evidence.verified:
        return PortableCheckpointWitnessDecision(False, "checkpoint_evidence_unverified")
    if checkpoint_evidence.subject_sha256 != candidate_digest:
        return PortableCheckpointWitnessDecision(False, "checkpoint_subject_digest_mismatch")

    expected = trusted_witness["checkpoint_authority"]
    comparisons = (
        (checkpoint_evidence.logical_producer_id, expected["logical_producer_id"], "logical_producer_mismatch"),
        (checkpoint_evidence.producer_contract_sha256, expected["producer_contract_sha256"], "producer_contract_mismatch"),
        (
            checkpoint_evidence.authorization_contract_sha256,
            expected["authorization_contract_sha256"],
            "authorization_contract_mismatch",
        ),
        (checkpoint_evidence.evidence_type, expected["evidence_type"], "evidence_type_mismatch"),
    )
    for actual, required, reason in comparisons:
        if actual != required:
            return PortableCheckpointWitnessDecision(False, reason)

    next_witness = {
        "schema_version": WITNESS_SCHEMA_VERSION_V3,
        "trust_domain": trusted_witness["trust_domain"],
        "observed_generation": generation,
        "checkpoint_sha256": candidate_digest,
        "accepted_registry_sha256": candidate_checkpoint["accepted_registry_sha256"],
        "accepted_manifest_sha256": candidate_checkpoint["accepted_manifest_sha256"],
        "previous_witness_sha256": witness_v3_sha256(trusted_witness),
        "checkpoint_authority": dict(expected),
        "authority_migration": dict(trusted_witness["authority_migration"]),
    }
    if not validate_witness_v3(next_witness, trusted_witness):
        return PortableCheckpointWitnessDecision(False, "next_witness_invalid")
    return PortableCheckpointWitnessDecision(True, "checkpoint_witness_advanced", next_witness)
