"""Witness v0.4 adapter for provider-neutral checkpoint v0.3.

Witness v0.3 remains immutable historical semantics and validates historical checkpoint
v0.2. v0.4 is a separate schema that explicitly migrates a generation-0 v0.3 witness
to the corresponding provider-neutral checkpoint-v0.3 genesis, then advances only over
checkpoint-v0.3 candidates.

Cryptographic verification remains external. All migration and checkpoint-producer
evidence must already be verified.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Any

from liminal.checkpoint_rotation_authority_v3 import (
    checkpoint_v3_sha256,
    validate_checkpoint_v3,
)
from liminal.checkpoint_witness_authority_v3 import (
    PortableCheckpointAuthorityEvidence,
    validate_witness_v3,
    witness_v3_sha256,
)
from liminal.recovery_trust_root_registry import canonical_json_bytes, sha256_hex

WITNESS_SCHEMA_VERSION_V4 = "liminal.recovery-trust-consumer-checkpoint-witness.v0.4"
CHECKPOINT_SCHEMA_MIGRATION = "liminal-witness-checkpoint-schema-migration-claim/v0.1"
CHECKPOINT_SCHEMA_MIGRATION_REASON = "checkpoint_v0_2_to_v0_3_authority_portability"
_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")


@dataclass(frozen=True)
class WitnessCheckpointSchemaMigrationEvidence:
    verified: bool
    trust_domain: str
    legacy_witness_v3_sha256: str
    legacy_checkpoint_sha256: str
    checkpoint_v3_sha256: str
    checkpoint_rotation_migration_claim_sha256: str


@dataclass(frozen=True)
class WitnessCheckpointSchemaMigrationDecision:
    authorized: bool
    reason: str
    witness: dict[str, Any] | None = None


@dataclass(frozen=True)
class CheckpointWitnessDecisionV4:
    authorized: bool
    reason: str
    next_witness: dict[str, Any] | None = None


def witness_v4_sha256(payload: object) -> str:
    return sha256_hex(canonical_json_bytes(payload))


def _valid_sha256(value: object) -> bool:
    return isinstance(value, str) and _SHA256_RE.fullmatch(value) is not None


def _valid_checkpoint_authority(value: object) -> bool:
    if not isinstance(value, dict) or set(value) != {
        "schema",
        "logical_producer_id",
        "producer_contract_sha256",
        "authorization_contract_sha256",
        "evidence_type",
    }:
        return False
    return (
        value.get("schema") == "liminal.checkpoint-authority/v0.1"
        and isinstance(value.get("logical_producer_id"), str)
        and bool(value.get("logical_producer_id"))
        and _valid_sha256(value.get("producer_contract_sha256"))
        and _valid_sha256(value.get("authorization_contract_sha256"))
        and isinstance(value.get("evidence_type"), str)
        and bool(value.get("evidence_type"))
    )


def _valid_migration_origin(value: object) -> bool:
    if not isinstance(value, dict) or set(value) != {
        "from_witness_schema",
        "from_witness_sha256",
        "from_checkpoint_sha256",
        "to_checkpoint_schema",
        "to_checkpoint_sha256",
        "checkpoint_rotation_migration_claim_sha256",
        "reason",
        "claim_sha256",
    }:
        return False
    return (
        value.get("from_witness_schema")
        == "liminal.recovery-trust-consumer-checkpoint-witness.v0.3"
        and _valid_sha256(value.get("from_witness_sha256"))
        and _valid_sha256(value.get("from_checkpoint_sha256"))
        and value.get("to_checkpoint_schema")
        == "liminal.recovery-trust-consumer-checkpoint.v0.3"
        and _valid_sha256(value.get("to_checkpoint_sha256"))
        and _valid_sha256(value.get("checkpoint_rotation_migration_claim_sha256"))
        and value.get("reason") == CHECKPOINT_SCHEMA_MIGRATION_REASON
        and _valid_sha256(value.get("claim_sha256"))
    )


def _valid_witness_v4_body(payload: object) -> bool:
    if not isinstance(payload, dict) or set(payload) != {
        "schema_version",
        "trust_domain",
        "observed_generation",
        "checkpoint_schema",
        "checkpoint_sha256",
        "accepted_registry_sha256",
        "accepted_manifest_sha256",
        "previous_witness_sha256",
        "checkpoint_authority",
        "authority_origin",
        "checkpoint_schema_migration",
    }:
        return False
    if payload.get("schema_version") != WITNESS_SCHEMA_VERSION_V4:
        return False
    if payload.get("checkpoint_schema") != "liminal.recovery-trust-consumer-checkpoint.v0.3":
        return False
    if not isinstance(payload.get("trust_domain"), str) or not payload.get("trust_domain"):
        return False
    generation = payload.get("observed_generation")
    if not isinstance(generation, int) or isinstance(generation, bool) or generation < 0:
        return False
    for key in ("checkpoint_sha256", "accepted_registry_sha256", "accepted_manifest_sha256"):
        if not _valid_sha256(payload.get(key)):
            return False
    previous = payload.get("previous_witness_sha256")
    if previous is not None and not _valid_sha256(previous):
        return False
    return (
        _valid_checkpoint_authority(payload.get("checkpoint_authority"))
        and isinstance(payload.get("authority_origin"), dict)
        and _valid_migration_origin(payload.get("checkpoint_schema_migration"))
    )


def validate_witness_v4(payload: object, previous_witness: object | None = None) -> bool:
    if not _valid_witness_v4_body(payload):
        return False
    assert isinstance(payload, dict)
    generation = payload["observed_generation"]
    if generation == 0:
        return payload["previous_witness_sha256"] is None and previous_witness is None
    if not _valid_witness_v4_body(previous_witness):
        return False
    assert isinstance(previous_witness, dict)
    if previous_witness["observed_generation"] != generation - 1:
        return False
    for key in (
        "trust_domain",
        "checkpoint_schema",
        "checkpoint_authority",
        "authority_origin",
        "checkpoint_schema_migration",
    ):
        if previous_witness[key] != payload[key]:
            return False
    return payload["previous_witness_sha256"] == witness_v4_sha256(previous_witness)


def _valid_migration_evidence(value: object) -> bool:
    if not isinstance(value, WitnessCheckpointSchemaMigrationEvidence):
        return False
    return (
        isinstance(value.verified, bool)
        and bool(value.trust_domain)
        and _valid_sha256(value.legacy_witness_v3_sha256)
        and _valid_sha256(value.legacy_checkpoint_sha256)
        and _valid_sha256(value.checkpoint_v3_sha256)
        and _valid_sha256(value.checkpoint_rotation_migration_claim_sha256)
    )


def checkpoint_schema_migration_claim_sha256(
    evidence: WitnessCheckpointSchemaMigrationEvidence,
) -> str:
    if not _valid_migration_evidence(evidence):
        raise ValueError("migration_evidence_invalid")
    claim = {
        "schema": CHECKPOINT_SCHEMA_MIGRATION,
        "trust_domain": evidence.trust_domain,
        "legacy_witness_v3_sha256": evidence.legacy_witness_v3_sha256,
        "legacy_checkpoint_sha256": evidence.legacy_checkpoint_sha256,
        "checkpoint_v3_sha256": evidence.checkpoint_v3_sha256,
        "checkpoint_rotation_migration_claim_sha256": (
            evidence.checkpoint_rotation_migration_claim_sha256
        ),
        "reason": CHECKPOINT_SCHEMA_MIGRATION_REASON,
    }
    return sha256_hex(canonical_json_bytes(claim))


def migrate_witness_v3_genesis_to_v4(
    witness_v3: object,
    checkpoint_v3: object,
    *,
    migration_evidence: WitnessCheckpointSchemaMigrationEvidence | None,
) -> WitnessCheckpointSchemaMigrationDecision:
    if not validate_witness_v3(witness_v3):
        return WitnessCheckpointSchemaMigrationDecision(False, "legacy_witness_v3_invalid")
    assert isinstance(witness_v3, dict)
    if witness_v3.get("observed_generation") != 0:
        return WitnessCheckpointSchemaMigrationDecision(False, "legacy_witness_v3_not_genesis")
    if not validate_checkpoint_v3(checkpoint_v3):
        return WitnessCheckpointSchemaMigrationDecision(False, "checkpoint_v3_invalid")
    assert isinstance(checkpoint_v3, dict)
    if checkpoint_v3.get("accepted_generation") != 0:
        return WitnessCheckpointSchemaMigrationDecision(False, "checkpoint_v3_not_genesis")
    if not _valid_migration_evidence(migration_evidence):
        return WitnessCheckpointSchemaMigrationDecision(False, "migration_evidence_invalid")
    assert migration_evidence is not None
    if not migration_evidence.verified:
        return WitnessCheckpointSchemaMigrationDecision(False, "migration_evidence_unverified")
    if migration_evidence.trust_domain != witness_v3["trust_domain"]:
        return WitnessCheckpointSchemaMigrationDecision(False, "migration_trust_domain_mismatch")
    if checkpoint_v3["trust_domain"] != witness_v3["trust_domain"]:
        return WitnessCheckpointSchemaMigrationDecision(False, "checkpoint_trust_domain_mismatch")

    legacy_witness_digest = witness_v3_sha256(witness_v3)
    if migration_evidence.legacy_witness_v3_sha256 != legacy_witness_digest:
        return WitnessCheckpointSchemaMigrationDecision(False, "legacy_witness_digest_mismatch")
    if migration_evidence.legacy_checkpoint_sha256 != witness_v3["checkpoint_sha256"]:
        return WitnessCheckpointSchemaMigrationDecision(False, "legacy_checkpoint_digest_mismatch")
    checkpoint_digest = checkpoint_v3_sha256(checkpoint_v3)
    if migration_evidence.checkpoint_v3_sha256 != checkpoint_digest:
        return WitnessCheckpointSchemaMigrationDecision(False, "checkpoint_v3_digest_mismatch")
    if checkpoint_v3["accepted_registry_sha256"] != witness_v3["accepted_registry_sha256"]:
        return WitnessCheckpointSchemaMigrationDecision(False, "registry_identity_mismatch")
    if checkpoint_v3["accepted_manifest_sha256"] != witness_v3["accepted_manifest_sha256"]:
        return WitnessCheckpointSchemaMigrationDecision(False, "manifest_identity_mismatch")

    checkpoint_origin = checkpoint_v3.get("authority_origin")
    if not isinstance(checkpoint_origin, dict):
        return WitnessCheckpointSchemaMigrationDecision(False, "checkpoint_authority_origin_missing")
    if (
        migration_evidence.checkpoint_rotation_migration_claim_sha256
        != checkpoint_origin.get("migration_claim_sha256")
    ):
        return WitnessCheckpointSchemaMigrationDecision(
            False, "checkpoint_rotation_migration_claim_mismatch"
        )

    witness = {
        "schema_version": WITNESS_SCHEMA_VERSION_V4,
        "trust_domain": witness_v3["trust_domain"],
        "observed_generation": 0,
        "checkpoint_schema": "liminal.recovery-trust-consumer-checkpoint.v0.3",
        "checkpoint_sha256": checkpoint_digest,
        "accepted_registry_sha256": checkpoint_v3["accepted_registry_sha256"],
        "accepted_manifest_sha256": checkpoint_v3["accepted_manifest_sha256"],
        "previous_witness_sha256": None,
        "checkpoint_authority": witness_v3["checkpoint_authority"],
        "authority_origin": witness_v3["authority_origin"],
        "checkpoint_schema_migration": {
            "from_witness_schema": witness_v3["schema_version"],
            "from_witness_sha256": legacy_witness_digest,
            "from_checkpoint_sha256": witness_v3["checkpoint_sha256"],
            "to_checkpoint_schema": checkpoint_v3["schema_version"],
            "to_checkpoint_sha256": checkpoint_digest,
            "checkpoint_rotation_migration_claim_sha256": (
                migration_evidence.checkpoint_rotation_migration_claim_sha256
            ),
            "reason": CHECKPOINT_SCHEMA_MIGRATION_REASON,
            "claim_sha256": checkpoint_schema_migration_claim_sha256(migration_evidence),
        },
    }
    if not validate_witness_v4(witness):
        return WitnessCheckpointSchemaMigrationDecision(False, "migrated_witness_invalid")
    return WitnessCheckpointSchemaMigrationDecision(
        True, "witness_checkpoint_schema_migrated", witness
    )


def _valid_checkpoint_authority_evidence(value: object) -> bool:
    if not isinstance(value, PortableCheckpointAuthorityEvidence):
        return False
    return (
        isinstance(value.verified, bool)
        and _valid_sha256(value.subject_sha256)
        and bool(value.logical_producer_id)
        and _valid_sha256(value.producer_contract_sha256)
        and _valid_sha256(value.authorization_contract_sha256)
        and bool(value.evidence_type)
        and isinstance(value.generation, int)
        and not isinstance(value.generation, bool)
        and value.generation >= 0
    )


def evaluate_checkpoint_candidate_v4(
    trusted_witness: object,
    candidate_checkpoint: object,
    *,
    previous_checkpoint: object | None,
    authority_evidence: PortableCheckpointAuthorityEvidence | None,
) -> CheckpointWitnessDecisionV4:
    if not _valid_witness_v4_body(trusted_witness):
        return CheckpointWitnessDecisionV4(False, "trusted_witness_invalid")
    assert isinstance(trusted_witness, dict)
    if not isinstance(candidate_checkpoint, dict):
        return CheckpointWitnessDecisionV4(False, "candidate_checkpoint_invalid")
    generation = candidate_checkpoint.get("accepted_generation")
    if not isinstance(generation, int) or isinstance(generation, bool):
        return CheckpointWitnessDecisionV4(False, "candidate_checkpoint_invalid")

    witnessed_generation = trusted_witness["observed_generation"]
    candidate_digest = checkpoint_v3_sha256(candidate_checkpoint)
    if generation < witnessed_generation:
        return CheckpointWitnessDecisionV4(False, "stale_checkpoint")
    if generation == witnessed_generation:
        if (
            candidate_digest == trusted_witness["checkpoint_sha256"]
            and candidate_checkpoint.get("accepted_registry_sha256")
            == trusted_witness["accepted_registry_sha256"]
            and candidate_checkpoint.get("accepted_manifest_sha256")
            == trusted_witness["accepted_manifest_sha256"]
        ):
            return CheckpointWitnessDecisionV4(
                True, "checkpoint_already_witnessed", trusted_witness
            )
        return CheckpointWitnessDecisionV4(False, "same_generation_checkpoint_conflict")
    if generation != witnessed_generation + 1:
        return CheckpointWitnessDecisionV4(False, "checkpoint_generation_gap")
    if not validate_checkpoint_v3(candidate_checkpoint, previous_checkpoint):
        return CheckpointWitnessDecisionV4(False, "candidate_checkpoint_invalid")
    if not isinstance(previous_checkpoint, dict):
        return CheckpointWitnessDecisionV4(False, "previous_checkpoint_missing")
    if checkpoint_v3_sha256(previous_checkpoint) != trusted_witness["checkpoint_sha256"]:
        return CheckpointWitnessDecisionV4(False, "witness_checkpoint_prefix_mismatch")

    if not _valid_checkpoint_authority_evidence(authority_evidence):
        return CheckpointWitnessDecisionV4(False, "checkpoint_authority_evidence_invalid")
    assert authority_evidence is not None
    if not authority_evidence.verified:
        return CheckpointWitnessDecisionV4(False, "checkpoint_authority_evidence_unverified")
    if authority_evidence.subject_sha256 != candidate_digest:
        return CheckpointWitnessDecisionV4(False, "checkpoint_authority_subject_mismatch")
    if authority_evidence.generation != generation:
        return CheckpointWitnessDecisionV4(False, "checkpoint_authority_generation_mismatch")

    authority = trusted_witness["checkpoint_authority"]
    assert isinstance(authority, dict)
    if authority_evidence.logical_producer_id != authority["logical_producer_id"]:
        return CheckpointWitnessDecisionV4(False, "logical_producer_mismatch")
    if authority_evidence.producer_contract_sha256 != authority["producer_contract_sha256"]:
        return CheckpointWitnessDecisionV4(False, "producer_contract_mismatch")
    if (
        authority_evidence.authorization_contract_sha256
        != authority["authorization_contract_sha256"]
    ):
        return CheckpointWitnessDecisionV4(False, "authorization_contract_mismatch")
    if authority_evidence.evidence_type != authority["evidence_type"]:
        return CheckpointWitnessDecisionV4(False, "evidence_type_mismatch")

    next_witness = {
        "schema_version": WITNESS_SCHEMA_VERSION_V4,
        "trust_domain": trusted_witness["trust_domain"],
        "observed_generation": generation,
        "checkpoint_schema": trusted_witness["checkpoint_schema"],
        "checkpoint_sha256": candidate_digest,
        "accepted_registry_sha256": candidate_checkpoint["accepted_registry_sha256"],
        "accepted_manifest_sha256": candidate_checkpoint["accepted_manifest_sha256"],
        "previous_witness_sha256": witness_v4_sha256(trusted_witness),
        "checkpoint_authority": trusted_witness["checkpoint_authority"],
        "authority_origin": trusted_witness["authority_origin"],
        "checkpoint_schema_migration": trusted_witness["checkpoint_schema_migration"],
    }
    if not validate_witness_v4(next_witness, trusted_witness):
        return CheckpointWitnessDecisionV4(False, "next_witness_invalid")
    return CheckpointWitnessDecisionV4(True, "checkpoint_witness_advanced", next_witness)
