"""Provider-neutral checkpoint witness authority v0.3.

The v0.1/v0.2 checkpoint witness schemas pin a concrete GitHub workflow signer.
That is correct historical behavior, but it cannot represent source-producer or
control-plane portability without pretending an external producer is the GitHub signer.

This module introduces a separate v0.3 witness schema. Its checkpoint authority is a
logical contract:

    logical_producer_id
    + producer_contract_sha256
    + authorization_contract_sha256
    + evidence_type

Concrete signer/provider identities remain external evidence about that authority. They
are never copied into the v0.3 authority identity.

The module performs no cryptographic verification. Callers must pass already-verified
migration and checkpoint-authority evidence. All mismatches fail closed.
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
    validate_witness as validate_legacy_witness,
)
from liminal.recovery_trust_consumer_checkpoint_witness import (
    witness_sha256 as legacy_witness_sha256,
)
from liminal.recovery_trust_root_registry import canonical_json_bytes, sha256_hex

WITNESS_SCHEMA_VERSION_V3 = "liminal.recovery-trust-consumer-checkpoint-witness.v0.3"
AUTHORITY_SCHEMA_VERSION = "liminal.checkpoint-authority/v0.1"
AUTHORITY_ORIGIN_KIND = "legacy-signer-to-logical-authority"
AUTHORITY_MIGRATION_REASON = "source_control_portability_authority_migration"
_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")
_GIT_SHA_RE = re.compile(r"^[0-9a-f]{40}$")


@dataclass(frozen=True)
class WitnessAuthorityMigrationEvidence:
    """Externally verified authorization to migrate a legacy genesis witness."""

    verified: bool
    legacy_witness_sha256: str
    legacy_signer_workflow_path: str
    legacy_signer_workflow_sha: str
    logical_producer_id: str
    producer_contract_sha256: str
    authorization_contract_sha256: str
    evidence_type: str
    migration_verification_sha256: str


@dataclass(frozen=True)
class PortableCheckpointAuthorityEvidence:
    """Provider-neutral claims authorizing one checkpoint candidate.

    Provider, signer and control-plane identities are intentionally absent. Those
    concrete facts belong to the external verification record that established this
    evidence, not to the logical authority consumed by the witness.
    """

    verified: bool
    subject_sha256: str
    logical_producer_id: str
    producer_contract_sha256: str
    authorization_contract_sha256: str
    evidence_type: str
    generation: int


@dataclass(frozen=True)
class WitnessAuthorityMigrationDecision:
    authorized: bool
    reason: str
    witness: dict[str, Any] | None = None


@dataclass(frozen=True)
class CheckpointWitnessDecisionV3:
    authorized: bool
    reason: str
    next_witness: dict[str, Any] | None = None


def witness_v3_sha256(payload: object) -> str:
    """Return SHA-256 of canonical v0.3 witness bytes."""

    return sha256_hex(canonical_json_bytes(payload))


def _valid_sha256(value: object) -> bool:
    return isinstance(value, str) and _SHA256_RE.fullmatch(value) is not None


def _valid_git_sha(value: object) -> bool:
    return isinstance(value, str) and _GIT_SHA_RE.fullmatch(value) is not None


def _valid_authority(value: object) -> bool:
    if not isinstance(value, dict) or set(value) != {
        "schema",
        "logical_producer_id",
        "producer_contract_sha256",
        "authorization_contract_sha256",
        "evidence_type",
    }:
        return False
    return (
        value.get("schema") == AUTHORITY_SCHEMA_VERSION
        and isinstance(value.get("logical_producer_id"), str)
        and bool(value.get("logical_producer_id"))
        and _valid_sha256(value.get("producer_contract_sha256"))
        and _valid_sha256(value.get("authorization_contract_sha256"))
        and isinstance(value.get("evidence_type"), str)
        and bool(value.get("evidence_type"))
    )


def _valid_authority_origin(value: object) -> bool:
    if not isinstance(value, dict) or set(value) != {
        "kind",
        "legacy_witness_schema",
        "legacy_witness_sha256",
        "legacy_signer_workflow_path",
        "legacy_signer_workflow_sha",
        "migration_reason",
        "migration_verification_sha256",
    }:
        return False
    return (
        value.get("kind") == AUTHORITY_ORIGIN_KIND
        and value.get("legacy_witness_schema")
        in {
            "liminal.recovery-trust-consumer-checkpoint-witness.v0.1",
            "liminal.recovery-trust-consumer-checkpoint-witness.v0.2",
        }
        and _valid_sha256(value.get("legacy_witness_sha256"))
        and isinstance(value.get("legacy_signer_workflow_path"), str)
        and value.get("legacy_signer_workflow_path", "").startswith(
            ".github/workflows/"
        )
        and _valid_git_sha(value.get("legacy_signer_workflow_sha"))
        and value.get("migration_reason") == AUTHORITY_MIGRATION_REASON
        and _valid_sha256(value.get("migration_verification_sha256"))
    )


def _valid_witness_v3_body(payload: object) -> bool:
    if not isinstance(payload, dict) or set(payload) != {
        "schema_version",
        "trust_domain",
        "observed_generation",
        "checkpoint_sha256",
        "accepted_registry_sha256",
        "accepted_manifest_sha256",
        "previous_witness_sha256",
        "checkpoint_authority",
        "authority_origin",
    }:
        return False
    if payload.get("schema_version") != WITNESS_SCHEMA_VERSION_V3:
        return False
    if not isinstance(payload.get("trust_domain"), str) or not payload.get("trust_domain"):
        return False
    generation = payload.get("observed_generation")
    if not isinstance(generation, int) or isinstance(generation, bool) or generation < 0:
        return False
    for key in (
        "checkpoint_sha256",
        "accepted_registry_sha256",
        "accepted_manifest_sha256",
    ):
        if not _valid_sha256(payload.get(key)):
            return False
    previous = payload.get("previous_witness_sha256")
    if previous is not None and not _valid_sha256(previous):
        return False
    return _valid_authority(payload.get("checkpoint_authority")) and _valid_authority_origin(
        payload.get("authority_origin")
    )


def validate_witness_v3(payload: object, previous_witness: object | None = None) -> bool:
    """Validate a v0.3 witness and its immediate v0.3 predecessor link."""

    if not _valid_witness_v3_body(payload):
        return False
    assert isinstance(payload, dict)
    generation = payload["observed_generation"]
    previous_digest = payload["previous_witness_sha256"]
    if generation == 0:
        return previous_digest is None and previous_witness is None
    if not _valid_witness_v3_body(previous_witness):
        return False
    assert isinstance(previous_witness, dict)
    if previous_witness["observed_generation"] != generation - 1:
        return False
    if previous_witness["trust_domain"] != payload["trust_domain"]:
        return False
    if previous_witness["checkpoint_authority"] != payload["checkpoint_authority"]:
        return False
    if previous_witness["authority_origin"] != payload["authority_origin"]:
        return False
    return previous_digest == witness_v3_sha256(previous_witness)


def _valid_migration_evidence(evidence: object) -> bool:
    if not isinstance(evidence, WitnessAuthorityMigrationEvidence):
        return False
    return (
        isinstance(evidence.verified, bool)
        and _valid_sha256(evidence.legacy_witness_sha256)
        and isinstance(evidence.legacy_signer_workflow_path, str)
        and evidence.legacy_signer_workflow_path.startswith(".github/workflows/")
        and _valid_git_sha(evidence.legacy_signer_workflow_sha)
        and bool(evidence.logical_producer_id)
        and _valid_sha256(evidence.producer_contract_sha256)
        and _valid_sha256(evidence.authorization_contract_sha256)
        and bool(evidence.evidence_type)
        and _valid_sha256(evidence.migration_verification_sha256)
    )


def migrate_legacy_genesis_witness_to_v3(
    legacy_witness: object,
    *,
    trust_domain: str,
    migration_evidence: WitnessAuthorityMigrationEvidence | None,
) -> WitnessAuthorityMigrationDecision:
    """Create a v0.3 genesis only from an explicitly verified legacy mapping.

    The migration is intentionally limited to a legacy generation-0 witness. Later
    generations must remain linked through normal v0.3 predecessor hashes rather than
    being independently re-rooted into the new schema.
    """

    if not isinstance(trust_domain, str) or not trust_domain:
        return WitnessAuthorityMigrationDecision(False, "trust_domain_invalid")
    if not validate_legacy_witness(legacy_witness):
        return WitnessAuthorityMigrationDecision(False, "legacy_witness_invalid")
    assert isinstance(legacy_witness, dict)
    if legacy_witness.get("observed_generation") != 0:
        return WitnessAuthorityMigrationDecision(False, "legacy_witness_not_genesis")
    if not _valid_migration_evidence(migration_evidence):
        return WitnessAuthorityMigrationDecision(False, "migration_evidence_invalid")
    assert migration_evidence is not None
    if not migration_evidence.verified:
        return WitnessAuthorityMigrationDecision(False, "migration_evidence_unverified")

    legacy_digest = legacy_witness_sha256(legacy_witness)
    if migration_evidence.legacy_witness_sha256 != legacy_digest:
        return WitnessAuthorityMigrationDecision(False, "legacy_witness_digest_mismatch")

    legacy_signer = legacy_witness.get("checkpoint_signer")
    if not isinstance(legacy_signer, dict):
        return WitnessAuthorityMigrationDecision(False, "legacy_signer_missing")
    if (
        migration_evidence.legacy_signer_workflow_path
        != legacy_signer.get("workflow_path")
        or migration_evidence.legacy_signer_workflow_sha
        != legacy_signer.get("workflow_sha")
    ):
        return WitnessAuthorityMigrationDecision(False, "legacy_signer_mismatch")

    witness = {
        "schema_version": WITNESS_SCHEMA_VERSION_V3,
        "trust_domain": trust_domain,
        "observed_generation": 0,
        "checkpoint_sha256": legacy_witness["checkpoint_sha256"],
        "accepted_registry_sha256": legacy_witness["accepted_registry_sha256"],
        "accepted_manifest_sha256": legacy_witness["accepted_manifest_sha256"],
        "previous_witness_sha256": None,
        "checkpoint_authority": {
            "schema": AUTHORITY_SCHEMA_VERSION,
            "logical_producer_id": migration_evidence.logical_producer_id,
            "producer_contract_sha256": migration_evidence.producer_contract_sha256,
            "authorization_contract_sha256": (
                migration_evidence.authorization_contract_sha256
            ),
            "evidence_type": migration_evidence.evidence_type,
        },
        "authority_origin": {
            "kind": AUTHORITY_ORIGIN_KIND,
            "legacy_witness_schema": legacy_witness["schema_version"],
            "legacy_witness_sha256": legacy_digest,
            "legacy_signer_workflow_path": (
                migration_evidence.legacy_signer_workflow_path
            ),
            "legacy_signer_workflow_sha": migration_evidence.legacy_signer_workflow_sha,
            "migration_reason": AUTHORITY_MIGRATION_REASON,
            "migration_verification_sha256": (
                migration_evidence.migration_verification_sha256
            ),
        },
    }
    if not validate_witness_v3(witness):
        return WitnessAuthorityMigrationDecision(False, "migrated_witness_invalid")
    return WitnessAuthorityMigrationDecision(True, "witness_authority_migrated", witness)


def _valid_checkpoint_authority_evidence(evidence: object) -> bool:
    if not isinstance(evidence, PortableCheckpointAuthorityEvidence):
        return False
    return (
        isinstance(evidence.verified, bool)
        and _valid_sha256(evidence.subject_sha256)
        and bool(evidence.logical_producer_id)
        and _valid_sha256(evidence.producer_contract_sha256)
        and _valid_sha256(evidence.authorization_contract_sha256)
        and bool(evidence.evidence_type)
        and isinstance(evidence.generation, int)
        and not isinstance(evidence.generation, bool)
        and evidence.generation >= 0
    )


def evaluate_checkpoint_candidate_v3(
    trusted_witness: object,
    candidate_checkpoint: object,
    *,
    previous_checkpoint: object | None,
    authority_evidence: PortableCheckpointAuthorityEvidence | None,
) -> CheckpointWitnessDecisionV3:
    """Evaluate one checkpoint against provider-neutral logical authority."""

    if not _valid_witness_v3_body(trusted_witness):
        return CheckpointWitnessDecisionV3(False, "trusted_witness_invalid")
    assert isinstance(trusted_witness, dict)

    if not isinstance(candidate_checkpoint, dict):
        return CheckpointWitnessDecisionV3(False, "candidate_checkpoint_invalid")
    generation = candidate_checkpoint.get("accepted_generation")
    if not isinstance(generation, int) or isinstance(generation, bool):
        return CheckpointWitnessDecisionV3(False, "candidate_checkpoint_invalid")

    witnessed_generation = trusted_witness["observed_generation"]
    candidate_digest = checkpoint_sha256(candidate_checkpoint)

    if generation < witnessed_generation:
        return CheckpointWitnessDecisionV3(False, "stale_checkpoint")

    if generation == witnessed_generation:
        if (
            candidate_digest == trusted_witness["checkpoint_sha256"]
            and candidate_checkpoint.get("accepted_registry_sha256")
            == trusted_witness["accepted_registry_sha256"]
            and candidate_checkpoint.get("accepted_manifest_sha256")
            == trusted_witness["accepted_manifest_sha256"]
        ):
            return CheckpointWitnessDecisionV3(
                True,
                "checkpoint_already_witnessed",
                trusted_witness,
            )
        return CheckpointWitnessDecisionV3(False, "same_generation_checkpoint_conflict")

    if generation != witnessed_generation + 1:
        return CheckpointWitnessDecisionV3(False, "checkpoint_generation_gap")
    if not validate_checkpoint(candidate_checkpoint, previous_checkpoint):
        return CheckpointWitnessDecisionV3(False, "candidate_checkpoint_invalid")
    if not isinstance(previous_checkpoint, dict):
        return CheckpointWitnessDecisionV3(False, "previous_checkpoint_missing")
    if checkpoint_sha256(previous_checkpoint) != trusted_witness["checkpoint_sha256"]:
        return CheckpointWitnessDecisionV3(False, "witness_checkpoint_prefix_mismatch")

    if not _valid_checkpoint_authority_evidence(authority_evidence):
        return CheckpointWitnessDecisionV3(False, "checkpoint_authority_evidence_invalid")
    assert authority_evidence is not None
    if not authority_evidence.verified:
        return CheckpointWitnessDecisionV3(False, "checkpoint_authority_evidence_unverified")
    if authority_evidence.subject_sha256 != candidate_digest:
        return CheckpointWitnessDecisionV3(False, "checkpoint_authority_subject_mismatch")
    if authority_evidence.generation != generation:
        return CheckpointWitnessDecisionV3(False, "checkpoint_authority_generation_mismatch")

    authority = trusted_witness["checkpoint_authority"]
    assert isinstance(authority, dict)
    if authority_evidence.logical_producer_id != authority["logical_producer_id"]:
        return CheckpointWitnessDecisionV3(False, "logical_producer_mismatch")
    if authority_evidence.producer_contract_sha256 != authority["producer_contract_sha256"]:
        return CheckpointWitnessDecisionV3(False, "producer_contract_mismatch")
    if (
        authority_evidence.authorization_contract_sha256
        != authority["authorization_contract_sha256"]
    ):
        return CheckpointWitnessDecisionV3(False, "authorization_contract_mismatch")
    if authority_evidence.evidence_type != authority["evidence_type"]:
        return CheckpointWitnessDecisionV3(False, "evidence_type_mismatch")

    next_witness = {
        "schema_version": WITNESS_SCHEMA_VERSION_V3,
        "trust_domain": trusted_witness["trust_domain"],
        "observed_generation": generation,
        "checkpoint_sha256": candidate_digest,
        "accepted_registry_sha256": candidate_checkpoint["accepted_registry_sha256"],
        "accepted_manifest_sha256": candidate_checkpoint["accepted_manifest_sha256"],
        "previous_witness_sha256": witness_v3_sha256(trusted_witness),
        "checkpoint_authority": dict(authority),
        "authority_origin": dict(trusted_witness["authority_origin"]),
    }
    if not validate_witness_v3(next_witness, trusted_witness):
        return CheckpointWitnessDecisionV3(False, "next_witness_invalid")
    return CheckpointWitnessDecisionV3(True, "checkpoint_witness_advanced", next_witness)
