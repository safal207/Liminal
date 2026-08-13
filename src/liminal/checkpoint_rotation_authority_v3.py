"""Provider-neutral consumer checkpoint v0.3 for portable rotation authority.

Historical checkpoint v0.2 binds registry-advance authority to a concrete GitHub
workflow signer. That remains valid historical behavior. v0.3 introduces a separate
schema where rotation authority is a logical contract and concrete signer/provider facts
remain external evidence about that authority.

This module performs no cryptographic verification. Migration and rotation evidence must
already be verified by callers. All semantic mismatches fail closed.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Any, Mapping

from liminal.recovery_trust_consumer_checkpoint_attested import (
    checkpoint_sha256 as legacy_checkpoint_sha256,
)
from liminal.recovery_trust_consumer_checkpoint_attested import (
    validate_checkpoint as validate_legacy_checkpoint,
)
from liminal.recovery_trust_root_registry import (
    canonical_json_bytes,
    evaluate_registry_rotation,
    sha256_hex,
    validate_registry,
)
from liminal.rotation_authority_portability import (
    RotationAuthorityObservation,
    rotation_authority_receipt_sha256,
)

CHECKPOINT_SCHEMA_VERSION_V3 = "liminal.recovery-trust-consumer-checkpoint.v0.3"
ROTATION_AUTHORITY_SCHEMA = "liminal.rotation-authority/v0.1"
MIGRATION_CLAIM_SCHEMA = "liminal-checkpoint-rotation-authority-migration-claim/v0.1"
AUTHORITY_ORIGIN_KIND = "legacy-rotation-signer-to-logical-authority"
AUTHORITY_MIGRATION_REASON = "upstream_rotation_authority_portability_migration"
_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")
_GIT_SHA_RE = re.compile(r"^[0-9a-f]{40}$")


@dataclass(frozen=True)
class CheckpointRotationAuthorityMigrationEvidence:
    """Externally verified mapping from legacy authorizer to logical rotation authority."""

    verified: bool
    trust_domain: str
    legacy_checkpoint_sha256: str
    legacy_authorizer_workflow_path: str
    legacy_authorizer_workflow_sha: str
    logical_rotation_id: str
    rotation_contract_sha256: str
    authorization_contract_sha256: str


@dataclass(frozen=True)
class PortableRotationAuthorityEvidence:
    """Provider-neutral verified claims authorizing one registry rotation."""

    verified: bool
    receipt_sha256: str
    logical_rotation_id: str
    rotation_contract_sha256: str
    authorization_contract_sha256: str
    previous_registry_sha256: str
    current_registry_sha256: str
    previous_manifest_sha256: str
    current_manifest_sha256: str
    from_generation: int
    to_generation: int
    rotation_reason: str


@dataclass(frozen=True)
class CheckpointRotationMigrationDecision:
    authorized: bool
    reason: str
    checkpoint: dict[str, Any] | None = None


@dataclass(frozen=True)
class PortableCheckpointDecisionV3:
    authorized: bool
    reason: str
    next_checkpoint: dict[str, Any] | None = None


def checkpoint_v3_sha256(payload: object) -> str:
    return sha256_hex(canonical_json_bytes(payload))


def _valid_sha256(value: object) -> bool:
    return isinstance(value, str) and _SHA256_RE.fullmatch(value) is not None


def _valid_git_sha(value: object) -> bool:
    return isinstance(value, str) and _GIT_SHA_RE.fullmatch(value) is not None


def _valid_rotation_authority(value: object) -> bool:
    if not isinstance(value, dict) or set(value) != {
        "schema",
        "logical_rotation_id",
        "rotation_contract_sha256",
        "authorization_contract_sha256",
    }:
        return False
    return (
        value.get("schema") == ROTATION_AUTHORITY_SCHEMA
        and isinstance(value.get("logical_rotation_id"), str)
        and bool(value.get("logical_rotation_id"))
        and _valid_sha256(value.get("rotation_contract_sha256"))
        and _valid_sha256(value.get("authorization_contract_sha256"))
    )


def _valid_authority_origin(value: object) -> bool:
    if not isinstance(value, dict) or set(value) != {
        "kind",
        "legacy_checkpoint_schema",
        "legacy_checkpoint_sha256",
        "legacy_authorizer_workflow_path",
        "legacy_authorizer_workflow_sha",
        "migration_reason",
        "migration_claim_sha256",
    }:
        return False
    return (
        value.get("kind") == AUTHORITY_ORIGIN_KIND
        and value.get("legacy_checkpoint_schema")
        == "liminal.recovery-trust-consumer-checkpoint.v0.2"
        and _valid_sha256(value.get("legacy_checkpoint_sha256"))
        and isinstance(value.get("legacy_authorizer_workflow_path"), str)
        and value.get("legacy_authorizer_workflow_path", "").startswith(
            ".github/workflows/"
        )
        and _valid_git_sha(value.get("legacy_authorizer_workflow_sha"))
        and value.get("migration_reason") == AUTHORITY_MIGRATION_REASON
        and _valid_sha256(value.get("migration_claim_sha256"))
    )


def _valid_accepted_rotation(value: object, generation: int) -> bool:
    if generation == 0:
        return value is None
    if not isinstance(value, dict) or set(value) != {
        "receipt_sha256",
        "previous_registry_sha256",
        "current_registry_sha256",
        "previous_manifest_sha256",
        "current_manifest_sha256",
        "from_generation",
        "to_generation",
        "rotation_reason",
    }:
        return False
    return (
        _valid_sha256(value.get("receipt_sha256"))
        and _valid_sha256(value.get("previous_registry_sha256"))
        and _valid_sha256(value.get("current_registry_sha256"))
        and _valid_sha256(value.get("previous_manifest_sha256"))
        and _valid_sha256(value.get("current_manifest_sha256"))
        and isinstance(value.get("from_generation"), int)
        and not isinstance(value.get("from_generation"), bool)
        and isinstance(value.get("to_generation"), int)
        and not isinstance(value.get("to_generation"), bool)
        and value.get("to_generation") == generation
        and value.get("from_generation") == generation - 1
        and value.get("rotation_reason") == "registry_rotation_authorized"
    )


def _valid_checkpoint_v3_body(payload: object) -> bool:
    if not isinstance(payload, dict) or set(payload) != {
        "schema_version",
        "trust_domain",
        "accepted_generation",
        "accepted_registry_sha256",
        "accepted_manifest_sha256",
        "previous_checkpoint_sha256",
        "rotation_authority",
        "authority_origin",
        "accepted_rotation",
    }:
        return False
    if payload.get("schema_version") != CHECKPOINT_SCHEMA_VERSION_V3:
        return False
    if not isinstance(payload.get("trust_domain"), str) or not payload.get("trust_domain"):
        return False
    generation = payload.get("accepted_generation")
    if not isinstance(generation, int) or isinstance(generation, bool) or generation < 0:
        return False
    if not _valid_sha256(payload.get("accepted_registry_sha256")):
        return False
    if not _valid_sha256(payload.get("accepted_manifest_sha256")):
        return False
    previous = payload.get("previous_checkpoint_sha256")
    if previous is not None and not _valid_sha256(previous):
        return False
    return (
        _valid_rotation_authority(payload.get("rotation_authority"))
        and _valid_authority_origin(payload.get("authority_origin"))
        and _valid_accepted_rotation(payload.get("accepted_rotation"), generation)
    )


def validate_checkpoint_v3(payload: object, previous_checkpoint: object | None = None) -> bool:
    if not _valid_checkpoint_v3_body(payload):
        return False
    assert isinstance(payload, dict)
    generation = payload["accepted_generation"]
    previous_digest = payload["previous_checkpoint_sha256"]
    if generation == 0:
        return previous_digest is None and previous_checkpoint is None
    if not _valid_checkpoint_v3_body(previous_checkpoint):
        return False
    assert isinstance(previous_checkpoint, dict)
    if previous_checkpoint["accepted_generation"] != generation - 1:
        return False
    if previous_checkpoint["trust_domain"] != payload["trust_domain"]:
        return False
    if previous_checkpoint["rotation_authority"] != payload["rotation_authority"]:
        return False
    if previous_checkpoint["authority_origin"] != payload["authority_origin"]:
        return False
    if previous_digest != checkpoint_v3_sha256(previous_checkpoint):
        return False
    accepted_rotation = payload["accepted_rotation"]
    assert isinstance(accepted_rotation, dict)
    return (
        accepted_rotation["previous_registry_sha256"]
        == previous_checkpoint["accepted_registry_sha256"]
        and accepted_rotation["previous_manifest_sha256"]
        == previous_checkpoint["accepted_manifest_sha256"]
        and accepted_rotation["current_registry_sha256"]
        == payload["accepted_registry_sha256"]
        and accepted_rotation["current_manifest_sha256"]
        == payload["accepted_manifest_sha256"]
    )


def _valid_migration_evidence(value: object) -> bool:
    if not isinstance(value, CheckpointRotationAuthorityMigrationEvidence):
        return False
    return (
        isinstance(value.verified, bool)
        and bool(value.trust_domain)
        and _valid_sha256(value.legacy_checkpoint_sha256)
        and isinstance(value.legacy_authorizer_workflow_path, str)
        and value.legacy_authorizer_workflow_path.startswith(".github/workflows/")
        and _valid_git_sha(value.legacy_authorizer_workflow_sha)
        and bool(value.logical_rotation_id)
        and _valid_sha256(value.rotation_contract_sha256)
        and _valid_sha256(value.authorization_contract_sha256)
    )


def migration_claim_sha256(
    evidence: CheckpointRotationAuthorityMigrationEvidence,
) -> str:
    if not _valid_migration_evidence(evidence):
        raise ValueError("migration_evidence_invalid")
    claim = {
        "schema": MIGRATION_CLAIM_SCHEMA,
        "trust_domain": evidence.trust_domain,
        "legacy_checkpoint_sha256": evidence.legacy_checkpoint_sha256,
        "legacy_authorizer_workflow_path": evidence.legacy_authorizer_workflow_path,
        "legacy_authorizer_workflow_sha": evidence.legacy_authorizer_workflow_sha,
        "logical_rotation_id": evidence.logical_rotation_id,
        "rotation_contract_sha256": evidence.rotation_contract_sha256,
        "authorization_contract_sha256": evidence.authorization_contract_sha256,
        "migration_reason": AUTHORITY_MIGRATION_REASON,
    }
    return sha256_hex(canonical_json_bytes(claim))


def migrate_legacy_genesis_checkpoint_to_v3(
    legacy_checkpoint: object,
    *,
    trust_domain: str,
    migration_evidence: CheckpointRotationAuthorityMigrationEvidence | None,
) -> CheckpointRotationMigrationDecision:
    if not trust_domain:
        return CheckpointRotationMigrationDecision(False, "trust_domain_invalid")
    if not validate_legacy_checkpoint(legacy_checkpoint):
        return CheckpointRotationMigrationDecision(False, "legacy_checkpoint_invalid")
    assert isinstance(legacy_checkpoint, dict)
    if legacy_checkpoint.get("accepted_generation") != 0:
        return CheckpointRotationMigrationDecision(False, "legacy_checkpoint_not_genesis")
    if not _valid_migration_evidence(migration_evidence):
        return CheckpointRotationMigrationDecision(False, "migration_evidence_invalid")
    assert migration_evidence is not None
    if not migration_evidence.verified:
        return CheckpointRotationMigrationDecision(False, "migration_evidence_unverified")
    if migration_evidence.trust_domain != trust_domain:
        return CheckpointRotationMigrationDecision(False, "migration_trust_domain_mismatch")

    legacy_digest = legacy_checkpoint_sha256(legacy_checkpoint)
    if migration_evidence.legacy_checkpoint_sha256 != legacy_digest:
        return CheckpointRotationMigrationDecision(False, "legacy_checkpoint_digest_mismatch")
    legacy_authorizer = legacy_checkpoint.get("advance_authorizer")
    if not isinstance(legacy_authorizer, dict):
        return CheckpointRotationMigrationDecision(False, "legacy_authorizer_missing")
    if (
        migration_evidence.legacy_authorizer_workflow_path
        != legacy_authorizer.get("workflow_path")
        or migration_evidence.legacy_authorizer_workflow_sha
        != legacy_authorizer.get("workflow_sha")
    ):
        return CheckpointRotationMigrationDecision(False, "legacy_authorizer_mismatch")

    checkpoint = {
        "schema_version": CHECKPOINT_SCHEMA_VERSION_V3,
        "trust_domain": trust_domain,
        "accepted_generation": 0,
        "accepted_registry_sha256": legacy_checkpoint["accepted_registry_sha256"],
        "accepted_manifest_sha256": legacy_checkpoint["accepted_manifest_sha256"],
        "previous_checkpoint_sha256": None,
        "rotation_authority": {
            "schema": ROTATION_AUTHORITY_SCHEMA,
            "logical_rotation_id": migration_evidence.logical_rotation_id,
            "rotation_contract_sha256": migration_evidence.rotation_contract_sha256,
            "authorization_contract_sha256": (
                migration_evidence.authorization_contract_sha256
            ),
        },
        "authority_origin": {
            "kind": AUTHORITY_ORIGIN_KIND,
            "legacy_checkpoint_schema": legacy_checkpoint["schema_version"],
            "legacy_checkpoint_sha256": legacy_digest,
            "legacy_authorizer_workflow_path": (
                migration_evidence.legacy_authorizer_workflow_path
            ),
            "legacy_authorizer_workflow_sha": (
                migration_evidence.legacy_authorizer_workflow_sha
            ),
            "migration_reason": AUTHORITY_MIGRATION_REASON,
            "migration_claim_sha256": migration_claim_sha256(migration_evidence),
        },
        "accepted_rotation": None,
    }
    if not validate_checkpoint_v3(checkpoint):
        return CheckpointRotationMigrationDecision(False, "migrated_checkpoint_invalid")
    return CheckpointRotationMigrationDecision(
        True, "checkpoint_rotation_authority_migrated", checkpoint
    )


def _valid_rotation_evidence(value: object) -> bool:
    if not isinstance(value, PortableRotationAuthorityEvidence):
        return False
    return (
        isinstance(value.verified, bool)
        and _valid_sha256(value.receipt_sha256)
        and bool(value.logical_rotation_id)
        and _valid_sha256(value.rotation_contract_sha256)
        and _valid_sha256(value.authorization_contract_sha256)
        and _valid_sha256(value.previous_registry_sha256)
        and _valid_sha256(value.current_registry_sha256)
        and _valid_sha256(value.previous_manifest_sha256)
        and _valid_sha256(value.current_manifest_sha256)
        and isinstance(value.from_generation, int)
        and not isinstance(value.from_generation, bool)
        and isinstance(value.to_generation, int)
        and not isinstance(value.to_generation, bool)
        and value.to_generation == value.from_generation + 1
        and value.rotation_reason == "registry_rotation_authorized"
    )


def _expected_receipt_sha256(value: PortableRotationAuthorityEvidence) -> str:
    observation = RotationAuthorityObservation(
        verified=True,
        rotation_producer_provider="portable-evidence",
        rotation_producer_instance_id="portable-evidence",
        control_plane_provider="portable-evidence",
        control_plane_id="portable-evidence",
        logical_rotation_id=value.logical_rotation_id,
        rotation_contract_sha256=value.rotation_contract_sha256,
        authorization_contract_sha256=value.authorization_contract_sha256,
        previous_registry_sha256=value.previous_registry_sha256,
        current_registry_sha256=value.current_registry_sha256,
        previous_manifest_sha256=value.previous_manifest_sha256,
        current_manifest_sha256=value.current_manifest_sha256,
        from_generation=value.from_generation,
        to_generation=value.to_generation,
        rotation_reason=value.rotation_reason,
    )
    return rotation_authority_receipt_sha256(observation)


def evaluate_registry_candidate_v3(
    trusted_checkpoint: object,
    candidate_registry: object,
    manifests: Mapping[str, object],
    *,
    rotation_evidence: PortableRotationAuthorityEvidence | None,
) -> PortableCheckpointDecisionV3:
    """Advance checkpoint v0.3 from already verified portable rotation authority."""

    if not _valid_checkpoint_v3_body(trusted_checkpoint):
        return PortableCheckpointDecisionV3(False, "trusted_checkpoint_invalid")
    assert isinstance(trusted_checkpoint, dict)
    if not validate_registry(candidate_registry, manifests):
        return PortableCheckpointDecisionV3(False, "candidate_registry_invalid")
    assert isinstance(candidate_registry, dict)

    trusted_generation = trusted_checkpoint["accepted_generation"]
    candidate_generation = candidate_registry["active_generation"]
    candidate_registry_sha = sha256_hex(canonical_json_bytes(candidate_registry))
    candidate_manifest_sha = candidate_registry["active_manifest_sha256"]

    if candidate_generation < trusted_generation:
        return PortableCheckpointDecisionV3(False, "stale_registry")
    if candidate_generation == trusted_generation:
        if (
            candidate_registry_sha == trusted_checkpoint["accepted_registry_sha256"]
            and candidate_manifest_sha == trusted_checkpoint["accepted_manifest_sha256"]
        ):
            return PortableCheckpointDecisionV3(
                True, "registry_already_accepted", trusted_checkpoint
            )
        return PortableCheckpointDecisionV3(False, "same_generation_registry_conflict")
    if candidate_generation != trusted_generation + 1:
        return PortableCheckpointDecisionV3(False, "registry_generation_gap")

    if not _valid_rotation_evidence(rotation_evidence):
        return PortableCheckpointDecisionV3(False, "rotation_authority_evidence_invalid")
    assert rotation_evidence is not None
    if not rotation_evidence.verified:
        return PortableCheckpointDecisionV3(False, "rotation_authority_evidence_unverified")

    authority = trusted_checkpoint["rotation_authority"]
    assert isinstance(authority, dict)
    if rotation_evidence.logical_rotation_id != authority["logical_rotation_id"]:
        return PortableCheckpointDecisionV3(False, "logical_rotation_mismatch")
    if rotation_evidence.rotation_contract_sha256 != authority["rotation_contract_sha256"]:
        return PortableCheckpointDecisionV3(False, "rotation_contract_mismatch")
    if (
        rotation_evidence.authorization_contract_sha256
        != authority["authorization_contract_sha256"]
    ):
        return PortableCheckpointDecisionV3(False, "rotation_authorization_contract_mismatch")
    if rotation_evidence.from_generation != trusted_generation:
        return PortableCheckpointDecisionV3(False, "rotation_from_generation_mismatch")
    if rotation_evidence.to_generation != candidate_generation:
        return PortableCheckpointDecisionV3(False, "rotation_to_generation_mismatch")
    if rotation_evidence.previous_registry_sha256 != trusted_checkpoint[
        "accepted_registry_sha256"
    ]:
        return PortableCheckpointDecisionV3(False, "rotation_previous_registry_mismatch")
    if rotation_evidence.previous_manifest_sha256 != trusted_checkpoint[
        "accepted_manifest_sha256"
    ]:
        return PortableCheckpointDecisionV3(False, "rotation_previous_manifest_mismatch")
    if rotation_evidence.current_registry_sha256 != candidate_registry_sha:
        return PortableCheckpointDecisionV3(False, "rotation_current_registry_mismatch")
    if rotation_evidence.current_manifest_sha256 != candidate_manifest_sha:
        return PortableCheckpointDecisionV3(False, "rotation_current_manifest_mismatch")
    if rotation_evidence.receipt_sha256 != _expected_receipt_sha256(rotation_evidence):
        return PortableCheckpointDecisionV3(False, "rotation_receipt_digest_mismatch")

    previous_history = candidate_registry["history"][:-1]
    if len(previous_history) != trusted_generation + 1:
        return PortableCheckpointDecisionV3(False, "candidate_history_prefix_invalid")
    previous_registry = {
        "schema_version": candidate_registry["schema_version"],
        "active_generation": trusted_generation,
        "active_manifest_sha256": previous_history[-1]["manifest_sha256"],
        "history": previous_history,
    }
    if sha256_hex(canonical_json_bytes(previous_registry)) != trusted_checkpoint[
        "accepted_registry_sha256"
    ]:
        return PortableCheckpointDecisionV3(False, "checkpoint_registry_prefix_mismatch")
    if previous_registry["active_manifest_sha256"] != trusted_checkpoint[
        "accepted_manifest_sha256"
    ]:
        return PortableCheckpointDecisionV3(False, "checkpoint_manifest_prefix_mismatch")

    rotation = evaluate_registry_rotation(previous_registry, candidate_registry, manifests)
    if not rotation.authorized:
        return PortableCheckpointDecisionV3(False, rotation.reason)

    next_checkpoint = {
        "schema_version": CHECKPOINT_SCHEMA_VERSION_V3,
        "trust_domain": trusted_checkpoint["trust_domain"],
        "accepted_generation": candidate_generation,
        "accepted_registry_sha256": candidate_registry_sha,
        "accepted_manifest_sha256": candidate_manifest_sha,
        "previous_checkpoint_sha256": checkpoint_v3_sha256(trusted_checkpoint),
        "rotation_authority": trusted_checkpoint["rotation_authority"],
        "authority_origin": trusted_checkpoint["authority_origin"],
        "accepted_rotation": {
            "receipt_sha256": rotation_evidence.receipt_sha256,
            "previous_registry_sha256": rotation_evidence.previous_registry_sha256,
            "current_registry_sha256": rotation_evidence.current_registry_sha256,
            "previous_manifest_sha256": rotation_evidence.previous_manifest_sha256,
            "current_manifest_sha256": rotation_evidence.current_manifest_sha256,
            "from_generation": rotation_evidence.from_generation,
            "to_generation": rotation_evidence.to_generation,
            "rotation_reason": rotation_evidence.rotation_reason,
        },
    }
    if not validate_checkpoint_v3(next_checkpoint, trusted_checkpoint):
        return PortableCheckpointDecisionV3(False, "next_checkpoint_invalid")
    return PortableCheckpointDecisionV3(
        True, "consumer_checkpoint_advanced", next_checkpoint
    )
