"""Attested monotonic consumer checkpoint for accepted trust-root registry state."""

from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Any, Mapping

from liminal.recovery_trust_root_registry import (
    canonical_json_bytes,
    evaluate_registry_rotation,
    sha256_hex,
    validate_registry,
)


CHECKPOINT_SCHEMA_VERSION = "liminal.recovery-trust-consumer-checkpoint.v0.2"
_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")
_GIT_SHA_RE = re.compile(r"^[0-9a-f]{40}$")


@dataclass(frozen=True)
class VerifiedRegistryAdvanceEvidence:
    """Cryptographically verified authorization evidence for one registry advance."""

    verified: bool
    evidence_kind: str
    signer_workflow_path: str
    signer_workflow_sha: str
    subject_sha256: str
    candidate_registry_sha256: str
    candidate_manifest_sha256: str
    rotation_authorized: bool
    rotation_reason: str


@dataclass(frozen=True)
class AttestedConsumerCheckpointDecision:
    """Machine-readable decision for one candidate registry presentation."""

    authorized: bool
    reason: str
    next_checkpoint: dict[str, Any] | None = None


def checkpoint_sha256(payload: object) -> str:
    """Return the digest of canonical checkpoint bytes."""

    return sha256_hex(canonical_json_bytes(payload))


def _valid_signer(value: object) -> bool:
    if not isinstance(value, dict) or set(value) != {"workflow_path", "workflow_sha"}:
        return False
    path = value.get("workflow_path")
    sha = value.get("workflow_sha")
    return (
        isinstance(path, str)
        and path.startswith(".github/workflows/")
        and isinstance(sha, str)
        and _GIT_SHA_RE.fullmatch(sha) is not None
    )


def _valid_evidence(value: object) -> bool:
    if not isinstance(value, dict) or set(value) != {
        "kind",
        "subject_sha256",
        "signer_workflow_path",
        "signer_workflow_sha",
    }:
        return False
    if value.get("kind") not in {"registry_attestation", "rotation_authorization"}:
        return False
    subject = value.get("subject_sha256")
    path = value.get("signer_workflow_path")
    sha = value.get("signer_workflow_sha")
    return (
        isinstance(subject, str)
        and _SHA256_RE.fullmatch(subject) is not None
        and isinstance(path, str)
        and path.startswith(".github/workflows/")
        and isinstance(sha, str)
        and _GIT_SHA_RE.fullmatch(sha) is not None
    )


def _validate_checkpoint_body(payload: object) -> bool:
    if not isinstance(payload, dict) or set(payload) != {
        "schema_version",
        "repository",
        "accepted_generation",
        "accepted_registry_sha256",
        "accepted_manifest_sha256",
        "previous_checkpoint_sha256",
        "accepted_evidence",
        "advance_authorizer",
    }:
        return False
    if payload.get("schema_version") != CHECKPOINT_SCHEMA_VERSION:
        return False
    if payload.get("repository") != "safal207/Liminal":
        return False

    generation = payload.get("accepted_generation")
    if not isinstance(generation, int) or isinstance(generation, bool) or generation < 0:
        return False
    for name in ("accepted_registry_sha256", "accepted_manifest_sha256"):
        value = payload.get(name)
        if not isinstance(value, str) or _SHA256_RE.fullmatch(value) is None:
            return False

    previous_digest = payload.get("previous_checkpoint_sha256")
    if previous_digest is not None and (
        not isinstance(previous_digest, str) or _SHA256_RE.fullmatch(previous_digest) is None
    ):
        return False
    if not _valid_evidence(payload.get("accepted_evidence")):
        return False
    if not _valid_signer(payload.get("advance_authorizer")):
        return False

    accepted_evidence = payload["accepted_evidence"]
    if generation == 0:
        return (
            accepted_evidence["kind"] == "registry_attestation"
            and accepted_evidence["subject_sha256"] == payload["accepted_registry_sha256"]
        )
    return accepted_evidence["kind"] == "rotation_authorization"


def validate_checkpoint(payload: object, previous_checkpoint: object | None = None) -> bool:
    """Validate a checkpoint and its immediate predecessor link."""

    if not _validate_checkpoint_body(payload):
        return False
    assert isinstance(payload, dict)
    generation = payload["accepted_generation"]
    previous_digest = payload["previous_checkpoint_sha256"]
    if generation == 0:
        return previous_digest is None and previous_checkpoint is None

    if not _validate_checkpoint_body(previous_checkpoint):
        return False
    assert isinstance(previous_checkpoint, dict)
    if previous_checkpoint["accepted_generation"] != generation - 1:
        return False
    return previous_digest == checkpoint_sha256(previous_checkpoint)


def evaluate_candidate(
    trusted_checkpoint: object,
    candidate_registry: object,
    manifests: Mapping[str, object],
    *,
    advance_evidence: VerifiedRegistryAdvanceEvidence | None,
) -> AttestedConsumerCheckpointDecision:
    """Evaluate one registry against the consumer's last trusted checkpoint.

    Same-generation replay of exactly the accepted bytes is idempotently accepted. Older
    generations are rejected before any new evidence is considered. Advancing by one generation
    requires cryptographically verified rotation-authorization evidence whose subject binds the
    exact candidate registry and manifest digests.
    """

    if not _validate_checkpoint_body(trusted_checkpoint):
        return AttestedConsumerCheckpointDecision(False, "trusted_checkpoint_invalid")
    assert isinstance(trusted_checkpoint, dict)

    if not validate_registry(candidate_registry, manifests):
        return AttestedConsumerCheckpointDecision(False, "candidate_registry_invalid")
    assert isinstance(candidate_registry, dict)

    trusted_generation = trusted_checkpoint["accepted_generation"]
    candidate_generation = candidate_registry["active_generation"]
    candidate_registry_digest = sha256_hex(canonical_json_bytes(candidate_registry))
    candidate_manifest_digest = candidate_registry["active_manifest_sha256"]

    if candidate_generation < trusted_generation:
        return AttestedConsumerCheckpointDecision(False, "stale_registry")

    if candidate_generation == trusted_generation:
        if (
            candidate_registry_digest == trusted_checkpoint["accepted_registry_sha256"]
            and candidate_manifest_digest == trusted_checkpoint["accepted_manifest_sha256"]
        ):
            return AttestedConsumerCheckpointDecision(
                True, "registry_already_accepted", dict(trusted_checkpoint)
            )
        return AttestedConsumerCheckpointDecision(False, "same_generation_registry_conflict")

    if candidate_generation != trusted_generation + 1:
        return AttestedConsumerCheckpointDecision(False, "registry_generation_gap")

    if advance_evidence is None or not advance_evidence.verified:
        return AttestedConsumerCheckpointDecision(False, "rotation_evidence_unverified")
    if advance_evidence.evidence_kind != "rotation_authorization":
        return AttestedConsumerCheckpointDecision(False, "rotation_evidence_kind_invalid")

    expected_authorizer = trusted_checkpoint["advance_authorizer"]
    if (
        advance_evidence.signer_workflow_path != expected_authorizer["workflow_path"]
        or advance_evidence.signer_workflow_sha != expected_authorizer["workflow_sha"]
    ):
        return AttestedConsumerCheckpointDecision(False, "rotation_authorizer_mismatch")
    if advance_evidence.candidate_registry_sha256 != candidate_registry_digest:
        return AttestedConsumerCheckpointDecision(False, "rotation_registry_digest_mismatch")
    if advance_evidence.candidate_manifest_sha256 != candidate_manifest_digest:
        return AttestedConsumerCheckpointDecision(False, "rotation_manifest_digest_mismatch")
    if not advance_evidence.rotation_authorized:
        return AttestedConsumerCheckpointDecision(False, "rotation_evidence_not_authorized")
    if advance_evidence.rotation_reason != "registry_rotation_authorized":
        return AttestedConsumerCheckpointDecision(False, "rotation_evidence_reason_invalid")
    if _SHA256_RE.fullmatch(advance_evidence.subject_sha256) is None:
        return AttestedConsumerCheckpointDecision(False, "rotation_evidence_subject_invalid")

    previous_history = candidate_registry["history"][:-1]
    if len(previous_history) != trusted_generation + 1:
        return AttestedConsumerCheckpointDecision(False, "candidate_history_prefix_invalid")
    previous_registry = {
        "schema_version": candidate_registry["schema_version"],
        "active_generation": trusted_generation,
        "active_manifest_sha256": previous_history[-1]["manifest_sha256"],
        "history": previous_history,
    }
    if sha256_hex(canonical_json_bytes(previous_registry)) != trusted_checkpoint[
        "accepted_registry_sha256"
    ]:
        return AttestedConsumerCheckpointDecision(False, "checkpoint_registry_prefix_mismatch")
    if previous_registry["active_manifest_sha256"] != trusted_checkpoint[
        "accepted_manifest_sha256"
    ]:
        return AttestedConsumerCheckpointDecision(False, "checkpoint_manifest_prefix_mismatch")

    rotation = evaluate_registry_rotation(previous_registry, candidate_registry, manifests)
    if not rotation.authorized:
        return AttestedConsumerCheckpointDecision(False, rotation.reason)

    next_checkpoint = {
        "schema_version": CHECKPOINT_SCHEMA_VERSION,
        "repository": trusted_checkpoint["repository"],
        "accepted_generation": candidate_generation,
        "accepted_registry_sha256": candidate_registry_digest,
        "accepted_manifest_sha256": candidate_manifest_digest,
        "previous_checkpoint_sha256": checkpoint_sha256(trusted_checkpoint),
        "accepted_evidence": {
            "kind": "rotation_authorization",
            "subject_sha256": advance_evidence.subject_sha256,
            "signer_workflow_path": advance_evidence.signer_workflow_path,
            "signer_workflow_sha": advance_evidence.signer_workflow_sha,
        },
        "advance_authorizer": dict(trusted_checkpoint["advance_authorizer"]),
    }
    return AttestedConsumerCheckpointDecision(True, "consumer_checkpoint_advanced", next_checkpoint)
