"""Monotonic consumer checkpoint for accepted trust-root registry state."""

from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Any

from liminal.recovery_trust_root_registry import (
    canonical_json_bytes,
    evaluate_registry_rotation,
    sha256_hex,
    validate_registry,
)


CHECKPOINT_SCHEMA_VERSION = "liminal.recovery-trust-consumer-checkpoint.v0.1"
REGISTRY_ATTESTOR_WORKFLOW = ".github/workflows/trusted-recovery-trust-root-registry-attestor.yml"
_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")
_GIT_SHA_RE = re.compile(r"^[0-9a-f]{40}$")


@dataclass(frozen=True)
class ConsumerCheckpointDecision:
    """Machine-readable decision for one candidate registry presentation."""

    authorized: bool
    reason: str
    next_checkpoint: dict[str, Any] | None = None


def checkpoint_sha256(payload: object) -> str:
    """Return the digest of canonical checkpoint bytes."""

    return sha256_hex(canonical_json_bytes(payload))


def _validate_checkpoint_body(payload: object) -> bool:
    if not isinstance(payload, dict) or set(payload) != {
        "schema_version",
        "repository",
        "accepted_generation",
        "accepted_registry_sha256",
        "accepted_manifest_sha256",
        "previous_checkpoint_sha256",
        "registry_attestor",
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

    attestor = payload.get("registry_attestor")
    if not isinstance(attestor, dict) or set(attestor) != {"workflow_path", "workflow_sha"}:
        return False
    if attestor.get("workflow_path") != REGISTRY_ATTESTOR_WORKFLOW:
        return False
    workflow_sha = attestor.get("workflow_sha")
    return isinstance(workflow_sha, str) and _GIT_SHA_RE.fullmatch(workflow_sha) is not None


def validate_checkpoint(payload: object, previous_checkpoint: object | None = None) -> bool:
    """Validate a checkpoint and, for generation > 0, its immediate predecessor link."""

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


def evaluate_checkpoint_candidate(
    trusted_checkpoint: object,
    candidate_registry: object,
    manifests: dict[str, object],
    *,
    candidate_trust_evidence_verified: bool,
    candidate_attestor_workflow_sha: str,
) -> ConsumerCheckpointDecision:
    """Evaluate one registry against the consumer's last trusted checkpoint.

    Cryptographic provenance verification is deliberately external. Callers must only set
    ``candidate_trust_evidence_verified`` after their upstream registry/attestation checks pass.
    """

    if not _validate_checkpoint_body(trusted_checkpoint):
        return ConsumerCheckpointDecision(False, "trusted_checkpoint_invalid")
    assert isinstance(trusted_checkpoint, dict)

    if not candidate_trust_evidence_verified:
        return ConsumerCheckpointDecision(False, "candidate_trust_evidence_unverified")

    expected_attestor = trusted_checkpoint["registry_attestor"]["workflow_sha"]
    if candidate_attestor_workflow_sha != expected_attestor:
        return ConsumerCheckpointDecision(False, "registry_attestor_mismatch")

    if not validate_registry(candidate_registry, manifests):
        return ConsumerCheckpointDecision(False, "candidate_registry_invalid")
    assert isinstance(candidate_registry, dict)

    trusted_generation = trusted_checkpoint["accepted_generation"]
    candidate_generation = candidate_registry["active_generation"]
    candidate_registry_digest = sha256_hex(canonical_json_bytes(candidate_registry))
    candidate_manifest_digest = candidate_registry["active_manifest_sha256"]

    if candidate_generation < trusted_generation:
        return ConsumerCheckpointDecision(False, "stale_registry")

    if candidate_generation == trusted_generation:
        if (
            candidate_registry_digest == trusted_checkpoint["accepted_registry_sha256"]
            and candidate_manifest_digest == trusted_checkpoint["accepted_manifest_sha256"]
        ):
            return ConsumerCheckpointDecision(True, "registry_already_accepted", trusted_checkpoint)
        return ConsumerCheckpointDecision(False, "same_generation_registry_conflict")

    if candidate_generation != trusted_generation + 1:
        return ConsumerCheckpointDecision(False, "registry_generation_gap")

    previous_history = candidate_registry["history"][:-1]
    if len(previous_history) != trusted_generation + 1:
        return ConsumerCheckpointDecision(False, "candidate_history_prefix_invalid")

    previous_registry = {
        "schema_version": candidate_registry["schema_version"],
        "active_generation": trusted_generation,
        "active_manifest_sha256": previous_history[-1]["manifest_sha256"],
        "history": previous_history,
    }
    if sha256_hex(canonical_json_bytes(previous_registry)) != trusted_checkpoint[
        "accepted_registry_sha256"
    ]:
        return ConsumerCheckpointDecision(False, "checkpoint_registry_prefix_mismatch")
    if previous_registry["active_manifest_sha256"] != trusted_checkpoint[
        "accepted_manifest_sha256"
    ]:
        return ConsumerCheckpointDecision(False, "checkpoint_manifest_prefix_mismatch")

    rotation = evaluate_registry_rotation(previous_registry, candidate_registry, manifests)
    if not rotation.authorized:
        return ConsumerCheckpointDecision(False, rotation.reason)

    next_checkpoint = {
        "schema_version": CHECKPOINT_SCHEMA_VERSION,
        "repository": trusted_checkpoint["repository"],
        "accepted_generation": candidate_generation,
        "accepted_registry_sha256": candidate_registry_digest,
        "accepted_manifest_sha256": candidate_manifest_digest,
        "previous_checkpoint_sha256": checkpoint_sha256(trusted_checkpoint),
        "registry_attestor": dict(trusted_checkpoint["registry_attestor"]),
    }
    return ConsumerCheckpointDecision(True, "consumer_checkpoint_advanced", next_checkpoint)
