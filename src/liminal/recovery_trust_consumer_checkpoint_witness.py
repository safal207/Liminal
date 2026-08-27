"""External witness for recovering monotonic consumer-checkpoint state.

The witness is deliberately separate from the consumer checkpoint itself. A consumer that
loses local checkpoint state can recover a cryptographically verified witness and still reject
older, historically valid checkpoints. Cryptographic verification of checkpoint attestations
is external to this module; callers pass only normalized verified evidence.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Any

from liminal.recovery_trust_consumer_checkpoint_attested import (
    checkpoint_sha256,
    validate_checkpoint,
)
from liminal.recovery_trust_root_registry import canonical_json_bytes, sha256_hex


WITNESS_SCHEMA_VERSION = "liminal.recovery-trust-consumer-checkpoint-witness.v0.1"
WITNESS_SCHEMA_VERSION_V2 = "liminal.recovery-trust-consumer-checkpoint-witness.v0.2"
_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")
_GIT_SHA_RE = re.compile(r"^[0-9a-f]{40}$")
_SIGNER_TRANSITION_REASONS = {"manifest_backed_checkpoint_producer_rotation"}


@dataclass(frozen=True)
class VerifiedCheckpointEvidence:
    """Normalized result of external cryptographic checkpoint verification."""

    verified: bool
    signer_workflow_path: str
    signer_workflow_sha: str
    subject_sha256: str


@dataclass(frozen=True)
class CheckpointWitnessDecision:
    """Machine-readable decision for one checkpoint presentation."""

    authorized: bool
    reason: str
    next_witness: dict[str, Any] | None = None


def witness_sha256(payload: object) -> str:
    """Return SHA-256 of canonical witness bytes."""

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


def _valid_signer_transition(value: object, current_signer: object) -> bool:
    if not isinstance(value, dict) or set(value) != {
        "previous_signer",
        "reason",
        "previous_witness_workflow_sha",
    }:
        return False
    previous_signer = value.get("previous_signer")
    reason = value.get("reason")
    previous_witness_sha = value.get("previous_witness_workflow_sha")
    return (
        _valid_signer(previous_signer)
        and previous_signer != current_signer
        and reason in _SIGNER_TRANSITION_REASONS
        and isinstance(previous_witness_sha, str)
        and _GIT_SHA_RE.fullmatch(previous_witness_sha) is not None
    )


def _validate_witness_body(payload: object) -> bool:
    if not isinstance(payload, dict):
        return False
    schema = payload.get("schema_version")
    common = {
        "schema_version",
        "repository",
        "observed_generation",
        "checkpoint_sha256",
        "accepted_registry_sha256",
        "accepted_manifest_sha256",
        "previous_witness_sha256",
        "checkpoint_signer",
    }
    if schema == WITNESS_SCHEMA_VERSION:
        if set(payload) != common:
            return False
    elif schema == WITNESS_SCHEMA_VERSION_V2:
        if set(payload) != common | {"checkpoint_signer_transition"}:
            return False
        if not _valid_signer_transition(
            payload.get("checkpoint_signer_transition"), payload.get("checkpoint_signer")
        ):
            return False
    else:
        return False

    if payload.get("repository") != "safal207/Liminal":
        return False
    generation = payload.get("observed_generation")
    if not isinstance(generation, int) or isinstance(generation, bool) or generation < 0:
        return False
    for key in ("checkpoint_sha256", "accepted_registry_sha256", "accepted_manifest_sha256"):
        value = payload.get(key)
        if not isinstance(value, str) or _SHA256_RE.fullmatch(value) is None:
            return False
    previous = payload.get("previous_witness_sha256")
    if previous is not None and (
        not isinstance(previous, str) or _SHA256_RE.fullmatch(previous) is None
    ):
        return False
    return _valid_signer(payload.get("checkpoint_signer"))


def validate_witness(payload: object, previous_witness: object | None = None) -> bool:
    """Validate a witness and its immediate predecessor link."""

    if not _validate_witness_body(payload):
        return False
    assert isinstance(payload, dict)
    generation = payload["observed_generation"]
    previous_digest = payload["previous_witness_sha256"]
    if generation == 0:
        return previous_digest is None and previous_witness is None
    if not _validate_witness_body(previous_witness):
        return False
    assert isinstance(previous_witness, dict)
    if previous_witness["schema_version"] != payload["schema_version"]:
        return False
    if previous_witness["observed_generation"] != generation - 1:
        return False
    return previous_digest == witness_sha256(previous_witness)


def evaluate_checkpoint_candidate(
    trusted_witness: object,
    candidate_checkpoint: object,
    *,
    previous_checkpoint: object | None,
    checkpoint_evidence: VerifiedCheckpointEvidence | None,
) -> CheckpointWitnessDecision:
    """Evaluate a checkpoint against externally recovered witness state.

    Same-generation replay of the exact witnessed checkpoint is idempotently accepted. Older
    generations are rejected even when the old checkpoint is structurally valid. Advancement by
    exactly one generation requires a valid checkpoint chain plus cryptographically verified
    evidence from the checkpoint signer pinned in the witness.
    """

    if not _validate_witness_body(trusted_witness):
        return CheckpointWitnessDecision(False, "trusted_witness_invalid")
    assert isinstance(trusted_witness, dict)

    if not isinstance(candidate_checkpoint, dict):
        return CheckpointWitnessDecision(False, "candidate_checkpoint_invalid")
    generation = candidate_checkpoint.get("accepted_generation")
    if not isinstance(generation, int) or isinstance(generation, bool):
        return CheckpointWitnessDecision(False, "candidate_checkpoint_invalid")

    witnessed_generation = trusted_witness["observed_generation"]
    candidate_digest = checkpoint_sha256(candidate_checkpoint)

    if generation < witnessed_generation:
        return CheckpointWitnessDecision(False, "stale_checkpoint")

    if generation == witnessed_generation:
        if (
            candidate_digest == trusted_witness["checkpoint_sha256"]
            and candidate_checkpoint.get("accepted_registry_sha256")
            == trusted_witness["accepted_registry_sha256"]
            and candidate_checkpoint.get("accepted_manifest_sha256")
            == trusted_witness["accepted_manifest_sha256"]
        ):
            return CheckpointWitnessDecision(True, "checkpoint_already_witnessed", trusted_witness)
        return CheckpointWitnessDecision(False, "same_generation_checkpoint_conflict")

    if generation != witnessed_generation + 1:
        return CheckpointWitnessDecision(False, "checkpoint_generation_gap")

    if not validate_checkpoint(candidate_checkpoint, previous_checkpoint):
        return CheckpointWitnessDecision(False, "candidate_checkpoint_invalid")
    if not isinstance(previous_checkpoint, dict):
        return CheckpointWitnessDecision(False, "previous_checkpoint_missing")
    if checkpoint_sha256(previous_checkpoint) != trusted_witness["checkpoint_sha256"]:
        return CheckpointWitnessDecision(False, "witness_checkpoint_prefix_mismatch")

    if checkpoint_evidence is None or not checkpoint_evidence.verified:
        return CheckpointWitnessDecision(False, "checkpoint_attestation_unverified")
    expected_signer = trusted_witness["checkpoint_signer"]
    if (
        checkpoint_evidence.signer_workflow_path != expected_signer["workflow_path"]
        or checkpoint_evidence.signer_workflow_sha != expected_signer["workflow_sha"]
    ):
        return CheckpointWitnessDecision(False, "checkpoint_signer_mismatch")
    if checkpoint_evidence.subject_sha256 != candidate_digest:
        return CheckpointWitnessDecision(False, "checkpoint_subject_digest_mismatch")

    next_witness = {
        "schema_version": trusted_witness["schema_version"],
        "repository": trusted_witness["repository"],
        "observed_generation": generation,
        "checkpoint_sha256": candidate_digest,
        "accepted_registry_sha256": candidate_checkpoint["accepted_registry_sha256"],
        "accepted_manifest_sha256": candidate_checkpoint["accepted_manifest_sha256"],
        "previous_witness_sha256": witness_sha256(trusted_witness),
        "checkpoint_signer": dict(expected_signer),
    }
    if trusted_witness["schema_version"] == WITNESS_SCHEMA_VERSION_V2:
        next_witness["checkpoint_signer_transition"] = dict(
            trusted_witness["checkpoint_signer_transition"]
        )
    return CheckpointWitnessDecision(True, "checkpoint_witness_advanced", next_witness)
