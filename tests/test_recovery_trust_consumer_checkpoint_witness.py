from __future__ import annotations

import copy
import json
from pathlib import Path

from liminal.recovery_trust_consumer_checkpoint_attested import checkpoint_sha256
from liminal.recovery_trust_consumer_checkpoint_witness import (
    VerifiedCheckpointEvidence,
    evaluate_checkpoint_candidate,
    validate_witness,
    witness_sha256,
)


ROOT = Path(__file__).resolve().parents[1]
CHECKPOINT_PATH = ROOT / "policies/recovery-trust-consumer-checkpoint-v0.2.json"
WITNESS_PATH = ROOT / "policies/recovery-trust-consumer-checkpoint-witness-v0.1.json"
ROTATION_SUBJECT = "17d97510206db3323c2a1642675659670563ff3401beef63de15a0ee1df31bd7"
GEN1_REGISTRY = "5441072b0e550995a9ad0b27b4f3af7c7b5bf531f59e27c870ab1a8cf61789a1"
GEN1_MANIFEST = "b9cb0b37da2d74ece6c1cf780b06b17fbbb96f02e073ac64fb26be49cae24277"
CHECKPOINT_SIGNER_PATH = ".github/workflows/trusted-recovery-trust-consumer-checkpoint-attested.yml"
CHECKPOINT_SIGNER_SHA = "d0688725bd76fdf7221e84ca7c5bfb51e363ff72"


def _load(path: Path) -> dict[str, object]:
    payload = json.loads(path.read_text())
    assert isinstance(payload, dict)
    return payload


def _checkpoint_1(checkpoint_0: dict[str, object]) -> dict[str, object]:
    checkpoint_1 = copy.deepcopy(checkpoint_0)
    checkpoint_1["accepted_generation"] = 1
    checkpoint_1["accepted_registry_sha256"] = GEN1_REGISTRY
    checkpoint_1["accepted_manifest_sha256"] = GEN1_MANIFEST
    checkpoint_1["previous_checkpoint_sha256"] = checkpoint_sha256(checkpoint_0)
    checkpoint_1["accepted_evidence"] = {
        "kind": "rotation_authorization",
        "subject_sha256": ROTATION_SUBJECT,
        "signer_workflow_path": ".github/workflows/trusted-recovery-trust-root-rotation-drill.yml",
        "signer_workflow_sha": "e2cb6a014236bc561d03c405f4986146026041fa",
    }
    return checkpoint_1


def _evidence(checkpoint: dict[str, object]) -> VerifiedCheckpointEvidence:
    return VerifiedCheckpointEvidence(
        verified=True,
        signer_workflow_path=CHECKPOINT_SIGNER_PATH,
        signer_workflow_sha=CHECKPOINT_SIGNER_SHA,
        subject_sha256=checkpoint_sha256(checkpoint),
    )


def test_genesis_witness_matches_permanent_checkpoint() -> None:
    checkpoint_0 = _load(CHECKPOINT_PATH)
    witness_0 = _load(WITNESS_PATH)

    assert validate_witness(witness_0)
    assert witness_0["checkpoint_sha256"] == checkpoint_sha256(checkpoint_0)
    assert witness_0["accepted_registry_sha256"] == checkpoint_0["accepted_registry_sha256"]
    assert witness_0["accepted_manifest_sha256"] == checkpoint_0["accepted_manifest_sha256"]


def test_exact_same_generation_checkpoint_is_idempotently_accepted() -> None:
    checkpoint_0 = _load(CHECKPOINT_PATH)
    witness_0 = _load(WITNESS_PATH)

    decision = evaluate_checkpoint_candidate(
        witness_0,
        checkpoint_0,
        previous_checkpoint=None,
        checkpoint_evidence=None,
    )

    assert decision.authorized is True
    assert decision.reason == "checkpoint_already_witnessed"
    assert decision.next_witness == witness_0


def test_same_generation_conflict_is_rejected() -> None:
    checkpoint_0 = _load(CHECKPOINT_PATH)
    witness_0 = _load(WITNESS_PATH)
    conflict = copy.deepcopy(checkpoint_0)
    conflict["accepted_registry_sha256"] = "0" * 64

    decision = evaluate_checkpoint_candidate(
        witness_0,
        conflict,
        previous_checkpoint=None,
        checkpoint_evidence=None,
    )

    assert decision.authorized is False
    assert decision.reason == "same_generation_checkpoint_conflict"


def test_verified_generation_one_checkpoint_advances_witness() -> None:
    checkpoint_0 = _load(CHECKPOINT_PATH)
    checkpoint_1 = _checkpoint_1(checkpoint_0)
    witness_0 = _load(WITNESS_PATH)

    decision = evaluate_checkpoint_candidate(
        witness_0,
        checkpoint_1,
        previous_checkpoint=checkpoint_0,
        checkpoint_evidence=_evidence(checkpoint_1),
    )

    assert decision.authorized is True
    assert decision.reason == "checkpoint_witness_advanced"
    assert decision.next_witness is not None
    witness_1 = decision.next_witness
    assert witness_1["observed_generation"] == 1
    assert witness_1["checkpoint_sha256"] == checkpoint_sha256(checkpoint_1)
    assert witness_1["previous_witness_sha256"] == witness_sha256(witness_0)
    assert validate_witness(witness_1, witness_0)


def test_old_checkpoint_is_stale_after_witness_advances() -> None:
    checkpoint_0 = _load(CHECKPOINT_PATH)
    checkpoint_1 = _checkpoint_1(checkpoint_0)
    witness_0 = _load(WITNESS_PATH)
    advance = evaluate_checkpoint_candidate(
        witness_0,
        checkpoint_1,
        previous_checkpoint=checkpoint_0,
        checkpoint_evidence=_evidence(checkpoint_1),
    )
    assert advance.next_witness is not None

    stale = evaluate_checkpoint_candidate(
        advance.next_witness,
        checkpoint_0,
        previous_checkpoint=None,
        checkpoint_evidence=None,
    )

    assert stale.authorized is False
    assert stale.reason == "stale_checkpoint"


def test_advance_requires_verified_exact_checkpoint_attestation() -> None:
    checkpoint_0 = _load(CHECKPOINT_PATH)
    checkpoint_1 = _checkpoint_1(checkpoint_0)
    witness_0 = _load(WITNESS_PATH)

    unverified = evaluate_checkpoint_candidate(
        witness_0,
        checkpoint_1,
        previous_checkpoint=checkpoint_0,
        checkpoint_evidence=None,
    )
    assert unverified.reason == "checkpoint_attestation_unverified"

    wrong_signer = VerifiedCheckpointEvidence(
        verified=True,
        signer_workflow_path=CHECKPOINT_SIGNER_PATH,
        signer_workflow_sha="0" * 40,
        subject_sha256=checkpoint_sha256(checkpoint_1),
    )
    rejected_signer = evaluate_checkpoint_candidate(
        witness_0,
        checkpoint_1,
        previous_checkpoint=checkpoint_0,
        checkpoint_evidence=wrong_signer,
    )
    assert rejected_signer.reason == "checkpoint_signer_mismatch"

    wrong_subject = VerifiedCheckpointEvidence(
        verified=True,
        signer_workflow_path=CHECKPOINT_SIGNER_PATH,
        signer_workflow_sha=CHECKPOINT_SIGNER_SHA,
        subject_sha256="0" * 64,
    )
    rejected_subject = evaluate_checkpoint_candidate(
        witness_0,
        checkpoint_1,
        previous_checkpoint=checkpoint_0,
        checkpoint_evidence=wrong_subject,
    )
    assert rejected_subject.reason == "checkpoint_subject_digest_mismatch"
