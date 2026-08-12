from __future__ import annotations

import copy
import json
from pathlib import Path

from liminal.recovery_trust_consumer_checkpoint_attested import (
    VerifiedRegistryAdvanceEvidence,
    checkpoint_sha256,
    evaluate_candidate,
    validate_checkpoint,
)
from liminal.recovery_trust_root_registry import (
    REGISTRY_SCHEMA_VERSION,
    canonical_json_bytes,
    sha256_hex,
)


CHECKPOINT_PATH = Path("policies/recovery-trust-consumer-checkpoint-v0.2.json")
REGISTRY_PATH = Path("policies/recovery-trust-root-registry-v0.1.json")
MANIFEST_PATH = Path("policies/recovery-trust-root-manifest-v0.1.json")
GENESIS_MANIFEST_KEY = "policies/recovery-trust-root-manifest-v0.1.json"
GENERATION_1_MANIFEST_KEY = "drill/generation-1-manifest.json"
ROTATED_VERIFIER_SHA = "ca69a7d342a4e00dfc2d11b13016a8eb7733da6b"
ROTATION_SUBJECT_SHA256 = "17d97510206db3323c2a1642675659670563ff3401beef63de15a0ee1df31bd7"


def _load(path: Path) -> dict[str, object]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    assert isinstance(payload, dict)
    return payload


def _candidate() -> tuple[dict[str, object], dict[str, object], dict[str, object]]:
    checkpoint = _load(CHECKPOINT_PATH)
    registry_0 = _load(REGISTRY_PATH)
    manifest_0 = _load(MANIFEST_PATH)

    manifest_1 = copy.deepcopy(manifest_0)
    manifest_1["generation"] = 1
    manifest_1["previous_manifest_sha256"] = registry_0["active_manifest_sha256"]
    roots = manifest_1["roots"]
    assert isinstance(roots, dict)
    verifier = roots["verifier"]
    assert isinstance(verifier, dict)
    verifier["workflow_sha"] = ROTATED_VERIFIER_SHA

    manifest_1_sha256 = sha256_hex(canonical_json_bytes(manifest_1))
    history = registry_0["history"]
    assert isinstance(history, list)
    registry_1 = {
        "schema_version": REGISTRY_SCHEMA_VERSION,
        "active_generation": 1,
        "active_manifest_sha256": manifest_1_sha256,
        "history": [
            *history,
            {
                "generation": 1,
                "manifest_path": GENERATION_1_MANIFEST_KEY,
                "manifest_sha256": manifest_1_sha256,
            },
        ],
    }
    manifests = {
        GENESIS_MANIFEST_KEY: manifest_0,
        GENERATION_1_MANIFEST_KEY: manifest_1,
    }
    return checkpoint, registry_1, manifests


def _evidence(
    checkpoint: dict[str, object],
    registry_1: dict[str, object],
    *,
    verified: bool = True,
) -> VerifiedRegistryAdvanceEvidence:
    authorizer = checkpoint["advance_authorizer"]
    assert isinstance(authorizer, dict)
    return VerifiedRegistryAdvanceEvidence(
        verified=verified,
        evidence_kind="rotation_authorization",
        signer_workflow_path=str(authorizer["workflow_path"]),
        signer_workflow_sha=str(authorizer["workflow_sha"]),
        subject_sha256=ROTATION_SUBJECT_SHA256,
        candidate_registry_sha256=sha256_hex(canonical_json_bytes(registry_1)),
        candidate_manifest_sha256=str(registry_1["active_manifest_sha256"]),
        rotation_authorized=True,
        rotation_reason="registry_rotation_authorized",
    )


def test_genesis_checkpoint_v02_validates() -> None:
    checkpoint = _load(CHECKPOINT_PATH)
    assert validate_checkpoint(checkpoint)


def test_verified_rotation_advances_checkpoint() -> None:
    checkpoint, registry_1, manifests = _candidate()
    decision = evaluate_candidate(
        checkpoint,
        registry_1,
        manifests,
        advance_evidence=_evidence(checkpoint, registry_1),
    )
    assert decision.authorized
    assert decision.reason == "consumer_checkpoint_advanced"
    assert decision.next_checkpoint is not None
    assert decision.next_checkpoint["accepted_generation"] == 1
    assert decision.next_checkpoint["previous_checkpoint_sha256"] == checkpoint_sha256(
        checkpoint
    )
    assert validate_checkpoint(decision.next_checkpoint, checkpoint)


def test_unverified_rotation_evidence_rejected() -> None:
    checkpoint, registry_1, manifests = _candidate()
    decision = evaluate_candidate(
        checkpoint,
        registry_1,
        manifests,
        advance_evidence=_evidence(checkpoint, registry_1, verified=False),
    )
    assert not decision.authorized
    assert decision.reason == "rotation_evidence_unverified"


def test_wrong_rotation_authorizer_rejected() -> None:
    checkpoint, registry_1, manifests = _candidate()
    evidence = _evidence(checkpoint, registry_1)
    wrong = VerifiedRegistryAdvanceEvidence(
        **{**evidence.__dict__, "signer_workflow_sha": "1" * 40}
    )
    decision = evaluate_candidate(
        checkpoint,
        registry_1,
        manifests,
        advance_evidence=wrong,
    )
    assert not decision.authorized
    assert decision.reason == "rotation_authorizer_mismatch"


def test_rotation_registry_digest_mismatch_rejected() -> None:
    checkpoint, registry_1, manifests = _candidate()
    evidence = _evidence(checkpoint, registry_1)
    wrong = VerifiedRegistryAdvanceEvidence(
        **{**evidence.__dict__, "candidate_registry_sha256": "0" * 64}
    )
    decision = evaluate_candidate(
        checkpoint,
        registry_1,
        manifests,
        advance_evidence=wrong,
    )
    assert not decision.authorized
    assert decision.reason == "rotation_registry_digest_mismatch"


def test_rotation_manifest_digest_mismatch_rejected() -> None:
    checkpoint, registry_1, manifests = _candidate()
    evidence = _evidence(checkpoint, registry_1)
    wrong = VerifiedRegistryAdvanceEvidence(
        **{**evidence.__dict__, "candidate_manifest_sha256": "0" * 64}
    )
    decision = evaluate_candidate(
        checkpoint,
        registry_1,
        manifests,
        advance_evidence=wrong,
    )
    assert not decision.authorized
    assert decision.reason == "rotation_manifest_digest_mismatch"


def test_same_generation_conflict_rejected() -> None:
    checkpoint = _load(CHECKPOINT_PATH)
    registry_0 = _load(REGISTRY_PATH)
    manifest_0 = _load(MANIFEST_PATH)
    conflicting = copy.deepcopy(registry_0)
    conflicting["active_manifest_sha256"] = "0" * 64
    history = conflicting["history"]
    assert isinstance(history, list)
    entry = history[-1]
    assert isinstance(entry, dict)
    entry["manifest_sha256"] = "0" * 64
    fake_manifest = copy.deepcopy(manifest_0)
    manifests = {GENESIS_MANIFEST_KEY: fake_manifest}

    decision = evaluate_candidate(
        checkpoint,
        conflicting,
        manifests,
        advance_evidence=None,
    )
    assert not decision.authorized
    assert decision.reason == "candidate_registry_invalid"


def test_generation_gap_rejected() -> None:
    checkpoint, registry_1, manifests = _candidate()
    registry_gap = copy.deepcopy(registry_1)
    registry_gap["active_generation"] = 2
    decision = evaluate_candidate(
        checkpoint,
        registry_gap,
        manifests,
        advance_evidence=_evidence(checkpoint, registry_1),
    )
    assert not decision.authorized
    assert decision.reason == "candidate_registry_invalid"


def test_old_registry_rejected_after_checkpoint_advance() -> None:
    checkpoint, registry_1, manifests = _candidate()
    advance = evaluate_candidate(
        checkpoint,
        registry_1,
        manifests,
        advance_evidence=_evidence(checkpoint, registry_1),
    )
    assert advance.next_checkpoint is not None

    registry_0 = _load(REGISTRY_PATH)
    manifest_0 = _load(MANIFEST_PATH)
    stale = evaluate_candidate(
        advance.next_checkpoint,
        registry_0,
        {GENESIS_MANIFEST_KEY: manifest_0},
        advance_evidence=None,
    )
    assert not stale.authorized
    assert stale.reason == "stale_registry"
