from __future__ import annotations

import copy
import json
from pathlib import Path

from liminal.recovery_trust_consumer_checkpoint import (
    checkpoint_sha256,
    evaluate_checkpoint_candidate,
    validate_checkpoint,
)
from liminal.recovery_trust_root_registry import REGISTRY_SCHEMA_VERSION, canonical_json_bytes, sha256_hex


ROOT = Path(__file__).resolve().parents[1]
MANIFEST_PATH = "policies/recovery-trust-root-manifest-v0.1.json"
MANIFEST_FILE = ROOT / MANIFEST_PATH
REGISTRY_FILE = ROOT / "policies" / "recovery-trust-root-registry-v0.1.json"
CHECKPOINT_FILE = ROOT / "policies" / "recovery-trust-consumer-checkpoint-v0.1.json"
ATTESTOR_SHA = "73ae4e387815f936aa41f0a6cbdd3d654c30b9b4"
ROTATED_VERIFIER_SHA = "ca69a7d342a4e00dfc2d11b13016a8eb7733da6b"


def _load(path: Path) -> dict[str, object]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    assert isinstance(payload, dict)
    return payload


def _entry(generation: int, path: str, manifest: dict[str, object]) -> dict[str, object]:
    return {
        "generation": generation,
        "manifest_path": path,
        "manifest_sha256": sha256_hex(canonical_json_bytes(manifest)),
    }


def _generation_one() -> tuple[dict[str, object], dict[str, object], dict[str, object]]:
    generation_0 = _load(MANIFEST_FILE)
    registry_0 = _load(REGISTRY_FILE)
    generation_1 = copy.deepcopy(generation_0)
    generation_1["generation"] = 1
    generation_1["previous_manifest_sha256"] = sha256_hex(canonical_json_bytes(generation_0))
    generation_1["roots"]["verifier"]["workflow_sha"] = ROTATED_VERIFIER_SHA

    path_1 = "drill/generation-1-manifest.json"
    history = [*registry_0["history"], _entry(1, path_1, generation_1)]
    registry_1 = {
        "schema_version": REGISTRY_SCHEMA_VERSION,
        "active_generation": 1,
        "active_manifest_sha256": history[-1]["manifest_sha256"],
        "history": history,
    }
    manifests = {MANIFEST_PATH: generation_0, path_1: generation_1}
    return registry_0, registry_1, manifests


def test_genesis_checkpoint_is_valid_and_pins_genesis_registry() -> None:
    checkpoint = _load(CHECKPOINT_FILE)
    registry = _load(REGISTRY_FILE)

    assert validate_checkpoint(checkpoint)
    assert checkpoint["accepted_generation"] == 0
    assert checkpoint["accepted_registry_sha256"] == sha256_hex(canonical_json_bytes(registry))
    assert checkpoint["registry_attestor"]["workflow_sha"] == ATTESTOR_SHA


def test_checkpoint_advances_one_authorized_generation_and_links_predecessor() -> None:
    checkpoint_0 = _load(CHECKPOINT_FILE)
    _, registry_1, manifests = _generation_one()

    decision = evaluate_checkpoint_candidate(
        checkpoint_0,
        registry_1,
        manifests,
        candidate_trust_evidence_verified=True,
        candidate_attestor_workflow_sha=ATTESTOR_SHA,
    )

    assert decision.authorized
    assert decision.reason == "consumer_checkpoint_advanced"
    assert decision.next_checkpoint is not None
    checkpoint_1 = decision.next_checkpoint
    assert checkpoint_1["accepted_generation"] == 1
    assert checkpoint_1["previous_checkpoint_sha256"] == checkpoint_sha256(checkpoint_0)
    assert validate_checkpoint(checkpoint_1, checkpoint_0)


def test_checkpoint_rejects_old_but_structurally_valid_registry_as_stale() -> None:
    checkpoint_0 = _load(CHECKPOINT_FILE)
    registry_0, registry_1, manifests = _generation_one()
    advance = evaluate_checkpoint_candidate(
        checkpoint_0,
        registry_1,
        manifests,
        candidate_trust_evidence_verified=True,
        candidate_attestor_workflow_sha=ATTESTOR_SHA,
    )
    assert advance.next_checkpoint is not None

    stale = evaluate_checkpoint_candidate(
        advance.next_checkpoint,
        registry_0,
        {MANIFEST_PATH: manifests[MANIFEST_PATH]},
        candidate_trust_evidence_verified=True,
        candidate_attestor_workflow_sha=ATTESTOR_SHA,
    )

    assert not stale.authorized
    assert stale.reason == "stale_registry"


def test_checkpoint_accepts_exact_current_registry_idempotently() -> None:
    checkpoint = _load(CHECKPOINT_FILE)
    registry = _load(REGISTRY_FILE)
    manifest = _load(MANIFEST_FILE)

    decision = evaluate_checkpoint_candidate(
        checkpoint,
        registry,
        {MANIFEST_PATH: manifest},
        candidate_trust_evidence_verified=True,
        candidate_attestor_workflow_sha=ATTESTOR_SHA,
    )

    assert decision.authorized
    assert decision.reason == "registry_already_accepted"
    assert decision.next_checkpoint == checkpoint


def test_checkpoint_fails_closed_on_unverified_evidence_or_wrong_attestor() -> None:
    checkpoint = _load(CHECKPOINT_FILE)
    registry = _load(REGISTRY_FILE)
    manifest = _load(MANIFEST_FILE)
    manifests = {MANIFEST_PATH: manifest}

    unverified = evaluate_checkpoint_candidate(
        checkpoint,
        registry,
        manifests,
        candidate_trust_evidence_verified=False,
        candidate_attestor_workflow_sha=ATTESTOR_SHA,
    )
    assert not unverified.authorized
    assert unverified.reason == "candidate_trust_evidence_unverified"

    wrong_attestor = evaluate_checkpoint_candidate(
        checkpoint,
        registry,
        manifests,
        candidate_trust_evidence_verified=True,
        candidate_attestor_workflow_sha="1" * 40,
    )
    assert not wrong_attestor.authorized
    assert wrong_attestor.reason == "registry_attestor_mismatch"


def test_checkpoint_rejects_generation_gap() -> None:
    checkpoint_0 = _load(CHECKPOINT_FILE)
    registry_0, registry_1, manifests = _generation_one()
    generation_1 = manifests["drill/generation-1-manifest.json"]
    generation_2 = copy.deepcopy(generation_1)
    generation_2["generation"] = 2
    generation_2["previous_manifest_sha256"] = sha256_hex(canonical_json_bytes(generation_1))
    generation_2["roots"]["verifier"]["workflow_sha"] = "2" * 40
    path_2 = "drill/generation-2-manifest.json"
    history_2 = [*registry_1["history"], _entry(2, path_2, generation_2)]
    registry_2 = {
        "schema_version": REGISTRY_SCHEMA_VERSION,
        "active_generation": 2,
        "active_manifest_sha256": history_2[-1]["manifest_sha256"],
        "history": history_2,
    }
    manifests[path_2] = generation_2

    decision = evaluate_checkpoint_candidate(
        checkpoint_0,
        registry_2,
        manifests,
        candidate_trust_evidence_verified=True,
        candidate_attestor_workflow_sha=ATTESTOR_SHA,
    )

    assert registry_0["active_generation"] == 0
    assert not decision.authorized
    assert decision.reason == "registry_generation_gap"
