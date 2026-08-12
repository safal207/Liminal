#!/usr/bin/env python3
"""Run a non-destructive trusted-consumer checkpoint anti-rollback drill."""

from __future__ import annotations

import argparse
import copy
import json
from pathlib import Path
from typing import Any

from liminal.recovery_trust_consumer_checkpoint import (
    checkpoint_sha256,
    evaluate_checkpoint_candidate,
    validate_checkpoint,
)
from liminal.recovery_trust_root_registry import (
    REGISTRY_SCHEMA_VERSION,
    canonical_json_bytes,
    evaluate_registry_rotation,
    sha256_hex,
    validate_registry,
)


DRILL_SCHEMA_VERSION = "liminal.recovery-trust-consumer-checkpoint-drill.v0.1"
MANIFEST_PATH = "policies/recovery-trust-root-manifest-v0.1.json"
ATTESTOR_SHA = "73ae4e387815f936aa41f0a6cbdd3d654c30b9b4"
ROTATED_VERIFIER_SHA = "ca69a7d342a4e00dfc2d11b13016a8eb7733da6b"
EXPECTED_GENERATION_1_MANIFEST_SHA256 = (
    "b9cb0b37da2d74ece6c1cf780b06b17fbbb96f02e073ac64fb26be49cae24277"
)
EXPECTED_GENERATION_1_REGISTRY_SHA256 = (
    "5441072b0e550995a9ad0b27b4f3af7c7b5bf531f59e27c870ab1a8cf61789a1"
)


def _load_json(path: Path) -> dict[str, Any]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise ValueError(f"object_required:{path}")
    return payload


def _write_canonical(path: Path, payload: object) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(canonical_json_bytes(payload))


def _entry(generation: int, path: str, manifest: dict[str, Any]) -> dict[str, Any]:
    return {
        "generation": generation,
        "manifest_path": path,
        "manifest_sha256": sha256_hex(canonical_json_bytes(manifest)),
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--checkpoint",
        default="policies/recovery-trust-consumer-checkpoint-v0.1.json",
    )
    parser.add_argument(
        "--registry",
        default="policies/recovery-trust-root-registry-v0.1.json",
    )
    parser.add_argument("--manifest", default=MANIFEST_PATH)
    parser.add_argument("--output-dir", required=True)
    args = parser.parse_args()

    checkpoint_0 = _load_json(Path(args.checkpoint))
    registry_0 = _load_json(Path(args.registry))
    manifest_0 = _load_json(Path(args.manifest))
    output_dir = Path(args.output_dir)

    if not validate_checkpoint(checkpoint_0):
        raise ValueError("genesis_checkpoint_invalid")
    if not validate_registry(registry_0, {MANIFEST_PATH: manifest_0}):
        raise ValueError("genesis_registry_invalid")
    if checkpoint_0["accepted_registry_sha256"] != sha256_hex(canonical_json_bytes(registry_0)):
        raise ValueError("checkpoint_registry_digest_mismatch")
    if checkpoint_0["accepted_manifest_sha256"] != registry_0["active_manifest_sha256"]:
        raise ValueError("checkpoint_manifest_digest_mismatch")

    manifest_1 = copy.deepcopy(manifest_0)
    manifest_1["generation"] = 1
    manifest_1["previous_manifest_sha256"] = registry_0["active_manifest_sha256"]
    manifest_1["roots"]["verifier"]["workflow_sha"] = ROTATED_VERIFIER_SHA
    path_1 = "drill/generation-1-manifest.json"
    history_1 = [*registry_0["history"], _entry(1, path_1, manifest_1)]
    registry_1 = {
        "schema_version": REGISTRY_SCHEMA_VERSION,
        "active_generation": 1,
        "active_manifest_sha256": history_1[-1]["manifest_sha256"],
        "history": history_1,
    }
    manifests = {MANIFEST_PATH: manifest_0, path_1: manifest_1}

    manifest_1_sha256 = sha256_hex(canonical_json_bytes(manifest_1))
    registry_1_sha256 = sha256_hex(canonical_json_bytes(registry_1))
    if manifest_1_sha256 != EXPECTED_GENERATION_1_MANIFEST_SHA256:
        raise ValueError("generation_1_manifest_digest_drift")
    if registry_1_sha256 != EXPECTED_GENERATION_1_REGISTRY_SHA256:
        raise ValueError("generation_1_registry_digest_drift")

    rotation = evaluate_registry_rotation(registry_0, registry_1, manifests)
    if not rotation.authorized:
        raise ValueError(f"generation_1_rotation_rejected:{rotation.reason}")

    advance = evaluate_checkpoint_candidate(
        checkpoint_0,
        registry_1,
        manifests,
        candidate_trust_evidence_verified=True,
        candidate_attestor_workflow_sha=ATTESTOR_SHA,
    )
    if not advance.authorized or advance.next_checkpoint is None:
        raise ValueError(f"checkpoint_advance_rejected:{advance.reason}")
    checkpoint_1 = advance.next_checkpoint
    if not validate_checkpoint(checkpoint_1, checkpoint_0):
        raise ValueError("generation_1_checkpoint_invalid")

    stale = evaluate_checkpoint_candidate(
        checkpoint_1,
        registry_0,
        {MANIFEST_PATH: manifest_0},
        candidate_trust_evidence_verified=True,
        candidate_attestor_workflow_sha=ATTESTOR_SHA,
    )
    if stale.authorized or stale.reason != "stale_registry":
        raise ValueError(f"stale_registry_not_rejected:{stale.reason}")

    result = {
        "schema_version": DRILL_SCHEMA_VERSION,
        "verified": True,
        "reason": "consumer_checkpoint_advance_and_stale_rejection_verified",
        "external_provider_calls": 0,
        "permanent_registry_mutated": False,
        "permanent_checkpoint_mutated": False,
        "candidate_trust_evidence_basis": "deterministic_registry_rotation_authorization",
        "candidate_registry_cryptographic_attestation_exercised": False,
        "baseline": {
            "checkpoint_sha256": checkpoint_sha256(checkpoint_0),
            "accepted_generation": checkpoint_0["accepted_generation"],
            "registry_sha256": checkpoint_0["accepted_registry_sha256"],
            "manifest_sha256": checkpoint_0["accepted_manifest_sha256"],
        },
        "advance": {
            "authorized": advance.authorized,
            "reason": advance.reason,
            "accepted_generation": checkpoint_1["accepted_generation"],
            "candidate_registry_sha256": registry_1_sha256,
            "candidate_manifest_sha256": manifest_1_sha256,
            "checkpoint_sha256": checkpoint_sha256(checkpoint_1),
            "previous_checkpoint_sha256": checkpoint_1["previous_checkpoint_sha256"],
            "rotation_reason": rotation.reason,
        },
        "stale_replay": {
            "presented_generation": registry_0["active_generation"],
            "presented_registry_sha256": sha256_hex(canonical_json_bytes(registry_0)),
            "structurally_valid": validate_registry(registry_0, {MANIFEST_PATH: manifest_0}),
            "authorized": stale.authorized,
            "reason": stale.reason,
        },
    }

    _write_canonical(output_dir / "checkpoint-generation-0.json", checkpoint_0)
    _write_canonical(output_dir / "generation-1-manifest.json", manifest_1)
    _write_canonical(output_dir / "generation-1-registry.json", registry_1)
    _write_canonical(output_dir / "checkpoint-generation-1.json", checkpoint_1)
    _write_canonical(output_dir / "consumer-checkpoint-drill-result.json", result)

    print(json.dumps(result, sort_keys=True, separators=(",", ":")))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
