#!/usr/bin/env python3
"""Run a non-destructive trust-root rotation and downgrade-rejection drill."""

from __future__ import annotations

import argparse
import copy
import hashlib
import json
import subprocess
from pathlib import Path
from typing import Any

from liminal.recovery_trust_root_registry import (
    canonical_json_bytes,
    evaluate_registry_rotation,
    sha256_hex,
    validate_registry,
)


DRILL_SCHEMA_VERSION = "liminal.recovery-trust-root-rotation-drill.v0.1"


def _load_json(path: Path) -> dict[str, Any]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise ValueError(f"object_required:{path}")
    return payload


def _git(root: Path, *args: str) -> bytes:
    return subprocess.check_output(["git", *args], cwd=root)


def _git_blob_sha(root: Path, commit: str, path: str) -> str:
    return _git(root, "rev-parse", f"{commit}:{path}").decode("ascii").strip()


def _git_file(root: Path, commit: str, path: str) -> bytes:
    return _git(root, "show", f"{commit}:{path}")


def _verify_manifest_material(root: Path, manifest: dict[str, Any]) -> None:
    roots = manifest["roots"]
    for root_entry in roots.values():
        actual_blob = _git_blob_sha(
            root,
            root_entry["workflow_sha"],
            root_entry["workflow_path"],
        )
        if actual_blob != root_entry["git_blob_sha"]:
            raise ValueError(f"workflow_blob_mismatch:{root_entry['workflow_path']}")

    for item in manifest["policy_material"].values():
        actual_sha256 = hashlib.sha256(
            _git_file(root, item["source_sha"], item["path"])
        ).hexdigest()
        if actual_sha256 != item["sha256"]:
            raise ValueError(f"policy_material_mismatch:{item['path']}")


def _entry(generation: int, path: str, manifest: dict[str, Any]) -> dict[str, Any]:
    return {
        "generation": generation,
        "manifest_path": path,
        "manifest_sha256": sha256_hex(canonical_json_bytes(manifest)),
    }


def _registry_from_history(
    baseline_registry: dict[str, Any],
    history: list[dict[str, Any]],
) -> dict[str, Any]:
    return {
        "schema_version": baseline_registry["schema_version"],
        "active_generation": len(history) - 1,
        "active_manifest_sha256": history[-1]["manifest_sha256"],
        "history": history,
    }


def _write_canonical(path: Path, payload: object) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(canonical_json_bytes(payload))


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--registry",
        default="policies/recovery-trust-root-registry-v0.1.json",
    )
    parser.add_argument("--rotated-verifier-sha", required=True)
    parser.add_argument("--repository-root", default=".")
    parser.add_argument("--output-dir", required=True)
    args = parser.parse_args()

    root = Path(args.repository_root).resolve()
    registry_path = (root / args.registry).resolve()
    output_dir = Path(args.output_dir).resolve()
    baseline_registry = _load_json(registry_path)

    manifests: dict[str, dict[str, Any]] = {}
    for entry in baseline_registry.get("history", []):
        if not isinstance(entry, dict) or not isinstance(entry.get("manifest_path"), str):
            raise ValueError("baseline_registry_history_invalid")
        manifest_path = entry["manifest_path"]
        manifests[manifest_path] = _load_json(root / manifest_path)

    if not validate_registry(baseline_registry, manifests):
        raise ValueError("baseline_registry_invalid")

    active_entry = baseline_registry["history"][-1]
    active_manifest = manifests[active_entry["manifest_path"]]
    _verify_manifest_material(root, active_manifest)

    old_verifier = active_manifest["roots"]["verifier"]
    old_verifier_sha = old_verifier["workflow_sha"]
    rotated_verifier_sha = args.rotated_verifier_sha
    if rotated_verifier_sha == old_verifier_sha:
        raise ValueError("rotated_verifier_sha_must_change")

    rotated_verifier_blob = _git_blob_sha(
        root,
        rotated_verifier_sha,
        old_verifier["workflow_path"],
    )
    if rotated_verifier_blob != old_verifier["git_blob_sha"]:
        raise ValueError("rotation_drill_verifier_blob_changed")

    generation_1 = copy.deepcopy(active_manifest)
    generation_1_number = baseline_registry["active_generation"] + 1
    generation_1["generation"] = generation_1_number
    generation_1["previous_manifest_sha256"] = baseline_registry["active_manifest_sha256"]
    generation_1["roots"]["verifier"]["workflow_sha"] = rotated_verifier_sha
    generation_1["roots"]["verifier"]["git_blob_sha"] = rotated_verifier_blob
    _verify_manifest_material(root, generation_1)

    generation_1_path = f"drill/generation-{generation_1_number}-manifest.json"
    generation_1_entry = _entry(generation_1_number, generation_1_path, generation_1)
    generation_1_registry = _registry_from_history(
        baseline_registry,
        [*baseline_registry["history"], generation_1_entry],
    )
    generation_1_manifests = {**manifests, generation_1_path: generation_1}
    generation_1_decision = evaluate_registry_rotation(
        baseline_registry,
        generation_1_registry,
        generation_1_manifests,
    )
    if not generation_1_decision.authorized:
        raise ValueError(f"legitimate_rotation_rejected:{generation_1_decision.reason}")

    generation_2 = copy.deepcopy(generation_1)
    generation_2_number = generation_1_number + 1
    generation_2["generation"] = generation_2_number
    generation_2["previous_manifest_sha256"] = generation_1_entry["manifest_sha256"]
    generation_2["roots"]["verifier"] = copy.deepcopy(old_verifier)
    _verify_manifest_material(root, generation_2)

    generation_2_path = f"drill/generation-{generation_2_number}-downgrade-manifest.json"
    generation_2_entry = _entry(generation_2_number, generation_2_path, generation_2)
    generation_2_registry = _registry_from_history(
        baseline_registry,
        [*generation_1_registry["history"], generation_2_entry],
    )
    generation_2_manifests = {
        **generation_1_manifests,
        generation_2_path: generation_2,
    }
    structurally_valid_downgrade = validate_registry(
        generation_2_registry,
        generation_2_manifests,
    )
    if not structurally_valid_downgrade:
        raise ValueError("downgrade_candidate_not_structurally_valid")

    downgrade_decision = evaluate_registry_rotation(
        generation_1_registry,
        generation_2_registry,
        generation_2_manifests,
    )
    if downgrade_decision.authorized:
        raise ValueError("downgrade_candidate_unexpectedly_authorized")
    if downgrade_decision.reason != "verifier_root_downgrade":
        raise ValueError(f"unexpected_downgrade_reason:{downgrade_decision.reason}")

    result = {
        "schema_version": DRILL_SCHEMA_VERSION,
        "verified": True,
        "reason": "trust_root_rotation_and_downgrade_rejection_verified",
        "external_provider_calls": 0,
        "baseline": {
            "registry_sha256": sha256_hex(canonical_json_bytes(baseline_registry)),
            "active_generation": baseline_registry["active_generation"],
            "active_manifest_sha256": baseline_registry["active_manifest_sha256"],
            "verifier_workflow_sha": old_verifier_sha,
            "verifier_git_blob_sha": old_verifier["git_blob_sha"],
        },
        "legitimate_rotation": {
            "generation": generation_1_number,
            "manifest_sha256": generation_1_entry["manifest_sha256"],
            "registry_sha256": sha256_hex(canonical_json_bytes(generation_1_registry)),
            "verifier_workflow_sha": rotated_verifier_sha,
            "verifier_git_blob_sha": rotated_verifier_blob,
            "workflow_blob_unchanged": rotated_verifier_blob == old_verifier["git_blob_sha"],
            "authorized": generation_1_decision.authorized,
            "reason": generation_1_decision.reason,
        },
        "downgrade_attempt": {
            "generation": generation_2_number,
            "manifest_sha256": generation_2_entry["manifest_sha256"],
            "registry_sha256": sha256_hex(canonical_json_bytes(generation_2_registry)),
            "attempted_verifier_workflow_sha": old_verifier_sha,
            "structurally_valid": structurally_valid_downgrade,
            "authorized": downgrade_decision.authorized,
            "reason": downgrade_decision.reason,
        },
    }

    _write_canonical(output_dir / "generation-1-manifest.json", generation_1)
    _write_canonical(output_dir / "generation-1-registry.json", generation_1_registry)
    _write_canonical(output_dir / "downgrade-generation-2-manifest.json", generation_2)
    _write_canonical(output_dir / "downgrade-generation-2-registry.json", generation_2_registry)
    _write_canonical(output_dir / "rotation-drill-result.json", result)

    print(json.dumps(result, sort_keys=True, separators=(",", ":")))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
