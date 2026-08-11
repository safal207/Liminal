#!/usr/bin/env python3
"""Verify the recovery trust-root registry against historical Git material."""

from __future__ import annotations

import argparse
import hashlib
import json
import subprocess
from pathlib import Path
from typing import Any

from liminal.recovery_trust_root_registry import (
    canonical_json_bytes,
    sha256_hex,
    validate_registry,
)


def _git(*args: str) -> bytes:
    return subprocess.check_output(["git", *args])


def _git_blob_sha(commit: str, path: str) -> str:
    return _git("rev-parse", f"{commit}:{path}").decode("ascii").strip()


def _git_file(commit: str, path: str) -> bytes:
    return _git("show", f"{commit}:{path}")


def _load_json(path: Path) -> dict[str, Any]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise ValueError(f"object_required:{path}")
    return payload


def _verify_material(manifest: dict[str, Any]) -> None:
    roots = manifest["roots"]
    for root in roots.values():
        actual_blob = _git_blob_sha(root["workflow_sha"], root["workflow_path"])
        if actual_blob != root["git_blob_sha"]:
            raise ValueError(f"workflow_blob_mismatch:{root['workflow_path']}")

    for item in manifest["policy_material"].values():
        actual_sha256 = hashlib.sha256(_git_file(item["source_sha"], item["path"])).hexdigest()
        if actual_sha256 != item["sha256"]:
            raise ValueError(f"policy_material_mismatch:{item['path']}")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--registry", required=True)
    parser.add_argument("--repository-root", default=".")
    parser.add_argument("--output", required=True)
    args = parser.parse_args()

    root = Path(args.repository_root).resolve()
    registry_path = (root / args.registry).resolve()
    registry = _load_json(registry_path)

    manifests: dict[str, dict[str, Any]] = {}
    for entry in registry.get("history", []):
        if not isinstance(entry, dict) or not isinstance(entry.get("manifest_path"), str):
            raise ValueError("registry_history_entry_invalid")
        path = entry["manifest_path"]
        manifests[path] = _load_json(root / path)

    if not validate_registry(registry, manifests):
        raise ValueError("recovery_trust_root_registry_invalid")

    for manifest in manifests.values():
        _verify_material(manifest)

    active_digest = registry["active_manifest_sha256"]
    active_entry = registry["history"][-1]
    active_manifest = manifests[active_entry["manifest_path"]]
    receipt = {
        "schema_version": "liminal.recovery-trust-root-registry-verification.v0.1",
        "verified": True,
        "reason": "recovery_trust_root_registry_verified",
        "registry": {
            "path": args.registry,
            "sha256": sha256_hex(canonical_json_bytes(registry)),
            "active_generation": registry["active_generation"],
            "active_manifest_sha256": active_digest,
        },
        "active_roots": active_manifest["roots"],
        "policy_material": active_manifest["policy_material"],
        "authorization_scope": active_manifest["authorization_scope"],
    }
    output = Path(args.output)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_bytes(canonical_json_bytes(receipt))
    print(output)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
