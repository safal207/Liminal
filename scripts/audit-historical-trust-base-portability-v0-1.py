#!/usr/bin/env python3
"""Independent recomputation of Historical Trust-Base Portability v0.1."""

from __future__ import annotations

import argparse
import json
from collections.abc import Iterable, Mapping
from pathlib import Path
from typing import Any

from liminal.historical_root_b_evidence import (
    load_json,
    sha256_bytes,
    verify_ed25519_envelope,
)
from liminal.recovery_trust_root_registry import (
    canonical_json_bytes,
    sha256_hex,
    validate_registry,
)

RECEIPT_SCHEMA = "liminal-historical-trust-base-portability-receipt/v0.1"
REASON = "historical_trust_base_portability_verified"
STATE_SCHEMA = "liminal-semantic-trust-state/v0.1"


def digest(value: object) -> str:
    return sha256_hex(canonical_json_bytes(value))


def strings(value: object) -> Iterable[str]:
    if isinstance(value, str):
        yield value
    elif isinstance(value, Mapping):
        for item in value.values():
            yield from strings(item)
    elif isinstance(value, (list, tuple)):
        for item in value:
            yield from strings(item)


def state(manifest: dict[str, Any], controls: dict[str, Any]) -> dict[str, Any]:
    roots = manifest["roots"]
    material = manifest["policy_material"]
    return {
        "schema": STATE_SCHEMA,
        "trust_domain": "liminal.trusted-recovery",
        "authorities": {
            "ids": sorted(controls["authority_ids"]),
            "threshold": controls["threshold"],
        },
        "contracts": {
            "rotation_sha256": controls["rotation_contract_sha256"],
            "authorization_sha256": controls["authorization_contract_sha256"],
        },
        "roots": {
            name: {
                "workflow_path": roots[name]["workflow_path"],
                "workflow_sha": roots[name]["workflow_sha"],
                "git_blob_sha": roots[name]["git_blob_sha"],
            }
            for name in sorted(roots)
        },
        "policy_material": {
            name: {
                "path": material[name]["path"],
                "sha256": material[name]["sha256"],
            }
            for name in sorted(material)
        },
        "authorization_scope": manifest["authorization_scope"],
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--proof-dir", required=True)
    parser.add_argument("--output", required=True)
    args = parser.parse_args()

    proof = Path(args.proof_dir).resolve()
    root_a = proof / "materials/root-a"
    root_b = proof / "materials/root-b"
    result_path = proof / "historical-trust-base-portability-result.json"
    result = load_json(result_path)

    a0 = load_json(root_a / "generation-0-manifest.json")
    ar0 = load_json(root_a / "generation-0-registry.json")
    a1 = load_json(root_a / "generation-1-manifest.json")
    ar1 = load_json(root_a / "generation-1-registry.json")
    b0 = load_json(root_b / "generation-0-manifest.json")
    b1 = load_json(root_b / "generation-1-manifest.json")
    br1 = load_json(root_b / "registry.json")
    envelope = load_json(root_b / "signed-history-envelope.json")

    a0_path = str(ar0["history"][0]["manifest_path"])
    a_manifests = {
        a0_path: a0,
        "drill/generation-1-manifest.json": a1,
    }
    b_manifests = {
        str(br1["history"][0]["manifest_path"]): b0,
        str(br1["history"][1]["manifest_path"]): b1,
    }
    if not validate_registry(ar0, {a0_path: a0}):
        raise ValueError("audit_root_a_genesis_invalid")
    if not validate_registry(ar1, a_manifests):
        raise ValueError("audit_root_a_terminal_invalid")
    if not validate_registry(br1, b_manifests):
        raise ValueError("audit_root_b_terminal_invalid")

    a_history = tuple(str(entry["manifest_sha256"]) for entry in ar1["history"])
    b_history = tuple(str(entry["manifest_sha256"]) for entry in br1["history"])
    if set(a_history) & set(b_history):
        raise ValueError("audit_shared_manifest_digest")
    ar1_sha = digest(ar1)
    if ar1_sha == digest(br1):
        raise ValueError("audit_registry_not_independent")
    forbidden = set(a_history) | {ar1_sha}
    for manifest in b_manifests.values():
        if forbidden.intersection(strings(manifest)):
            raise ValueError("audit_cross_root_dependency")

    b_authority = verify_ed25519_envelope(
        root_b / "root-b-public-key.pem",
        envelope,
    )
    claim = envelope["claim"]
    assert isinstance(claim, dict)
    if claim.get("registry_sha256") != digest(br1):
        raise ValueError("audit_root_b_registry_digest_mismatch")
    if claim.get("genesis_manifest_sha256") != digest(b0):
        raise ValueError("audit_root_b_genesis_digest_mismatch")
    if claim.get("terminal_manifest_sha256") != digest(b1):
        raise ValueError("audit_root_b_terminal_digest_mismatch")

    controls = claim.get("terminal_controls")
    if not isinstance(controls, dict):
        raise ValueError("audit_terminal_controls_missing")
    a_state_sha = digest(state(a1, controls))
    b_state_sha = digest(state(b1, controls))
    if a_state_sha != b_state_sha:
        raise ValueError("audit_terminal_trust_state_mismatch")

    a_authority = str(result["primary"]["genesis_authority_id"])
    receipt = {
        "schema": RECEIPT_SCHEMA,
        "verified": True,
        "reason": REASON,
        "genesis_a_manifest_sha256": a_history[0],
        "genesis_b_manifest_sha256": b_history[0],
        "registry_a_sha256": ar1_sha,
        "registry_b_sha256": digest(br1),
        "genesis_a_authority_id": a_authority,
        "genesis_b_authority_id": b_authority,
        "history_a_tip_sha256": a_history[-1],
        "history_b_tip_sha256": b_history[-1],
        "trust_state_digest_a": a_state_sha,
        "trust_state_digest_b": b_state_sha,
        "histories_independently_valid": True,
        "cross_root_dependency": False,
        "equivalent_terminal_state": True,
    }
    if receipt != result.get("receipt"):
        raise ValueError("audit_receipt_bytes_mismatch")
    receipt_sha = digest(receipt)
    if receipt_sha != result.get("receipt_sha256"):
        raise ValueError("audit_receipt_digest_mismatch")

    audit = {
        "schema": "liminal-historical-trust-base-portability-external-audit/v0.1",
        "verified": True,
        "reason": "historical_trust_base_portability_recomputed",
        "result_sha256": sha256_bytes(result_path.read_bytes()),
        "receipt_sha256": receipt_sha,
        "root_a_registry_sha256": ar1_sha,
        "root_b_registry_sha256": digest(br1),
        "root_b_genesis_authority_id": b_authority,
        "trust_state_digest": a_state_sha,
    }
    output = Path(args.output)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_bytes(canonical_json_bytes(audit))
    print(json.dumps(audit, sort_keys=True, separators=(",", ":")))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
