#!/usr/bin/env python3
"""Consume verified rotation evidence and exercise an attested anti-rollback checkpoint."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
from typing import Any

from liminal.recovery_trust_consumer_checkpoint_attested import (
    VerifiedRegistryAdvanceEvidence,
    checkpoint_sha256,
    evaluate_candidate,
    validate_checkpoint,
)
from liminal.recovery_trust_root_registry import canonical_json_bytes, sha256_hex, validate_registry


DRILL_SCHEMA_VERSION = "liminal.recovery-trust-consumer-checkpoint-attested-drill.v0.2"
GENESIS_MANIFEST_KEY = "policies/recovery-trust-root-manifest-v0.1.json"
GENERATION_1_MANIFEST_KEY = "drill/generation-1-manifest.json"


def _load_object(path: Path) -> dict[str, Any]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise ValueError(f"object_required:{path}")
    return payload


def _require_verification_json(path: Path) -> str:
    raw = path.read_bytes()
    payload = json.loads(raw)
    if isinstance(payload, dict):
        if not payload:
            raise ValueError("rotation_attestation_verification_empty")
    elif isinstance(payload, list):
        if not payload:
            raise ValueError("rotation_attestation_verification_empty")
    else:
        raise ValueError("rotation_attestation_verification_invalid")
    return hashlib.sha256(raw).hexdigest()


def _write_canonical(path: Path, payload: object) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(canonical_json_bytes(payload))


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--checkpoint",
        default="policies/recovery-trust-consumer-checkpoint-v0.2.json",
    )
    parser.add_argument(
        "--baseline-registry",
        default="policies/recovery-trust-root-registry-v0.1.json",
    )
    parser.add_argument(
        "--baseline-manifest",
        default="policies/recovery-trust-root-manifest-v0.1.json",
    )
    parser.add_argument("--candidate-registry", required=True)
    parser.add_argument("--candidate-manifest", required=True)
    parser.add_argument("--rotation-result", required=True)
    parser.add_argument("--rotation-verification-json", required=True)
    parser.add_argument("--output-dir", required=True)
    args = parser.parse_args()

    checkpoint_0 = _load_object(Path(args.checkpoint))
    baseline_registry = _load_object(Path(args.baseline_registry))
    baseline_manifest = _load_object(Path(args.baseline_manifest))
    candidate_registry_path = Path(args.candidate_registry)
    candidate_manifest_path = Path(args.candidate_manifest)
    rotation_result_path = Path(args.rotation_result)
    rotation_verification_path = Path(args.rotation_verification_json)
    candidate_registry = _load_object(candidate_registry_path)
    candidate_manifest = _load_object(candidate_manifest_path)
    rotation_result = _load_object(rotation_result_path)
    output_dir = Path(args.output_dir)

    if not validate_checkpoint(checkpoint_0):
        raise ValueError("genesis_checkpoint_invalid")
    if not validate_registry(
        baseline_registry,
        {GENESIS_MANIFEST_KEY: baseline_manifest},
    ):
        raise ValueError("genesis_registry_invalid")

    baseline_registry_sha256 = sha256_hex(canonical_json_bytes(baseline_registry))
    if checkpoint_0["accepted_registry_sha256"] != baseline_registry_sha256:
        raise ValueError("checkpoint_registry_digest_mismatch")
    if checkpoint_0["accepted_manifest_sha256"] != baseline_registry["active_manifest_sha256"]:
        raise ValueError("checkpoint_manifest_digest_mismatch")

    rotation_verification_sha256 = _require_verification_json(rotation_verification_path)
    rotation_subject_sha256 = hashlib.sha256(rotation_result_path.read_bytes()).hexdigest()
    candidate_registry_raw_sha256 = hashlib.sha256(candidate_registry_path.read_bytes()).hexdigest()
    candidate_manifest_raw_sha256 = hashlib.sha256(candidate_manifest_path.read_bytes()).hexdigest()

    if rotation_result.get("verified") is not True:
        raise ValueError("rotation_result_not_verified")
    if rotation_result.get("external_provider_calls") != 0:
        raise ValueError("rotation_result_provider_calls_unexpected")

    legitimate_rotation = rotation_result.get("legitimate_rotation")
    if not isinstance(legitimate_rotation, dict):
        raise ValueError("legitimate_rotation_missing")
    if legitimate_rotation.get("authorized") is not True:
        raise ValueError("rotation_not_authorized")
    if legitimate_rotation.get("reason") != "registry_rotation_authorized":
        raise ValueError("rotation_reason_invalid")

    candidate_registry_sha256 = sha256_hex(canonical_json_bytes(candidate_registry))
    candidate_manifest_sha256 = sha256_hex(canonical_json_bytes(candidate_manifest))
    if candidate_registry_raw_sha256 != candidate_registry_sha256:
        raise ValueError("candidate_registry_not_canonical")
    if candidate_manifest_raw_sha256 != candidate_manifest_sha256:
        raise ValueError("candidate_manifest_not_canonical")
    if legitimate_rotation.get("registry_sha256") != candidate_registry_sha256:
        raise ValueError("rotation_registry_digest_mismatch")
    if legitimate_rotation.get("manifest_sha256") != candidate_manifest_sha256:
        raise ValueError("rotation_manifest_digest_mismatch")

    manifests = {
        GENESIS_MANIFEST_KEY: baseline_manifest,
        GENERATION_1_MANIFEST_KEY: candidate_manifest,
    }
    if not validate_registry(candidate_registry, manifests):
        raise ValueError("candidate_registry_invalid")

    authorizer = checkpoint_0["advance_authorizer"]
    evidence = VerifiedRegistryAdvanceEvidence(
        verified=True,
        evidence_kind="rotation_authorization",
        signer_workflow_path=authorizer["workflow_path"],
        signer_workflow_sha=authorizer["workflow_sha"],
        subject_sha256=rotation_subject_sha256,
        candidate_registry_sha256=candidate_registry_sha256,
        candidate_manifest_sha256=candidate_manifest_sha256,
        rotation_authorized=True,
        rotation_reason="registry_rotation_authorized",
    )
    advance = evaluate_candidate(
        checkpoint_0,
        candidate_registry,
        manifests,
        advance_evidence=evidence,
    )
    if not advance.authorized or advance.next_checkpoint is None:
        raise ValueError(f"checkpoint_advance_rejected:{advance.reason}")
    checkpoint_1 = advance.next_checkpoint
    if not validate_checkpoint(checkpoint_1, checkpoint_0):
        raise ValueError("generation_1_checkpoint_invalid")

    stale = evaluate_candidate(
        checkpoint_1,
        baseline_registry,
        {GENESIS_MANIFEST_KEY: baseline_manifest},
        advance_evidence=None,
    )
    if stale.authorized or stale.reason != "stale_registry":
        raise ValueError(f"stale_registry_not_rejected:{stale.reason}")

    result = {
        "schema_version": DRILL_SCHEMA_VERSION,
        "verified": True,
        "reason": "attested_consumer_checkpoint_advance_and_stale_rejection_verified",
        "external_provider_calls": 0,
        "permanent_registry_mutated": False,
        "permanent_checkpoint_mutated": False,
        "rotation_authorization": {
            "cryptographically_verified": True,
            "subject_sha256": rotation_subject_sha256,
            "verification_json_sha256": rotation_verification_sha256,
            "signer_workflow_path": authorizer["workflow_path"],
            "signer_workflow_sha": authorizer["workflow_sha"],
            "candidate_registry_sha256": candidate_registry_sha256,
            "candidate_manifest_sha256": candidate_manifest_sha256,
        },
        "baseline": {
            "accepted_generation": checkpoint_0["accepted_generation"],
            "checkpoint_sha256": checkpoint_sha256(checkpoint_0),
            "registry_sha256": checkpoint_0["accepted_registry_sha256"],
            "manifest_sha256": checkpoint_0["accepted_manifest_sha256"],
        },
        "advance": {
            "authorized": advance.authorized,
            "reason": advance.reason,
            "accepted_generation": checkpoint_1["accepted_generation"],
            "checkpoint_sha256": checkpoint_sha256(checkpoint_1),
            "previous_checkpoint_sha256": checkpoint_1["previous_checkpoint_sha256"],
            "accepted_registry_sha256": checkpoint_1["accepted_registry_sha256"],
            "accepted_manifest_sha256": checkpoint_1["accepted_manifest_sha256"],
        },
        "stale_replay": {
            "presented_generation": baseline_registry["active_generation"],
            "presented_registry_sha256": baseline_registry_sha256,
            "structurally_valid": True,
            "authorized": stale.authorized,
            "reason": stale.reason,
        },
    }

    _write_canonical(output_dir / "checkpoint-generation-0.json", checkpoint_0)
    _write_canonical(output_dir / "checkpoint-generation-1.json", checkpoint_1)
    _write_canonical(output_dir / "consumer-checkpoint-attested-result.json", result)
    print(json.dumps(result, sort_keys=True, separators=(",", ":")))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
