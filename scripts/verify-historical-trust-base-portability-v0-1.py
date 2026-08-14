#!/usr/bin/env python3
"""Reconstruct Root A and signed independent Root B, then compare semantics."""

from __future__ import annotations

import argparse
import json
import shutil
from pathlib import Path

from liminal.historical_root_b_evidence import (
    load_json,
    sha256_bytes,
    verify_ed25519_envelope,
    verify_genesis_source_material,
)
from liminal.historical_trust_base_portability import (
    HistoricalTrustPath,
    TerminalTrustControls,
    compare_historical_trust_bases,
    trust_state_digest,
)
from liminal.recovery_trust_root_registry import (
    canonical_json_bytes,
    evaluate_registry_rotation,
    sha256_hex,
    validate_registry,
)

SCHEMA = "liminal-historical-trust-base-portability-proof/v0.1"
ROOT_A_AUTHORITY = (
    "github-oidc:safal207/Liminal:"
    "trusted-recovery-trust-root-rotation-drill@"
    "e2cb6a014236bc561d03c405f4986146026041fa"
)
AUTHORITY_IDS = (
    "liminal.trusted-recovery.builder",
    "liminal.trusted-recovery.verifier",
)


def digest(value: object) -> str:
    return sha256_hex(canonical_json_bytes(value))


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--primary-rotation-dir", required=True)
    parser.add_argument("--primary-baseline-registry", required=True)
    parser.add_argument("--primary-baseline-manifest", required=True)
    parser.add_argument("--primary-attestation", required=True)
    parser.add_argument("--root-b-dir", required=True)
    parser.add_argument("--rotation-contract", required=True)
    parser.add_argument("--authorization-contract", required=True)
    parser.add_argument("--repository-root", default=".")
    parser.add_argument("--workflow-sha", required=True)
    parser.add_argument("--output-dir", required=True)
    args = parser.parse_args()

    primary_dir = Path(args.primary_rotation_dir).resolve()
    root_b_dir = Path(args.root_b_dir).resolve()
    output_dir = Path(args.output_dir).resolve()
    baseline_registry_path = Path(args.primary_baseline_registry).resolve()
    baseline_manifest_path = Path(args.primary_baseline_manifest).resolve()
    attestation_path = Path(args.primary_attestation).resolve()

    if not attestation_path.is_file() or attestation_path.stat().st_size == 0:
        raise ValueError("primary_attestation_verification_missing")

    a_reg0 = load_json(baseline_registry_path)
    a_gen0 = load_json(baseline_manifest_path)
    a_gen1 = load_json(primary_dir / "generation-1-manifest.json")
    a_reg1 = load_json(primary_dir / "generation-1-registry.json")
    a_result = load_json(primary_dir / "rotation-drill-result.json")

    a_gen0_path = str(a_reg0["history"][0]["manifest_path"])
    a_manifests = {
        a_gen0_path: a_gen0,
        "drill/generation-1-manifest.json": a_gen1,
    }
    if not validate_registry(a_reg0, {a_gen0_path: a_gen0}):
        raise ValueError("root_a_genesis_registry_invalid")
    if not validate_registry(a_reg1, a_manifests):
        raise ValueError("root_a_terminal_registry_invalid")

    legitimate = a_result.get("legitimate_rotation")
    if a_result.get("verified") is not True or not isinstance(legitimate, dict):
        raise ValueError("root_a_rotation_result_invalid")
    if legitimate.get("authorized") is not True:
        raise ValueError("root_a_rotation_not_authorized")
    if legitimate.get("reason") != "registry_rotation_authorized":
        raise ValueError("root_a_rotation_reason_invalid")
    if a_result["baseline"]["registry_sha256"] != digest(a_reg0):
        raise ValueError("root_a_genesis_registry_digest_mismatch")
    if a_result["baseline"]["active_manifest_sha256"] != digest(a_gen0):
        raise ValueError("root_a_genesis_manifest_digest_mismatch")
    if legitimate["manifest_sha256"] != digest(a_gen1):
        raise ValueError("root_a_terminal_manifest_digest_mismatch")
    if legitimate["registry_sha256"] != digest(a_reg1):
        raise ValueError("root_a_terminal_registry_digest_mismatch")

    b_gen0 = load_json(root_b_dir / "generation-0-manifest.json")
    b_gen1 = load_json(root_b_dir / "generation-1-manifest.json")
    b_reg1 = load_json(root_b_dir / "registry.json")
    envelope = load_json(root_b_dir / "signed-history-envelope.json")
    b_manifests = {
        str(b_reg1["history"][0]["manifest_path"]): b_gen0,
        str(b_reg1["history"][1]["manifest_path"]): b_gen1,
    }
    if not validate_registry(b_reg1, b_manifests):
        raise ValueError("root_b_terminal_registry_invalid")

    b_reg0 = {
        "schema_version": b_reg1["schema_version"],
        "active_generation": 0,
        "active_manifest_sha256": b_reg1["history"][0]["manifest_sha256"],
        "history": b_reg1["history"][:1],
    }
    if not validate_registry(
        b_reg0,
        {str(b_reg1["history"][0]["manifest_path"]): b_gen0},
    ):
        raise ValueError("root_b_genesis_registry_invalid")
    b_rotation = evaluate_registry_rotation(b_reg0, b_reg1, b_manifests)
    if not b_rotation.authorized:
        raise ValueError(f"root_b_rotation_rejected:{b_rotation.reason}")

    verify_genesis_source_material(Path(args.repository_root).resolve(), b_gen0)
    b_authority = verify_ed25519_envelope(
        root_b_dir / "root-b-public-key.pem",
        envelope,
    )
    claim = envelope["claim"]
    assert isinstance(claim, dict)
    if claim.get("registry_sha256") != digest(b_reg1):
        raise ValueError("root_b_signed_registry_digest_mismatch")
    if claim.get("genesis_manifest_sha256") != digest(b_gen0):
        raise ValueError("root_b_signed_genesis_digest_mismatch")
    if claim.get("terminal_manifest_sha256") != digest(b_gen1):
        raise ValueError("root_b_signed_terminal_digest_mismatch")

    controls = TerminalTrustControls(
        trust_domain="liminal.trusted-recovery",
        authority_ids=AUTHORITY_IDS,
        threshold=2,
        rotation_contract_sha256=digest(load_json(Path(args.rotation_contract))),
        authorization_contract_sha256=digest(
            load_json(Path(args.authorization_contract))
        ),
    )
    expected_controls = {
        "authority_ids": list(controls.authority_ids),
        "threshold": controls.threshold,
        "rotation_contract_sha256": controls.rotation_contract_sha256,
        "authorization_contract_sha256": controls.authorization_contract_sha256,
    }
    if claim.get("terminal_controls") != expected_controls:
        raise ValueError("root_b_terminal_controls_mismatch")

    a_path = HistoricalTrustPath(
        verified=True,
        genesis_authority_id=ROOT_A_AUTHORITY,
        registry=a_reg1,
        manifests=a_manifests,
        controls=controls,
    )
    b_path = HistoricalTrustPath(
        verified=True,
        genesis_authority_id=b_authority,
        registry=b_reg1,
        manifests=b_manifests,
        controls=controls,
    )
    agreement = compare_historical_trust_bases(a_path, b_path)
    if not agreement.verified or agreement.receipt is None:
        raise ValueError(f"historical_trust_base_rejected:{agreement.reason}")

    result = {
        "schema": SCHEMA,
        "verified": True,
        "reason": agreement.reason,
        "workflow_sha": args.workflow_sha,
        "primary": {
            "genesis_authority_id": ROOT_A_AUTHORITY,
            "registry_sha256": digest(a_reg1),
            "trust_state_digest": trust_state_digest(a_path),
            "attestation_verified": True,
        },
        "secondary": {
            "genesis_authority_id": b_authority,
            "registry_sha256": digest(b_reg1),
            "trust_state_digest": trust_state_digest(b_path),
            "signature_verified": True,
            "source_material_verified": True,
        },
        "receipt": agreement.receipt,
        "receipt_sha256": digest(agreement.receipt),
        "root_b_envelope_sha256": sha256_bytes(
            (root_b_dir / "signed-history-envelope.json").read_bytes()
        ),
        "claim_boundary": claim["claim_boundary"],
    }

    output_dir.mkdir(parents=True, exist_ok=True)
    materials = output_dir / "materials"
    (materials / "root-a").mkdir(parents=True, exist_ok=True)
    (materials / "root-b").mkdir(parents=True, exist_ok=True)
    copies = {
        baseline_manifest_path: materials / "root-a/generation-0-manifest.json",
        baseline_registry_path: materials / "root-a/generation-0-registry.json",
        primary_dir / "generation-1-manifest.json": (
            materials / "root-a/generation-1-manifest.json"
        ),
        primary_dir / "generation-1-registry.json": (
            materials / "root-a/generation-1-registry.json"
        ),
        primary_dir / "rotation-drill-result.json": (
            materials / "root-a/rotation-drill-result.json"
        ),
        attestation_path: (
            materials / "root-a/rotation-result-attestation-verification.json"
        ),
    }
    for source, target in copies.items():
        shutil.copyfile(source, target)
    for name in (
        "generation-0-manifest.json",
        "generation-1-manifest.json",
        "registry.json",
        "root-b-public-key.pem",
        "signed-history-envelope.json",
    ):
        shutil.copyfile(root_b_dir / name, materials / "root-b" / name)

    result_path = output_dir / "historical-trust-base-portability-result.json"
    result_path.write_bytes(canonical_json_bytes(result))
    print(json.dumps(result, sort_keys=True, separators=(",", ":")))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
