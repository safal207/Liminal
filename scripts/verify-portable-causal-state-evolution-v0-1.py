#!/usr/bin/env python3
"""Verify portable two-epoch causal evolution across independent history paths."""
from __future__ import annotations

import argparse
import json
import shutil
from pathlib import Path

from liminal.causal_evolution_evidence import load_json, sha256_bytes
from liminal.causal_evolution_proof_materials import (
    ANCHOR_WORKFLOW_SHA,
    BOOTSTRAP_SOURCE_SHA,
    controls,
    digest,
    verify_path_a,
    verify_path_b,
)
from liminal.causal_state_evolution import (
    compare_multi_epoch_causal_evolution,
    validate_anchor_checkpoint,
    validate_anchor_witness,
)
from liminal.recovery_trust_root_registry import canonical_json_bytes

SCHEMA = "liminal-portable-causal-state-evolution-proof/v0.1"


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--anchor-proof-dir", required=True)
    parser.add_argument("--path-a-dir", required=True)
    parser.add_argument("--path-a-attestation", required=True)
    parser.add_argument("--path-b-dir", required=True)
    parser.add_argument("--rotation-contract", required=True)
    parser.add_argument("--historical-authorization-contract", required=True)
    parser.add_argument("--transition-contract", required=True)
    parser.add_argument("--transition-authorization-contract", required=True)
    parser.add_argument("--repository-root", default=".")
    parser.add_argument("--workflow-sha", required=True)
    parser.add_argument("--output-dir", required=True)
    args = parser.parse_args()

    anchor_dir = Path(args.anchor_proof_dir).resolve()
    path_a_dir = Path(args.path_a_dir).resolve()
    path_b_dir = Path(args.path_b_dir).resolve()
    output_dir = Path(args.output_dir).resolve()
    path_a_attestation = Path(args.path_a_attestation).resolve()
    if not path_a_attestation.is_file() or path_a_attestation.stat().st_size == 0:
        raise ValueError("path_a_attestation_verification_missing")

    anchor_path = anchor_dir / "downstream-causal-state-portability-result.json"
    anchor = load_json(anchor_path)
    if anchor.get("verified") is not True or anchor.get("reason") != "downstream_causal_state_portability_verified":
        raise ValueError("downstream_anchor_unverified")
    if anchor.get("workflow_sha") != ANCHOR_WORKFLOW_SHA:
        raise ValueError("downstream_anchor_workflow_invalid")
    checkpoint, witness = anchor.get("checkpoint"), anchor.get("witness")
    if not isinstance(checkpoint, dict) or not isinstance(witness, dict):
        raise ValueError("downstream_anchor_objects_missing")
    if not validate_anchor_checkpoint(checkpoint) or not validate_anchor_witness(witness, checkpoint):
        raise ValueError("downstream_anchor_invalid")

    rotation = load_json(Path(args.rotation_contract).resolve())
    historical_auth = load_json(Path(args.historical_authorization_contract).resolve())
    transition_contract = load_json(Path(args.transition_contract).resolve())
    transition_auth = load_json(Path(args.transition_authorization_contract).resolve())
    ctl = controls(rotation, historical_auth)
    contract_sha, auth_sha = digest(transition_contract), digest(transition_auth)

    primary, a_steps, a_semantics, a_result = verify_path_a(anchor_dir, path_a_dir, ctl, contract_sha, auth_sha)
    secondary, b_steps, b_semantics, b_signer, _ = verify_path_b(
        anchor_dir, path_b_dir, ctl, contract_sha, auth_sha, Path(args.repository_root).resolve())
    if a_semantics != b_semantics:
        raise ValueError("path_semantic_convergence_mismatch")
    if a_semantics[0] != anchor.get("semantic_state_sha256"):
        raise ValueError("anchor_semantic_state_mismatch")

    agreement = compare_multi_epoch_causal_evolution(
        primary, secondary, anchor_checkpoint=checkpoint, anchor_witness=witness,
        primary_transitions=a_steps, secondary_transitions=b_steps)
    if not agreement.verified or agreement.receipt is None:
        raise ValueError(f"portable_causal_evolution_rejected:{agreement.reason}")
    if len(agreement.checkpoints) != 2 or len(agreement.witnesses) != 2:
        raise ValueError("portable_causal_evolution_length_invalid")

    result = {
        "schema": SCHEMA,
        "verified": True,
        "reason": agreement.reason,
        "workflow_sha": args.workflow_sha,
        "anchor_workflow_sha": ANCHOR_WORKFLOW_SHA,
        "bootstrap_source_sha": BOOTSTRAP_SOURCE_SHA,
        "anchor_result_sha256": sha256_bytes(anchor_path.read_bytes()),
        "path_a_result_sha256": sha256_bytes((path_a_dir / "path-a-causal-evolution-result.json").read_bytes()),
        "path_b_envelope_sha256": sha256_bytes((path_b_dir / "signed-evolution-envelope.json").read_bytes()),
        "path_b_evolution_signer_authority_id": b_signer,
        "semantic_states": {"epoch_0": a_semantics[0], "epoch_1": a_semantics[1], "epoch_2": a_semantics[2]},
        "history_schedules": {"path_a": [[1, 3], [3, 4]], "path_b": [[1, 2], [2, 5]]},
        "checkpoints": list(agreement.checkpoints),
        "witnesses": list(agreement.witnesses),
        "receipt": agreement.receipt,
        "receipt_sha256": digest(agreement.receipt),
        "claim_boundary": {
            "tested_causal_epochs_advanced": 2,
            "full_checkpoint_prefix_validated": True,
            "full_witness_prefix_validated": True,
            "raw_history_embedded_in_portable_state": False,
            "path_a_github_oidc_attestation_verified": True,
            "path_b_detached_ed25519_signature_verified": True,
            "arbitrary_unbounded_epoch_evolution": False,
            "organizational_governance_independence": False,
            "hardware_provenance_independence": False,
            "storage_provider_independence": False,
            "network_path_independence": False,
        },
    }
    if a_result.get("workflow_sha") != BOOTSTRAP_SOURCE_SHA:
        raise ValueError("path_a_bootstrap_binding_mismatch")

    output_dir.mkdir(parents=True, exist_ok=True)
    materials = output_dir / "materials"
    shutil.copytree(anchor_dir, materials / "anchor-proof")
    shutil.copytree(path_a_dir, materials / "path-a")
    shutil.copytree(path_b_dir, materials / "path-b")
    shutil.copyfile(path_a_attestation, materials / "path-a-attestation-verification.json")
    for item in (
        Path(args.rotation_contract).resolve(), Path(args.historical_authorization_contract).resolve(),
        Path(args.transition_contract).resolve(), Path(args.transition_authorization_contract).resolve()):
        shutil.copyfile(item, materials / item.name)
    result_path = output_dir / "portable-causal-state-evolution-result.json"
    result_path.write_bytes(canonical_json_bytes(result))
    print(json.dumps(result, sort_keys=True, separators=(",", ":")))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
