#!/usr/bin/env python3
"""Independently recompute portable multi-epoch causal evolution from proof bytes."""
from __future__ import annotations

import argparse
import json
from pathlib import Path

from liminal.causal_evolution_evidence import load_json, sha256_bytes
from liminal.causal_evolution_proof_materials import controls, digest, verify_path_a, verify_path_b
from liminal.causal_state_evolution import compare_multi_epoch_causal_evolution, validate_anchor_checkpoint, validate_anchor_witness
from liminal.recovery_trust_root_registry import canonical_json_bytes

SCHEMA = "liminal-portable-causal-state-evolution-external-audit/v0.1"


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--proof-dir", required=True)
    parser.add_argument("--repository-root", default=".")
    parser.add_argument("--output", required=True)
    args = parser.parse_args()

    proof_dir = Path(args.proof_dir).resolve()
    result_path = proof_dir / "portable-causal-state-evolution-result.json"
    result = load_json(result_path)
    if result.get("verified") is not True or result.get("reason") != "portable_causal_state_evolution_verified":
        raise ValueError("evolution_result_unverified")
    materials = proof_dir / "materials"
    anchor_dir, path_a_dir, path_b_dir = materials / "anchor-proof", materials / "path-a", materials / "path-b"
    anchor_path = anchor_dir / "downstream-causal-state-portability-result.json"
    anchor = load_json(anchor_path)
    checkpoint, witness = anchor.get("checkpoint"), anchor.get("witness")
    if not isinstance(checkpoint, dict) or not isinstance(witness, dict):
        raise ValueError("anchor_objects_missing")
    if not validate_anchor_checkpoint(checkpoint) or not validate_anchor_witness(witness, checkpoint):
        raise ValueError("anchor_invalid")
    if sha256_bytes(anchor_path.read_bytes()) != result.get("anchor_result_sha256"):
        raise ValueError("anchor_result_digest_mismatch")

    rotation = load_json(materials / "portable-rotation-producer-contract-v0.1.json")
    historical_auth = load_json(materials / "portable-rotation-authorization-contract-v0.1.json")
    transition_contract = load_json(materials / "portable-causal-evolution-transition-contract-v0.1.json")
    transition_auth = load_json(materials / "portable-causal-evolution-transition-authorization-contract-v0.1.json")
    ctl = controls(rotation, historical_auth)
    contract_sha, auth_sha = digest(transition_contract), digest(transition_auth)
    primary, a_steps, a_semantics, a_result = verify_path_a(anchor_dir, path_a_dir, ctl, contract_sha, auth_sha)
    secondary, b_steps, b_semantics, signer, _ = verify_path_b(
        anchor_dir, path_b_dir, ctl, contract_sha, auth_sha, Path(args.repository_root).resolve())
    if a_semantics != b_semantics:
        raise ValueError("semantic_recompute_mismatch")
    expected_semantics = {"epoch_0": a_semantics[0], "epoch_1": a_semantics[1], "epoch_2": a_semantics[2]}
    if result.get("semantic_states") != expected_semantics:
        raise ValueError("result_semantic_states_mismatch")
    if sha256_bytes((path_a_dir / "path-a-causal-evolution-result.json").read_bytes()) != result.get("path_a_result_sha256"):
        raise ValueError("path_a_result_digest_mismatch")
    if sha256_bytes((path_b_dir / "signed-evolution-envelope.json").read_bytes()) != result.get("path_b_envelope_sha256"):
        raise ValueError("path_b_envelope_digest_mismatch")
    if signer != result.get("path_b_evolution_signer_authority_id"):
        raise ValueError("path_b_signer_mismatch")
    if a_result.get("transition_sequence_sha256") != digest([step.__dict__ for step in a_steps]):
        raise ValueError("path_a_transition_digest_mismatch")

    agreement = compare_multi_epoch_causal_evolution(
        primary, secondary, anchor_checkpoint=checkpoint, anchor_witness=witness,
        primary_transitions=a_steps, secondary_transitions=b_steps)
    if not agreement.verified or agreement.receipt is None:
        raise ValueError(f"evolution_recompute_rejected:{agreement.reason}")
    if list(agreement.checkpoints) != result.get("checkpoints"):
        raise ValueError("checkpoint_chain_bytes_mismatch")
    if list(agreement.witnesses) != result.get("witnesses"):
        raise ValueError("witness_chain_bytes_mismatch")
    if canonical_json_bytes(agreement.receipt) != canonical_json_bytes(result.get("receipt")):
        raise ValueError("receipt_bytes_mismatch")
    if digest(agreement.receipt) != result.get("receipt_sha256"):
        raise ValueError("receipt_digest_mismatch")

    audit = {
        "schema": SCHEMA, "verified": True,
        "reason": "portable_causal_state_evolution_recomputed",
        "result_sha256": sha256_bytes(result_path.read_bytes()),
        "path_b_evolution_signer_authority_id": signer,
        "epochs_advanced": agreement.receipt["epochs_advanced"],
        "final_causal_epoch": agreement.receipt["final_causal_epoch"],
        "final_semantic_state_sha256": agreement.receipt["final_semantic_state_sha256"],
        "final_checkpoint_sha256": agreement.receipt["final_checkpoint_sha256"],
        "final_witness_sha256": agreement.receipt["final_witness_sha256"],
        "receipt_sha256": result["receipt_sha256"],
        "raw_history_embedded": agreement.receipt["raw_history_embedded"],
    }
    output = Path(args.output).resolve()
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_bytes(canonical_json_bytes(audit))
    print(json.dumps(audit, sort_keys=True, separators=(",", ":")))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
