#!/usr/bin/env python3
"""Independently recompute downstream causal-state portability from proof bytes."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from liminal.downstream_causal_state_portability import (
    CHECKPOINT_ROLE,
    WITNESS_ROLE,
    CausalAuthority,
    HistoricalStateObservation,
    compare_downstream_causal_states,
)
from liminal.recovery_trust_root_registry import canonical_json_bytes, sha256_hex

SCHEMA = "liminal-downstream-causal-state-portability-external-audit/v0.1"
LOGICAL_STATE_ID = "liminal.trusted-recovery.authorization-state"


def load_json(path: Path) -> dict:
    value = json.loads(path.read_text())
    if not isinstance(value, dict):
        raise ValueError(f"json_object_required:{path}")
    return value


def digest(value: object) -> str:
    return sha256_hex(canonical_json_bytes(value))


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--proof-dir", required=True)
    parser.add_argument("--output", required=True)
    args = parser.parse_args()

    proof_dir = Path(args.proof_dir).resolve()
    result_path = proof_dir / "downstream-causal-state-portability-result.json"
    result = load_json(result_path)
    if result.get("verified") is not True:
        raise ValueError("downstream_result_unverified")
    if result.get("reason") != "downstream_causal_state_portability_verified":
        raise ValueError("downstream_result_reason_invalid")

    materials = proof_dir / "materials"
    historical_dir = materials / "historical-proof"
    historical_result = load_json(
        historical_dir / "historical-trust-base-portability-result.json"
    )
    historical_audit = load_json(materials / "historical-audit.json")
    historical_attestation = materials / "historical-attestation-verification.json"
    if not historical_attestation.is_file() or historical_attestation.stat().st_size == 0:
        raise ValueError("historical_attestation_verification_missing")
    if historical_result.get("verified") is not True:
        raise ValueError("historical_result_unverified")
    if historical_audit.get("verified") is not True:
        raise ValueError("historical_audit_unverified")
    if historical_audit.get("receipt_sha256") != historical_result.get("receipt_sha256"):
        raise ValueError("historical_audit_receipt_mismatch")

    receipt = historical_result.get("receipt")
    if not isinstance(receipt, dict):
        raise ValueError("historical_receipt_missing")
    if receipt.get("trust_state_digest_a") != receipt.get("trust_state_digest_b"):
        raise ValueError("historical_semantic_state_divergence")

    a_registry = load_json(historical_dir / "materials/root-a/generation-1-registry.json")
    b_registry = load_json(historical_dir / "materials/root-b/registry.json")
    if digest(a_registry) != historical_result["primary"]["registry_sha256"]:
        raise ValueError("root_a_registry_digest_mismatch")
    if digest(b_registry) != historical_result["secondary"]["registry_sha256"]:
        raise ValueError("root_b_registry_digest_mismatch")

    primary = HistoricalStateObservation(
        verified=True,
        provider_id=str(result["primary_provenance"]["provider_id"]),
        genesis_authority_id=str(receipt["genesis_a_authority_id"]),
        history_generation=int(a_registry["active_generation"]),
        registry_sha256=digest(a_registry),
        manifest_sha256=str(a_registry["active_manifest_sha256"]),
        semantic_state_sha256=str(receipt["trust_state_digest_a"]),
        trust_domain="liminal.trusted-recovery",
    )
    secondary = HistoricalStateObservation(
        verified=True,
        provider_id=str(result["secondary_provenance"]["provider_id"]),
        genesis_authority_id=str(receipt["genesis_b_authority_id"]),
        history_generation=int(b_registry["active_generation"]),
        registry_sha256=digest(b_registry),
        manifest_sha256=str(b_registry["active_manifest_sha256"]),
        semantic_state_sha256=str(receipt["trust_state_digest_b"]),
        trust_domain="liminal.trusted-recovery",
    )

    checkpoint_producer = load_json(
        materials / "portable-causal-checkpoint-producer-contract-v0.1.json"
    )
    checkpoint_authorization = load_json(
        materials / "portable-causal-checkpoint-authorization-contract-v0.1.json"
    )
    witness_producer = load_json(
        materials / "portable-causal-witness-producer-contract-v0.1.json"
    )
    witness_authorization = load_json(
        materials / "portable-causal-witness-authorization-contract-v0.1.json"
    )
    checkpoint_authority = CausalAuthority(
        role=CHECKPOINT_ROLE,
        logical_authority_id="liminal.trusted-recovery.causal-checkpoint",
        producer_contract_sha256=digest(checkpoint_producer),
        authorization_contract_sha256=digest(checkpoint_authorization),
    )
    witness_authority = CausalAuthority(
        role=WITNESS_ROLE,
        logical_authority_id="liminal.trusted-recovery.causal-witness",
        producer_contract_sha256=digest(witness_producer),
        authorization_contract_sha256=digest(witness_authorization),
    )
    agreement = compare_downstream_causal_states(
        primary,
        secondary,
        logical_state_id=LOGICAL_STATE_ID,
        causal_epoch=0,
        checkpoint_authority=checkpoint_authority,
        witness_authority=witness_authority,
    )
    if not agreement.verified:
        raise ValueError(f"downstream_recompute_rejected:{agreement.reason}")
    if agreement.checkpoint is None or agreement.witness is None or agreement.receipt is None:
        raise ValueError("downstream_recompute_missing")

    if canonical_json_bytes(agreement.checkpoint) != canonical_json_bytes(result["checkpoint"]):
        raise ValueError("checkpoint_bytes_mismatch")
    if canonical_json_bytes(agreement.witness) != canonical_json_bytes(result["witness"]):
        raise ValueError("witness_bytes_mismatch")
    if canonical_json_bytes(agreement.receipt) != canonical_json_bytes(result["receipt"]):
        raise ValueError("receipt_bytes_mismatch")
    if digest(agreement.checkpoint) != result["checkpoint_sha256"]:
        raise ValueError("checkpoint_digest_mismatch")
    if digest(agreement.witness) != result["witness_sha256"]:
        raise ValueError("witness_digest_mismatch")
    if digest(agreement.receipt) != result["receipt_sha256"]:
        raise ValueError("receipt_digest_mismatch")

    audit = {
        "schema": SCHEMA,
        "verified": True,
        "reason": "downstream_causal_state_portability_recomputed",
        "result_sha256": sha256_hex(result_path.read_bytes()),
        "upstream_historical_receipt_sha256": historical_result["receipt_sha256"],
        "semantic_state_sha256": primary.semantic_state_sha256,
        "checkpoint_sha256": result["checkpoint_sha256"],
        "witness_sha256": result["witness_sha256"],
        "receipt_sha256": result["receipt_sha256"],
        "raw_history_embedded": result["receipt"]["raw_history_embedded"],
    }
    output = Path(args.output).resolve()
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_bytes(canonical_json_bytes(audit))
    print(json.dumps(audit, sort_keys=True, separators=(",", ":")))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
