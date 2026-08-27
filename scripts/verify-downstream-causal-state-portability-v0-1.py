#!/usr/bin/env python3
"""Build portable downstream checkpoint/witness from a verified historical proof."""

from __future__ import annotations

import argparse
import json
import shutil
from pathlib import Path

from liminal.downstream_causal_state_portability import (
    CHECKPOINT_ROLE,
    WITNESS_ROLE,
    CausalAuthority,
    HistoricalStateObservation,
    compare_downstream_causal_states,
)
from liminal.recovery_trust_root_registry import canonical_json_bytes, sha256_hex

SCHEMA = "liminal-downstream-causal-state-portability-proof/v0.1"
LOGICAL_STATE_ID = "liminal.trusted-recovery.authorization-state"
ROOT_A_PROVIDER = "github-oidc-root-a"
ROOT_B_PROVIDER = "offline-ed25519-root-b"


def load_json(path: Path) -> dict:
    value = json.loads(path.read_text())
    if not isinstance(value, dict):
        raise ValueError(f"json_object_required:{path}")
    return value


def digest(value: object) -> str:
    return sha256_hex(canonical_json_bytes(value))


def _require_verified_upstream(result: dict, audit: dict) -> None:
    if result.get("verified") is not True:
        raise ValueError("historical_result_unverified")
    if result.get("reason") != "historical_trust_base_portability_verified":
        raise ValueError("historical_result_reason_invalid")
    if audit.get("verified") is not True:
        raise ValueError("historical_audit_unverified")
    if audit.get("reason") != "historical_trust_base_portability_recomputed":
        raise ValueError("historical_audit_reason_invalid")
    if audit.get("receipt_sha256") != result.get("receipt_sha256"):
        raise ValueError("historical_audit_receipt_mismatch")
    if audit.get("trust_state_digest") != result["primary"]["trust_state_digest"]:
        raise ValueError("historical_audit_state_mismatch")


def _observation(
    *,
    result_side: dict,
    receipt: dict,
    registry: dict,
    provider_id: str,
    manifest_key: str,
    genesis_key: str,
) -> HistoricalStateObservation:
    registry_digest = digest(registry)
    if registry_digest != result_side.get("registry_sha256"):
        raise ValueError("historical_registry_digest_mismatch")
    receipt_registry_key = (
        "registry_a_sha256" if provider_id == ROOT_A_PROVIDER else "registry_b_sha256"
    )
    if registry_digest != receipt[receipt_registry_key]:
        raise ValueError("historical_receipt_registry_mismatch")
    active_manifest = registry.get("active_manifest_sha256")
    if active_manifest != receipt[manifest_key]:
        raise ValueError("historical_terminal_manifest_mismatch")
    semantic = result_side.get("trust_state_digest")
    if semantic != receipt["trust_state_digest_a"]:
        if semantic != receipt["trust_state_digest_b"]:
            raise ValueError("historical_semantic_state_mismatch")
    return HistoricalStateObservation(
        verified=True,
        provider_id=provider_id,
        genesis_authority_id=str(receipt[genesis_key]),
        history_generation=int(registry["active_generation"]),
        registry_sha256=registry_digest,
        manifest_sha256=str(active_manifest),
        semantic_state_sha256=str(semantic),
        trust_domain="liminal.trusted-recovery",
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--historical-proof-dir", required=True)
    parser.add_argument("--historical-audit", required=True)
    parser.add_argument("--historical-attestation", required=True)
    parser.add_argument("--checkpoint-producer-contract", required=True)
    parser.add_argument("--checkpoint-authorization-contract", required=True)
    parser.add_argument("--witness-producer-contract", required=True)
    parser.add_argument("--witness-authorization-contract", required=True)
    parser.add_argument("--workflow-sha", required=True)
    parser.add_argument("--output-dir", required=True)
    args = parser.parse_args()

    proof_dir = Path(args.historical_proof_dir).resolve()
    output_dir = Path(args.output_dir).resolve()
    attestation_path = Path(args.historical_attestation).resolve()
    if not attestation_path.is_file() or attestation_path.stat().st_size == 0:
        raise ValueError("historical_attestation_verification_missing")

    historical_result = load_json(proof_dir / "historical-trust-base-portability-result.json")
    historical_audit = load_json(Path(args.historical_audit).resolve())
    _require_verified_upstream(historical_result, historical_audit)
    receipt = historical_result.get("receipt")
    if not isinstance(receipt, dict):
        raise ValueError("historical_receipt_missing")
    if receipt.get("histories_independently_valid") is not True:
        raise ValueError("historical_independence_missing")
    if receipt.get("cross_root_dependency") is not False:
        raise ValueError("historical_cross_root_dependency")
    if receipt.get("trust_state_digest_a") != receipt.get("trust_state_digest_b"):
        raise ValueError("historical_semantic_state_divergence")

    a_registry = load_json(proof_dir / "materials/root-a/generation-1-registry.json")
    b_registry = load_json(proof_dir / "materials/root-b/registry.json")
    primary = _observation(
        result_side=historical_result["primary"],
        receipt=receipt,
        registry=a_registry,
        provider_id=ROOT_A_PROVIDER,
        manifest_key="history_a_tip_sha256",
        genesis_key="genesis_a_authority_id",
    )
    secondary = _observation(
        result_side=historical_result["secondary"],
        receipt=receipt,
        registry=b_registry,
        provider_id=ROOT_B_PROVIDER,
        manifest_key="history_b_tip_sha256",
        genesis_key="genesis_b_authority_id",
    )

    checkpoint_producer_path = Path(args.checkpoint_producer_contract).resolve()
    checkpoint_authorization_path = Path(args.checkpoint_authorization_contract).resolve()
    witness_producer_path = Path(args.witness_producer_contract).resolve()
    witness_authorization_path = Path(args.witness_authorization_contract).resolve()
    checkpoint_producer = load_json(checkpoint_producer_path)
    checkpoint_authorization = load_json(checkpoint_authorization_path)
    witness_producer = load_json(witness_producer_path)
    witness_authorization = load_json(witness_authorization_path)

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
        raise ValueError(f"downstream_causal_state_rejected:{agreement.reason}")
    if agreement.checkpoint is None or agreement.witness is None or agreement.receipt is None:
        raise ValueError("downstream_causal_state_result_missing")

    result = {
        "schema": SCHEMA,
        "verified": True,
        "reason": agreement.reason,
        "workflow_sha": args.workflow_sha,
        "upstream_historical_receipt_sha256": historical_result["receipt_sha256"],
        "primary_provenance": {
            "provider_id": primary.provider_id,
            "genesis_authority_id": primary.genesis_authority_id,
            "history_generation": primary.history_generation,
            "registry_sha256": primary.registry_sha256,
            "manifest_sha256": primary.manifest_sha256,
        },
        "secondary_provenance": {
            "provider_id": secondary.provider_id,
            "genesis_authority_id": secondary.genesis_authority_id,
            "history_generation": secondary.history_generation,
            "registry_sha256": secondary.registry_sha256,
            "manifest_sha256": secondary.manifest_sha256,
        },
        "semantic_state_sha256": primary.semantic_state_sha256,
        "checkpoint": agreement.checkpoint,
        "checkpoint_sha256": digest(agreement.checkpoint),
        "witness": agreement.witness,
        "witness_sha256": digest(agreement.witness),
        "receipt": agreement.receipt,
        "receipt_sha256": digest(agreement.receipt),
        "claim_boundary": {
            "raw_downstream_causal_state_portability": True,
            "history_provenance_embedded_in_portable_state": False,
            "organizational_governance_independence": False,
            "hardware_provenance_independence": False,
            "network_path_independence": False,
        },
    }

    output_dir.mkdir(parents=True, exist_ok=True)
    materials = output_dir / "materials"
    materials.mkdir(parents=True, exist_ok=True)
    shutil.copytree(proof_dir, materials / "historical-proof")
    shutil.copyfile(Path(args.historical_audit), materials / "historical-audit.json")
    shutil.copyfile(attestation_path, materials / "historical-attestation-verification.json")
    contract_paths = (
        checkpoint_producer_path,
        checkpoint_authorization_path,
        witness_producer_path,
        witness_authorization_path,
    )
    for contract_path in contract_paths:
        shutil.copyfile(contract_path, materials / contract_path.name)

    result_path = output_dir / "downstream-causal-state-portability-result.json"
    result_path.write_bytes(canonical_json_bytes(result))
    print(json.dumps(result, sort_keys=True, separators=(",", ":")))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
