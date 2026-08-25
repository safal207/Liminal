#!/usr/bin/env python3
"""Verify portable causal fork/reconciliation from independent branch evidence."""

from __future__ import annotations

import argparse
import json
import shutil
from pathlib import Path
from typing import Any

from liminal.causal_fork_evidence import (
    load_json,
    sha256_bytes,
    verify_ed25519_envelope,
    verify_source_material,
)
from liminal.causal_fork_proof_materials import (
    BRANCH_A_ID,
    BRANCH_A_PROVIDER,
    BRANCH_B_ID,
    BRANCH_B_PROVIDER,
    build_branch_package,
    common_chain,
    digest,
    observation,
    vote,
)
from liminal.causal_fork_reconciliation import compare_causal_fork_reconciliation
from liminal.recovery_trust_root_registry import canonical_json_bytes

SCHEMA = "liminal-causal-fork-reconciliation-portability-proof/v0.1"
BRANCH_A_SCHEMA = "liminal-causal-fork-branch-a-result/v0.1"
BRANCH_A_REASON = "causal_fork_branch_a_evidence_verified"
BRANCH_B_CLAIM_SCHEMA = "liminal-causal-fork-branch-b-claim/v0.1"


def _assert_package(actual: dict[str, Any], expected: dict[str, Any], *, label: str) -> None:
    for key in (
        "branch_observation",
        "branch_checkpoint",
        "branch_witness",
        "reconciliation_vote",
        "branch_semantic_state_sha256",
        "sibling_branch_semantic_state_sha256",
        "reconciliation_target_semantic_state_sha256",
    ):
        if canonical_json_bytes(actual.get(key)) != canonical_json_bytes(expected[key]):
            raise ValueError(f"{label}_package_mismatch:{key}")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--evolution-proof-dir", required=True)
    parser.add_argument("--branch-a-dir", required=True)
    parser.add_argument("--branch-a-attestation", required=True)
    parser.add_argument("--branch-a-workflow-sha", required=True)
    parser.add_argument("--branch-b-dir", required=True)
    parser.add_argument("--branch-a-policy", required=True)
    parser.add_argument("--branch-b-policy", required=True)
    parser.add_argument("--reconciliation-policy", required=True)
    parser.add_argument("--branch-contract", required=True)
    parser.add_argument("--branch-authorization-contract", required=True)
    parser.add_argument("--reconciliation-contract", required=True)
    parser.add_argument("--reconciliation-authorization-contract", required=True)
    parser.add_argument("--repository-root", default=".")
    parser.add_argument("--workflow-sha", required=True)
    parser.add_argument("--output-dir", required=True)
    args = parser.parse_args()

    evolution_dir = Path(args.evolution_proof_dir).resolve()
    evolution_result_path = evolution_dir / "portable-causal-state-evolution-result.json"
    evolution_result = load_json(evolution_result_path)
    anchor_result = load_json(
        evolution_dir
        / "materials/anchor-proof/downstream-causal-state-portability-result.json"
    )
    anchor_checkpoint, anchor_witness, common_checkpoints, common_witnesses = common_chain(
        evolution_result, anchor_result
    )
    common_checkpoint = common_checkpoints[-1]
    common_witness = common_witnesses[-1]
    common_result_sha256 = sha256_bytes(evolution_result_path.read_bytes())

    policy_paths = {
        "branch_a_policy": Path(args.branch_a_policy).resolve(),
        "branch_b_policy": Path(args.branch_b_policy).resolve(),
        "reconciliation_policy": Path(args.reconciliation_policy).resolve(),
        "branch_contract": Path(args.branch_contract).resolve(),
        "branch_authorization_contract": Path(
            args.branch_authorization_contract
        ).resolve(),
        "reconciliation_contract": Path(args.reconciliation_contract).resolve(),
        "reconciliation_authorization_contract": Path(
            args.reconciliation_authorization_contract
        ).resolve(),
    }
    values = {name: load_json(path) for name, path in policy_paths.items()}

    branch_a_attestation = Path(args.branch_a_attestation).resolve()
    if not branch_a_attestation.is_file() or branch_a_attestation.stat().st_size == 0:
        raise ValueError("branch_a_attestation_verification_missing")
    branch_a_dir = Path(args.branch_a_dir).resolve()
    branch_a_result_path = branch_a_dir / "causal-fork-branch-a-result.json"
    branch_a_result = load_json(branch_a_result_path)
    branch_a_authority = (
        "github-oidc:safal207/Liminal:causal-fork-branch-a-producer@"
        f"{args.branch_a_workflow_sha}"
    )
    if (
        branch_a_result.get("schema") != BRANCH_A_SCHEMA
        or branch_a_result.get("verified") is not True
        or branch_a_result.get("reason") != BRANCH_A_REASON
        or branch_a_result.get("workflow_sha") != args.branch_a_workflow_sha
        or branch_a_result.get("provider_id") != BRANCH_A_PROVIDER
        or branch_a_result.get("authority_id") != branch_a_authority
        or branch_a_result.get("common_result_sha256") != common_result_sha256
    ):
        raise ValueError("branch_a_result_invalid")
    expected_a = build_branch_package(
        common_result_sha256=common_result_sha256,
        common_checkpoint=common_checkpoint,
        common_witness=common_witness,
        provider_id=BRANCH_A_PROVIDER,
        authority_id=branch_a_authority,
        evidence_identity=args.branch_a_workflow_sha,
        logical_branch_id=BRANCH_A_ID,
        branch_policy=values["branch_a_policy"],
        sibling_branch_policy=values["branch_b_policy"],
        sibling_logical_branch_id=BRANCH_B_ID,
        reconciliation_policy=values["reconciliation_policy"],
        branch_contract=values["branch_contract"],
        branch_authorization_contract=values["branch_authorization_contract"],
        reconciliation_contract=values["reconciliation_contract"],
        reconciliation_authorization_contract=values[
            "reconciliation_authorization_contract"
        ],
    )
    _assert_package(branch_a_result, expected_a, label="branch_a")

    branch_b_dir = Path(args.branch_b_dir).resolve()
    branch_b_envelope_path = branch_b_dir / "signed-branch-envelope.json"
    branch_b_authority, branch_b_claim = verify_ed25519_envelope(
        branch_b_dir / "branch-b-public-key.pem",
        load_json(branch_b_envelope_path),
    )
    if (
        branch_b_claim.get("schema") != BRANCH_B_CLAIM_SCHEMA
        or branch_b_claim.get("provider_id") != BRANCH_B_PROVIDER
        or branch_b_claim.get("common_result_sha256") != common_result_sha256
    ):
        raise ValueError("branch_b_claim_invalid")
    verify_source_material(
        Path(args.repository_root).resolve(),
        branch_b_claim.get("source_material"),
    )
    evidence_identity = str(branch_b_claim.get("evidence_identity", ""))
    if not evidence_identity:
        raise ValueError("branch_b_evidence_identity_missing")
    expected_b = build_branch_package(
        common_result_sha256=common_result_sha256,
        common_checkpoint=common_checkpoint,
        common_witness=common_witness,
        provider_id=BRANCH_B_PROVIDER,
        authority_id=branch_b_authority,
        evidence_identity=evidence_identity,
        logical_branch_id=BRANCH_B_ID,
        branch_policy=values["branch_b_policy"],
        sibling_branch_policy=values["branch_a_policy"],
        sibling_logical_branch_id=BRANCH_A_ID,
        reconciliation_policy=values["reconciliation_policy"],
        branch_contract=values["branch_contract"],
        branch_authorization_contract=values["branch_authorization_contract"],
        reconciliation_contract=values["reconciliation_contract"],
        reconciliation_authorization_contract=values[
            "reconciliation_authorization_contract"
        ],
    )
    _assert_package(branch_b_claim, expected_b, label="branch_b")

    agreement = compare_causal_fork_reconciliation(
        anchor_checkpoint=anchor_checkpoint,
        anchor_witness=anchor_witness,
        common_checkpoints=common_checkpoints,
        common_witnesses=common_witnesses,
        primary_branch=observation(branch_a_result["branch_observation"]),
        secondary_branch=observation(branch_b_claim["branch_observation"]),
        primary_vote=vote(branch_a_result["reconciliation_vote"]),
        secondary_vote=vote(branch_b_claim["reconciliation_vote"]),
    )
    if not agreement.verified or agreement.receipt is None:
        raise ValueError(f"fork_reconciliation_rejected:{agreement.reason}")
    if (
        agreement.reconciliation_checkpoint is None
        or agreement.reconciliation_witness is None
    ):
        raise ValueError("fork_reconciliation_objects_missing")

    result = {
        "schema": SCHEMA,
        "verified": True,
        "reason": agreement.reason,
        "workflow_sha": args.workflow_sha,
        "common_evolution_workflow_sha": evolution_result["workflow_sha"],
        "common_result_sha256": common_result_sha256,
        "branch_a_workflow_sha": args.branch_a_workflow_sha,
        "branch_a_result_sha256": sha256_bytes(branch_a_result_path.read_bytes()),
        "branch_b_authority_id": branch_b_authority,
        "branch_b_envelope_sha256": sha256_bytes(branch_b_envelope_path.read_bytes()),
        "branch_checkpoints": list(agreement.branch_checkpoints),
        "branch_witnesses": list(agreement.branch_witnesses),
        "reconciliation_checkpoint": agreement.reconciliation_checkpoint,
        "reconciliation_witness": agreement.reconciliation_witness,
        "receipt": agreement.receipt,
        "receipt_sha256": digest(agreement.receipt),
        "claim_boundary": {
            "common_multi_epoch_prefix_validated": True,
            "branch_a_github_oidc_attestation_verified": True,
            "branch_b_detached_ed25519_signature_verified": True,
            "branch_semantics_genuinely_divergent": True,
            "both_parent_lineages_preserved": True,
            "canonical_branch_order": True,
            "raw_provider_evidence_embedded_in_portable_objects": False,
            "arbitrary_multi_parent_reconciliation": False,
            "organizational_governance_independence": False,
            "hardware_provenance_independence": False,
            "storage_provider_independence": False,
            "network_path_independence": False,
        },
    }

    output_dir = Path(args.output_dir).resolve()
    output_dir.mkdir(parents=True, exist_ok=True)
    materials = output_dir / "materials"
    shutil.copytree(evolution_dir, materials / "evolution-proof")
    shutil.copytree(branch_a_dir, materials / "branch-a")
    shutil.copytree(branch_b_dir, materials / "branch-b")
    shutil.copyfile(
        branch_a_attestation,
        materials / "branch-a-attestation-verification.json",
    )
    for path in policy_paths.values():
        shutil.copyfile(path, materials / path.name)
    result_path = output_dir / "causal-fork-reconciliation-portability-result.json"
    result_path.write_bytes(canonical_json_bytes(result))
    print(json.dumps(result, sort_keys=True, separators=(",", ":")))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
