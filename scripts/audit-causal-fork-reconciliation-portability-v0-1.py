#!/usr/bin/env python3
"""Independently recompute causal fork/reconciliation from bundled proof bytes."""

from __future__ import annotations

import argparse
import json
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

SCHEMA = "liminal-causal-fork-reconciliation-portability-external-audit/v0.1"


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
    parser.add_argument("--proof-dir", required=True)
    parser.add_argument("--repository-root", default=".")
    parser.add_argument("--output", required=True)
    args = parser.parse_args()

    proof_dir = Path(args.proof_dir).resolve()
    result_path = proof_dir / "causal-fork-reconciliation-portability-result.json"
    result = load_json(result_path)
    if (
        result.get("verified") is not True
        or result.get("reason") != "causal_fork_reconciliation_portability_verified"
    ):
        raise ValueError("fork_reconciliation_result_unverified")

    materials = proof_dir / "materials"
    evolution_dir = materials / "evolution-proof"
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
    if common_result_sha256 != result.get("common_result_sha256"):
        raise ValueError("common_result_digest_mismatch")

    policy_names = (
        "portable-causal-fork-state-a-v0.1.json",
        "portable-causal-fork-state-b-v0.1.json",
        "portable-causal-reconciled-state-v0.1.json",
        "portable-causal-fork-branch-contract-v0.1.json",
        "portable-causal-fork-authorization-contract-v0.1.json",
        "portable-causal-reconciliation-contract-v0.1.json",
        "portable-causal-reconciliation-authorization-contract-v0.1.json",
    )
    policy_values = {name: load_json(materials / name) for name in policy_names}

    branch_a_dir = materials / "branch-a"
    branch_a_result_path = branch_a_dir / "causal-fork-branch-a-result.json"
    branch_a_result = load_json(branch_a_result_path)
    if sha256_bytes(branch_a_result_path.read_bytes()) != result.get(
        "branch_a_result_sha256"
    ):
        raise ValueError("branch_a_result_digest_mismatch")
    branch_a_workflow_sha = str(result.get("branch_a_workflow_sha", ""))
    branch_a_authority = (
        "github-oidc:safal207/Liminal:causal-fork-branch-a-producer@"
        f"{branch_a_workflow_sha}"
    )
    expected_a = build_branch_package(
        common_result_sha256=common_result_sha256,
        common_checkpoint=common_checkpoint,
        common_witness=common_witness,
        provider_id=BRANCH_A_PROVIDER,
        authority_id=branch_a_authority,
        evidence_identity=branch_a_workflow_sha,
        logical_branch_id=BRANCH_A_ID,
        branch_policy=policy_values["portable-causal-fork-state-a-v0.1.json"],
        sibling_branch_policy=policy_values[
            "portable-causal-fork-state-b-v0.1.json"
        ],
        sibling_logical_branch_id=BRANCH_B_ID,
        reconciliation_policy=policy_values[
            "portable-causal-reconciled-state-v0.1.json"
        ],
        branch_contract=policy_values[
            "portable-causal-fork-branch-contract-v0.1.json"
        ],
        branch_authorization_contract=policy_values[
            "portable-causal-fork-authorization-contract-v0.1.json"
        ],
        reconciliation_contract=policy_values[
            "portable-causal-reconciliation-contract-v0.1.json"
        ],
        reconciliation_authorization_contract=policy_values[
            "portable-causal-reconciliation-authorization-contract-v0.1.json"
        ],
    )
    _assert_package(branch_a_result, expected_a, label="branch_a")

    branch_b_dir = materials / "branch-b"
    branch_b_envelope_path = branch_b_dir / "signed-branch-envelope.json"
    branch_b_authority, branch_b_claim = verify_ed25519_envelope(
        branch_b_dir / "branch-b-public-key.pem",
        load_json(branch_b_envelope_path),
    )
    verify_source_material(
        Path(args.repository_root).resolve(),
        branch_b_claim.get("source_material"),
    )
    if branch_b_authority != result.get("branch_b_authority_id"):
        raise ValueError("branch_b_authority_mismatch")
    if sha256_bytes(branch_b_envelope_path.read_bytes()) != result.get(
        "branch_b_envelope_sha256"
    ):
        raise ValueError("branch_b_envelope_digest_mismatch")
    expected_b = build_branch_package(
        common_result_sha256=common_result_sha256,
        common_checkpoint=common_checkpoint,
        common_witness=common_witness,
        provider_id=BRANCH_B_PROVIDER,
        authority_id=branch_b_authority,
        evidence_identity=str(branch_b_claim.get("evidence_identity", "")),
        logical_branch_id=BRANCH_B_ID,
        branch_policy=policy_values["portable-causal-fork-state-b-v0.1.json"],
        sibling_branch_policy=policy_values[
            "portable-causal-fork-state-a-v0.1.json"
        ],
        sibling_logical_branch_id=BRANCH_A_ID,
        reconciliation_policy=policy_values[
            "portable-causal-reconciled-state-v0.1.json"
        ],
        branch_contract=policy_values[
            "portable-causal-fork-branch-contract-v0.1.json"
        ],
        branch_authorization_contract=policy_values[
            "portable-causal-fork-authorization-contract-v0.1.json"
        ],
        reconciliation_contract=policy_values[
            "portable-causal-reconciliation-contract-v0.1.json"
        ],
        reconciliation_authorization_contract=policy_values[
            "portable-causal-reconciliation-authorization-contract-v0.1.json"
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
        raise ValueError(f"fork_reconciliation_recompute_rejected:{agreement.reason}")
    if list(agreement.branch_checkpoints) != result.get("branch_checkpoints"):
        raise ValueError("branch_checkpoint_bytes_mismatch")
    if list(agreement.branch_witnesses) != result.get("branch_witnesses"):
        raise ValueError("branch_witness_bytes_mismatch")
    if canonical_json_bytes(agreement.reconciliation_checkpoint) != canonical_json_bytes(
        result.get("reconciliation_checkpoint")
    ):
        raise ValueError("reconciliation_checkpoint_bytes_mismatch")
    if canonical_json_bytes(agreement.reconciliation_witness) != canonical_json_bytes(
        result.get("reconciliation_witness")
    ):
        raise ValueError("reconciliation_witness_bytes_mismatch")
    if canonical_json_bytes(agreement.receipt) != canonical_json_bytes(
        result.get("receipt")
    ):
        raise ValueError("reconciliation_receipt_bytes_mismatch")
    if digest(agreement.receipt) != result.get("receipt_sha256"):
        raise ValueError("reconciliation_receipt_digest_mismatch")

    assert agreement.reconciliation_checkpoint is not None
    assert agreement.reconciliation_witness is not None
    reconciliation_ref = agreement.reconciliation_checkpoint["reconciliation_ref"]
    audit = {
        "schema": SCHEMA,
        "verified": True,
        "reason": "causal_fork_reconciliation_portability_recomputed",
        "result_sha256": sha256_bytes(result_path.read_bytes()),
        "branch_b_authority_id": branch_b_authority,
        "fork_causal_epoch": agreement.receipt["fork_causal_epoch"],
        "reconciled_causal_epoch": agreement.receipt["reconciled_causal_epoch"],
        "target_semantic_state_sha256": agreement.receipt[
            "target_semantic_state_sha256"
        ],
        "parent_set_sha256": agreement.receipt["parent_set_sha256"],
        "reconciliation_ref_sha256": digest(reconciliation_ref),
        "reconciliation_checkpoint_sha256": digest(
            agreement.reconciliation_checkpoint
        ),
        "reconciliation_witness_sha256": digest(agreement.reconciliation_witness),
        "receipt_sha256": result["receipt_sha256"],
        "both_lineages_preserved": agreement.receipt["both_lineages_preserved"],
        "raw_evidence_embedded": agreement.receipt["raw_evidence_embedded"],
    }
    output = Path(args.output).resolve()
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_bytes(canonical_json_bytes(audit))
    print(json.dumps(audit, sort_keys=True, separators=(",", ":")))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
