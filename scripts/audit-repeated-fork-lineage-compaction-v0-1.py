#!/usr/bin/env python3
"""Independently recompute repeated fork lineage compaction from proof bytes."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from liminal.causal_fork_evidence import (
    load_json,
    sha256_bytes,
    verify_ed25519_envelope,
    verify_source_material,
)
from liminal.causal_lineage_compaction import (
    COMPACTION_REASON,
    build_initial_lineage_accumulator,
    compare_repeated_fork_reconciliation,
)
from liminal.causal_lineage_compaction_proof_materials import (
    BRANCH_A_ID,
    BRANCH_A_PROVIDER,
    BRANCH_B_ID,
    BRANCH_B_PROVIDER,
    build_branch_package,
    digest,
    observation,
    predecessor_tip,
    vote,
)
from liminal.recovery_trust_root_registry import canonical_json_bytes

SCHEMA = "liminal-repeated-fork-lineage-compaction-external-audit/v0.1"
RESULT_SCHEMA = "liminal-repeated-fork-lineage-compaction-proof/v0.1"
BRANCH_B_CLAIM_SCHEMA = "liminal-repeated-causal-fork-branch-b-claim/v0.1"


def _assert_package(actual: dict, expected: dict, *, label: str) -> None:
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
    parser.add_argument("--predecessor-proof-dir", required=True)
    parser.add_argument("--predecessor-audit", required=True)
    parser.add_argument("--repository-root", default=".")
    parser.add_argument("--output", required=True)
    args = parser.parse_args()

    proof_dir = Path(args.proof_dir).resolve()
    result_path = proof_dir / "repeated-fork-lineage-compaction-result.json"
    result = load_json(result_path)
    if (
        result.get("schema") != RESULT_SCHEMA
        or result.get("verified") is not True
        or result.get("reason") != COMPACTION_REASON
    ):
        raise ValueError("repeat_compaction_result_unverified")

    materials = proof_dir / "materials"
    compact_predecessor_dir = materials / "predecessor"
    compact_predecessor_path = (
        compact_predecessor_dir
        / "causal-fork-reconciliation-portability-result.json"
    )
    compact_predecessor = load_json(compact_predecessor_path)
    predecessor_dir = Path(args.predecessor_proof_dir).resolve()
    raw_predecessor_path = (
        predecessor_dir / "causal-fork-reconciliation-portability-result.json"
    )
    raw_predecessor = load_json(raw_predecessor_path)
    if canonical_json_bytes(compact_predecessor) != canonical_json_bytes(
        raw_predecessor
    ):
        raise ValueError("compacted_predecessor_result_mismatch")
    predecessor_result_sha256 = sha256_bytes(raw_predecessor_path.read_bytes())
    if predecessor_result_sha256 != result.get("predecessor_result_sha256"):
        raise ValueError("predecessor_result_digest_mismatch")
    common_checkpoint, common_witness = predecessor_tip(raw_predecessor)

    predecessor_audit_path = Path(args.predecessor_audit).resolve()
    predecessor_audit = load_json(predecessor_audit_path)
    compact_audit_path = compact_predecessor_dir / "predecessor-external-audit.json"
    compact_audit = load_json(compact_audit_path)
    if canonical_json_bytes(compact_audit) != canonical_json_bytes(predecessor_audit):
        raise ValueError("compacted_predecessor_audit_mismatch")
    if (
        predecessor_audit.get("verified") is not True
        or predecessor_audit.get("result_sha256") != predecessor_result_sha256
        or predecessor_audit.get("receipt_sha256")
        != raw_predecessor.get("receipt_sha256")
    ):
        raise ValueError("predecessor_audit_invalid")
    if sha256_bytes(predecessor_audit_path.read_bytes()) != result.get(
        "predecessor_audit_sha256"
    ):
        raise ValueError("predecessor_audit_digest_mismatch")
    if (compact_predecessor_dir / "materials").exists():
        raise ValueError("raw_predecessor_proof_embedded")

    policy_names = {
        "branch_a_policy": "portable-repeated-causal-fork-state-a-v0.1.json",
        "branch_b_policy": "portable-repeated-causal-fork-state-b-v0.1.json",
        "reconciliation_policy": (
            "portable-repeated-causal-reconciled-state-v0.1.json"
        ),
        "branch_contract": (
            "portable-repeated-causal-fork-branch-contract-v0.1.json"
        ),
        "branch_authorization_contract": (
            "portable-repeated-causal-fork-authorization-contract-v0.1.json"
        ),
        "reconciliation_contract": (
            "portable-repeated-causal-reconciliation-contract-v0.1.json"
        ),
        "reconciliation_authorization_contract": (
            "portable-repeated-causal-reconciliation-authorization-contract-v0.1.json"
        ),
        "lineage_compaction_contract": (
            "portable-causal-lineage-compaction-contract-v0.1.json"
        ),
    }
    values = {
        name: load_json(materials / filename)
        for name, filename in policy_names.items()
    }
    compaction_contract_sha256 = digest(values["lineage_compaction_contract"])

    branch_a_dir = materials / "branch-a"
    branch_a_path = branch_a_dir / "repeated-causal-fork-branch-a-result.json"
    branch_a = load_json(branch_a_path)
    verify_source_material(
        Path(args.repository_root).resolve(),
        branch_a.get("source_material"),
    )
    expected_a = build_branch_package(
        predecessor_result_sha256=predecessor_result_sha256,
        common_checkpoint=common_checkpoint,
        common_witness=common_witness,
        provider_id=BRANCH_A_PROVIDER,
        authority_id=str(branch_a.get("authority_id", "")),
        evidence_identity=str(branch_a.get("workflow_sha", "")),
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
    _assert_package(branch_a, expected_a, label="repeat_branch_a")
    if sha256_bytes(branch_a_path.read_bytes()) != result.get("branch_a_result_sha256"):
        raise ValueError("repeat_branch_a_result_digest_mismatch")

    branch_b_dir = materials / "branch-b"
    branch_b_envelope_path = branch_b_dir / "signed-branch-envelope.json"
    branch_b_authority, branch_b = verify_ed25519_envelope(
        branch_b_dir / "branch-b-public-key.pem",
        load_json(branch_b_envelope_path),
    )
    if branch_b.get("schema") != BRANCH_B_CLAIM_SCHEMA:
        raise ValueError("repeat_branch_b_schema_invalid")
    if branch_b.get("provider_id") != BRANCH_B_PROVIDER:
        raise ValueError("repeat_branch_b_provider_invalid")
    if branch_b.get("lineage_compaction_contract_sha256") != compaction_contract_sha256:
        raise ValueError("repeat_branch_b_compaction_contract_mismatch")
    verify_source_material(
        Path(args.repository_root).resolve(),
        branch_b.get("source_material"),
    )
    expected_b = build_branch_package(
        predecessor_result_sha256=predecessor_result_sha256,
        common_checkpoint=common_checkpoint,
        common_witness=common_witness,
        provider_id=BRANCH_B_PROVIDER,
        authority_id=branch_b_authority,
        evidence_identity=str(branch_b.get("evidence_identity", "")),
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
    _assert_package(branch_b, expected_b, label="repeat_branch_b")
    if branch_b_authority != result.get("branch_b_authority_id"):
        raise ValueError("repeat_branch_b_authority_mismatch")
    if sha256_bytes(branch_b_envelope_path.read_bytes()) != result.get(
        "branch_b_envelope_sha256"
    ):
        raise ValueError("repeat_branch_b_envelope_digest_mismatch")

    previous_event, previous_accumulator = build_initial_lineage_accumulator(
        raw_predecessor,
        predecessor_result_sha256=predecessor_result_sha256,
        compaction_contract_sha256=compaction_contract_sha256,
    )
    agreement = compare_repeated_fork_reconciliation(
        common_checkpoint=common_checkpoint,
        common_witness=common_witness,
        previous_lineage_event=previous_event,
        previous_lineage_accumulator=previous_accumulator,
        primary_branch=observation(branch_a["branch_observation"]),
        secondary_branch=observation(branch_b["branch_observation"]),
        primary_vote=vote(branch_a["reconciliation_vote"]),
        secondary_vote=vote(branch_b["reconciliation_vote"]),
    )
    if not agreement.verified or agreement.receipt is None:
        raise ValueError(f"repeat_compaction_recompute_rejected:{agreement.reason}")
    exact = {
        "previous_lineage_event": agreement.previous_lineage_event,
        "previous_lineage_accumulator": agreement.previous_lineage_accumulator,
        "branch_checkpoints": list(agreement.branch_checkpoints),
        "branch_witnesses": list(agreement.branch_witnesses),
        "reconciliation_checkpoint": agreement.reconciliation_checkpoint,
        "reconciliation_witness": agreement.reconciliation_witness,
        "lineage_event": agreement.lineage_event,
        "lineage_accumulator": agreement.lineage_accumulator,
        "receipt": agreement.receipt,
    }
    for key, expected in exact.items():
        if canonical_json_bytes(result.get(key)) != canonical_json_bytes(expected):
            raise ValueError(f"repeat_compaction_bytes_mismatch:{key}")
    if digest(agreement.receipt) != result.get("receipt_sha256"):
        raise ValueError("repeat_compaction_receipt_digest_mismatch")

    audit = {
        "schema": SCHEMA,
        "verified": True,
        "reason": "repeated_fork_lineage_compaction_recomputed",
        "result_sha256": sha256_bytes(result_path.read_bytes()),
        "predecessor_result_sha256": predecessor_result_sha256,
        "predecessor_audit_sha256": result["predecessor_audit_sha256"],
        "branch_b_authority_id": branch_b_authority,
        "reconciliation_count": agreement.receipt["reconciliation_count"],
        "total_branch_event_count": agreement.receipt["total_branch_event_count"],
        "reconciled_causal_epoch": agreement.receipt["reconciled_causal_epoch"],
        "lineage_root_sha256": agreement.receipt["lineage_root_sha256"],
        "lineage_accumulator_sha256": agreement.receipt[
            "lineage_accumulator_sha256"
        ],
        "reconciliation_checkpoint_sha256": agreement.receipt[
            "reconciliation_checkpoint_sha256"
        ],
        "reconciliation_witness_sha256": agreement.receipt[
            "reconciliation_witness_sha256"
        ],
        "receipt_sha256": result["receipt_sha256"],
        "bounded_accumulator_shape": agreement.receipt[
            "bounded_accumulator_shape"
        ],
        "raw_predecessor_proof_embedded": False,
        "raw_provider_evidence_embedded": False,
    }
    output = Path(args.output).resolve()
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_bytes(canonical_json_bytes(audit))
    print(json.dumps(audit, sort_keys=True, separators=(",", ":")))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
