#!/usr/bin/env python3
"""Verify a second fork/reconciliation with bounded lineage compaction."""

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
from liminal.causal_lineage_compaction import (
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

SCHEMA = "liminal-repeated-fork-lineage-compaction-proof/v0.1"
BRANCH_A_SCHEMA = "liminal-repeated-causal-fork-branch-a-result/v0.1"
BRANCH_A_REASON = "repeated_causal_fork_branch_a_evidence_verified"
BRANCH_B_CLAIM_SCHEMA = "liminal-repeated-causal-fork-branch-b-claim/v0.1"
PREDECESSOR_AUDIT_REASON = "causal_fork_reconciliation_portability_recomputed"


def _assert_package(
    actual: dict[str, Any],
    expected: dict[str, Any],
    *,
    label: str,
) -> None:
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
    parser.add_argument("--predecessor-proof-dir", required=True)
    parser.add_argument("--predecessor-attestation", required=True)
    parser.add_argument("--predecessor-audit", required=True)
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
    parser.add_argument("--lineage-compaction-contract", required=True)
    parser.add_argument("--repository-root", default=".")
    parser.add_argument("--workflow-sha", required=True)
    parser.add_argument("--output-dir", required=True)
    args = parser.parse_args()

    repository_root = Path(args.repository_root).resolve()
    predecessor_dir = Path(args.predecessor_proof_dir).resolve()
    predecessor_result_path = (
        predecessor_dir / "causal-fork-reconciliation-portability-result.json"
    )
    predecessor_result = load_json(predecessor_result_path)
    common_checkpoint, common_witness = predecessor_tip(predecessor_result)
    predecessor_result_sha256 = sha256_bytes(predecessor_result_path.read_bytes())

    predecessor_attestation = Path(args.predecessor_attestation).resolve()
    if (
        not predecessor_attestation.is_file()
        or predecessor_attestation.stat().st_size == 0
    ):
        raise ValueError("predecessor_attestation_verification_missing")
    predecessor_audit_path = Path(args.predecessor_audit).resolve()
    predecessor_audit = load_json(predecessor_audit_path)
    if (
        predecessor_audit.get("verified") is not True
        or predecessor_audit.get("reason") != PREDECESSOR_AUDIT_REASON
        or predecessor_audit.get("result_sha256") != predecessor_result_sha256
        or predecessor_audit.get("raw_evidence_embedded") is not False
        or predecessor_audit.get("receipt_sha256")
        != predecessor_result.get("receipt_sha256")
    ):
        raise ValueError("predecessor_independent_audit_invalid")

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
        "lineage_compaction_contract": Path(
            args.lineage_compaction_contract
        ).resolve(),
    }
    values = {name: load_json(path) for name, path in policy_paths.items()}
    compaction_contract_sha256 = digest(values["lineage_compaction_contract"])

    branch_a_attestation = Path(args.branch_a_attestation).resolve()
    if not branch_a_attestation.is_file() or branch_a_attestation.stat().st_size == 0:
        raise ValueError("repeat_branch_a_attestation_verification_missing")
    branch_a_dir = Path(args.branch_a_dir).resolve()
    branch_a_result_path = branch_a_dir / "repeated-causal-fork-branch-a-result.json"
    branch_a_result = load_json(branch_a_result_path)
    branch_a_authority = (
        "github-oidc:safal207/Liminal:repeated-causal-fork-branch-a-producer@"
        f"{args.branch_a_workflow_sha}"
    )
    if (
        branch_a_result.get("schema") != BRANCH_A_SCHEMA
        or branch_a_result.get("verified") is not True
        or branch_a_result.get("reason") != BRANCH_A_REASON
        or branch_a_result.get("workflow_sha") != args.branch_a_workflow_sha
        or branch_a_result.get("provider_id") != BRANCH_A_PROVIDER
        or branch_a_result.get("authority_id") != branch_a_authority
        or branch_a_result.get("predecessor_result_sha256")
        != predecessor_result_sha256
        or branch_a_result.get("lineage_compaction_contract_sha256")
        != compaction_contract_sha256
    ):
        raise ValueError("repeat_branch_a_result_invalid")
    verify_source_material(repository_root, branch_a_result.get("source_material"))
    expected_a = build_branch_package(
        predecessor_result_sha256=predecessor_result_sha256,
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
    _assert_package(branch_a_result, expected_a, label="repeat_branch_a")

    branch_b_dir = Path(args.branch_b_dir).resolve()
    branch_b_envelope_path = branch_b_dir / "signed-branch-envelope.json"
    branch_b_authority, branch_b_claim = verify_ed25519_envelope(
        branch_b_dir / "branch-b-public-key.pem",
        load_json(branch_b_envelope_path),
    )
    if (
        branch_b_claim.get("schema") != BRANCH_B_CLAIM_SCHEMA
        or branch_b_claim.get("provider_id") != BRANCH_B_PROVIDER
        or branch_b_claim.get("predecessor_result_sha256")
        != predecessor_result_sha256
        or branch_b_claim.get("lineage_compaction_contract_sha256")
        != compaction_contract_sha256
    ):
        raise ValueError("repeat_branch_b_claim_invalid")
    verify_source_material(repository_root, branch_b_claim.get("source_material"))
    evidence_identity = str(branch_b_claim.get("evidence_identity", ""))
    if not evidence_identity:
        raise ValueError("repeat_branch_b_evidence_identity_missing")
    expected_b = build_branch_package(
        predecessor_result_sha256=predecessor_result_sha256,
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
    _assert_package(branch_b_claim, expected_b, label="repeat_branch_b")

    previous_event, previous_accumulator = build_initial_lineage_accumulator(
        predecessor_result,
        predecessor_result_sha256=predecessor_result_sha256,
        compaction_contract_sha256=compaction_contract_sha256,
    )
    agreement = compare_repeated_fork_reconciliation(
        common_checkpoint=common_checkpoint,
        common_witness=common_witness,
        previous_lineage_event=previous_event,
        previous_lineage_accumulator=previous_accumulator,
        primary_branch=observation(branch_a_result["branch_observation"]),
        secondary_branch=observation(branch_b_claim["branch_observation"]),
        primary_vote=vote(branch_a_result["reconciliation_vote"]),
        secondary_vote=vote(branch_b_claim["reconciliation_vote"]),
    )
    if not agreement.verified or agreement.receipt is None:
        raise ValueError(f"repeat_fork_compaction_rejected:{agreement.reason}")
    required = (
        agreement.reconciliation_checkpoint,
        agreement.reconciliation_witness,
        agreement.previous_lineage_event,
        agreement.previous_lineage_accumulator,
        agreement.lineage_event,
        agreement.lineage_accumulator,
    )
    if any(item is None for item in required):
        raise ValueError("repeat_fork_compaction_objects_missing")

    result = {
        "schema": SCHEMA,
        "verified": True,
        "reason": agreement.reason,
        "workflow_sha": args.workflow_sha,
        "predecessor_workflow_sha": predecessor_result["workflow_sha"],
        "predecessor_result_sha256": predecessor_result_sha256,
        "predecessor_receipt_sha256": predecessor_result["receipt_sha256"],
        "predecessor_audit_sha256": sha256_bytes(predecessor_audit_path.read_bytes()),
        "branch_a_workflow_sha": args.branch_a_workflow_sha,
        "branch_a_result_sha256": sha256_bytes(branch_a_result_path.read_bytes()),
        "branch_b_authority_id": branch_b_authority,
        "branch_b_envelope_sha256": sha256_bytes(branch_b_envelope_path.read_bytes()),
        "previous_lineage_event": agreement.previous_lineage_event,
        "previous_lineage_accumulator": agreement.previous_lineage_accumulator,
        "branch_checkpoints": list(agreement.branch_checkpoints),
        "branch_witnesses": list(agreement.branch_witnesses),
        "reconciliation_checkpoint": agreement.reconciliation_checkpoint,
        "reconciliation_witness": agreement.reconciliation_witness,
        "lineage_event": agreement.lineage_event,
        "lineage_accumulator": agreement.lineage_accumulator,
        "receipt": agreement.receipt,
        "receipt_sha256": digest(agreement.receipt),
        "claim_boundary": {
            "predecessor_result_attestation_verified": True,
            "predecessor_result_independently_recomputed": True,
            "predecessor_raw_proof_embedded_in_compact_result": False,
            "second_fork_semantics_genuinely_divergent": True,
            "branch_a_github_oidc_attestation_verified": True,
            "branch_b_detached_ed25519_signature_verified": True,
            "second_reconciliation_preserves_both_parent_lineages": True,
            "lineage_accumulator_shape_bounded": True,
            "prior_parent_lineages_embedded_in_accumulator": False,
            "raw_provider_evidence_embedded_in_portable_objects": False,
            "arbitrary_unbounded_cycles": False,
            "constant_time_full_history_verification": False,
            "organizational_governance_independence": False,
            "hardware_provenance_independence": False,
            "storage_provider_independence": False,
            "network_path_independence": False,
        },
    }

    output_dir = Path(args.output_dir).resolve()
    output_dir.mkdir(parents=True, exist_ok=True)
    materials = output_dir / "materials"
    predecessor_materials = materials / "predecessor"
    predecessor_materials.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(
        predecessor_result_path,
        predecessor_materials / predecessor_result_path.name,
    )
    shutil.copyfile(
        predecessor_attestation,
        predecessor_materials / "predecessor-attestation-verification.json",
    )
    shutil.copyfile(
        predecessor_audit_path,
        predecessor_materials / "predecessor-external-audit.json",
    )
    shutil.copytree(branch_a_dir, materials / "branch-a")
    shutil.copytree(branch_b_dir, materials / "branch-b")
    shutil.copyfile(
        branch_a_attestation,
        materials / "branch-a-attestation-verification.json",
    )
    for path in policy_paths.values():
        shutil.copyfile(path, materials / path.name)
    result_path = output_dir / "repeated-fork-lineage-compaction-result.json"
    result_path.write_bytes(canonical_json_bytes(result))
    print(json.dumps(result, sort_keys=True, separators=(",", ":")))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
