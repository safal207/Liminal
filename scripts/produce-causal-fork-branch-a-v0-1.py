#!/usr/bin/env python3
"""Produce GitHub-attested branch-A and reconciliation-vote evidence."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from liminal.causal_fork_evidence import load_json, sha256_bytes
from liminal.causal_fork_proof_materials import (
    BRANCH_A_ID,
    BRANCH_A_PROVIDER,
    BRANCH_B_ID,
    build_branch_package,
    common_chain,
    raw_sha256,
)
from liminal.recovery_trust_root_registry import canonical_json_bytes

SCHEMA = "liminal-causal-fork-branch-a-result/v0.1"
REASON = "causal_fork_branch_a_evidence_verified"


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--evolution-proof-dir", required=True)
    parser.add_argument("--branch-a-policy", required=True)
    parser.add_argument("--branch-b-policy", required=True)
    parser.add_argument("--reconciliation-policy", required=True)
    parser.add_argument("--branch-contract", required=True)
    parser.add_argument("--branch-authorization-contract", required=True)
    parser.add_argument("--reconciliation-contract", required=True)
    parser.add_argument("--reconciliation-authorization-contract", required=True)
    parser.add_argument("--workflow-sha", required=True)
    parser.add_argument("--output-dir", required=True)
    args = parser.parse_args()

    proof_dir = Path(args.evolution_proof_dir).resolve()
    result_path = proof_dir / "portable-causal-state-evolution-result.json"
    result = load_json(result_path)
    anchor_result = load_json(
        proof_dir / "materials/anchor-proof/downstream-causal-state-portability-result.json"
    )
    _, _, checkpoints, witnesses = common_chain(result, anchor_result)
    common_checkpoint = checkpoints[-1]
    common_witness = witnesses[-1]
    common_result_sha256 = sha256_bytes(result_path.read_bytes())

    paths = {
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
    values = {name: load_json(path) for name, path in paths.items()}
    authority_id = (
        "github-oidc:safal207/Liminal:causal-fork-branch-a-producer@"
        f"{args.workflow_sha}"
    )
    package = build_branch_package(
        common_result_sha256=common_result_sha256,
        common_checkpoint=common_checkpoint,
        common_witness=common_witness,
        provider_id=BRANCH_A_PROVIDER,
        authority_id=authority_id,
        evidence_identity=args.workflow_sha,
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
    source_material = [
        {
            "path": str(path.relative_to(Path.cwd().resolve())),
            "source_sha": args.workflow_sha,
            "sha256": raw_sha256(path),
        }
        for path in paths.values()
    ]
    output = {
        "schema": SCHEMA,
        "verified": True,
        "reason": REASON,
        "workflow_sha": args.workflow_sha,
        "provider_id": BRANCH_A_PROVIDER,
        "authority_id": authority_id,
        "common_result_sha256": common_result_sha256,
        "common_state_ref_sha256": package["branch_observation"][
            "from_state_ref_sha256"
        ],
        "common_checkpoint_sha256": sha256_bytes(
            canonical_json_bytes(common_checkpoint)
        ),
        "common_witness_sha256": sha256_bytes(canonical_json_bytes(common_witness)),
        **package,
        "source_material": source_material,
        "claim_boundary": {
            "github_oidc_attestation_required": True,
            "branch_semantics_divergent": True,
            "reconciliation_vote_branch_bound": True,
            "raw_provider_evidence_embedded_in_portable_objects": False,
        },
    }
    output_dir = Path(args.output_dir).resolve()
    output_dir.mkdir(parents=True, exist_ok=True)
    output_path = output_dir / "causal-fork-branch-a-result.json"
    output_path.write_bytes(canonical_json_bytes(output))
    print(json.dumps(output, sort_keys=True, separators=(",", ":")))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
