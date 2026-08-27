"""Deterministic proof material for repeated fork lineage compaction."""

from __future__ import annotations

from pathlib import Path
from typing import Any

from liminal.causal_fork_reconciliation import (
    PortableForkBranchObservation,
    PortableReconciliationVote,
    build_fork_branch_checkpoint,
    build_fork_branch_witness,
    build_reconciliation_vote,
)
from liminal.recovery_trust_root_registry import canonical_json_bytes, sha256_hex

PREDECESSOR_WORKFLOW_SHA = "51894987f038e6c24fadf5b3c2768feda4117d6f"
PREDECESSOR_SCHEMA = "liminal-causal-fork-reconciliation-portability-proof/v0.1"
PREDECESSOR_REASON = "causal_fork_reconciliation_portability_verified"
BRANCH_A_PROVIDER = "github-oidc-repeat-fork-a"
BRANCH_B_PROVIDER = "offline-ed25519-repeat-fork-b"
BRANCH_A_ID = "authorization-policy-repeat-fork-a"
BRANCH_B_ID = "authorization-policy-repeat-fork-b"
RECONCILIATION_ID = "authorization-policy-repeat-fork-reconcile-v0.1"


def digest(value: object) -> str:
    return sha256_hex(canonical_json_bytes(value))


def raw_sha256(path: Path) -> str:
    return sha256_hex(path.read_bytes())


def branch_semantic_state(
    common_semantic_state_sha256: str,
    *,
    logical_branch_id: str,
    branch_policy: dict[str, Any],
) -> str:
    return digest(
        {
            "schema": "liminal-repeated-causal-fork-branch-semantic-state/v0.1",
            "common_semantic_state_sha256": common_semantic_state_sha256,
            "logical_branch_id": logical_branch_id,
            "branch_policy_sha256": digest(branch_policy),
        }
    )


def reconciliation_semantic_state(
    common_semantic_state_sha256: str,
    branch_semantic_states: tuple[str, str],
    reconciliation_policy: dict[str, Any],
) -> str:
    return digest(
        {
            "schema": "liminal-repeated-causal-reconciled-semantic-state/v0.1",
            "common_semantic_state_sha256": common_semantic_state_sha256,
            "branch_semantic_state_sha256": sorted(branch_semantic_states),
            "reconciliation_policy_sha256": digest(reconciliation_policy),
        }
    )


def predecessor_tip(
    result: dict[str, Any],
) -> tuple[dict[str, Any], dict[str, Any]]:
    if (
        result.get("schema") != PREDECESSOR_SCHEMA
        or result.get("verified") is not True
        or result.get("reason") != PREDECESSOR_REASON
        or result.get("workflow_sha") != PREDECESSOR_WORKFLOW_SHA
    ):
        raise ValueError("predecessor_reconciliation_result_invalid")
    checkpoint = result.get("reconciliation_checkpoint")
    witness = result.get("reconciliation_witness")
    receipt = result.get("receipt")
    if not isinstance(checkpoint, dict) or not isinstance(witness, dict):
        raise ValueError("predecessor_reconciliation_tip_missing")
    if not isinstance(receipt, dict):
        raise ValueError("predecessor_reconciliation_receipt_missing")
    if receipt.get("reconciliation_checkpoint_sha256") != digest(checkpoint):
        raise ValueError("predecessor_reconciliation_checkpoint_mismatch")
    if receipt.get("reconciliation_witness_sha256") != digest(witness):
        raise ValueError("predecessor_reconciliation_witness_mismatch")
    if result.get("receipt_sha256") != digest(receipt):
        raise ValueError("predecessor_reconciliation_receipt_mismatch")
    return checkpoint, witness


def observation(value: object) -> PortableForkBranchObservation:
    if not isinstance(value, dict):
        raise ValueError("repeat_fork_branch_observation_object_required")
    return PortableForkBranchObservation(
        verified=value.get("verified") is True,
        provider_id=str(value.get("provider_id", "")),
        authority_id=str(value.get("authority_id", "")),
        branch_provenance_sha256=str(value.get("branch_provenance_sha256", "")),
        trust_domain=str(value.get("trust_domain", "")),
        logical_branch_id=str(value.get("logical_branch_id", "")),
        branch_contract_sha256=str(value.get("branch_contract_sha256", "")),
        authorization_contract_sha256=str(
            value.get("authorization_contract_sha256", "")
        ),
        from_state_ref_sha256=str(value.get("from_state_ref_sha256", "")),
        to_semantic_state_sha256=str(value.get("to_semantic_state_sha256", "")),
    )


def vote(value: object) -> PortableReconciliationVote:
    if not isinstance(value, dict):
        raise ValueError("repeat_reconciliation_vote_object_required")
    return PortableReconciliationVote(
        verified=value.get("verified") is True,
        provider_id=str(value.get("provider_id", "")),
        authority_id=str(value.get("authority_id", "")),
        vote_provenance_sha256=str(value.get("vote_provenance_sha256", "")),
        trust_domain=str(value.get("trust_domain", "")),
        logical_reconciliation_id=str(value.get("logical_reconciliation_id", "")),
        branch_ref_sha256=str(value.get("branch_ref_sha256", "")),
        branch_state_ref_sha256=str(value.get("branch_state_ref_sha256", "")),
        branch_checkpoint_sha256=str(value.get("branch_checkpoint_sha256", "")),
        branch_witness_sha256=str(value.get("branch_witness_sha256", "")),
        target_semantic_state_sha256=str(
            value.get("target_semantic_state_sha256", "")
        ),
        reconciliation_contract_sha256=str(
            value.get("reconciliation_contract_sha256", "")
        ),
        authorization_contract_sha256=str(
            value.get("authorization_contract_sha256", "")
        ),
    )


def build_branch_package(
    *,
    predecessor_result_sha256: str,
    common_checkpoint: dict[str, Any],
    common_witness: dict[str, Any],
    provider_id: str,
    authority_id: str,
    evidence_identity: str,
    logical_branch_id: str,
    branch_policy: dict[str, Any],
    sibling_branch_policy: dict[str, Any],
    sibling_logical_branch_id: str,
    reconciliation_policy: dict[str, Any],
    branch_contract: dict[str, Any],
    branch_authorization_contract: dict[str, Any],
    reconciliation_contract: dict[str, Any],
    reconciliation_authorization_contract: dict[str, Any],
) -> dict[str, Any]:
    common_state_ref = common_checkpoint.get("state_ref")
    if not isinstance(common_state_ref, dict):
        raise ValueError("repeat_common_state_ref_missing")
    common_semantic = str(common_state_ref.get("semantic_state_sha256", ""))
    branch_semantic = branch_semantic_state(
        common_semantic,
        logical_branch_id=logical_branch_id,
        branch_policy=branch_policy,
    )
    sibling_semantic = branch_semantic_state(
        common_semantic,
        logical_branch_id=sibling_logical_branch_id,
        branch_policy=sibling_branch_policy,
    )
    target_semantic = reconciliation_semantic_state(
        common_semantic,
        (branch_semantic, sibling_semantic),
        reconciliation_policy,
    )
    branch_contract_sha = digest(branch_contract)
    branch_authorization_sha = digest(branch_authorization_contract)
    reconciliation_contract_sha = digest(reconciliation_contract)
    reconciliation_authorization_sha = digest(
        reconciliation_authorization_contract
    )
    branch_provenance_sha = digest(
        {
            "schema": "liminal-repeated-causal-fork-branch-provenance/v0.1",
            "provider_id": provider_id,
            "authority_id": authority_id,
            "evidence_identity": evidence_identity,
            "predecessor_result_sha256": predecessor_result_sha256,
            "common_checkpoint_sha256": digest(common_checkpoint),
            "common_witness_sha256": digest(common_witness),
            "logical_branch_id": logical_branch_id,
            "branch_policy_sha256": digest(branch_policy),
        }
    )
    branch = PortableForkBranchObservation(
        verified=True,
        provider_id=provider_id,
        authority_id=authority_id,
        branch_provenance_sha256=branch_provenance_sha,
        trust_domain=str(common_state_ref["trust_domain"]),
        logical_branch_id=logical_branch_id,
        branch_contract_sha256=branch_contract_sha,
        authorization_contract_sha256=branch_authorization_sha,
        from_state_ref_sha256=digest(common_state_ref),
        to_semantic_state_sha256=branch_semantic,
    )
    checkpoint = build_fork_branch_checkpoint(common_checkpoint, branch)
    witness = build_fork_branch_witness(common_witness, checkpoint)
    vote_provenance_sha = digest(
        {
            "schema": "liminal-repeated-causal-reconciliation-vote-provenance/v0.1",
            "provider_id": provider_id,
            "authority_id": authority_id,
            "evidence_identity": evidence_identity,
            "logical_reconciliation_id": RECONCILIATION_ID,
            "branch_checkpoint_sha256": digest(checkpoint),
            "branch_witness_sha256": digest(witness),
            "target_semantic_state_sha256": target_semantic,
            "reconciliation_policy_sha256": digest(reconciliation_policy),
        }
    )
    reconciliation_vote = build_reconciliation_vote(
        verified=True,
        provider_id=provider_id,
        authority_id=authority_id,
        vote_provenance_sha256=vote_provenance_sha,
        logical_reconciliation_id=RECONCILIATION_ID,
        branch_checkpoint=checkpoint,
        branch_witness=witness,
        target_semantic_state_sha256=target_semantic,
        reconciliation_contract_sha256=reconciliation_contract_sha,
        authorization_contract_sha256=reconciliation_authorization_sha,
    )
    return {
        "branch_observation": branch.__dict__,
        "branch_checkpoint": checkpoint,
        "branch_witness": witness,
        "reconciliation_vote": reconciliation_vote.__dict__,
        "branch_semantic_state_sha256": branch_semantic,
        "sibling_branch_semantic_state_sha256": sibling_semantic,
        "reconciliation_target_semantic_state_sha256": target_semantic,
    }
