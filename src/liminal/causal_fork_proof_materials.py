"""Deterministic construction of causal fork/reconciliation proof material."""

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
from liminal.causal_state_evolution import (
    validate_anchor_checkpoint,
    validate_anchor_witness,
    validate_evolution_checkpoint_chain,
    validate_evolution_witness_chain,
)
from liminal.recovery_trust_root_registry import canonical_json_bytes, sha256_hex

COMMON_WORKFLOW_SHA = "5f5cee5749eaa15814323f563c1544347524d000"
BRANCH_A_PROVIDER = "github-oidc-fork-a"
BRANCH_B_PROVIDER = "offline-ed25519-fork-b"
BRANCH_A_ID = "authorization-policy-fork-a"
BRANCH_B_ID = "authorization-policy-fork-b"
RECONCILIATION_ID = "authorization-policy-fork-reconcile-v0.1"


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
            "schema": "liminal-causal-fork-branch-semantic-state/v0.1",
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
            "schema": "liminal-causal-reconciled-semantic-state/v0.1",
            "common_semantic_state_sha256": common_semantic_state_sha256,
            "branch_semantic_state_sha256": sorted(branch_semantic_states),
            "reconciliation_policy_sha256": digest(reconciliation_policy),
        }
    )


def common_chain(
    evolution_result: dict[str, Any],
    anchor_result: dict[str, Any],
) -> tuple[dict[str, Any], dict[str, Any], list[dict[str, Any]], list[dict[str, Any]]]:
    if evolution_result.get("verified") is not True:
        raise ValueError("common_evolution_result_unverified")
    if evolution_result.get("reason") != "portable_causal_state_evolution_verified":
        raise ValueError("common_evolution_reason_invalid")
    if evolution_result.get("workflow_sha") != COMMON_WORKFLOW_SHA:
        raise ValueError("common_evolution_workflow_invalid")
    anchor_checkpoint = anchor_result.get("checkpoint")
    anchor_witness = anchor_result.get("witness")
    checkpoints = evolution_result.get("checkpoints")
    witnesses = evolution_result.get("witnesses")
    if not isinstance(anchor_checkpoint, dict) or not isinstance(anchor_witness, dict):
        raise ValueError("common_anchor_objects_missing")
    if not isinstance(checkpoints, list) or not isinstance(witnesses, list):
        raise ValueError("common_evolution_chain_missing")
    if not validate_anchor_checkpoint(anchor_checkpoint):
        raise ValueError("common_anchor_checkpoint_invalid")
    if not validate_anchor_witness(anchor_witness, anchor_checkpoint):
        raise ValueError("common_anchor_witness_invalid")
    if not validate_evolution_checkpoint_chain(anchor_checkpoint, checkpoints):
        raise ValueError("common_checkpoint_chain_invalid")
    if not validate_evolution_witness_chain(
        anchor_witness,
        anchor_checkpoint,
        checkpoints,
        witnesses,
    ):
        raise ValueError("common_witness_chain_invalid")
    if not checkpoints:
        raise ValueError("common_evolution_chain_empty")
    return anchor_checkpoint, anchor_witness, checkpoints, witnesses


def observation(value: object) -> PortableForkBranchObservation:
    if not isinstance(value, dict):
        raise ValueError("fork_branch_observation_object_required")
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
        raise ValueError("reconciliation_vote_object_required")
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
    common_result_sha256: str,
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
        raise ValueError("common_state_ref_missing")
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
            "schema": "liminal-causal-fork-branch-evidence-provenance/v0.1",
            "provider_id": provider_id,
            "authority_id": authority_id,
            "evidence_identity": evidence_identity,
            "common_result_sha256": common_result_sha256,
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
            "schema": "liminal-causal-reconciliation-vote-provenance/v0.1",
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
