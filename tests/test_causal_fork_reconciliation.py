from __future__ import annotations

from copy import deepcopy
from dataclasses import replace

import pytest

from liminal.causal_fork_reconciliation import (
    PortableForkBranchObservation,
    build_fork_branch_checkpoint,
    build_fork_branch_witness,
    build_reconciliation_vote,
    compare_causal_fork_reconciliation,
    validate_reconciliation_checkpoint,
    validate_reconciliation_witness,
)
from liminal.causal_state_evolution import (
    HistoricalTransitionObservation,
    build_evolution_checkpoint,
    build_evolution_witness,
)
from liminal.recovery_trust_root_registry import canonical_json_bytes, sha256_hex


def _sha(value: object) -> str:
    return sha256_hex(canonical_json_bytes(value))


def _anchor_objects() -> tuple[dict, dict]:
    state_ref = {
        "schema": "liminal-causal-trust-state-ref/v0.1",
        "trust_domain": "liminal.trusted-recovery",
        "logical_state_id": "liminal.trusted-recovery.authorization-state",
        "causal_epoch": 0,
        "semantic_state_sha256": _sha("semantic-0"),
    }
    checkpoint = {
        "schema_version": "liminal-causal-trust-checkpoint/v0.1",
        "state_ref": state_ref,
        "previous_checkpoint_sha256": None,
        "checkpoint_authority": {
            "schema": "liminal-causal-state-authority/v0.1",
            "role": "checkpoint-producer",
            "logical_authority_id": "liminal.trusted-recovery.causal-checkpoint",
            "producer_contract_sha256": _sha("checkpoint-producer"),
            "authorization_contract_sha256": _sha("checkpoint-authorization"),
        },
    }
    witness = {
        "schema_version": "liminal-causal-trust-witness/v0.1",
        "state_ref": state_ref,
        "checkpoint_sha256": _sha(checkpoint),
        "previous_witness_sha256": None,
        "witness_authority": {
            "schema": "liminal-causal-state-authority/v0.1",
            "role": "witness-producer",
            "logical_authority_id": "liminal.trusted-recovery.causal-witness",
            "producer_contract_sha256": _sha("witness-producer"),
            "authorization_contract_sha256": _sha("witness-authorization"),
        },
    }
    return checkpoint, witness


def _common_prefix(anchor_checkpoint: dict, anchor_witness: dict) -> tuple[list[dict], list[dict]]:
    checkpoints: list[dict] = []
    witnesses: list[dict] = []
    previous_checkpoint = anchor_checkpoint
    previous_witness = anchor_witness
    previous_semantic = anchor_checkpoint["state_ref"]["semantic_state_sha256"]
    for index in (1, 2):
        next_semantic = _sha({"common-epoch": index, "previous": previous_semantic})
        transition = HistoricalTransitionObservation(
            verified=True,
            provider_id="verified-common-provider",
            genesis_authority_id="verified-common-root",
            from_history_generation=index - 1,
            to_history_generation=index,
            from_registry_sha256=_sha({"registry": index - 1}),
            to_registry_sha256=_sha({"registry": index}),
            from_manifest_sha256=_sha({"manifest": index - 1}),
            to_manifest_sha256=_sha({"manifest": index}),
            transition_provenance_sha256=_sha({"proof": index}),
            trust_domain="liminal.trusted-recovery",
            logical_transition_id=f"common-step-{index}",
            transition_contract_sha256=_sha("common-transition-contract"),
            authorization_contract_sha256=_sha("common-transition-authorization"),
            from_semantic_state_sha256=previous_semantic,
            to_semantic_state_sha256=next_semantic,
        )
        checkpoint = build_evolution_checkpoint(previous_checkpoint, transition)
        witness = build_evolution_witness(previous_witness, checkpoint)
        checkpoints.append(checkpoint)
        witnesses.append(witness)
        previous_checkpoint = checkpoint
        previous_witness = witness
        previous_semantic = next_semantic
    return checkpoints, witnesses


def _branch_observations(common_checkpoint: dict) -> tuple[PortableForkBranchObservation, PortableForkBranchObservation]:
    common_state_ref_sha256 = _sha(common_checkpoint["state_ref"])
    contract = _sha("fork-branch-contract")
    authorization = _sha("fork-branch-authorization")
    primary = PortableForkBranchObservation(
        verified=True,
        provider_id="github-oidc-fork-a",
        authority_id="github-oidc:fork-a",
        branch_provenance_sha256=_sha("fork-a-proof"),
        trust_domain="liminal.trusted-recovery",
        logical_branch_id="authorization-policy-fork-a",
        branch_contract_sha256=contract,
        authorization_contract_sha256=authorization,
        from_state_ref_sha256=common_state_ref_sha256,
        to_semantic_state_sha256=_sha("fork-state-a"),
    )
    secondary = PortableForkBranchObservation(
        verified=True,
        provider_id="offline-ed25519-fork-b",
        authority_id="ed25519-sha256:fork-b",
        branch_provenance_sha256=_sha("fork-b-proof"),
        trust_domain="liminal.trusted-recovery",
        logical_branch_id="authorization-policy-fork-b",
        branch_contract_sha256=contract,
        authorization_contract_sha256=authorization,
        from_state_ref_sha256=common_state_ref_sha256,
        to_semantic_state_sha256=_sha("fork-state-b"),
    )
    return primary, secondary


def _votes(
    common_witness: dict,
    common_checkpoint: dict,
    primary: PortableForkBranchObservation,
    secondary: PortableForkBranchObservation,
):
    primary_checkpoint = build_fork_branch_checkpoint(common_checkpoint, primary)
    secondary_checkpoint = build_fork_branch_checkpoint(common_checkpoint, secondary)
    primary_witness = build_fork_branch_witness(common_witness, primary_checkpoint)
    secondary_witness = build_fork_branch_witness(common_witness, secondary_checkpoint)
    target = _sha("reconciled-state")
    contract = _sha("reconciliation-contract")
    authorization = _sha("reconciliation-authorization")
    primary_vote = build_reconciliation_vote(
        verified=True,
        provider_id=primary.provider_id,
        authority_id=primary.authority_id,
        vote_provenance_sha256=_sha("vote-a-proof"),
        logical_reconciliation_id="authorization-policy-reconcile",
        branch_checkpoint=primary_checkpoint,
        branch_witness=primary_witness,
        target_semantic_state_sha256=target,
        reconciliation_contract_sha256=contract,
        authorization_contract_sha256=authorization,
    )
    secondary_vote = build_reconciliation_vote(
        verified=True,
        provider_id=secondary.provider_id,
        authority_id=secondary.authority_id,
        vote_provenance_sha256=_sha("vote-b-proof"),
        logical_reconciliation_id="authorization-policy-reconcile",
        branch_checkpoint=secondary_checkpoint,
        branch_witness=secondary_witness,
        target_semantic_state_sha256=target,
        reconciliation_contract_sha256=contract,
        authorization_contract_sha256=authorization,
    )
    return primary_vote, secondary_vote


def _agreement():
    anchor_checkpoint, anchor_witness = _anchor_objects()
    common_checkpoints, common_witnesses = _common_prefix(anchor_checkpoint, anchor_witness)
    common_checkpoint = common_checkpoints[-1]
    common_witness = common_witnesses[-1]
    primary, secondary = _branch_observations(common_checkpoint)
    primary_vote, secondary_vote = _votes(
        common_witness, common_checkpoint, primary, secondary
    )
    agreement = compare_causal_fork_reconciliation(
        anchor_checkpoint=anchor_checkpoint,
        anchor_witness=anchor_witness,
        common_checkpoints=common_checkpoints,
        common_witnesses=common_witnesses,
        primary_branch=primary,
        secondary_branch=secondary,
        primary_vote=primary_vote,
        secondary_vote=secondary_vote,
    )
    return (
        agreement,
        anchor_checkpoint,
        anchor_witness,
        common_checkpoints,
        common_witnesses,
        primary,
        secondary,
        primary_vote,
        secondary_vote,
    )


def test_divergent_branches_reconcile_without_erasing_lineage() -> None:
    agreement, _, _, common_checkpoints, common_witnesses, *_ = _agreement()
    assert agreement.verified is True
    assert agreement.reason == "causal_fork_reconciliation_portability_verified"
    assert len(agreement.branch_checkpoints) == 2
    assert len(agreement.branch_witnesses) == 2
    assert agreement.reconciliation_checkpoint is not None
    assert agreement.reconciliation_witness is not None
    assert agreement.receipt is not None
    assert agreement.receipt["fork_causal_epoch"] == 3
    assert agreement.receipt["reconciled_causal_epoch"] == 4
    assert agreement.receipt["lineage_parent_count"] == 2
    assert agreement.receipt["both_lineages_preserved"] is True
    assert agreement.receipt["fork_semantics_divergent"] is True
    assert agreement.receipt["raw_evidence_embedded"] is False
    assert validate_reconciliation_checkpoint(
        agreement.reconciliation_checkpoint,
        common_checkpoints[-1],
        common_witnesses[-1],
        agreement.branch_checkpoints,
        agreement.branch_witnesses,
    )
    assert validate_reconciliation_witness(
        agreement.reconciliation_witness,
        common_witnesses[-1],
        agreement.reconciliation_checkpoint,
        agreement.branch_witnesses,
    )


def test_branch_input_order_does_not_change_reconciliation_identity() -> None:
    (
        agreement,
        anchor_checkpoint,
        anchor_witness,
        common_checkpoints,
        common_witnesses,
        primary,
        secondary,
        primary_vote,
        secondary_vote,
    ) = _agreement()
    reversed_agreement = compare_causal_fork_reconciliation(
        anchor_checkpoint=anchor_checkpoint,
        anchor_witness=anchor_witness,
        common_checkpoints=common_checkpoints,
        common_witnesses=common_witnesses,
        primary_branch=secondary,
        secondary_branch=primary,
        primary_vote=secondary_vote,
        secondary_vote=primary_vote,
    )
    assert reversed_agreement.verified is True
    assert agreement.reconciliation_checkpoint == reversed_agreement.reconciliation_checkpoint
    assert agreement.reconciliation_witness == reversed_agreement.reconciliation_witness
    assert agreement.receipt == reversed_agreement.receipt


def test_provider_evidence_is_not_embedded_in_portable_objects() -> None:
    agreement, _, _, _, _, primary, secondary, primary_vote, secondary_vote = _agreement()
    portable = repr(
        (
            agreement.branch_checkpoints,
            agreement.branch_witnesses,
            agreement.reconciliation_checkpoint,
            agreement.reconciliation_witness,
        )
    )
    forbidden = {
        primary.provider_id,
        secondary.provider_id,
        primary.authority_id,
        secondary.authority_id,
        primary.branch_provenance_sha256,
        secondary.branch_provenance_sha256,
        primary_vote.vote_provenance_sha256,
        secondary_vote.vote_provenance_sha256,
    }
    assert all(value not in portable for value in forbidden)


@pytest.mark.parametrize(
    ("mutation", "reason"),
    [
        (
            lambda p, s, pv, sv: (replace(p, verified=False), s, pv, sv),
            "fork_branch_observation_invalid",
        ),
        (
            lambda p, s, pv, sv: (p, replace(s, provider_id=p.provider_id), pv, sv),
            "branch_provider_not_independent",
        ),
        (
            lambda p, s, pv, sv: (p, replace(s, authority_id=p.authority_id), pv, sv),
            "branch_authority_not_independent",
        ),
        (
            lambda p, s, pv, sv: (
                p,
                replace(s, branch_provenance_sha256=p.branch_provenance_sha256),
                pv,
                sv,
            ),
            "branch_provenance_not_independent",
        ),
        (
            lambda p, s, pv, sv: (p, replace(s, logical_branch_id=p.logical_branch_id), pv, sv),
            "branch_identity_not_distinct",
        ),
        (
            lambda p, s, pv, sv: (
                p,
                replace(s, to_semantic_state_sha256=p.to_semantic_state_sha256),
                pv,
                sv,
            ),
            "fork_not_semantically_divergent",
        ),
        (
            lambda p, s, pv, sv: (
                p,
                s,
                pv,
                replace(sv, branch_checkpoint_sha256=_sha("wrong-branch")),
            ),
            "reconciliation_vote_branch_mismatch",
        ),
        (
            lambda p, s, pv, sv: (
                p,
                s,
                pv,
                replace(sv, vote_provenance_sha256=pv.vote_provenance_sha256),
            ),
            "vote_provenance_not_independent",
        ),
        (
            lambda p, s, pv, sv: (
                p,
                s,
                pv,
                replace(sv, logical_reconciliation_id="other-reconciliation"),
            ),
            "logical_reconciliation_mismatch",
        ),
        (
            lambda p, s, pv, sv: (
                p,
                s,
                pv,
                replace(sv, target_semantic_state_sha256=_sha("other-target")),
            ),
            "reconciliation_target_mismatch",
        ),
        (
            lambda p, s, pv, sv: (
                p,
                s,
                pv,
                replace(sv, reconciliation_contract_sha256=_sha("other-contract")),
            ),
            "reconciliation_contract_mismatch",
        ),
        (
            lambda p, s, pv, sv: (
                p,
                s,
                pv,
                replace(sv, authorization_contract_sha256=_sha("other-authorization")),
            ),
            "reconciliation_authorization_mismatch",
        ),
    ],
)
def test_fork_and_reconciliation_failures_fail_closed(mutation, reason: str) -> None:
    (
        _,
        anchor_checkpoint,
        anchor_witness,
        common_checkpoints,
        common_witnesses,
        primary,
        secondary,
        primary_vote,
        secondary_vote,
    ) = _agreement()
    primary, secondary, primary_vote, secondary_vote = mutation(
        primary, secondary, primary_vote, secondary_vote
    )
    agreement = compare_causal_fork_reconciliation(
        anchor_checkpoint=anchor_checkpoint,
        anchor_witness=anchor_witness,
        common_checkpoints=common_checkpoints,
        common_witnesses=common_witnesses,
        primary_branch=primary,
        secondary_branch=secondary,
        primary_vote=primary_vote,
        secondary_vote=secondary_vote,
    )
    assert agreement.verified is False
    assert agreement.reason == reason


def test_reconciliation_checkpoint_rejects_missing_parent() -> None:
    agreement, _, _, common_checkpoints, common_witnesses, *_ = _agreement()
    assert agreement.reconciliation_checkpoint is not None
    tampered = deepcopy(agreement.reconciliation_checkpoint)
    tampered["parent_checkpoint_sha256"] = tampered["parent_checkpoint_sha256"][:1]
    assert not validate_reconciliation_checkpoint(
        tampered,
        common_checkpoints[-1],
        common_witnesses[-1],
        agreement.branch_checkpoints,
        agreement.branch_witnesses,
    )


def test_reconciliation_rejects_branch_that_does_not_descend_from_common_tip() -> None:
    agreement, _, _, common_checkpoints, common_witnesses, *_ = _agreement()
    assert agreement.reconciliation_checkpoint is not None
    branch_checkpoints = list(deepcopy(agreement.branch_checkpoints))
    branch_checkpoints[0]["previous_checkpoint_sha256"] = _sha("wrong-common-parent")
    assert not validate_reconciliation_checkpoint(
        agreement.reconciliation_checkpoint,
        common_checkpoints[-1],
        common_witnesses[-1],
        branch_checkpoints,
        agreement.branch_witnesses,
    )


def test_reconciliation_witness_rejects_duplicate_parent() -> None:
    agreement, _, _, _, common_witnesses, *_ = _agreement()
    assert agreement.reconciliation_checkpoint is not None
    assert agreement.reconciliation_witness is not None
    tampered = deepcopy(agreement.reconciliation_witness)
    first = tampered["parent_witness_sha256"][0]
    tampered["parent_witness_sha256"] = [first, first]
    assert not validate_reconciliation_witness(
        tampered,
        common_witnesses[-1],
        agreement.reconciliation_checkpoint,
        agreement.branch_witnesses,
    )


def test_raw_provider_smuggled_as_reconciliation_id_fails_closed() -> None:
    (
        _,
        anchor_checkpoint,
        anchor_witness,
        common_checkpoints,
        common_witnesses,
        primary,
        secondary,
        primary_vote,
        secondary_vote,
    ) = _agreement()
    leaked = primary.provider_id
    primary_vote = replace(primary_vote, logical_reconciliation_id=leaked)
    secondary_vote = replace(secondary_vote, logical_reconciliation_id=leaked)
    agreement = compare_causal_fork_reconciliation(
        anchor_checkpoint=anchor_checkpoint,
        anchor_witness=anchor_witness,
        common_checkpoints=common_checkpoints,
        common_witnesses=common_witnesses,
        primary_branch=primary,
        secondary_branch=secondary,
        primary_vote=primary_vote,
        secondary_vote=secondary_vote,
    )
    assert agreement.verified is False
    assert agreement.reason == "raw_evidence_dependency"


def test_broken_common_prefix_fails_closed() -> None:
    (
        _,
        anchor_checkpoint,
        anchor_witness,
        common_checkpoints,
        common_witnesses,
        primary,
        secondary,
        primary_vote,
        secondary_vote,
    ) = _agreement()
    broken = list(deepcopy(common_checkpoints))
    broken[-1]["previous_checkpoint_sha256"] = _sha("broken-prefix")
    agreement = compare_causal_fork_reconciliation(
        anchor_checkpoint=anchor_checkpoint,
        anchor_witness=anchor_witness,
        common_checkpoints=broken,
        common_witnesses=common_witnesses,
        primary_branch=primary,
        secondary_branch=secondary,
        primary_vote=primary_vote,
        secondary_vote=secondary_vote,
    )
    assert agreement.verified is False
    assert agreement.reason == "portable_common_prefix_invalid"
