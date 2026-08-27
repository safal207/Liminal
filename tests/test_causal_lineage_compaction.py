from __future__ import annotations

from copy import deepcopy
from dataclasses import replace

from liminal.causal_fork_reconciliation import (
    PortableForkBranchObservation,
    build_fork_branch_checkpoint,
    build_fork_branch_witness,
    build_reconciliation_vote,
)
from liminal.causal_lineage_compaction import (
    COMPACTION_REASON,
    build_initial_lineage_accumulator,
    compare_repeated_fork_reconciliation,
)
from liminal.recovery_trust_root_registry import canonical_json_bytes, sha256_hex


def _sha(value: object) -> str:
    return sha256_hex(canonical_json_bytes(value))


def _authority(role: str) -> dict:
    return {
        "schema": "liminal-causal-state-authority/v0.1",
        "role": role,
        "logical_authority_id": f"test.{role}",
        "producer_contract_sha256": _sha(f"producer:{role}"),
        "authorization_contract_sha256": _sha(f"authorization:{role}"),
    }


def _previous_result() -> dict:
    common_state = {
        "schema": "liminal-causal-trust-state-ref/v0.1",
        "trust_domain": "liminal.trusted-recovery",
        "logical_state_id": "liminal.trusted-recovery.authorization-state",
        "causal_epoch": 2,
        "semantic_state_sha256": _sha("common-semantic"),
    }
    parent_lineages = [
        {
            "logical_branch_id": "first-a",
            "semantic_state_sha256": _sha("first-a-semantic"),
            "state_ref_sha256": _sha("first-a-state"),
            "branch_ref_sha256": _sha("first-a-ref"),
            "checkpoint_sha256": _sha("first-a-checkpoint"),
            "witness_sha256": _sha("first-a-witness"),
        },
        {
            "logical_branch_id": "first-b",
            "semantic_state_sha256": _sha("first-b-semantic"),
            "state_ref_sha256": _sha("first-b-state"),
            "branch_ref_sha256": _sha("first-b-ref"),
            "checkpoint_sha256": _sha("first-b-checkpoint"),
            "witness_sha256": _sha("first-b-witness"),
        },
    ]
    parent_lineages = sorted(
        parent_lineages, key=lambda item: item["checkpoint_sha256"]
    )
    state_ref = {
        **common_state,
        "causal_epoch": 4,
        "semantic_state_sha256": _sha("first-reconciled-semantic"),
    }
    reconciliation_ref = {
        "schema": "liminal-causal-reconciliation-ref/v0.1",
        "trust_domain": common_state["trust_domain"],
        "logical_state_id": common_state["logical_state_id"],
        "logical_reconciliation_id": "first-reconcile",
        "from_causal_epoch": 3,
        "to_causal_epoch": 4,
        "common_ancestor_state_ref_sha256": _sha(common_state),
        "common_ancestor_checkpoint_sha256": _sha("common-checkpoint"),
        "common_ancestor_witness_sha256": _sha("common-witness"),
        "parent_lineages": parent_lineages,
        "parent_set_sha256": _sha(parent_lineages),
        "result_state_ref_sha256": _sha(state_ref),
        "reconciliation_contract_sha256": _sha("first-reconciliation-contract"),
        "authorization_contract_sha256": _sha("first-reconciliation-authorization"),
    }
    checkpoint = {
        "schema_version": "liminal-causal-trust-reconciliation-checkpoint/v0.1",
        "state_ref": state_ref,
        "reconciliation_ref": reconciliation_ref,
        "parent_checkpoint_sha256": sorted(
            item["checkpoint_sha256"] for item in parent_lineages
        ),
        "checkpoint_authority": _authority("checkpoint-producer"),
    }
    witness = {
        "schema_version": "liminal-causal-trust-reconciliation-witness/v0.1",
        "state_ref": state_ref,
        "reconciliation_ref_sha256": _sha(reconciliation_ref),
        "checkpoint_sha256": _sha(checkpoint),
        "parent_witness_sha256": sorted(
            item["witness_sha256"] for item in parent_lineages
        ),
        "witness_authority": _authority("witness-producer"),
    }
    receipt = {
        "schema": "liminal-causal-fork-reconciliation-portability-receipt/v0.1",
        "verified": True,
        "reason": "causal_fork_reconciliation_portability_verified",
        "common_ancestor_state_ref_sha256": _sha(common_state),
        "common_ancestor_checkpoint_sha256": _sha("common-checkpoint"),
        "common_ancestor_witness_sha256": _sha("common-witness"),
        "fork_causal_epoch": 3,
        "reconciled_causal_epoch": 4,
        "parent_lineages": parent_lineages,
        "parent_set_sha256": _sha(parent_lineages),
        "target_semantic_state_sha256": state_ref["semantic_state_sha256"],
        "reconciliation_ref_sha256": _sha(reconciliation_ref),
        "reconciliation_checkpoint_sha256": _sha(checkpoint),
        "reconciliation_witness_sha256": _sha(witness),
        "lineage_parent_count": 2,
        "branch_order_canonical": True,
        "both_lineages_preserved": True,
        "fork_semantics_divergent": True,
        "raw_evidence_embedded": False,
    }
    result = {
        "schema": "liminal-causal-fork-reconciliation-portability-proof/v0.1",
        "verified": True,
        "reason": "causal_fork_reconciliation_portability_verified",
        "workflow_sha": "1" * 40,
        "reconciliation_checkpoint": checkpoint,
        "reconciliation_witness": witness,
        "receipt": receipt,
        "receipt_sha256": _sha(receipt),
    }
    return result


def _cycle_inputs(common_checkpoint: dict, common_witness: dict, suffix: str):
    common_state = common_checkpoint["state_ref"]
    contract = _sha(f"branch-contract:{suffix}")
    branch_auth = _sha(f"branch-auth:{suffix}")
    reconciliation_contract = _sha(f"reconciliation-contract:{suffix}")
    reconciliation_auth = _sha(f"reconciliation-auth:{suffix}")
    target = _sha(f"target:{suffix}")

    def branch(label: str, provider: str, authority: str):
        observation = PortableForkBranchObservation(
            verified=True,
            provider_id=provider,
            authority_id=authority,
            branch_provenance_sha256=_sha(f"provenance:{suffix}:{label}"),
            trust_domain=common_state["trust_domain"],
            logical_branch_id=f"branch-{suffix}-{label}",
            branch_contract_sha256=contract,
            authorization_contract_sha256=branch_auth,
            from_state_ref_sha256=_sha(common_state),
            to_semantic_state_sha256=_sha(f"semantic:{suffix}:{label}"),
        )
        checkpoint = build_fork_branch_checkpoint(common_checkpoint, observation)
        witness = build_fork_branch_witness(common_witness, checkpoint)
        vote = build_reconciliation_vote(
            verified=True,
            provider_id=provider,
            authority_id=authority,
            vote_provenance_sha256=_sha(f"vote:{suffix}:{label}"),
            logical_reconciliation_id=f"reconcile-{suffix}",
            branch_checkpoint=checkpoint,
            branch_witness=witness,
            target_semantic_state_sha256=target,
            reconciliation_contract_sha256=reconciliation_contract,
            authorization_contract_sha256=reconciliation_auth,
        )
        return observation, vote

    a = branch("a", f"provider-a-{suffix}", f"authority-a-{suffix}")
    b = branch("b", f"provider-b-{suffix}", f"authority-b-{suffix}")
    return a, b


def _first_repeat():
    predecessor = _previous_result()
    contract = _sha("compaction-contract")
    previous_event, previous_accumulator = build_initial_lineage_accumulator(
        predecessor,
        predecessor_result_sha256=_sha(predecessor),
        compaction_contract_sha256=contract,
    )
    common_checkpoint = predecessor["reconciliation_checkpoint"]
    common_witness = predecessor["reconciliation_witness"]
    (branch_a, vote_a), (branch_b, vote_b) = _cycle_inputs(
        common_checkpoint, common_witness, "two"
    )
    agreement = compare_repeated_fork_reconciliation(
        common_checkpoint=common_checkpoint,
        common_witness=common_witness,
        previous_lineage_event=previous_event,
        previous_lineage_accumulator=previous_accumulator,
        primary_branch=branch_a,
        secondary_branch=branch_b,
        primary_vote=vote_a,
        secondary_vote=vote_b,
    )
    return agreement, predecessor, previous_event, previous_accumulator


def test_second_reconciliation_uses_bounded_lineage_accumulator() -> None:
    agreement, _, _, previous_accumulator = _first_repeat()
    assert agreement.verified is True
    assert agreement.reason == COMPACTION_REASON
    assert agreement.receipt is not None
    assert agreement.lineage_accumulator is not None
    assert agreement.receipt["reconciliation_count"] == 2
    assert agreement.receipt["total_branch_event_count"] == 4
    assert agreement.receipt["common_tip_causal_epoch"] == 4
    assert agreement.receipt["fork_causal_epoch"] == 5
    assert agreement.receipt["reconciled_causal_epoch"] == 6
    assert agreement.receipt["bounded_accumulator_shape"] is True
    assert agreement.receipt["prior_parent_lineages_embedded_in_accumulator"] is False
    assert len(canonical_json_bytes(previous_accumulator)) == len(
        canonical_json_bytes(agreement.lineage_accumulator)
    )


def test_previous_parent_lineages_are_not_copied_into_new_accumulator() -> None:
    agreement, predecessor, _, _ = _first_repeat()
    assert agreement.lineage_accumulator is not None
    assert agreement.lineage_event is not None
    portable = canonical_json_bytes(
        (agreement.lineage_event, agreement.lineage_accumulator)
    ).decode()
    for lineage in predecessor["receipt"]["parent_lineages"]:
        for value in lineage.values():
            assert str(value) not in portable


def test_branch_order_is_canonical() -> None:
    agreement, predecessor, event, accumulator = _first_repeat()
    common_checkpoint = predecessor["reconciliation_checkpoint"]
    common_witness = predecessor["reconciliation_witness"]
    (branch_a, vote_a), (branch_b, vote_b) = _cycle_inputs(
        common_checkpoint, common_witness, "two"
    )
    reversed_agreement = compare_repeated_fork_reconciliation(
        common_checkpoint=common_checkpoint,
        common_witness=common_witness,
        previous_lineage_event=event,
        previous_lineage_accumulator=accumulator,
        primary_branch=branch_b,
        secondary_branch=branch_a,
        primary_vote=vote_b,
        secondary_vote=vote_a,
    )
    assert reversed_agreement.verified is True
    assert (
        reversed_agreement.reconciliation_checkpoint
        == agreement.reconciliation_checkpoint
    )
    assert reversed_agreement.reconciliation_witness == agreement.reconciliation_witness
    assert reversed_agreement.lineage_event == agreement.lineage_event
    assert reversed_agreement.lineage_accumulator == agreement.lineage_accumulator


def test_tampered_accumulator_tip_fails_closed() -> None:
    predecessor = _previous_result()
    event, accumulator = build_initial_lineage_accumulator(
        predecessor,
        predecessor_result_sha256=_sha(predecessor),
        compaction_contract_sha256=_sha("compaction-contract"),
    )
    accumulator = deepcopy(accumulator)
    accumulator["tip_checkpoint_sha256"] = _sha("wrong-tip")
    common_checkpoint = predecessor["reconciliation_checkpoint"]
    common_witness = predecessor["reconciliation_witness"]
    (branch_a, vote_a), (branch_b, vote_b) = _cycle_inputs(
        common_checkpoint, common_witness, "two"
    )
    agreement = compare_repeated_fork_reconciliation(
        common_checkpoint=common_checkpoint,
        common_witness=common_witness,
        previous_lineage_event=event,
        previous_lineage_accumulator=accumulator,
        primary_branch=branch_a,
        secondary_branch=branch_b,
        primary_vote=vote_a,
        secondary_vote=vote_b,
    )
    assert agreement.verified is False
    assert agreement.reason == "lineage_accumulator_tip_mismatch"


def test_vote_target_mismatch_fails_closed() -> None:
    predecessor = _previous_result()
    event, accumulator = build_initial_lineage_accumulator(
        predecessor,
        predecessor_result_sha256=_sha(predecessor),
        compaction_contract_sha256=_sha("compaction-contract"),
    )
    common_checkpoint = predecessor["reconciliation_checkpoint"]
    common_witness = predecessor["reconciliation_witness"]
    (branch_a, vote_a), (branch_b, vote_b) = _cycle_inputs(
        common_checkpoint, common_witness, "two"
    )
    vote_b = replace(vote_b, target_semantic_state_sha256=_sha("other-target"))
    agreement = compare_repeated_fork_reconciliation(
        common_checkpoint=common_checkpoint,
        common_witness=common_witness,
        previous_lineage_event=event,
        previous_lineage_accumulator=accumulator,
        primary_branch=branch_a,
        secondary_branch=branch_b,
        primary_vote=vote_a,
        secondary_vote=vote_b,
    )
    assert agreement.verified is False
    assert agreement.reason == "reconciliation_target_mismatch"


def test_provider_identity_smuggling_fails_closed() -> None:
    predecessor = _previous_result()
    event, accumulator = build_initial_lineage_accumulator(
        predecessor,
        predecessor_result_sha256=_sha(predecessor),
        compaction_contract_sha256=_sha("compaction-contract"),
    )
    common_checkpoint = predecessor["reconciliation_checkpoint"]
    common_witness = predecessor["reconciliation_witness"]
    (branch_a, _), (branch_b, vote_b) = _cycle_inputs(
        common_checkpoint, common_witness, "two"
    )
    branch_a = replace(branch_a, logical_branch_id=branch_a.provider_id)
    branch_checkpoint = build_fork_branch_checkpoint(common_checkpoint, branch_a)
    branch_witness = build_fork_branch_witness(common_witness, branch_checkpoint)
    vote_a = build_reconciliation_vote(
        verified=True,
        provider_id=branch_a.provider_id,
        authority_id=branch_a.authority_id,
        vote_provenance_sha256=_sha("smuggled-vote"),
        logical_reconciliation_id=vote_b.logical_reconciliation_id,
        branch_checkpoint=branch_checkpoint,
        branch_witness=branch_witness,
        target_semantic_state_sha256=vote_b.target_semantic_state_sha256,
        reconciliation_contract_sha256=vote_b.reconciliation_contract_sha256,
        authorization_contract_sha256=vote_b.authorization_contract_sha256,
    )
    agreement = compare_repeated_fork_reconciliation(
        common_checkpoint=common_checkpoint,
        common_witness=common_witness,
        previous_lineage_event=event,
        previous_lineage_accumulator=accumulator,
        primary_branch=branch_a,
        secondary_branch=branch_b,
        primary_vote=vote_a,
        secondary_vote=vote_b,
    )
    assert agreement.verified is False
    assert agreement.reason == "raw_evidence_dependency"


def test_third_reconciliation_keeps_same_accumulator_shape() -> None:
    second, _, _, _ = _first_repeat()
    assert second.verified is True
    assert second.reconciliation_checkpoint is not None
    assert second.reconciliation_witness is not None
    assert second.lineage_event is not None
    assert second.lineage_accumulator is not None
    (branch_a, vote_a), (branch_b, vote_b) = _cycle_inputs(
        second.reconciliation_checkpoint,
        second.reconciliation_witness,
        "three",
    )
    third = compare_repeated_fork_reconciliation(
        common_checkpoint=second.reconciliation_checkpoint,
        common_witness=second.reconciliation_witness,
        previous_lineage_event=second.lineage_event,
        previous_lineage_accumulator=second.lineage_accumulator,
        primary_branch=branch_a,
        secondary_branch=branch_b,
        primary_vote=vote_a,
        secondary_vote=vote_b,
    )
    assert third.verified is True
    assert third.receipt is not None
    assert third.lineage_accumulator is not None
    assert third.receipt["reconciliation_count"] == 3
    assert third.receipt["total_branch_event_count"] == 6
    assert third.receipt["reconciled_causal_epoch"] == 8
    assert len(canonical_json_bytes(second.lineage_accumulator)) == len(
        canonical_json_bytes(third.lineage_accumulator)
    )
