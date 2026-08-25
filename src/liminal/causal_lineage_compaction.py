"""Bounded lineage commitments for repeated causal fork reconciliation.

A verified reconciliation tip should be reusable as the common ancestor of a later
fork without embedding every earlier branch object in the new portable tip.  This
module separates:

* immediate fork/reconciliation objects, which preserve the two current parents;
* a fixed-shape lineage event, which summarizes one verified reconciliation; and
* a fixed-shape lineage accumulator, which recursively commits prior events.

The accumulator does not replace evidence verification.  It is accepted only when
it is bound to an attested predecessor result and its exact checkpoint/witness tip.
"""

from __future__ import annotations

import re
from collections.abc import Mapping, Sequence
from dataclasses import dataclass
from typing import Any

from liminal.causal_fork_reconciliation import (
    PortableForkBranchObservation,
    PortableReconciliationVote,
    build_fork_branch_checkpoint,
    build_fork_branch_witness,
    build_reconciliation_checkpoint,
    build_reconciliation_witness,
)
from liminal.recovery_trust_root_registry import canonical_json_bytes, sha256_hex

PRIOR_PROOF_SCHEMA = "liminal-causal-fork-reconciliation-portability-proof/v0.1"
PRIOR_PROOF_REASON = "causal_fork_reconciliation_portability_verified"
LINEAGE_EVENT_SCHEMA = "liminal-causal-lineage-event/v0.1"
LINEAGE_ACCUMULATOR_SCHEMA = "liminal-causal-lineage-accumulator/v0.1"
LINEAGE_ROOT_STEP_SCHEMA = "liminal-causal-lineage-root-step/v0.1"
COMPACTION_RECEIPT_SCHEMA = "liminal-repeated-fork-lineage-compaction-receipt/v0.1"
COMPACTION_REASON = "repeated_fork_lineage_compaction_verified"
STATE_REF_SCHEMA = "liminal-causal-trust-state-ref/v0.1"
ZERO_SHA256 = "0" * 64
_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")
_HEX_COUNTER_RE = re.compile(r"^[0-9a-f]{16}$")


@dataclass(frozen=True)
class RepeatedForkLineageCompactionAgreement:
    """Verified repeated fork plus bounded recursive lineage commitment."""

    verified: bool
    reason: str
    branch_checkpoints: tuple[dict[str, Any], ...] = ()
    branch_witnesses: tuple[dict[str, Any], ...] = ()
    reconciliation_checkpoint: dict[str, Any] | None = None
    reconciliation_witness: dict[str, Any] | None = None
    previous_lineage_event: dict[str, Any] | None = None
    previous_lineage_accumulator: dict[str, Any] | None = None
    lineage_event: dict[str, Any] | None = None
    lineage_accumulator: dict[str, Any] | None = None
    receipt: dict[str, Any] | None = None


def _digest(value: object) -> str:
    return sha256_hex(canonical_json_bytes(value))


def _valid_sha256(value: object) -> bool:
    return isinstance(value, str) and _SHA256_RE.fullmatch(value) is not None


def _counter(value: int) -> str:
    if isinstance(value, bool) or value < 0 or value > 0xFFFFFFFFFFFFFFFF:
        raise ValueError("lineage_counter_out_of_range")
    return f"{value:016x}"


def _counter_value(value: object) -> int:
    if not isinstance(value, str) or _HEX_COUNTER_RE.fullmatch(value) is None:
        raise ValueError("lineage_counter_invalid")
    return int(value, 16)


def _valid_state_ref(value: object) -> bool:
    if not isinstance(value, Mapping) or set(value) != {
        "schema",
        "trust_domain",
        "logical_state_id",
        "causal_epoch",
        "semantic_state_sha256",
    }:
        return False
    epoch = value.get("causal_epoch")
    return (
        value.get("schema") == STATE_REF_SCHEMA
        and isinstance(value.get("trust_domain"), str)
        and bool(value.get("trust_domain"))
        and isinstance(value.get("logical_state_id"), str)
        and bool(value.get("logical_state_id"))
        and isinstance(epoch, int)
        and not isinstance(epoch, bool)
        and epoch >= 0
        and _valid_sha256(value.get("semantic_state_sha256"))
    )


def _valid_branch_observation(value: object) -> bool:
    if not isinstance(value, PortableForkBranchObservation):
        return False
    digests = (
        value.branch_provenance_sha256,
        value.branch_contract_sha256,
        value.authorization_contract_sha256,
        value.from_state_ref_sha256,
        value.to_semantic_state_sha256,
    )
    return (
        isinstance(value.verified, bool)
        and value.verified
        and isinstance(value.provider_id, str)
        and bool(value.provider_id)
        and isinstance(value.authority_id, str)
        and bool(value.authority_id)
        and isinstance(value.trust_domain, str)
        and bool(value.trust_domain)
        and isinstance(value.logical_branch_id, str)
        and bool(value.logical_branch_id)
        and all(_valid_sha256(item) for item in digests)
    )


def _valid_vote(value: object) -> bool:
    if not isinstance(value, PortableReconciliationVote):
        return False
    digests = (
        value.vote_provenance_sha256,
        value.branch_ref_sha256,
        value.branch_state_ref_sha256,
        value.branch_checkpoint_sha256,
        value.branch_witness_sha256,
        value.target_semantic_state_sha256,
        value.reconciliation_contract_sha256,
        value.authorization_contract_sha256,
    )
    return (
        isinstance(value.verified, bool)
        and value.verified
        and isinstance(value.provider_id, str)
        and bool(value.provider_id)
        and isinstance(value.authority_id, str)
        and bool(value.authority_id)
        and isinstance(value.trust_domain, str)
        and bool(value.trust_domain)
        and isinstance(value.logical_reconciliation_id, str)
        and bool(value.logical_reconciliation_id)
        and all(_valid_sha256(item) for item in digests)
    )


def _all_strings(value: object) -> list[str]:
    if isinstance(value, str):
        return [value]
    if isinstance(value, Mapping):
        result: list[str] = []
        for item in value.values():
            result.extend(_all_strings(item))
        return result
    if isinstance(value, (list, tuple)):
        result = []
        for item in value:
            result.extend(_all_strings(item))
        return result
    return []


def _tip_objects(
    checkpoint: object,
    witness: object,
) -> tuple[Mapping[str, Any], Mapping[str, Any], Mapping[str, Any]] | None:
    if not isinstance(checkpoint, Mapping) or not isinstance(witness, Mapping):
        return None
    state_ref = checkpoint.get("state_ref")
    if not _valid_state_ref(state_ref) or witness.get("state_ref") != state_ref:
        return None
    if witness.get("checkpoint_sha256") != _digest(checkpoint):
        return None
    assert isinstance(state_ref, Mapping)
    return checkpoint, witness, state_ref


def _reconciliation_event(
    *,
    reconciliation_count: int,
    common_state_ref_sha256: str,
    common_checkpoint_sha256: str,
    common_witness_sha256: str,
    reconciliation_checkpoint: Mapping[str, Any],
    reconciliation_witness: Mapping[str, Any],
    compaction_contract_sha256: str,
) -> dict[str, Any]:
    tip = _tip_objects(reconciliation_checkpoint, reconciliation_witness)
    if tip is None:
        raise ValueError("reconciliation_tip_invalid")
    _, _, state_ref = tip
    reconciliation_ref = reconciliation_checkpoint.get("reconciliation_ref")
    parent_checkpoints = reconciliation_checkpoint.get("parent_checkpoint_sha256")
    parent_witnesses = reconciliation_witness.get("parent_witness_sha256")
    if not isinstance(reconciliation_ref, Mapping):
        raise ValueError("reconciliation_ref_missing")
    if not isinstance(parent_checkpoints, list) or len(parent_checkpoints) != 2:
        raise ValueError("reconciliation_checkpoint_parents_invalid")
    if not isinstance(parent_witnesses, list) or len(parent_witnesses) != 2:
        raise ValueError("reconciliation_witness_parents_invalid")
    if (
        parent_checkpoints != sorted(parent_checkpoints)
        or len(set(parent_checkpoints)) != 2
    ):
        raise ValueError("reconciliation_checkpoint_parents_not_canonical")
    if parent_witnesses != sorted(parent_witnesses) or len(set(parent_witnesses)) != 2:
        raise ValueError("reconciliation_witness_parents_not_canonical")
    if not all(
        _valid_sha256(item) for item in (*parent_checkpoints, *parent_witnesses)
    ):
        raise ValueError("reconciliation_parent_digest_invalid")
    parent_lineages = reconciliation_ref.get("parent_lineages")
    if not isinstance(parent_lineages, list) or len(parent_lineages) != 2:
        raise ValueError("reconciliation_parent_lineages_invalid")
    if reconciliation_ref.get("parent_set_sha256") != _digest(parent_lineages):
        raise ValueError("reconciliation_parent_set_mismatch")
    if not all(isinstance(item, Mapping) for item in parent_lineages):
        raise ValueError("reconciliation_parent_lineages_invalid")
    if (
        sorted(str(item.get("checkpoint_sha256", "")) for item in parent_lineages)
        != parent_checkpoints
    ):
        raise ValueError("reconciliation_checkpoint_lineage_mismatch")
    if (
        sorted(str(item.get("witness_sha256", "")) for item in parent_lineages)
        != parent_witnesses
    ):
        raise ValueError("reconciliation_witness_lineage_mismatch")
    if reconciliation_ref.get("result_state_ref_sha256") != _digest(state_ref):
        raise ValueError("reconciliation_result_state_mismatch")
    if (
        reconciliation_witness.get("reconciliation_ref_sha256")
        != _digest(reconciliation_ref)
    ):
        raise ValueError("reconciliation_witness_ref_mismatch")
    if (
        reconciliation_ref.get("common_ancestor_state_ref_sha256")
        != common_state_ref_sha256
    ):
        raise ValueError("reconciliation_common_state_mismatch")
    if (
        reconciliation_ref.get("common_ancestor_checkpoint_sha256")
        != common_checkpoint_sha256
    ):
        raise ValueError("reconciliation_common_checkpoint_mismatch")
    if (
        reconciliation_ref.get("common_ancestor_witness_sha256")
        != common_witness_sha256
    ):
        raise ValueError("reconciliation_common_witness_mismatch")
    from_epoch = reconciliation_ref.get("from_causal_epoch")
    to_epoch = reconciliation_ref.get("to_causal_epoch")
    if (
        not isinstance(from_epoch, int)
        or isinstance(from_epoch, bool)
        or not isinstance(to_epoch, int)
        or isinstance(to_epoch, bool)
        or to_epoch != from_epoch + 1
        or state_ref.get("causal_epoch") != to_epoch
    ):
        raise ValueError("reconciliation_epoch_invalid")
    for value in (
        common_state_ref_sha256,
        common_checkpoint_sha256,
        common_witness_sha256,
        reconciliation_ref.get("parent_set_sha256"),
        reconciliation_ref.get("reconciliation_contract_sha256"),
        reconciliation_ref.get("authorization_contract_sha256"),
        state_ref.get("semantic_state_sha256"),
        compaction_contract_sha256,
    ):
        if not _valid_sha256(value):
            raise ValueError("lineage_event_digest_invalid")
    return {
        "schema": LINEAGE_EVENT_SCHEMA,
        "event_kind": "two-parent-reconciliation",
        "reconciliation_count_hex": _counter(reconciliation_count),
        "from_causal_epoch_hex": _counter(from_epoch),
        "to_causal_epoch_hex": _counter(to_epoch),
        "common_tip_state_ref_sha256": common_state_ref_sha256,
        "common_tip_checkpoint_sha256": common_checkpoint_sha256,
        "common_tip_witness_sha256": common_witness_sha256,
        "parent_set_sha256": reconciliation_ref["parent_set_sha256"],
        "reconciliation_ref_sha256": _digest(reconciliation_ref),
        "result_state_ref_sha256": _digest(state_ref),
        "result_checkpoint_sha256": _digest(reconciliation_checkpoint),
        "result_witness_sha256": _digest(reconciliation_witness),
        "target_semantic_state_sha256": state_ref["semantic_state_sha256"],
        "reconciliation_contract_sha256": reconciliation_ref[
            "reconciliation_contract_sha256"
        ],
        "authorization_contract_sha256": reconciliation_ref[
            "authorization_contract_sha256"
        ],
        "compaction_contract_sha256": compaction_contract_sha256,
    }


def build_lineage_event_from_result(
    result: Mapping[str, Any],
    *,
    reconciliation_count: int,
    compaction_contract_sha256: str,
) -> dict[str, Any]:
    if (
        result.get("schema") != PRIOR_PROOF_SCHEMA
        or result.get("verified") is not True
        or result.get("reason") != PRIOR_PROOF_REASON
    ):
        raise ValueError("predecessor_reconciliation_result_invalid")
    checkpoint = result.get("reconciliation_checkpoint")
    witness = result.get("reconciliation_witness")
    receipt = result.get("receipt")
    if not isinstance(checkpoint, Mapping) or not isinstance(witness, Mapping):
        raise ValueError("predecessor_reconciliation_tip_missing")
    if not isinstance(receipt, Mapping):
        raise ValueError("predecessor_reconciliation_receipt_missing")
    if (
        receipt.get("verified") is not True
        or receipt.get("reason") != PRIOR_PROOF_REASON
        or receipt.get("both_lineages_preserved") is not True
        or receipt.get("fork_semantics_divergent") is not True
        or receipt.get("branch_order_canonical") is not True
        or receipt.get("raw_evidence_embedded") is not False
        or receipt.get("lineage_parent_count") != 2
    ):
        raise ValueError("predecessor_reconciliation_receipt_invalid")
    if receipt.get("reconciliation_checkpoint_sha256") != _digest(checkpoint):
        raise ValueError("predecessor_checkpoint_digest_mismatch")
    if receipt.get("reconciliation_witness_sha256") != _digest(witness):
        raise ValueError("predecessor_witness_digest_mismatch")
    if result.get("receipt_sha256") != _digest(receipt):
        raise ValueError("predecessor_receipt_digest_mismatch")
    event = _reconciliation_event(
        reconciliation_count=reconciliation_count,
        common_state_ref_sha256=str(
            receipt.get("common_ancestor_state_ref_sha256", "")
        ),
        common_checkpoint_sha256=str(
            receipt.get("common_ancestor_checkpoint_sha256", "")
        ),
        common_witness_sha256=str(
            receipt.get("common_ancestor_witness_sha256", "")
        ),
        reconciliation_checkpoint=checkpoint,
        reconciliation_witness=witness,
        compaction_contract_sha256=compaction_contract_sha256,
    )
    if event["parent_set_sha256"] != receipt.get("parent_set_sha256"):
        raise ValueError("predecessor_parent_set_mismatch")
    if event["target_semantic_state_sha256"] != receipt.get(
        "target_semantic_state_sha256"
    ):
        raise ValueError("predecessor_target_state_mismatch")
    return event


def _lineage_root(
    *,
    previous_lineage_root_sha256: str,
    event_sha256: str,
    reconciliation_count_hex: str,
    compaction_contract_sha256: str,
) -> str:
    return _digest(
        {
            "schema": LINEAGE_ROOT_STEP_SCHEMA,
            "previous_lineage_root_sha256": previous_lineage_root_sha256,
            "event_sha256": event_sha256,
            "reconciliation_count_hex": reconciliation_count_hex,
            "compaction_contract_sha256": compaction_contract_sha256,
        }
    )


def _build_accumulator(
    *,
    previous_accumulator_sha256: str,
    previous_lineage_root_sha256: str,
    event: Mapping[str, Any],
    trust_domain: str,
    logical_state_id: str,
    reconciliation_count: int,
    branch_event_count: int,
    previous_accumulator: Mapping[str, Any] | None = None,
) -> dict[str, Any]:
    event_sha256 = _digest(event)
    count_hex = _counter(reconciliation_count)
    accumulator = {
        "schema": LINEAGE_ACCUMULATOR_SCHEMA,
        "trust_domain": trust_domain,
        "logical_state_id": logical_state_id,
        "reconciliation_count_hex": count_hex,
        "branch_event_count_hex": _counter(branch_event_count),
        "tip_causal_epoch_hex": event["to_causal_epoch_hex"],
        "previous_accumulator_sha256": previous_accumulator_sha256,
        "previous_lineage_root_sha256": previous_lineage_root_sha256,
        "latest_event_sha256": event_sha256,
        "lineage_root_sha256": _lineage_root(
            previous_lineage_root_sha256=previous_lineage_root_sha256,
            event_sha256=event_sha256,
            reconciliation_count_hex=count_hex,
            compaction_contract_sha256=event["compaction_contract_sha256"],
        ),
        "tip_state_ref_sha256": event["result_state_ref_sha256"],
        "tip_checkpoint_sha256": event["result_checkpoint_sha256"],
        "tip_witness_sha256": event["result_witness_sha256"],
        "compaction_contract_sha256": event["compaction_contract_sha256"],
    }
    if not validate_lineage_accumulator(
        accumulator,
        event=event,
        previous_accumulator=previous_accumulator,
    ):
        raise ValueError("lineage_accumulator_invalid")
    return accumulator


def build_initial_lineage_accumulator(
    predecessor_result: Mapping[str, Any],
    *,
    predecessor_result_sha256: str,
    compaction_contract_sha256: str,
) -> tuple[dict[str, Any], dict[str, Any]]:
    if _digest(predecessor_result) != predecessor_result_sha256:
        raise ValueError("predecessor_result_digest_mismatch")
    event = build_lineage_event_from_result(
        predecessor_result,
        reconciliation_count=1,
        compaction_contract_sha256=compaction_contract_sha256,
    )
    checkpoint = predecessor_result["reconciliation_checkpoint"]
    state_ref = checkpoint["state_ref"]
    accumulator = _build_accumulator(
        previous_accumulator_sha256=ZERO_SHA256,
        previous_lineage_root_sha256=ZERO_SHA256,
        event=event,
        trust_domain=str(state_ref["trust_domain"]),
        logical_state_id=str(state_ref["logical_state_id"]),
        reconciliation_count=1,
        branch_event_count=2,
    )
    return event, accumulator


def validate_lineage_accumulator(
    value: object,
    *,
    event: Mapping[str, Any],
    previous_accumulator: Mapping[str, Any] | None = None,
    tip_checkpoint: Mapping[str, Any] | None = None,
    tip_witness: Mapping[str, Any] | None = None,
) -> bool:
    if not isinstance(value, Mapping) or set(value) != {
        "schema",
        "trust_domain",
        "logical_state_id",
        "reconciliation_count_hex",
        "branch_event_count_hex",
        "tip_causal_epoch_hex",
        "previous_accumulator_sha256",
        "previous_lineage_root_sha256",
        "latest_event_sha256",
        "lineage_root_sha256",
        "tip_state_ref_sha256",
        "tip_checkpoint_sha256",
        "tip_witness_sha256",
        "compaction_contract_sha256",
    }:
        return False
    if not isinstance(event, Mapping) or event.get("schema") != LINEAGE_EVENT_SCHEMA:
        return False
    try:
        count = _counter_value(value.get("reconciliation_count_hex"))
        branch_count = _counter_value(value.get("branch_event_count_hex"))
        tip_epoch = _counter_value(value.get("tip_causal_epoch_hex"))
        event_count = _counter_value(event.get("reconciliation_count_hex"))
        event_epoch = _counter_value(event.get("to_causal_epoch_hex"))
    except ValueError:
        return False
    if count != event_count or tip_epoch != event_epoch or branch_count != count * 2:
        return False
    if value.get("schema") != LINEAGE_ACCUMULATOR_SCHEMA:
        return False
    if not isinstance(value.get("trust_domain"), str) or not value["trust_domain"]:
        return False
    if (
        not isinstance(value.get("logical_state_id"), str)
        or not value["logical_state_id"]
    ):
        return False
    if value.get("latest_event_sha256") != _digest(event):
        return False
    if value.get("tip_state_ref_sha256") != event.get("result_state_ref_sha256"):
        return False
    if value.get("tip_checkpoint_sha256") != event.get("result_checkpoint_sha256"):
        return False
    if value.get("tip_witness_sha256") != event.get("result_witness_sha256"):
        return False
    if value.get("compaction_contract_sha256") != event.get(
        "compaction_contract_sha256"
    ):
        return False
    if not all(
        _valid_sha256(value.get(key))
        for key in (
            "previous_accumulator_sha256",
            "previous_lineage_root_sha256",
            "latest_event_sha256",
            "lineage_root_sha256",
            "tip_state_ref_sha256",
            "tip_checkpoint_sha256",
            "tip_witness_sha256",
            "compaction_contract_sha256",
        )
    ):
        return False
    if previous_accumulator is None:
        if count == 1:
            if value.get("previous_accumulator_sha256") != ZERO_SHA256:
                return False
            if value.get("previous_lineage_root_sha256") != ZERO_SHA256:
                return False
        else:
            if value.get("previous_accumulator_sha256") == ZERO_SHA256:
                return False
            if value.get("previous_lineage_root_sha256") == ZERO_SHA256:
                return False
    else:
        try:
            previous_count = _counter_value(
                previous_accumulator.get("reconciliation_count_hex")
            )
        except ValueError:
            return False
        if count != previous_count + 1:
            return False
        if value.get("previous_accumulator_sha256") != _digest(previous_accumulator):
            return False
        if value.get("previous_lineage_root_sha256") != previous_accumulator.get(
            "lineage_root_sha256"
        ):
            return False
        if event.get("common_tip_state_ref_sha256") != previous_accumulator.get(
            "tip_state_ref_sha256"
        ):
            return False
        if event.get("common_tip_checkpoint_sha256") != previous_accumulator.get(
            "tip_checkpoint_sha256"
        ):
            return False
        if event.get("common_tip_witness_sha256") != previous_accumulator.get(
            "tip_witness_sha256"
        ):
            return False
        try:
            previous_epoch = _counter_value(
                previous_accumulator.get("tip_causal_epoch_hex")
            )
            from_epoch = _counter_value(event.get("from_causal_epoch_hex"))
        except ValueError:
            return False
        if from_epoch != previous_epoch + 1 or tip_epoch != previous_epoch + 2:
            return False
    expected_root = _lineage_root(
        previous_lineage_root_sha256=str(
            value.get("previous_lineage_root_sha256", "")
        ),
        event_sha256=str(value.get("latest_event_sha256", "")),
        reconciliation_count_hex=str(value.get("reconciliation_count_hex", "")),
        compaction_contract_sha256=str(
            value.get("compaction_contract_sha256", "")
        ),
    )
    if value.get("lineage_root_sha256") != expected_root:
        return False
    if tip_checkpoint is not None or tip_witness is not None:
        tip = _tip_objects(tip_checkpoint, tip_witness)
        if tip is None:
            return False
        checkpoint, witness, state_ref = tip
        if value.get("trust_domain") != state_ref.get("trust_domain"):
            return False
        if value.get("logical_state_id") != state_ref.get("logical_state_id"):
            return False
        if value.get("tip_causal_epoch_hex") != _counter(
            int(state_ref["causal_epoch"])
        ):
            return False
        if value.get("tip_state_ref_sha256") != _digest(state_ref):
            return False
        if value.get("tip_checkpoint_sha256") != _digest(checkpoint):
            return False
        if value.get("tip_witness_sha256") != _digest(witness):
            return False
    return True


def _vote_matches_branch(
    vote: PortableReconciliationVote,
    branch: PortableForkBranchObservation,
    checkpoint: Mapping[str, Any],
    witness: Mapping[str, Any],
) -> bool:
    state_ref = checkpoint.get("state_ref")
    branch_ref = checkpoint.get("branch_ref")
    return (
        _valid_vote(vote)
        and vote.provider_id == branch.provider_id
        and vote.authority_id == branch.authority_id
        and isinstance(state_ref, Mapping)
        and isinstance(branch_ref, Mapping)
        and vote.trust_domain == state_ref.get("trust_domain")
        and vote.branch_ref_sha256 == _digest(branch_ref)
        and vote.branch_state_ref_sha256 == _digest(state_ref)
        and vote.branch_checkpoint_sha256 == _digest(checkpoint)
        and vote.branch_witness_sha256 == _digest(witness)
    )


def _canonical_pairs(
    pairs: Sequence[
        tuple[
            PortableForkBranchObservation,
            PortableReconciliationVote,
            Mapping[str, Any],
            Mapping[str, Any],
        ]
    ],
) -> tuple[
    tuple[
        PortableForkBranchObservation,
        PortableReconciliationVote,
        Mapping[str, Any],
        Mapping[str, Any],
    ],
    ...,
]:
    return tuple(sorted(pairs, key=lambda item: _digest(item[2])))


def compare_repeated_fork_reconciliation(
    *,
    common_checkpoint: Mapping[str, Any],
    common_witness: Mapping[str, Any],
    previous_lineage_event: Mapping[str, Any],
    previous_lineage_accumulator: Mapping[str, Any],
    primary_branch: PortableForkBranchObservation,
    secondary_branch: PortableForkBranchObservation,
    primary_vote: PortableReconciliationVote,
    secondary_vote: PortableReconciliationVote,
) -> RepeatedForkLineageCompactionAgreement:
    """Reconcile a second fork from a compactly committed prior DAG tip."""

    if not validate_lineage_accumulator(
        previous_lineage_accumulator,
        event=previous_lineage_event,
        tip_checkpoint=common_checkpoint,
        tip_witness=common_witness,
    ):
        return RepeatedForkLineageCompactionAgreement(
            False, "lineage_accumulator_tip_mismatch"
        )
    tip = _tip_objects(common_checkpoint, common_witness)
    if tip is None:
        return RepeatedForkLineageCompactionAgreement(False, "compacted_tip_invalid")
    _, _, common_state_ref = tip

    if not _valid_branch_observation(primary_branch) or not _valid_branch_observation(
        secondary_branch
    ):
        return RepeatedForkLineageCompactionAgreement(
            False, "fork_branch_observation_invalid"
        )
    if primary_branch.provider_id == secondary_branch.provider_id:
        return RepeatedForkLineageCompactionAgreement(
            False, "branch_provider_not_independent"
        )
    if primary_branch.authority_id == secondary_branch.authority_id:
        return RepeatedForkLineageCompactionAgreement(
            False, "branch_authority_not_independent"
        )
    if (
        primary_branch.branch_provenance_sha256
        == secondary_branch.branch_provenance_sha256
    ):
        return RepeatedForkLineageCompactionAgreement(
            False, "branch_provenance_not_independent"
        )
    if primary_branch.trust_domain != secondary_branch.trust_domain:
        return RepeatedForkLineageCompactionAgreement(
            False, "branch_trust_domain_mismatch"
        )
    if primary_branch.trust_domain != common_state_ref["trust_domain"]:
        return RepeatedForkLineageCompactionAgreement(
            False, "branch_common_domain_mismatch"
        )
    if primary_branch.logical_branch_id == secondary_branch.logical_branch_id:
        return RepeatedForkLineageCompactionAgreement(
            False, "branch_identity_not_distinct"
        )
    if primary_branch.branch_contract_sha256 != secondary_branch.branch_contract_sha256:
        return RepeatedForkLineageCompactionAgreement(False, "branch_contract_mismatch")
    if (
        primary_branch.authorization_contract_sha256
        != secondary_branch.authorization_contract_sha256
    ):
        return RepeatedForkLineageCompactionAgreement(
            False, "branch_authorization_mismatch"
        )
    common_state_digest = _digest(common_state_ref)
    if primary_branch.from_state_ref_sha256 != common_state_digest:
        return RepeatedForkLineageCompactionAgreement(
            False, "branch_common_state_mismatch"
        )
    if secondary_branch.from_state_ref_sha256 != common_state_digest:
        return RepeatedForkLineageCompactionAgreement(
            False, "branch_common_state_mismatch"
        )
    if (
        primary_branch.to_semantic_state_sha256
        == secondary_branch.to_semantic_state_sha256
    ):
        return RepeatedForkLineageCompactionAgreement(
            False, "fork_not_semantically_divergent"
        )
    if common_state_ref["semantic_state_sha256"] in {
        primary_branch.to_semantic_state_sha256,
        secondary_branch.to_semantic_state_sha256,
    }:
        return RepeatedForkLineageCompactionAgreement(False, "fork_state_not_advanced")

    primary_checkpoint = build_fork_branch_checkpoint(common_checkpoint, primary_branch)
    secondary_checkpoint = build_fork_branch_checkpoint(
        common_checkpoint, secondary_branch
    )
    primary_witness = build_fork_branch_witness(common_witness, primary_checkpoint)
    secondary_witness = build_fork_branch_witness(common_witness, secondary_checkpoint)
    if not _vote_matches_branch(
        primary_vote, primary_branch, primary_checkpoint, primary_witness
    ) or not _vote_matches_branch(
        secondary_vote, secondary_branch, secondary_checkpoint, secondary_witness
    ):
        return RepeatedForkLineageCompactionAgreement(
            False, "reconciliation_vote_branch_mismatch"
        )
    if primary_vote.vote_provenance_sha256 == secondary_vote.vote_provenance_sha256:
        return RepeatedForkLineageCompactionAgreement(
            False, "vote_provenance_not_independent"
        )
    if (
        primary_vote.logical_reconciliation_id
        != secondary_vote.logical_reconciliation_id
    ):
        return RepeatedForkLineageCompactionAgreement(
            False, "logical_reconciliation_mismatch"
        )
    if (
        primary_vote.target_semantic_state_sha256
        != secondary_vote.target_semantic_state_sha256
    ):
        return RepeatedForkLineageCompactionAgreement(
            False, "reconciliation_target_mismatch"
        )
    if (
        primary_vote.reconciliation_contract_sha256
        != secondary_vote.reconciliation_contract_sha256
    ):
        return RepeatedForkLineageCompactionAgreement(
            False, "reconciliation_contract_mismatch"
        )
    if (
        primary_vote.authorization_contract_sha256
        != secondary_vote.authorization_contract_sha256
    ):
        return RepeatedForkLineageCompactionAgreement(
            False, "reconciliation_authorization_mismatch"
        )
    target_semantic_state = primary_vote.target_semantic_state_sha256
    if target_semantic_state in {
        common_state_ref["semantic_state_sha256"],
        primary_branch.to_semantic_state_sha256,
        secondary_branch.to_semantic_state_sha256,
    }:
        return RepeatedForkLineageCompactionAgreement(
            False, "reconciliation_target_not_new"
        )

    canonical_pairs = _canonical_pairs(
        (
            (primary_branch, primary_vote, primary_checkpoint, primary_witness),
            (secondary_branch, secondary_vote, secondary_checkpoint, secondary_witness),
        )
    )
    branch_checkpoints = tuple(dict(item[2]) for item in canonical_pairs)
    branch_witnesses = tuple(dict(item[3]) for item in canonical_pairs)
    reconciliation_checkpoint = build_reconciliation_checkpoint(
        common_checkpoint,
        common_witness,
        branch_checkpoints,
        branch_witnesses,
        logical_reconciliation_id=primary_vote.logical_reconciliation_id,
        target_semantic_state_sha256=target_semantic_state,
        reconciliation_contract_sha256=primary_vote.reconciliation_contract_sha256,
        authorization_contract_sha256=primary_vote.authorization_contract_sha256,
    )
    reconciliation_witness = build_reconciliation_witness(
        common_witness,
        reconciliation_checkpoint,
        branch_witnesses,
    )
    previous_count = _counter_value(
        previous_lineage_accumulator["reconciliation_count_hex"]
    )
    event = _reconciliation_event(
        reconciliation_count=previous_count + 1,
        common_state_ref_sha256=_digest(common_state_ref),
        common_checkpoint_sha256=_digest(common_checkpoint),
        common_witness_sha256=_digest(common_witness),
        reconciliation_checkpoint=reconciliation_checkpoint,
        reconciliation_witness=reconciliation_witness,
        compaction_contract_sha256=str(
            previous_lineage_accumulator["compaction_contract_sha256"]
        ),
    )
    accumulator = _build_accumulator(
        previous_accumulator_sha256=_digest(previous_lineage_accumulator),
        previous_lineage_root_sha256=str(
            previous_lineage_accumulator["lineage_root_sha256"]
        ),
        event=event,
        trust_domain=str(common_state_ref["trust_domain"]),
        logical_state_id=str(common_state_ref["logical_state_id"]),
        reconciliation_count=previous_count + 1,
        branch_event_count=(previous_count + 1) * 2,
        previous_accumulator=previous_lineage_accumulator,
    )
    if not validate_lineage_accumulator(
        accumulator,
        event=event,
        previous_accumulator=previous_lineage_accumulator,
        tip_checkpoint=reconciliation_checkpoint,
        tip_witness=reconciliation_witness,
    ):
        return RepeatedForkLineageCompactionAgreement(
            False, "lineage_accumulator_advance_invalid"
        )

    forbidden = {
        primary_branch.provider_id,
        secondary_branch.provider_id,
        primary_branch.authority_id,
        secondary_branch.authority_id,
        primary_branch.branch_provenance_sha256,
        secondary_branch.branch_provenance_sha256,
        primary_vote.vote_provenance_sha256,
        secondary_vote.vote_provenance_sha256,
    }
    portable_objects = (
        *branch_checkpoints,
        *branch_witnesses,
        reconciliation_checkpoint,
        reconciliation_witness,
        event,
        accumulator,
    )
    if forbidden & set(_all_strings(portable_objects)):
        return RepeatedForkLineageCompactionAgreement(
            False, "raw_evidence_dependency"
        )

    previous_bytes = len(canonical_json_bytes(previous_lineage_accumulator))
    current_bytes = len(canonical_json_bytes(accumulator))
    if previous_bytes != current_bytes:
        return RepeatedForkLineageCompactionAgreement(
            False, "lineage_accumulator_size_drift"
        )
    receipt = {
        "schema": COMPACTION_RECEIPT_SCHEMA,
        "verified": True,
        "reason": COMPACTION_REASON,
        "previous_accumulator_sha256": _digest(previous_lineage_accumulator),
        "previous_lineage_root_sha256": previous_lineage_accumulator[
            "lineage_root_sha256"
        ],
        "lineage_event_sha256": _digest(event),
        "lineage_accumulator_sha256": _digest(accumulator),
        "lineage_root_sha256": accumulator["lineage_root_sha256"],
        "reconciliation_count": previous_count + 1,
        "total_branch_event_count": (previous_count + 1) * 2,
        "common_tip_causal_epoch": int(common_state_ref["causal_epoch"]),
        "fork_causal_epoch": int(common_state_ref["causal_epoch"]) + 1,
        "reconciled_causal_epoch": int(common_state_ref["causal_epoch"]) + 2,
        "parent_set_sha256": reconciliation_checkpoint["reconciliation_ref"][
            "parent_set_sha256"
        ],
        "target_semantic_state_sha256": target_semantic_state,
        "reconciliation_ref_sha256": _digest(
            reconciliation_checkpoint["reconciliation_ref"]
        ),
        "reconciliation_checkpoint_sha256": _digest(reconciliation_checkpoint),
        "reconciliation_witness_sha256": _digest(reconciliation_witness),
        "previous_accumulator_bytes": previous_bytes,
        "current_accumulator_bytes": current_bytes,
        "accumulator_field_count": len(accumulator),
        "bounded_accumulator_shape": True,
        "prior_parent_lineages_embedded_in_accumulator": False,
        "raw_provider_evidence_embedded": False,
    }
    return RepeatedForkLineageCompactionAgreement(
        True,
        COMPACTION_REASON,
        branch_checkpoints,
        branch_witnesses,
        reconciliation_checkpoint,
        reconciliation_witness,
        dict(previous_lineage_event),
        dict(previous_lineage_accumulator),
        event,
        accumulator,
        receipt,
    )
