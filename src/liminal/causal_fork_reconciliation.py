"""Portable causal fork and reconciliation across independent evidence providers.

A reconciliation is a DAG join, not a relaxed linear transition.  The portable
reconciliation checkpoint commits both divergent branch tips and their common
ancestor while provider-specific evidence remains outside portable identity.
"""

from __future__ import annotations

import re
from collections.abc import Mapping, Sequence
from dataclasses import dataclass
from typing import Any

from liminal.causal_state_evolution import (
    validate_anchor_checkpoint,
    validate_anchor_witness,
    validate_evolution_checkpoint_chain,
    validate_evolution_witness_chain,
)
from liminal.recovery_trust_root_registry import canonical_json_bytes, sha256_hex

STATE_REF_SCHEMA = "liminal-causal-trust-state-ref/v0.1"
AUTHORITY_SCHEMA = "liminal-causal-state-authority/v0.1"
FORK_BRANCH_REF_SCHEMA = "liminal-causal-fork-branch-ref/v0.1"
FORK_BRANCH_CHECKPOINT_SCHEMA = "liminal-causal-fork-branch-checkpoint/v0.1"
FORK_BRANCH_WITNESS_SCHEMA = "liminal-causal-fork-branch-witness/v0.1"
RECONCILIATION_REF_SCHEMA = "liminal-causal-reconciliation-ref/v0.1"
RECONCILIATION_CHECKPOINT_SCHEMA = (
    "liminal-causal-trust-reconciliation-checkpoint/v0.1"
)
RECONCILIATION_WITNESS_SCHEMA = "liminal-causal-trust-reconciliation-witness/v0.1"
RECONCILIATION_RECEIPT_SCHEMA = (
    "liminal-causal-fork-reconciliation-portability-receipt/v0.1"
)
RECONCILIATION_REASON = "causal_fork_reconciliation_portability_verified"
CHECKPOINT_ROLE = "checkpoint-producer"
WITNESS_ROLE = "witness-producer"
_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")


@dataclass(frozen=True)
class PortableForkBranchObservation:
    """Provider-bound evidence authorizing one portable branch tip."""

    verified: bool
    provider_id: str
    authority_id: str
    branch_provenance_sha256: str
    trust_domain: str
    logical_branch_id: str
    branch_contract_sha256: str
    authorization_contract_sha256: str
    from_state_ref_sha256: str
    to_semantic_state_sha256: str


@dataclass(frozen=True)
class PortableReconciliationVote:
    """Provider-bound authorization to reconcile one exact branch tip."""

    verified: bool
    provider_id: str
    authority_id: str
    vote_provenance_sha256: str
    trust_domain: str
    logical_reconciliation_id: str
    branch_ref_sha256: str
    branch_state_ref_sha256: str
    branch_checkpoint_sha256: str
    branch_witness_sha256: str
    target_semantic_state_sha256: str
    reconciliation_contract_sha256: str
    authorization_contract_sha256: str


@dataclass(frozen=True)
class CausalForkReconciliationAgreement:
    verified: bool
    reason: str
    branch_checkpoints: tuple[dict[str, Any], ...] = ()
    branch_witnesses: tuple[dict[str, Any], ...] = ()
    reconciliation_checkpoint: dict[str, Any] | None = None
    reconciliation_witness: dict[str, Any] | None = None
    receipt: dict[str, Any] | None = None


def _valid_sha256(value: object) -> bool:
    return isinstance(value, str) and _SHA256_RE.fullmatch(value) is not None


def _digest(value: object) -> str:
    return sha256_hex(canonical_json_bytes(value))


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


def _valid_authority_payload(value: object, *, expected_role: str) -> bool:
    if not isinstance(value, Mapping) or set(value) != {
        "schema",
        "role",
        "logical_authority_id",
        "producer_contract_sha256",
        "authorization_contract_sha256",
    }:
        return False
    return (
        value.get("schema") == AUTHORITY_SCHEMA
        and value.get("role") == expected_role
        and isinstance(value.get("logical_authority_id"), str)
        and bool(value.get("logical_authority_id"))
        and _valid_sha256(value.get("producer_contract_sha256"))
        and _valid_sha256(value.get("authorization_contract_sha256"))
    )


def _valid_branch_observation(value: object) -> bool:
    if not isinstance(value, PortableForkBranchObservation):
        return False
    return (
        isinstance(value.verified, bool)
        and value.verified
        and isinstance(value.provider_id, str)
        and bool(value.provider_id)
        and isinstance(value.authority_id, str)
        and bool(value.authority_id)
        and _valid_sha256(value.branch_provenance_sha256)
        and isinstance(value.trust_domain, str)
        and bool(value.trust_domain)
        and isinstance(value.logical_branch_id, str)
        and bool(value.logical_branch_id)
        and _valid_sha256(value.branch_contract_sha256)
        and _valid_sha256(value.authorization_contract_sha256)
        and _valid_sha256(value.from_state_ref_sha256)
        and _valid_sha256(value.to_semantic_state_sha256)
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


def _common_tip(
    anchor_checkpoint: Mapping[str, Any],
    anchor_witness: Mapping[str, Any],
    common_checkpoints: Sequence[Mapping[str, Any]],
    common_witnesses: Sequence[Mapping[str, Any]],
) -> tuple[Mapping[str, Any], Mapping[str, Any]] | None:
    if not validate_anchor_checkpoint(anchor_checkpoint):
        return None
    if not validate_anchor_witness(anchor_witness, anchor_checkpoint):
        return None
    if len(common_checkpoints) != len(common_witnesses):
        return None
    if not validate_evolution_checkpoint_chain(anchor_checkpoint, common_checkpoints):
        return None
    if not validate_evolution_witness_chain(
        anchor_witness,
        anchor_checkpoint,
        common_checkpoints,
        common_witnesses,
    ):
        return None
    if not common_checkpoints:
        return anchor_checkpoint, anchor_witness
    return common_checkpoints[-1], common_witnesses[-1]


def _next_state_ref(
    previous_state_ref: Mapping[str, Any],
    *,
    semantic_state_sha256: str,
) -> dict[str, Any]:
    if not _valid_state_ref(previous_state_ref):
        raise ValueError("previous_state_ref_invalid")
    if not _valid_sha256(semantic_state_sha256):
        raise ValueError("semantic_state_digest_invalid")
    return {
        "schema": STATE_REF_SCHEMA,
        "trust_domain": previous_state_ref["trust_domain"],
        "logical_state_id": previous_state_ref["logical_state_id"],
        "causal_epoch": int(previous_state_ref["causal_epoch"]) + 1,
        "semantic_state_sha256": semantic_state_sha256,
    }


def build_fork_branch_ref(
    observation: PortableForkBranchObservation,
    *,
    common_state_ref: Mapping[str, Any],
    branch_state_ref: Mapping[str, Any],
) -> dict[str, Any]:
    if not _valid_branch_observation(observation):
        raise ValueError("fork_branch_observation_invalid")
    if not _valid_state_ref(common_state_ref) or not _valid_state_ref(branch_state_ref):
        raise ValueError("causal_state_ref_invalid")
    if observation.trust_domain != common_state_ref["trust_domain"]:
        raise ValueError("fork_branch_trust_domain_mismatch")
    if observation.from_state_ref_sha256 != _digest(common_state_ref):
        raise ValueError("fork_branch_common_state_mismatch")
    if branch_state_ref["trust_domain"] != common_state_ref["trust_domain"]:
        raise ValueError("fork_branch_state_domain_mismatch")
    if branch_state_ref["logical_state_id"] != common_state_ref["logical_state_id"]:
        raise ValueError("fork_branch_logical_state_mismatch")
    if branch_state_ref["causal_epoch"] != common_state_ref["causal_epoch"] + 1:
        raise ValueError("fork_branch_epoch_gap")
    if branch_state_ref["semantic_state_sha256"] != observation.to_semantic_state_sha256:
        raise ValueError("fork_branch_semantic_state_mismatch")
    return {
        "schema": FORK_BRANCH_REF_SCHEMA,
        "trust_domain": observation.trust_domain,
        "logical_state_id": common_state_ref["logical_state_id"],
        "logical_branch_id": observation.logical_branch_id,
        "from_causal_epoch": common_state_ref["causal_epoch"],
        "to_causal_epoch": branch_state_ref["causal_epoch"],
        "from_state_ref_sha256": _digest(common_state_ref),
        "to_state_ref_sha256": _digest(branch_state_ref),
        "branch_contract_sha256": observation.branch_contract_sha256,
        "authorization_contract_sha256": observation.authorization_contract_sha256,
    }


def _valid_branch_ref(
    value: object,
    *,
    common_state_ref: Mapping[str, Any],
    branch_state_ref: Mapping[str, Any],
) -> bool:
    if not isinstance(value, Mapping) or set(value) != {
        "schema",
        "trust_domain",
        "logical_state_id",
        "logical_branch_id",
        "from_causal_epoch",
        "to_causal_epoch",
        "from_state_ref_sha256",
        "to_state_ref_sha256",
        "branch_contract_sha256",
        "authorization_contract_sha256",
    }:
        return False
    return (
        value.get("schema") == FORK_BRANCH_REF_SCHEMA
        and value.get("trust_domain") == common_state_ref["trust_domain"]
        and value.get("trust_domain") == branch_state_ref["trust_domain"]
        and value.get("logical_state_id") == common_state_ref["logical_state_id"]
        and value.get("logical_state_id") == branch_state_ref["logical_state_id"]
        and isinstance(value.get("logical_branch_id"), str)
        and bool(value.get("logical_branch_id"))
        and value.get("from_causal_epoch") == common_state_ref["causal_epoch"]
        and value.get("to_causal_epoch") == branch_state_ref["causal_epoch"]
        and value.get("to_causal_epoch") == value.get("from_causal_epoch") + 1
        and value.get("from_state_ref_sha256") == _digest(common_state_ref)
        and value.get("to_state_ref_sha256") == _digest(branch_state_ref)
        and _valid_sha256(value.get("branch_contract_sha256"))
        and _valid_sha256(value.get("authorization_contract_sha256"))
    )


def build_fork_branch_checkpoint(
    common_checkpoint: Mapping[str, Any],
    observation: PortableForkBranchObservation,
) -> dict[str, Any]:
    common_state_ref = common_checkpoint.get("state_ref")
    if not _valid_state_ref(common_state_ref):
        raise ValueError("common_checkpoint_state_ref_invalid")
    assert isinstance(common_state_ref, Mapping)
    branch_state_ref = _next_state_ref(
        common_state_ref,
        semantic_state_sha256=observation.to_semantic_state_sha256,
    )
    branch_ref = build_fork_branch_ref(
        observation,
        common_state_ref=common_state_ref,
        branch_state_ref=branch_state_ref,
    )
    checkpoint = {
        "schema_version": FORK_BRANCH_CHECKPOINT_SCHEMA,
        "state_ref": branch_state_ref,
        "branch_ref": branch_ref,
        "previous_checkpoint_sha256": _digest(common_checkpoint),
        "checkpoint_authority": common_checkpoint.get("checkpoint_authority"),
    }
    if not validate_fork_branch_checkpoint(checkpoint, common_checkpoint):
        raise ValueError("fork_branch_checkpoint_invalid")
    return checkpoint


def validate_fork_branch_checkpoint(
    value: object,
    common_checkpoint: object,
) -> bool:
    if not isinstance(common_checkpoint, Mapping):
        return False
    if not isinstance(value, Mapping) or set(value) != {
        "schema_version",
        "state_ref",
        "branch_ref",
        "previous_checkpoint_sha256",
        "checkpoint_authority",
    }:
        return False
    common_state_ref = common_checkpoint.get("state_ref")
    branch_state_ref = value.get("state_ref")
    if not _valid_state_ref(common_state_ref) or not _valid_state_ref(branch_state_ref):
        return False
    assert isinstance(common_state_ref, Mapping)
    assert isinstance(branch_state_ref, Mapping)
    return (
        value.get("schema_version") == FORK_BRANCH_CHECKPOINT_SCHEMA
        and value.get("previous_checkpoint_sha256") == _digest(common_checkpoint)
        and value.get("checkpoint_authority")
        == common_checkpoint.get("checkpoint_authority")
        and _valid_authority_payload(
            value.get("checkpoint_authority"), expected_role=CHECKPOINT_ROLE
        )
        and _valid_branch_ref(
            value.get("branch_ref"),
            common_state_ref=common_state_ref,
            branch_state_ref=branch_state_ref,
        )
    )


def build_fork_branch_witness(
    common_witness: Mapping[str, Any],
    branch_checkpoint: Mapping[str, Any],
) -> dict[str, Any]:
    branch_ref = branch_checkpoint.get("branch_ref")
    if not isinstance(branch_ref, Mapping):
        raise ValueError("fork_branch_ref_invalid")
    witness = {
        "schema_version": FORK_BRANCH_WITNESS_SCHEMA,
        "state_ref": branch_checkpoint.get("state_ref"),
        "branch_ref_sha256": _digest(branch_ref),
        "checkpoint_sha256": _digest(branch_checkpoint),
        "previous_witness_sha256": _digest(common_witness),
        "witness_authority": common_witness.get("witness_authority"),
    }
    if not validate_fork_branch_witness(
        witness, branch_checkpoint, common_witness
    ):
        raise ValueError("fork_branch_witness_invalid")
    return witness


def validate_fork_branch_witness(
    value: object,
    branch_checkpoint: object,
    common_witness: object,
) -> bool:
    if not isinstance(branch_checkpoint, Mapping) or not isinstance(
        common_witness, Mapping
    ):
        return False
    if not isinstance(value, Mapping) or set(value) != {
        "schema_version",
        "state_ref",
        "branch_ref_sha256",
        "checkpoint_sha256",
        "previous_witness_sha256",
        "witness_authority",
    }:
        return False
    branch_ref = branch_checkpoint.get("branch_ref")
    if not isinstance(branch_ref, Mapping):
        return False
    return (
        value.get("schema_version") == FORK_BRANCH_WITNESS_SCHEMA
        and value.get("state_ref") == branch_checkpoint.get("state_ref")
        and value.get("branch_ref_sha256") == _digest(branch_ref)
        and value.get("checkpoint_sha256") == _digest(branch_checkpoint)
        and value.get("previous_witness_sha256") == _digest(common_witness)
        and value.get("witness_authority")
        == common_witness.get("witness_authority")
        and _valid_authority_payload(
            value.get("witness_authority"), expected_role=WITNESS_ROLE
        )
    )


def build_reconciliation_vote(
    *,
    verified: bool,
    provider_id: str,
    authority_id: str,
    vote_provenance_sha256: str,
    logical_reconciliation_id: str,
    branch_checkpoint: Mapping[str, Any],
    branch_witness: Mapping[str, Any],
    target_semantic_state_sha256: str,
    reconciliation_contract_sha256: str,
    authorization_contract_sha256: str,
) -> PortableReconciliationVote:
    state_ref = branch_checkpoint.get("state_ref")
    branch_ref = branch_checkpoint.get("branch_ref")
    if not _valid_state_ref(state_ref) or not isinstance(branch_ref, Mapping):
        raise ValueError("fork_branch_checkpoint_invalid")
    assert isinstance(state_ref, Mapping)
    return PortableReconciliationVote(
        verified=verified,
        provider_id=provider_id,
        authority_id=authority_id,
        vote_provenance_sha256=vote_provenance_sha256,
        trust_domain=str(state_ref["trust_domain"]),
        logical_reconciliation_id=logical_reconciliation_id,
        branch_ref_sha256=_digest(branch_ref),
        branch_state_ref_sha256=_digest(state_ref),
        branch_checkpoint_sha256=_digest(branch_checkpoint),
        branch_witness_sha256=_digest(branch_witness),
        target_semantic_state_sha256=target_semantic_state_sha256,
        reconciliation_contract_sha256=reconciliation_contract_sha256,
        authorization_contract_sha256=authorization_contract_sha256,
    )


def _vote_matches_branch(
    vote: PortableReconciliationVote,
    observation: PortableForkBranchObservation,
    checkpoint: Mapping[str, Any],
    witness: Mapping[str, Any],
) -> bool:
    state_ref = checkpoint.get("state_ref")
    branch_ref = checkpoint.get("branch_ref")
    return (
        _valid_vote(vote)
        and vote.provider_id == observation.provider_id
        and vote.authority_id == observation.authority_id
        and isinstance(state_ref, Mapping)
        and isinstance(branch_ref, Mapping)
        and vote.trust_domain == state_ref.get("trust_domain")
        and vote.branch_ref_sha256 == _digest(branch_ref)
        and vote.branch_state_ref_sha256 == _digest(state_ref)
        and vote.branch_checkpoint_sha256 == _digest(checkpoint)
        and vote.branch_witness_sha256 == _digest(witness)
    )


def _parent_lineage(
    checkpoint: Mapping[str, Any], witness: Mapping[str, Any]
) -> dict[str, Any]:
    state_ref = checkpoint.get("state_ref")
    branch_ref = checkpoint.get("branch_ref")
    if not isinstance(state_ref, Mapping) or not isinstance(branch_ref, Mapping):
        raise ValueError("fork_branch_object_invalid")
    return {
        "logical_branch_id": branch_ref["logical_branch_id"],
        "semantic_state_sha256": state_ref["semantic_state_sha256"],
        "state_ref_sha256": _digest(state_ref),
        "branch_ref_sha256": _digest(branch_ref),
        "checkpoint_sha256": _digest(checkpoint),
        "witness_sha256": _digest(witness),
    }


def _canonical_branch_pairs(
    branch_pairs: Sequence[
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
    return tuple(sorted(branch_pairs, key=lambda item: _digest(item[2])))


def _canonical_parent_lineages(
    branch_checkpoints: Sequence[Mapping[str, Any]],
    branch_witnesses: Sequence[Mapping[str, Any]],
) -> list[dict[str, Any]]:
    lineages = [
        _parent_lineage(checkpoint, witness)
        for checkpoint, witness in zip(branch_checkpoints, branch_witnesses)
    ]
    return sorted(lineages, key=lambda item: item["checkpoint_sha256"])


def _build_reconciliation_state_ref(
    common_state_ref: Mapping[str, Any],
    branch_state_refs: Sequence[Mapping[str, Any]],
    *,
    target_semantic_state_sha256: str,
) -> dict[str, Any]:
    if len(branch_state_refs) != 2:
        raise ValueError("reconciliation_parent_count_invalid")
    epochs = {item.get("causal_epoch") for item in branch_state_refs}
    domains = {item.get("trust_domain") for item in branch_state_refs}
    logical_ids = {item.get("logical_state_id") for item in branch_state_refs}
    if len(epochs) != 1 or len(domains) != 1 or len(logical_ids) != 1:
        raise ValueError("reconciliation_parent_state_mismatch")
    branch_epoch = next(iter(epochs))
    if branch_epoch != common_state_ref["causal_epoch"] + 1:
        raise ValueError("reconciliation_parent_epoch_invalid")
    if domains != {common_state_ref["trust_domain"]}:
        raise ValueError("reconciliation_parent_domain_invalid")
    if logical_ids != {common_state_ref["logical_state_id"]}:
        raise ValueError("reconciliation_parent_logical_state_invalid")
    return {
        "schema": STATE_REF_SCHEMA,
        "trust_domain": common_state_ref["trust_domain"],
        "logical_state_id": common_state_ref["logical_state_id"],
        "causal_epoch": int(branch_epoch) + 1,
        "semantic_state_sha256": target_semantic_state_sha256,
    }


def build_causal_reconciliation_ref(
    common_checkpoint: Mapping[str, Any],
    common_witness: Mapping[str, Any],
    branch_checkpoints: Sequence[Mapping[str, Any]],
    branch_witnesses: Sequence[Mapping[str, Any]],
    *,
    logical_reconciliation_id: str,
    target_semantic_state_sha256: str,
    reconciliation_contract_sha256: str,
    authorization_contract_sha256: str,
) -> tuple[dict[str, Any], dict[str, Any]]:
    if len(branch_checkpoints) != 2 or len(branch_witnesses) != 2:
        raise ValueError("reconciliation_parent_count_invalid")
    if len({_digest(item) for item in branch_checkpoints}) != 2:
        raise ValueError("reconciliation_duplicate_checkpoint_parent")
    if len({_digest(item) for item in branch_witnesses}) != 2:
        raise ValueError("reconciliation_duplicate_witness_parent")
    common_state_ref = common_checkpoint.get("state_ref")
    if not _valid_state_ref(common_state_ref):
        raise ValueError("reconciliation_common_state_invalid")
    assert isinstance(common_state_ref, Mapping)
    branch_state_refs: list[Mapping[str, Any]] = []
    for checkpoint, witness in zip(branch_checkpoints, branch_witnesses):
        if not validate_fork_branch_checkpoint(checkpoint, common_checkpoint):
            raise ValueError("reconciliation_branch_checkpoint_invalid")
        if not validate_fork_branch_witness(witness, checkpoint, common_witness):
            raise ValueError("reconciliation_branch_witness_invalid")
        state_ref = checkpoint.get("state_ref")
        assert isinstance(state_ref, Mapping)
        branch_state_refs.append(state_ref)
    result_state_ref = _build_reconciliation_state_ref(
        common_state_ref,
        branch_state_refs,
        target_semantic_state_sha256=target_semantic_state_sha256,
    )
    parent_lineages = _canonical_parent_lineages(
        branch_checkpoints, branch_witnesses
    )
    reconciliation_ref = {
        "schema": RECONCILIATION_REF_SCHEMA,
        "trust_domain": common_state_ref["trust_domain"],
        "logical_state_id": common_state_ref["logical_state_id"],
        "logical_reconciliation_id": logical_reconciliation_id,
        "from_causal_epoch": branch_state_refs[0]["causal_epoch"],
        "to_causal_epoch": result_state_ref["causal_epoch"],
        "common_ancestor_state_ref_sha256": _digest(common_state_ref),
        "common_ancestor_checkpoint_sha256": _digest(common_checkpoint),
        "common_ancestor_witness_sha256": _digest(common_witness),
        "parent_lineages": parent_lineages,
        "parent_set_sha256": _digest(parent_lineages),
        "result_state_ref_sha256": _digest(result_state_ref),
        "reconciliation_contract_sha256": reconciliation_contract_sha256,
        "authorization_contract_sha256": authorization_contract_sha256,
    }
    return result_state_ref, reconciliation_ref


def _valid_reconciliation_ref(
    value: object,
    *,
    common_checkpoint: Mapping[str, Any],
    common_witness: Mapping[str, Any],
    branch_checkpoints: Sequence[Mapping[str, Any]],
    branch_witnesses: Sequence[Mapping[str, Any]],
    result_state_ref: Mapping[str, Any],
) -> bool:
    if not isinstance(value, Mapping) or set(value) != {
        "schema",
        "trust_domain",
        "logical_state_id",
        "logical_reconciliation_id",
        "from_causal_epoch",
        "to_causal_epoch",
        "common_ancestor_state_ref_sha256",
        "common_ancestor_checkpoint_sha256",
        "common_ancestor_witness_sha256",
        "parent_lineages",
        "parent_set_sha256",
        "result_state_ref_sha256",
        "reconciliation_contract_sha256",
        "authorization_contract_sha256",
    }:
        return False
    common_state_ref = common_checkpoint.get("state_ref")
    if not _valid_state_ref(common_state_ref) or not _valid_state_ref(result_state_ref):
        return False
    assert isinstance(common_state_ref, Mapping)
    parent_lineages = _canonical_parent_lineages(
        branch_checkpoints, branch_witnesses
    )
    parent_epochs = {
        checkpoint["state_ref"]["causal_epoch"] for checkpoint in branch_checkpoints
    }
    if len(parent_epochs) != 1:
        return False
    parent_epoch = next(iter(parent_epochs))
    return (
        value.get("schema") == RECONCILIATION_REF_SCHEMA
        and value.get("trust_domain") == common_state_ref["trust_domain"]
        and value.get("logical_state_id") == common_state_ref["logical_state_id"]
        and isinstance(value.get("logical_reconciliation_id"), str)
        and bool(value.get("logical_reconciliation_id"))
        and value.get("from_causal_epoch") == parent_epoch
        and value.get("to_causal_epoch") == result_state_ref["causal_epoch"]
        and value.get("to_causal_epoch") == value.get("from_causal_epoch") + 1
        and value.get("common_ancestor_state_ref_sha256")
        == _digest(common_state_ref)
        and value.get("common_ancestor_checkpoint_sha256")
        == _digest(common_checkpoint)
        and value.get("common_ancestor_witness_sha256") == _digest(common_witness)
        and value.get("parent_lineages") == parent_lineages
        and value.get("parent_set_sha256") == _digest(parent_lineages)
        and value.get("result_state_ref_sha256") == _digest(result_state_ref)
        and _valid_sha256(value.get("reconciliation_contract_sha256"))
        and _valid_sha256(value.get("authorization_contract_sha256"))
    )


def build_reconciliation_checkpoint(
    common_checkpoint: Mapping[str, Any],
    common_witness: Mapping[str, Any],
    branch_checkpoints: Sequence[Mapping[str, Any]],
    branch_witnesses: Sequence[Mapping[str, Any]],
    *,
    logical_reconciliation_id: str,
    target_semantic_state_sha256: str,
    reconciliation_contract_sha256: str,
    authorization_contract_sha256: str,
) -> dict[str, Any]:
    state_ref, reconciliation_ref = build_causal_reconciliation_ref(
        common_checkpoint,
        common_witness,
        branch_checkpoints,
        branch_witnesses,
        logical_reconciliation_id=logical_reconciliation_id,
        target_semantic_state_sha256=target_semantic_state_sha256,
        reconciliation_contract_sha256=reconciliation_contract_sha256,
        authorization_contract_sha256=authorization_contract_sha256,
    )
    checkpoint = {
        "schema_version": RECONCILIATION_CHECKPOINT_SCHEMA,
        "state_ref": state_ref,
        "reconciliation_ref": reconciliation_ref,
        "parent_checkpoint_sha256": sorted(
            _digest(item) for item in branch_checkpoints
        ),
        "checkpoint_authority": common_checkpoint.get("checkpoint_authority"),
    }
    if not validate_reconciliation_checkpoint(
        checkpoint,
        common_checkpoint,
        common_witness,
        branch_checkpoints,
        branch_witnesses,
    ):
        raise ValueError("reconciliation_checkpoint_invalid")
    return checkpoint


def validate_reconciliation_checkpoint(
    value: object,
    common_checkpoint: Mapping[str, Any],
    common_witness: Mapping[str, Any],
    branch_checkpoints: Sequence[Mapping[str, Any]],
    branch_witnesses: Sequence[Mapping[str, Any]],
) -> bool:
    if len(branch_checkpoints) != 2 or len(branch_witnesses) != 2:
        return False
    if len({_digest(item) for item in branch_checkpoints}) != 2:
        return False
    if len({_digest(item) for item in branch_witnesses}) != 2:
        return False
    for checkpoint, witness in zip(branch_checkpoints, branch_witnesses):
        if not validate_fork_branch_checkpoint(checkpoint, common_checkpoint):
            return False
        if not validate_fork_branch_witness(witness, checkpoint, common_witness):
            return False
    if not isinstance(value, Mapping) or set(value) != {
        "schema_version",
        "state_ref",
        "reconciliation_ref",
        "parent_checkpoint_sha256",
        "checkpoint_authority",
    }:
        return False
    state_ref = value.get("state_ref")
    if not _valid_state_ref(state_ref):
        return False
    assert isinstance(state_ref, Mapping)
    return (
        value.get("schema_version") == RECONCILIATION_CHECKPOINT_SCHEMA
        and value.get("parent_checkpoint_sha256")
        == sorted(_digest(item) for item in branch_checkpoints)
        and value.get("checkpoint_authority")
        == common_checkpoint.get("checkpoint_authority")
        and _valid_authority_payload(
            value.get("checkpoint_authority"), expected_role=CHECKPOINT_ROLE
        )
        and _valid_reconciliation_ref(
            value.get("reconciliation_ref"),
            common_checkpoint=common_checkpoint,
            common_witness=common_witness,
            branch_checkpoints=branch_checkpoints,
            branch_witnesses=branch_witnesses,
            result_state_ref=state_ref,
        )
    )


def build_reconciliation_witness(
    common_witness: Mapping[str, Any],
    reconciliation_checkpoint: Mapping[str, Any],
    branch_witnesses: Sequence[Mapping[str, Any]],
) -> dict[str, Any]:
    reconciliation_ref = reconciliation_checkpoint.get("reconciliation_ref")
    if not isinstance(reconciliation_ref, Mapping):
        raise ValueError("reconciliation_ref_invalid")
    witness = {
        "schema_version": RECONCILIATION_WITNESS_SCHEMA,
        "state_ref": reconciliation_checkpoint.get("state_ref"),
        "reconciliation_ref_sha256": _digest(reconciliation_ref),
        "checkpoint_sha256": _digest(reconciliation_checkpoint),
        "parent_witness_sha256": sorted(_digest(item) for item in branch_witnesses),
        "witness_authority": common_witness.get("witness_authority"),
    }
    if not validate_reconciliation_witness(
        witness,
        common_witness,
        reconciliation_checkpoint,
        branch_witnesses,
    ):
        raise ValueError("reconciliation_witness_invalid")
    return witness


def validate_reconciliation_witness(
    value: object,
    common_witness: Mapping[str, Any],
    reconciliation_checkpoint: Mapping[str, Any],
    branch_witnesses: Sequence[Mapping[str, Any]],
) -> bool:
    if len(branch_witnesses) != 2 or len({_digest(item) for item in branch_witnesses}) != 2:
        return False
    if not isinstance(value, Mapping) or set(value) != {
        "schema_version",
        "state_ref",
        "reconciliation_ref_sha256",
        "checkpoint_sha256",
        "parent_witness_sha256",
        "witness_authority",
    }:
        return False
    reconciliation_ref = reconciliation_checkpoint.get("reconciliation_ref")
    if not isinstance(reconciliation_ref, Mapping):
        return False
    return (
        value.get("schema_version") == RECONCILIATION_WITNESS_SCHEMA
        and value.get("state_ref") == reconciliation_checkpoint.get("state_ref")
        and value.get("reconciliation_ref_sha256") == _digest(reconciliation_ref)
        and value.get("checkpoint_sha256") == _digest(reconciliation_checkpoint)
        and value.get("parent_witness_sha256")
        == sorted(_digest(item) for item in branch_witnesses)
        and value.get("witness_authority") == common_witness.get("witness_authority")
        and _valid_authority_payload(
            value.get("witness_authority"), expected_role=WITNESS_ROLE
        )
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


def compare_causal_fork_reconciliation(
    *,
    anchor_checkpoint: Mapping[str, Any],
    anchor_witness: Mapping[str, Any],
    common_checkpoints: Sequence[Mapping[str, Any]],
    common_witnesses: Sequence[Mapping[str, Any]],
    primary_branch: PortableForkBranchObservation,
    secondary_branch: PortableForkBranchObservation,
    primary_vote: PortableReconciliationVote,
    secondary_vote: PortableReconciliationVote,
) -> CausalForkReconciliationAgreement:
    """Prove a two-parent portable reconciliation without erasing either lineage."""

    tip = _common_tip(
        anchor_checkpoint,
        anchor_witness,
        common_checkpoints,
        common_witnesses,
    )
    if tip is None:
        return CausalForkReconciliationAgreement(False, "portable_common_prefix_invalid")
    common_checkpoint, common_witness = tip
    common_state_ref = common_checkpoint.get("state_ref")
    if not _valid_state_ref(common_state_ref):
        return CausalForkReconciliationAgreement(False, "portable_common_state_invalid")
    assert isinstance(common_state_ref, Mapping)

    if not _valid_branch_observation(primary_branch) or not _valid_branch_observation(
        secondary_branch
    ):
        return CausalForkReconciliationAgreement(False, "fork_branch_observation_invalid")
    if primary_branch.provider_id == secondary_branch.provider_id:
        return CausalForkReconciliationAgreement(False, "branch_provider_not_independent")
    if primary_branch.authority_id == secondary_branch.authority_id:
        return CausalForkReconciliationAgreement(False, "branch_authority_not_independent")
    if (
        primary_branch.branch_provenance_sha256
        == secondary_branch.branch_provenance_sha256
    ):
        return CausalForkReconciliationAgreement(False, "branch_provenance_not_independent")
    if primary_branch.trust_domain != secondary_branch.trust_domain:
        return CausalForkReconciliationAgreement(False, "branch_trust_domain_mismatch")
    if primary_branch.trust_domain != common_state_ref["trust_domain"]:
        return CausalForkReconciliationAgreement(False, "branch_common_domain_mismatch")
    if primary_branch.logical_branch_id == secondary_branch.logical_branch_id:
        return CausalForkReconciliationAgreement(False, "branch_identity_not_distinct")
    if primary_branch.branch_contract_sha256 != secondary_branch.branch_contract_sha256:
        return CausalForkReconciliationAgreement(False, "branch_contract_mismatch")
    if (
        primary_branch.authorization_contract_sha256
        != secondary_branch.authorization_contract_sha256
    ):
        return CausalForkReconciliationAgreement(False, "branch_authorization_mismatch")
    common_state_digest = _digest(common_state_ref)
    if (
        primary_branch.from_state_ref_sha256 != common_state_digest
        or secondary_branch.from_state_ref_sha256 != common_state_digest
    ):
        return CausalForkReconciliationAgreement(False, "branch_common_state_mismatch")
    if (
        primary_branch.to_semantic_state_sha256
        == secondary_branch.to_semantic_state_sha256
    ):
        return CausalForkReconciliationAgreement(False, "fork_not_semantically_divergent")
    if common_state_ref["semantic_state_sha256"] in {
        primary_branch.to_semantic_state_sha256,
        secondary_branch.to_semantic_state_sha256,
    }:
        return CausalForkReconciliationAgreement(False, "fork_state_not_advanced")

    primary_checkpoint = build_fork_branch_checkpoint(
        common_checkpoint, primary_branch
    )
    secondary_checkpoint = build_fork_branch_checkpoint(
        common_checkpoint, secondary_branch
    )
    primary_witness = build_fork_branch_witness(
        common_witness, primary_checkpoint
    )
    secondary_witness = build_fork_branch_witness(
        common_witness, secondary_checkpoint
    )
    if _digest(primary_checkpoint) == _digest(secondary_checkpoint):
        return CausalForkReconciliationAgreement(False, "fork_checkpoint_not_distinct")
    if _digest(primary_witness) == _digest(secondary_witness):
        return CausalForkReconciliationAgreement(False, "fork_witness_not_distinct")

    if not _vote_matches_branch(
        primary_vote, primary_branch, primary_checkpoint, primary_witness
    ) or not _vote_matches_branch(
        secondary_vote, secondary_branch, secondary_checkpoint, secondary_witness
    ):
        return CausalForkReconciliationAgreement(False, "reconciliation_vote_branch_mismatch")
    if primary_vote.vote_provenance_sha256 == secondary_vote.vote_provenance_sha256:
        return CausalForkReconciliationAgreement(False, "vote_provenance_not_independent")
    if primary_vote.logical_reconciliation_id != secondary_vote.logical_reconciliation_id:
        return CausalForkReconciliationAgreement(False, "logical_reconciliation_mismatch")
    if (
        primary_vote.target_semantic_state_sha256
        != secondary_vote.target_semantic_state_sha256
    ):
        return CausalForkReconciliationAgreement(False, "reconciliation_target_mismatch")
    if (
        primary_vote.reconciliation_contract_sha256
        != secondary_vote.reconciliation_contract_sha256
    ):
        return CausalForkReconciliationAgreement(False, "reconciliation_contract_mismatch")
    if (
        primary_vote.authorization_contract_sha256
        != secondary_vote.authorization_contract_sha256
    ):
        return CausalForkReconciliationAgreement(False, "reconciliation_authorization_mismatch")
    target_semantic_state = primary_vote.target_semantic_state_sha256
    if target_semantic_state in {
        common_state_ref["semantic_state_sha256"],
        primary_branch.to_semantic_state_sha256,
        secondary_branch.to_semantic_state_sha256,
    }:
        return CausalForkReconciliationAgreement(False, "reconciliation_target_not_new")

    canonical_pairs = _canonical_branch_pairs(
        (
            (primary_branch, primary_vote, primary_checkpoint, primary_witness),
            (secondary_branch, secondary_vote, secondary_checkpoint, secondary_witness),
        )
    )
    branch_checkpoints = tuple(item[2] for item in canonical_pairs)
    branch_witnesses = tuple(item[3] for item in canonical_pairs)
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
    )
    if forbidden & set(_all_strings(portable_objects)):
        return CausalForkReconciliationAgreement(False, "raw_evidence_dependency")

    evidence = []
    for branch, vote, checkpoint, witness in canonical_pairs:
        evidence.append(
            {
                "logical_branch_id": branch.logical_branch_id,
                "provider_id": branch.provider_id,
                "authority_id": branch.authority_id,
                "branch_provenance_sha256": branch.branch_provenance_sha256,
                "vote_provenance_sha256": vote.vote_provenance_sha256,
                "branch_checkpoint_sha256": _digest(checkpoint),
                "branch_witness_sha256": _digest(witness),
                "branch_semantic_state_sha256": branch.to_semantic_state_sha256,
            }
        )
    receipt = {
        "schema": RECONCILIATION_RECEIPT_SCHEMA,
        "verified": True,
        "reason": RECONCILIATION_REASON,
        "common_ancestor_state_ref_sha256": _digest(common_state_ref),
        "common_ancestor_checkpoint_sha256": _digest(common_checkpoint),
        "common_ancestor_witness_sha256": _digest(common_witness),
        "fork_causal_epoch": common_state_ref["causal_epoch"] + 1,
        "reconciled_causal_epoch": common_state_ref["causal_epoch"] + 2,
        "branch_evidence": evidence,
        "parent_lineages": reconciliation_checkpoint["reconciliation_ref"][
            "parent_lineages"
        ],
        "parent_set_sha256": reconciliation_checkpoint["reconciliation_ref"][
            "parent_set_sha256"
        ],
        "target_semantic_state_sha256": target_semantic_state,
        "reconciliation_ref_sha256": _digest(
            reconciliation_checkpoint["reconciliation_ref"]
        ),
        "reconciliation_checkpoint_sha256": _digest(reconciliation_checkpoint),
        "reconciliation_witness_sha256": _digest(reconciliation_witness),
        "lineage_parent_count": 2,
        "branch_order_canonical": True,
        "both_lineages_preserved": True,
        "fork_semantics_divergent": True,
        "raw_evidence_embedded": False,
    }
    return CausalForkReconciliationAgreement(
        True,
        RECONCILIATION_REASON,
        branch_checkpoints,
        branch_witnesses,
        reconciliation_checkpoint,
        reconciliation_witness,
        receipt,
    )
