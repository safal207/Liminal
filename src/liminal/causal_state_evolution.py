"""Portable multi-epoch causal-state evolution across independent histories.

The v0.1 downstream portability proof established a history-free convergence anchor.
This layer advances that anchor across multiple causal epochs while keeping raw history
provenance outside portable checkpoint/witness identity.

The key distinction is explicit:
- object validity answers whether one checkpoint/witness has the right shape;
- chain validity answers whether every predecessor link back to the trusted anchor is valid.
"""

from __future__ import annotations

import re
from collections.abc import Mapping, Sequence
from dataclasses import dataclass
from typing import Any

from liminal.downstream_causal_state_portability import HistoricalStateObservation
from liminal.recovery_trust_root_registry import canonical_json_bytes, sha256_hex

STATE_REF_SCHEMA = "liminal-causal-trust-state-ref/v0.1"
ANCHOR_CHECKPOINT_SCHEMA = "liminal-causal-trust-checkpoint/v0.1"
ANCHOR_WITNESS_SCHEMA = "liminal-causal-trust-witness/v0.1"
AUTHORITY_SCHEMA = "liminal-causal-state-authority/v0.1"

CAUSAL_TRANSITION_REF_SCHEMA = "liminal-causal-transition-ref/v0.1"
EVOLUTION_CHECKPOINT_SCHEMA = "liminal-causal-trust-checkpoint/v0.2"
EVOLUTION_WITNESS_SCHEMA = "liminal-causal-trust-witness/v0.2"
EVOLUTION_RECEIPT_SCHEMA = "liminal-portable-causal-state-evolution-receipt/v0.1"
EVOLUTION_REASON = "portable_causal_state_evolution_verified"

CHECKPOINT_ROLE = "checkpoint-producer"
WITNESS_ROLE = "witness-producer"
_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")


@dataclass(frozen=True)
class HistoricalTransitionObservation:
    """One verified, history-bound transition observation.

    Raw registry/manifest/provenance identities are retained for audit and continuity,
    but are deliberately excluded from portable transition/checkpoint/witness objects.
    """

    verified: bool
    provider_id: str
    genesis_authority_id: str
    from_history_generation: int
    to_history_generation: int
    from_registry_sha256: str
    to_registry_sha256: str
    from_manifest_sha256: str
    to_manifest_sha256: str
    transition_provenance_sha256: str
    trust_domain: str
    logical_transition_id: str
    transition_contract_sha256: str
    authorization_contract_sha256: str
    from_semantic_state_sha256: str
    to_semantic_state_sha256: str


@dataclass(frozen=True)
class PortableCausalEvolutionAgreement:
    verified: bool
    reason: str
    checkpoints: tuple[dict[str, Any], ...] = ()
    witnesses: tuple[dict[str, Any], ...] = ()
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


def _valid_historical_observation(value: object) -> bool:
    if not isinstance(value, HistoricalStateObservation):
        return False
    return (
        isinstance(value.verified, bool)
        and value.verified
        and isinstance(value.provider_id, str)
        and bool(value.provider_id)
        and isinstance(value.genesis_authority_id, str)
        and bool(value.genesis_authority_id)
        and isinstance(value.history_generation, int)
        and not isinstance(value.history_generation, bool)
        and value.history_generation >= 0
        and _valid_sha256(value.registry_sha256)
        and _valid_sha256(value.manifest_sha256)
        and _valid_sha256(value.semantic_state_sha256)
        and isinstance(value.trust_domain, str)
        and bool(value.trust_domain)
    )


def _valid_transition_observation(value: object) -> bool:
    if not isinstance(value, HistoricalTransitionObservation):
        return False
    generations_valid = (
        isinstance(value.from_history_generation, int)
        and not isinstance(value.from_history_generation, bool)
        and isinstance(value.to_history_generation, int)
        and not isinstance(value.to_history_generation, bool)
        and 0 <= value.from_history_generation < value.to_history_generation
    )
    digests = (
        value.from_registry_sha256,
        value.to_registry_sha256,
        value.from_manifest_sha256,
        value.to_manifest_sha256,
        value.transition_provenance_sha256,
        value.transition_contract_sha256,
        value.authorization_contract_sha256,
        value.from_semantic_state_sha256,
        value.to_semantic_state_sha256,
    )
    return (
        isinstance(value.verified, bool)
        and value.verified
        and isinstance(value.provider_id, str)
        and bool(value.provider_id)
        and isinstance(value.genesis_authority_id, str)
        and bool(value.genesis_authority_id)
        and generations_valid
        and all(_valid_sha256(item) for item in digests)
        and isinstance(value.trust_domain, str)
        and bool(value.trust_domain)
        and isinstance(value.logical_transition_id, str)
        and bool(value.logical_transition_id)
        and value.from_semantic_state_sha256 != value.to_semantic_state_sha256
    )


def validate_anchor_checkpoint(value: object) -> bool:
    if not isinstance(value, Mapping) or set(value) != {
        "schema_version",
        "state_ref",
        "previous_checkpoint_sha256",
        "checkpoint_authority",
    }:
        return False
    state_ref = value.get("state_ref")
    return (
        value.get("schema_version") == ANCHOR_CHECKPOINT_SCHEMA
        and _valid_state_ref(state_ref)
        and isinstance(state_ref, Mapping)
        and state_ref.get("causal_epoch") == 0
        and value.get("previous_checkpoint_sha256") is None
        and _valid_authority_payload(
            value.get("checkpoint_authority"),
            expected_role=CHECKPOINT_ROLE,
        )
    )


def validate_anchor_witness(value: object, anchor_checkpoint: object) -> bool:
    if not validate_anchor_checkpoint(anchor_checkpoint):
        return False
    if not isinstance(value, Mapping) or set(value) != {
        "schema_version",
        "state_ref",
        "checkpoint_sha256",
        "previous_witness_sha256",
        "witness_authority",
    }:
        return False
    assert isinstance(anchor_checkpoint, Mapping)
    return (
        value.get("schema_version") == ANCHOR_WITNESS_SCHEMA
        and value.get("state_ref") == anchor_checkpoint.get("state_ref")
        and value.get("checkpoint_sha256") == _digest(anchor_checkpoint)
        and value.get("previous_witness_sha256") is None
        and _valid_authority_payload(
            value.get("witness_authority"),
            expected_role=WITNESS_ROLE,
        )
    )


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


def build_causal_transition_ref(
    transition: HistoricalTransitionObservation,
    *,
    previous_state_ref: Mapping[str, Any],
    next_state_ref: Mapping[str, Any],
) -> dict[str, Any]:
    if not _valid_transition_observation(transition):
        raise ValueError("historical_transition_invalid")
    if not _valid_state_ref(previous_state_ref) or not _valid_state_ref(next_state_ref):
        raise ValueError("causal_state_ref_invalid")
    if transition.trust_domain != previous_state_ref["trust_domain"]:
        raise ValueError("transition_trust_domain_mismatch")
    if next_state_ref["trust_domain"] != previous_state_ref["trust_domain"]:
        raise ValueError("state_ref_trust_domain_mismatch")
    if next_state_ref["logical_state_id"] != previous_state_ref["logical_state_id"]:
        raise ValueError("state_ref_logical_state_mismatch")
    if next_state_ref["causal_epoch"] != previous_state_ref["causal_epoch"] + 1:
        raise ValueError("causal_epoch_gap")
    if (
        transition.from_semantic_state_sha256
        != previous_state_ref["semantic_state_sha256"]
    ):
        raise ValueError("transition_from_semantic_state_mismatch")
    if transition.to_semantic_state_sha256 != next_state_ref["semantic_state_sha256"]:
        raise ValueError("transition_to_semantic_state_mismatch")
    return {
        "schema": CAUSAL_TRANSITION_REF_SCHEMA,
        "trust_domain": transition.trust_domain,
        "logical_state_id": previous_state_ref["logical_state_id"],
        "logical_transition_id": transition.logical_transition_id,
        "from_causal_epoch": previous_state_ref["causal_epoch"],
        "to_causal_epoch": next_state_ref["causal_epoch"],
        "from_state_ref_sha256": _digest(previous_state_ref),
        "to_state_ref_sha256": _digest(next_state_ref),
        "transition_contract_sha256": transition.transition_contract_sha256,
        "authorization_contract_sha256": transition.authorization_contract_sha256,
    }


def _valid_transition_ref(
    value: object,
    *,
    previous_state_ref: Mapping[str, Any],
    next_state_ref: Mapping[str, Any],
) -> bool:
    if not isinstance(value, Mapping) or set(value) != {
        "schema",
        "trust_domain",
        "logical_state_id",
        "logical_transition_id",
        "from_causal_epoch",
        "to_causal_epoch",
        "from_state_ref_sha256",
        "to_state_ref_sha256",
        "transition_contract_sha256",
        "authorization_contract_sha256",
    }:
        return False
    return (
        value.get("schema") == CAUSAL_TRANSITION_REF_SCHEMA
        and value.get("trust_domain") == previous_state_ref["trust_domain"]
        and value.get("trust_domain") == next_state_ref["trust_domain"]
        and value.get("logical_state_id") == previous_state_ref["logical_state_id"]
        and value.get("logical_state_id") == next_state_ref["logical_state_id"]
        and isinstance(value.get("logical_transition_id"), str)
        and bool(value.get("logical_transition_id"))
        and value.get("from_causal_epoch") == previous_state_ref["causal_epoch"]
        and value.get("to_causal_epoch") == next_state_ref["causal_epoch"]
        and value.get("to_causal_epoch") == value.get("from_causal_epoch") + 1
        and value.get("from_state_ref_sha256") == _digest(previous_state_ref)
        and value.get("to_state_ref_sha256") == _digest(next_state_ref)
        and _valid_sha256(value.get("transition_contract_sha256"))
        and _valid_sha256(value.get("authorization_contract_sha256"))
    )


def build_evolution_checkpoint(
    previous_checkpoint: Mapping[str, Any],
    transition: HistoricalTransitionObservation,
) -> dict[str, Any]:
    previous_state_ref = previous_checkpoint.get("state_ref")
    if not _valid_state_ref(previous_state_ref):
        raise ValueError("previous_checkpoint_state_ref_invalid")
    assert isinstance(previous_state_ref, Mapping)
    next_state_ref = _next_state_ref(
        previous_state_ref,
        semantic_state_sha256=transition.to_semantic_state_sha256,
    )
    transition_ref = build_causal_transition_ref(
        transition,
        previous_state_ref=previous_state_ref,
        next_state_ref=next_state_ref,
    )
    checkpoint = {
        "schema_version": EVOLUTION_CHECKPOINT_SCHEMA,
        "state_ref": next_state_ref,
        "transition_ref": transition_ref,
        "previous_checkpoint_sha256": _digest(previous_checkpoint),
        "checkpoint_authority": previous_checkpoint.get("checkpoint_authority"),
    }
    if not validate_evolution_checkpoint(checkpoint, previous_checkpoint):
        raise ValueError("evolution_checkpoint_invalid")
    return checkpoint


def validate_evolution_checkpoint(
    value: object,
    previous_checkpoint: object,
) -> bool:
    if not isinstance(previous_checkpoint, Mapping):
        return False
    if not isinstance(value, Mapping) or set(value) != {
        "schema_version",
        "state_ref",
        "transition_ref",
        "previous_checkpoint_sha256",
        "checkpoint_authority",
    }:
        return False
    previous_state_ref = previous_checkpoint.get("state_ref")
    next_state_ref = value.get("state_ref")
    if not _valid_state_ref(previous_state_ref) or not _valid_state_ref(next_state_ref):
        return False
    assert isinstance(previous_state_ref, Mapping)
    assert isinstance(next_state_ref, Mapping)
    if value.get("schema_version") != EVOLUTION_CHECKPOINT_SCHEMA:
        return False
    if value.get("checkpoint_authority") != previous_checkpoint.get(
        "checkpoint_authority"
    ):
        return False
    if not _valid_authority_payload(
        value.get("checkpoint_authority"),
        expected_role=CHECKPOINT_ROLE,
    ):
        return False
    if value.get("previous_checkpoint_sha256") != _digest(previous_checkpoint):
        return False
    if next_state_ref["causal_epoch"] != previous_state_ref["causal_epoch"] + 1:
        return False
    return _valid_transition_ref(
        value.get("transition_ref"),
        previous_state_ref=previous_state_ref,
        next_state_ref=next_state_ref,
    )


def validate_evolution_checkpoint_chain(
    anchor_checkpoint: object,
    checkpoints: Sequence[object],
) -> bool:
    """Validate every checkpoint link back to the trusted epoch-0 anchor."""

    if not validate_anchor_checkpoint(anchor_checkpoint):
        return False
    previous: object = anchor_checkpoint
    for checkpoint in checkpoints:
        if not validate_evolution_checkpoint(checkpoint, previous):
            return False
        previous = checkpoint
    return True


def build_evolution_witness(
    previous_witness: Mapping[str, Any],
    checkpoint: Mapping[str, Any],
) -> dict[str, Any]:
    if not _valid_authority_payload(
        previous_witness.get("witness_authority"),
        expected_role=WITNESS_ROLE,
    ):
        raise ValueError("previous_witness_authority_invalid")
    transition_ref = checkpoint.get("transition_ref")
    if not isinstance(transition_ref, Mapping):
        raise ValueError("checkpoint_transition_ref_invalid")
    witness = {
        "schema_version": EVOLUTION_WITNESS_SCHEMA,
        "state_ref": checkpoint.get("state_ref"),
        "transition_ref_sha256": _digest(transition_ref),
        "checkpoint_sha256": _digest(checkpoint),
        "previous_witness_sha256": _digest(previous_witness),
        "witness_authority": previous_witness.get("witness_authority"),
    }
    if not validate_evolution_witness(witness, checkpoint, previous_witness):
        raise ValueError("evolution_witness_invalid")
    return witness


def validate_evolution_witness(
    value: object,
    checkpoint: object,
    previous_witness: object,
) -> bool:
    if not isinstance(checkpoint, Mapping) or not isinstance(previous_witness, Mapping):
        return False
    if not isinstance(value, Mapping) or set(value) != {
        "schema_version",
        "state_ref",
        "transition_ref_sha256",
        "checkpoint_sha256",
        "previous_witness_sha256",
        "witness_authority",
    }:
        return False
    transition_ref = checkpoint.get("transition_ref")
    if not isinstance(transition_ref, Mapping):
        return False
    return (
        value.get("schema_version") == EVOLUTION_WITNESS_SCHEMA
        and value.get("state_ref") == checkpoint.get("state_ref")
        and value.get("transition_ref_sha256") == _digest(transition_ref)
        and value.get("checkpoint_sha256") == _digest(checkpoint)
        and value.get("previous_witness_sha256") == _digest(previous_witness)
        and value.get("witness_authority") == previous_witness.get("witness_authority")
        and _valid_authority_payload(
            value.get("witness_authority"),
            expected_role=WITNESS_ROLE,
        )
    )


def validate_evolution_witness_chain(
    anchor_witness: object,
    anchor_checkpoint: object,
    checkpoints: Sequence[object],
    witnesses: Sequence[object],
) -> bool:
    """Validate the witness prefix together with its full checkpoint prefix."""

    if len(checkpoints) != len(witnesses):
        return False
    if not validate_anchor_witness(anchor_witness, anchor_checkpoint):
        return False
    if not validate_evolution_checkpoint_chain(anchor_checkpoint, checkpoints):
        return False
    previous: object = anchor_witness
    for checkpoint, witness in zip(checkpoints, witnesses):
        if not validate_evolution_witness(witness, checkpoint, previous):
            return False
        previous = witness
    return True


def _transition_starts_from(
    transition: HistoricalTransitionObservation,
    observation: HistoricalStateObservation,
) -> bool:
    return (
        transition.provider_id == observation.provider_id
        and transition.genesis_authority_id == observation.genesis_authority_id
        and transition.from_history_generation == observation.history_generation
        and transition.from_registry_sha256 == observation.registry_sha256
        and transition.from_manifest_sha256 == observation.manifest_sha256
        and transition.from_semantic_state_sha256
        == observation.semantic_state_sha256
        and transition.trust_domain == observation.trust_domain
    )


def _transition_continues(
    previous: HistoricalTransitionObservation,
    current: HistoricalTransitionObservation,
) -> bool:
    return (
        current.provider_id == previous.provider_id
        and current.genesis_authority_id == previous.genesis_authority_id
        and current.from_history_generation == previous.to_history_generation
        and current.from_registry_sha256 == previous.to_registry_sha256
        and current.from_manifest_sha256 == previous.to_manifest_sha256
        and current.from_semantic_state_sha256 == previous.to_semantic_state_sha256
        and current.trust_domain == previous.trust_domain
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


def compare_multi_epoch_causal_evolution(
    primary_anchor: HistoricalStateObservation,
    secondary_anchor: HistoricalStateObservation,
    *,
    anchor_checkpoint: Mapping[str, Any],
    anchor_witness: Mapping[str, Any],
    primary_transitions: Sequence[HistoricalTransitionObservation],
    secondary_transitions: Sequence[HistoricalTransitionObservation],
) -> PortableCausalEvolutionAgreement:
    """Prove equal portable causal chains over distinct historical transitions."""

    if not _valid_historical_observation(primary_anchor) or not _valid_historical_observation(
        secondary_anchor
    ):
        return PortableCausalEvolutionAgreement(False, "historical_anchor_invalid")
    if not validate_anchor_checkpoint(anchor_checkpoint) or not validate_anchor_witness(
        anchor_witness, anchor_checkpoint
    ):
        return PortableCausalEvolutionAgreement(False, "portable_anchor_invalid")
    if not primary_transitions or len(primary_transitions) != len(secondary_transitions):
        return PortableCausalEvolutionAgreement(False, "transition_sequence_length_mismatch")

    anchor_ref = anchor_checkpoint["state_ref"]
    assert isinstance(anchor_ref, Mapping)
    if (
        primary_anchor.semantic_state_sha256 != anchor_ref["semantic_state_sha256"]
        or secondary_anchor.semantic_state_sha256 != anchor_ref["semantic_state_sha256"]
    ):
        return PortableCausalEvolutionAgreement(False, "anchor_semantic_state_mismatch")
    if (
        primary_anchor.trust_domain != anchor_ref["trust_domain"]
        or secondary_anchor.trust_domain != anchor_ref["trust_domain"]
    ):
        return PortableCausalEvolutionAgreement(False, "anchor_trust_domain_mismatch")
    if primary_anchor.provider_id == secondary_anchor.provider_id:
        return PortableCausalEvolutionAgreement(False, "history_provider_not_independent")
    if primary_anchor.genesis_authority_id == secondary_anchor.genesis_authority_id:
        return PortableCausalEvolutionAgreement(False, "genesis_authority_not_independent")
    if primary_anchor.registry_sha256 == secondary_anchor.registry_sha256:
        return PortableCausalEvolutionAgreement(False, "history_registry_not_independent")
    if primary_anchor.manifest_sha256 == secondary_anchor.manifest_sha256:
        return PortableCausalEvolutionAgreement(False, "history_manifest_not_independent")

    primary_checkpoints: list[dict[str, Any]] = []
    secondary_checkpoints: list[dict[str, Any]] = []
    primary_witnesses: list[dict[str, Any]] = []
    secondary_witnesses: list[dict[str, Any]] = []
    step_receipts: list[dict[str, Any]] = []

    previous_primary_checkpoint: Mapping[str, Any] = anchor_checkpoint
    previous_secondary_checkpoint: Mapping[str, Any] = anchor_checkpoint
    previous_primary_witness: Mapping[str, Any] = anchor_witness
    previous_secondary_witness: Mapping[str, Any] = anchor_witness

    previous_primary_transition: HistoricalTransitionObservation | None = None
    previous_secondary_transition: HistoricalTransitionObservation | None = None

    forbidden = {
        primary_anchor.provider_id,
        secondary_anchor.provider_id,
        primary_anchor.genesis_authority_id,
        secondary_anchor.genesis_authority_id,
        primary_anchor.registry_sha256,
        secondary_anchor.registry_sha256,
        primary_anchor.manifest_sha256,
        secondary_anchor.manifest_sha256,
    }

    for index, (primary, secondary) in enumerate(
        zip(primary_transitions, secondary_transitions),
        start=1,
    ):
        if not _valid_transition_observation(primary) or not _valid_transition_observation(
            secondary
        ):
            return PortableCausalEvolutionAgreement(False, "historical_transition_invalid")
        if previous_primary_transition is None:
            if not _transition_starts_from(primary, primary_anchor) or not _transition_starts_from(
                secondary, secondary_anchor
            ):
                return PortableCausalEvolutionAgreement(False, "transition_anchor_prefix_mismatch")
        else:
            assert previous_secondary_transition is not None
            if not _transition_continues(
                previous_primary_transition, primary
            ) or not _transition_continues(previous_secondary_transition, secondary):
                return PortableCausalEvolutionAgreement(False, "historical_transition_prefix_mismatch")

        if primary.provider_id == secondary.provider_id:
            return PortableCausalEvolutionAgreement(False, "transition_provider_not_independent")
        if primary.genesis_authority_id == secondary.genesis_authority_id:
            return PortableCausalEvolutionAgreement(False, "transition_genesis_not_independent")
        if primary.transition_provenance_sha256 == secondary.transition_provenance_sha256:
            return PortableCausalEvolutionAgreement(False, "transition_provenance_not_independent")
        if primary.to_registry_sha256 == secondary.to_registry_sha256:
            return PortableCausalEvolutionAgreement(False, "transition_registry_not_independent")
        if primary.to_manifest_sha256 == secondary.to_manifest_sha256:
            return PortableCausalEvolutionAgreement(False, "transition_manifest_not_independent")
        if primary.trust_domain != secondary.trust_domain:
            return PortableCausalEvolutionAgreement(False, "transition_trust_domain_mismatch")
        if (
            primary.from_semantic_state_sha256
            != secondary.from_semantic_state_sha256
        ):
            return PortableCausalEvolutionAgreement(False, "transition_from_state_mismatch")
        if primary.to_semantic_state_sha256 != secondary.to_semantic_state_sha256:
            return PortableCausalEvolutionAgreement(False, "transition_to_state_mismatch")
        if primary.logical_transition_id != secondary.logical_transition_id:
            return PortableCausalEvolutionAgreement(False, "logical_transition_mismatch")
        if primary.transition_contract_sha256 != secondary.transition_contract_sha256:
            return PortableCausalEvolutionAgreement(False, "transition_contract_mismatch")
        if (
            primary.authorization_contract_sha256
            != secondary.authorization_contract_sha256
        ):
            return PortableCausalEvolutionAgreement(False, "transition_authorization_mismatch")

        primary_checkpoint = build_evolution_checkpoint(
            previous_primary_checkpoint, primary
        )
        secondary_checkpoint = build_evolution_checkpoint(
            previous_secondary_checkpoint, secondary
        )
        if canonical_json_bytes(primary_checkpoint) != canonical_json_bytes(
            secondary_checkpoint
        ):
            return PortableCausalEvolutionAgreement(False, "evolution_checkpoint_mismatch")

        primary_witness = build_evolution_witness(
            previous_primary_witness, primary_checkpoint
        )
        secondary_witness = build_evolution_witness(
            previous_secondary_witness, secondary_checkpoint
        )
        if canonical_json_bytes(primary_witness) != canonical_json_bytes(
            secondary_witness
        ):
            return PortableCausalEvolutionAgreement(False, "evolution_witness_mismatch")

        step_forbidden = {
            primary.provider_id,
            secondary.provider_id,
            primary.genesis_authority_id,
            secondary.genesis_authority_id,
            primary.from_registry_sha256,
            secondary.from_registry_sha256,
            primary.to_registry_sha256,
            secondary.to_registry_sha256,
            primary.from_manifest_sha256,
            secondary.from_manifest_sha256,
            primary.to_manifest_sha256,
            secondary.to_manifest_sha256,
            primary.transition_provenance_sha256,
            secondary.transition_provenance_sha256,
        }
        forbidden.update(step_forbidden)
        portable_strings = set(_all_strings(primary_checkpoint)) | set(
            _all_strings(primary_witness)
        )
        if forbidden & portable_strings:
            return PortableCausalEvolutionAgreement(False, "raw_history_dependency")

        primary_checkpoints.append(primary_checkpoint)
        secondary_checkpoints.append(secondary_checkpoint)
        primary_witnesses.append(primary_witness)
        secondary_witnesses.append(secondary_witness)
        step_receipts.append(
            {
                "causal_epoch": anchor_ref["causal_epoch"] + index,
                "logical_transition_id": primary.logical_transition_id,
                "primary_history_generation_from": primary.from_history_generation,
                "primary_history_generation_to": primary.to_history_generation,
                "secondary_history_generation_from": secondary.from_history_generation,
                "secondary_history_generation_to": secondary.to_history_generation,
                "primary_transition_provenance_sha256": primary.transition_provenance_sha256,
                "secondary_transition_provenance_sha256": secondary.transition_provenance_sha256,
                "semantic_state_sha256": primary.to_semantic_state_sha256,
                "checkpoint_sha256": _digest(primary_checkpoint),
                "witness_sha256": _digest(primary_witness),
            }
        )

        previous_primary_checkpoint = primary_checkpoint
        previous_secondary_checkpoint = secondary_checkpoint
        previous_primary_witness = primary_witness
        previous_secondary_witness = secondary_witness
        previous_primary_transition = primary
        previous_secondary_transition = secondary

    if not validate_evolution_checkpoint_chain(anchor_checkpoint, primary_checkpoints):
        return PortableCausalEvolutionAgreement(False, "checkpoint_chain_invalid")
    if not validate_evolution_witness_chain(
        anchor_witness,
        anchor_checkpoint,
        primary_checkpoints,
        primary_witnesses,
    ):
        return PortableCausalEvolutionAgreement(False, "witness_chain_invalid")

    receipt = {
        "schema": EVOLUTION_RECEIPT_SCHEMA,
        "verified": True,
        "reason": EVOLUTION_REASON,
        "anchor_state_ref_sha256": _digest(anchor_ref),
        "anchor_checkpoint_sha256": _digest(anchor_checkpoint),
        "anchor_witness_sha256": _digest(anchor_witness),
        "epochs_advanced": len(primary_checkpoints),
        "final_causal_epoch": primary_checkpoints[-1]["state_ref"]["causal_epoch"],
        "final_semantic_state_sha256": primary_checkpoints[-1]["state_ref"][
            "semantic_state_sha256"
        ],
        "final_checkpoint_sha256": _digest(primary_checkpoints[-1]),
        "final_witness_sha256": _digest(primary_witnesses[-1]),
        "raw_history_embedded": False,
        "equivalent_checkpoint_chain": True,
        "equivalent_witness_chain": True,
        "steps": step_receipts,
    }
    return PortableCausalEvolutionAgreement(
        True,
        EVOLUTION_REASON,
        tuple(primary_checkpoints),
        tuple(primary_witnesses),
        receipt,
    )
