"""Downstream Causal-State Portability v0.1.

This layer separates downstream causal identity from concrete historical provenance.
A checkpoint should identify the authorized semantic trust state, not the registry,
manifest, provider, or generation path that happened to establish that state.
"""

from __future__ import annotations

import re
from collections.abc import Mapping
from dataclasses import dataclass
from typing import Any

from liminal.historical_trust_base_portability import (
    HistoricalTrustPath,
    trust_state_digest,
    validate_historical_path,
)
from liminal.recovery_trust_root_registry import canonical_json_bytes, sha256_hex

CAUSAL_STATE_REF_SCHEMA = "liminal-causal-trust-state-ref/v0.1"
CAUSAL_CHECKPOINT_SCHEMA = "liminal-causal-trust-checkpoint/v0.1"
CAUSAL_WITNESS_SCHEMA = "liminal-causal-trust-witness/v0.1"
CAUSAL_AUTHORITY_SCHEMA = "liminal-causal-state-authority/v0.1"
PORTABILITY_RECEIPT_SCHEMA = "liminal-downstream-causal-state-portability-receipt/v0.1"
PORTABILITY_REASON = "downstream_causal_state_portability_verified"
CHECKPOINT_ROLE = "checkpoint-producer"
WITNESS_ROLE = "witness-producer"
_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")


@dataclass(frozen=True)
class HistoricalStateObservation:
    """History-bound provenance plus its verified terminal semantic state."""

    verified: bool
    provider_id: str
    genesis_authority_id: str
    history_generation: int
    registry_sha256: str
    manifest_sha256: str
    semantic_state_sha256: str
    trust_domain: str


@dataclass(frozen=True)
class CausalAuthority:
    """Portable authority for a downstream checkpoint or witness role."""

    role: str
    logical_authority_id: str
    producer_contract_sha256: str
    authorization_contract_sha256: str


@dataclass(frozen=True)
class DownstreamCausalStateAgreement:
    verified: bool
    reason: str
    checkpoint: dict[str, Any] | None = None
    witness: dict[str, Any] | None = None
    receipt: dict[str, Any] | None = None


def _valid_sha256(value: object) -> bool:
    return isinstance(value, str) and _SHA256_RE.fullmatch(value) is not None


def _valid_observation(value: object) -> bool:
    if not isinstance(value, HistoricalStateObservation):
        return False
    return (
        isinstance(value.verified, bool)
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


def _valid_authority(value: object, *, expected_role: str | None = None) -> bool:
    if not isinstance(value, CausalAuthority):
        return False
    if value.role not in {CHECKPOINT_ROLE, WITNESS_ROLE}:
        return False
    if expected_role is not None and value.role != expected_role:
        return False
    return (
        isinstance(value.logical_authority_id, str)
        and bool(value.logical_authority_id)
        and _valid_sha256(value.producer_contract_sha256)
        and _valid_sha256(value.authorization_contract_sha256)
    )


def _authority_payload(value: CausalAuthority) -> dict[str, str]:
    if not _valid_authority(value):
        raise ValueError("causal_authority_invalid")
    return {
        "schema": CAUSAL_AUTHORITY_SCHEMA,
        "role": value.role,
        "logical_authority_id": value.logical_authority_id,
        "producer_contract_sha256": value.producer_contract_sha256,
        "authorization_contract_sha256": value.authorization_contract_sha256,
    }


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
        value.get("schema") == CAUSAL_AUTHORITY_SCHEMA
        and value.get("role") == expected_role
        and isinstance(value.get("logical_authority_id"), str)
        and bool(value.get("logical_authority_id"))
        and _valid_sha256(value.get("producer_contract_sha256"))
        and _valid_sha256(value.get("authorization_contract_sha256"))
    )


def observe_historical_state(
    path: HistoricalTrustPath,
    *,
    provider_id: str,
) -> HistoricalStateObservation:
    """Derive provenance and semantic state without promoting provenance to identity."""

    if not validate_historical_path(path):
        raise ValueError("historical_path_invalid")
    if not provider_id:
        raise ValueError("provider_id_invalid")
    history = path.registry["history"]
    active = history[-1]
    return HistoricalStateObservation(
        verified=path.verified,
        provider_id=provider_id,
        genesis_authority_id=path.genesis_authority_id,
        history_generation=int(path.registry["active_generation"]),
        registry_sha256=sha256_hex(canonical_json_bytes(path.registry)),
        manifest_sha256=str(active["manifest_sha256"]),
        semantic_state_sha256=trust_state_digest(path),
        trust_domain=path.controls.trust_domain,
    )


def causal_state_ref(
    observation: HistoricalStateObservation,
    *,
    logical_state_id: str,
    causal_epoch: int,
) -> dict[str, Any]:
    """Return history-free identity for one downstream causal trust state."""

    if not _valid_observation(observation):
        raise ValueError("historical_observation_invalid")
    if not observation.verified:
        raise ValueError("historical_observation_unverified")
    if not logical_state_id:
        raise ValueError("logical_state_id_invalid")
    if not isinstance(causal_epoch, int) or isinstance(causal_epoch, bool):
        raise ValueError("causal_epoch_invalid")
    if causal_epoch < 0:
        raise ValueError("causal_epoch_invalid")
    return {
        "schema": CAUSAL_STATE_REF_SCHEMA,
        "trust_domain": observation.trust_domain,
        "logical_state_id": logical_state_id,
        "causal_epoch": causal_epoch,
        "semantic_state_sha256": observation.semantic_state_sha256,
    }


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
        value.get("schema") == CAUSAL_STATE_REF_SCHEMA
        and isinstance(value.get("trust_domain"), str)
        and bool(value.get("trust_domain"))
        and isinstance(value.get("logical_state_id"), str)
        and bool(value.get("logical_state_id"))
        and isinstance(epoch, int)
        and not isinstance(epoch, bool)
        and epoch >= 0
        and _valid_sha256(value.get("semantic_state_sha256"))
    )


def causal_state_ref_sha256(value: object) -> str:
    if not _valid_state_ref(value):
        raise ValueError("causal_state_ref_invalid")
    return sha256_hex(canonical_json_bytes(value))


def build_causal_checkpoint(
    observation: HistoricalStateObservation,
    *,
    logical_state_id: str,
    causal_epoch: int,
    authority: CausalAuthority,
    previous_checkpoint: object | None = None,
) -> dict[str, Any]:
    if not _valid_authority(authority, expected_role=CHECKPOINT_ROLE):
        raise ValueError("causal_checkpoint_authority_invalid")
    state_ref = causal_state_ref(
        observation,
        logical_state_id=logical_state_id,
        causal_epoch=causal_epoch,
    )
    previous_digest = (
        None
        if previous_checkpoint is None
        else sha256_hex(canonical_json_bytes(previous_checkpoint))
    )
    checkpoint = {
        "schema_version": CAUSAL_CHECKPOINT_SCHEMA,
        "state_ref": state_ref,
        "previous_checkpoint_sha256": previous_digest,
        "checkpoint_authority": _authority_payload(authority),
    }
    if not validate_causal_checkpoint(checkpoint, previous_checkpoint):
        raise ValueError("causal_checkpoint_invalid")
    return checkpoint


def validate_causal_checkpoint(
    payload: object,
    previous_checkpoint: object | None = None,
) -> bool:
    if not isinstance(payload, Mapping) or set(payload) != {
        "schema_version",
        "state_ref",
        "previous_checkpoint_sha256",
        "checkpoint_authority",
    }:
        return False
    if payload.get("schema_version") != CAUSAL_CHECKPOINT_SCHEMA:
        return False
    state_ref = payload.get("state_ref")
    if not _valid_state_ref(state_ref):
        return False
    if not _valid_authority_payload(
        payload.get("checkpoint_authority"),
        expected_role=CHECKPOINT_ROLE,
    ):
        return False
    assert isinstance(state_ref, Mapping)
    epoch = state_ref["causal_epoch"]
    previous_digest = payload.get("previous_checkpoint_sha256")
    if epoch == 0:
        return previous_checkpoint is None and previous_digest is None
    if not validate_causal_checkpoint(previous_checkpoint):
        return False
    assert isinstance(previous_checkpoint, Mapping)
    previous_ref = previous_checkpoint["state_ref"]
    assert isinstance(previous_ref, Mapping)
    if previous_ref["causal_epoch"] != epoch - 1:
        return False
    if previous_ref["trust_domain"] != state_ref["trust_domain"]:
        return False
    if previous_ref["logical_state_id"] != state_ref["logical_state_id"]:
        return False
    if previous_checkpoint["checkpoint_authority"] != payload["checkpoint_authority"]:
        return False
    return previous_digest == sha256_hex(canonical_json_bytes(previous_checkpoint))


def causal_checkpoint_sha256(
    value: object,
    previous_checkpoint: object | None = None,
) -> str:
    if not validate_causal_checkpoint(value, previous_checkpoint):
        raise ValueError("causal_checkpoint_invalid")
    return sha256_hex(canonical_json_bytes(value))


def build_causal_witness(
    checkpoint: object,
    *,
    authority: CausalAuthority,
    previous_witness: object | None = None,
) -> dict[str, Any]:
    if not validate_causal_checkpoint(checkpoint):
        raise ValueError("causal_checkpoint_invalid")
    if not _valid_authority(authority, expected_role=WITNESS_ROLE):
        raise ValueError("causal_witness_authority_invalid")
    assert isinstance(checkpoint, Mapping)
    previous_digest = (
        None
        if previous_witness is None
        else sha256_hex(canonical_json_bytes(previous_witness))
    )
    witness = {
        "schema_version": CAUSAL_WITNESS_SCHEMA,
        "state_ref": checkpoint["state_ref"],
        "checkpoint_sha256": sha256_hex(canonical_json_bytes(checkpoint)),
        "previous_witness_sha256": previous_digest,
        "witness_authority": _authority_payload(authority),
    }
    if not validate_causal_witness(witness, checkpoint, previous_witness):
        raise ValueError("causal_witness_invalid")
    return witness


def _valid_witness_body(payload: object) -> bool:
    if not isinstance(payload, Mapping) or set(payload) != {
        "schema_version",
        "state_ref",
        "checkpoint_sha256",
        "previous_witness_sha256",
        "witness_authority",
    }:
        return False
    if payload.get("schema_version") != CAUSAL_WITNESS_SCHEMA:
        return False
    if not _valid_state_ref(payload.get("state_ref")):
        return False
    if not _valid_sha256(payload.get("checkpoint_sha256")):
        return False
    previous = payload.get("previous_witness_sha256")
    if previous is not None and not _valid_sha256(previous):
        return False
    return _valid_authority_payload(
        payload.get("witness_authority"),
        expected_role=WITNESS_ROLE,
    )


def validate_causal_witness(
    payload: object,
    checkpoint: object,
    previous_witness: object | None = None,
) -> bool:
    if not _valid_witness_body(payload):
        return False
    if not validate_causal_checkpoint(checkpoint):
        return False
    assert isinstance(payload, Mapping)
    assert isinstance(checkpoint, Mapping)
    if payload["state_ref"] != checkpoint["state_ref"]:
        return False
    if payload["checkpoint_sha256"] != sha256_hex(canonical_json_bytes(checkpoint)):
        return False
    state_ref = payload["state_ref"]
    assert isinstance(state_ref, Mapping)
    epoch = state_ref["causal_epoch"]
    previous_digest = payload.get("previous_witness_sha256")
    if epoch == 0:
        return previous_witness is None and previous_digest is None
    if not _valid_witness_body(previous_witness):
        return False
    assert isinstance(previous_witness, Mapping)
    previous_ref = previous_witness["state_ref"]
    assert isinstance(previous_ref, Mapping)
    if previous_ref["causal_epoch"] != epoch - 1:
        return False
    if previous_ref["trust_domain"] != state_ref["trust_domain"]:
        return False
    if previous_ref["logical_state_id"] != state_ref["logical_state_id"]:
        return False
    if previous_witness["witness_authority"] != payload["witness_authority"]:
        return False
    return previous_digest == sha256_hex(canonical_json_bytes(previous_witness))


def causal_witness_sha256(
    value: object,
    checkpoint: object,
    previous_witness: object | None = None,
) -> str:
    if not validate_causal_witness(value, checkpoint, previous_witness):
        raise ValueError("causal_witness_invalid")
    return sha256_hex(canonical_json_bytes(value))


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


def compare_downstream_causal_states(
    primary: HistoricalStateObservation,
    secondary: HistoricalStateObservation,
    *,
    logical_state_id: str,
    causal_epoch: int,
    checkpoint_authority: CausalAuthority,
    witness_authority: CausalAuthority,
) -> DownstreamCausalStateAgreement:
    """Prove identical downstream causal objects from distinct history provenance."""

    if not _valid_observation(primary) or not _valid_observation(secondary):
        return DownstreamCausalStateAgreement(False, "historical_observation_invalid")
    if not primary.verified or not secondary.verified:
        return DownstreamCausalStateAgreement(False, "historical_observation_unverified")
    if not logical_state_id:
        return DownstreamCausalStateAgreement(False, "logical_state_id_invalid")
    if not isinstance(causal_epoch, int) or isinstance(causal_epoch, bool):
        return DownstreamCausalStateAgreement(False, "causal_epoch_invalid")
    if causal_epoch < 0:
        return DownstreamCausalStateAgreement(False, "causal_epoch_invalid")
    if not _valid_authority(checkpoint_authority, expected_role=CHECKPOINT_ROLE):
        return DownstreamCausalStateAgreement(False, "causal_checkpoint_authority_invalid")
    if not _valid_authority(witness_authority, expected_role=WITNESS_ROLE):
        return DownstreamCausalStateAgreement(False, "causal_witness_authority_invalid")
    if primary.provider_id == secondary.provider_id:
        return DownstreamCausalStateAgreement(False, "history_provider_not_independent")
    if primary.genesis_authority_id == secondary.genesis_authority_id:
        return DownstreamCausalStateAgreement(False, "genesis_authority_not_independent")
    if primary.registry_sha256 == secondary.registry_sha256:
        return DownstreamCausalStateAgreement(False, "history_registry_not_independent")
    if primary.manifest_sha256 == secondary.manifest_sha256:
        return DownstreamCausalStateAgreement(False, "history_manifest_not_independent")
    if primary.trust_domain != secondary.trust_domain:
        return DownstreamCausalStateAgreement(False, "trust_domain_mismatch")
    if primary.semantic_state_sha256 != secondary.semantic_state_sha256:
        return DownstreamCausalStateAgreement(False, "terminal_semantic_state_mismatch")

    primary_checkpoint = build_causal_checkpoint(
        primary,
        logical_state_id=logical_state_id,
        causal_epoch=causal_epoch,
        authority=checkpoint_authority,
    )
    secondary_checkpoint = build_causal_checkpoint(
        secondary,
        logical_state_id=logical_state_id,
        causal_epoch=causal_epoch,
        authority=checkpoint_authority,
    )
    if canonical_json_bytes(primary_checkpoint) != canonical_json_bytes(secondary_checkpoint):
        return DownstreamCausalStateAgreement(False, "downstream_checkpoint_mismatch")

    primary_witness = build_causal_witness(
        primary_checkpoint,
        authority=witness_authority,
    )
    secondary_witness = build_causal_witness(
        secondary_checkpoint,
        authority=witness_authority,
    )
    if canonical_json_bytes(primary_witness) != canonical_json_bytes(secondary_witness):
        return DownstreamCausalStateAgreement(False, "downstream_witness_mismatch")

    forbidden = {
        primary.provider_id,
        secondary.provider_id,
        primary.genesis_authority_id,
        secondary.genesis_authority_id,
        primary.registry_sha256,
        secondary.registry_sha256,
        primary.manifest_sha256,
        secondary.manifest_sha256,
    }
    portable_strings = set(_all_strings(primary_checkpoint)) | set(_all_strings(primary_witness))
    if forbidden & portable_strings:
        return DownstreamCausalStateAgreement(False, "raw_history_dependency")

    state_ref = primary_checkpoint["state_ref"]
    checkpoint_digest = sha256_hex(canonical_json_bytes(primary_checkpoint))
    witness_digest = sha256_hex(canonical_json_bytes(primary_witness))
    receipt = {
        "schema": PORTABILITY_RECEIPT_SCHEMA,
        "verified": True,
        "reason": PORTABILITY_REASON,
        "logical_state_id": logical_state_id,
        "causal_epoch": causal_epoch,
        "primary_history_generation": primary.history_generation,
        "secondary_history_generation": secondary.history_generation,
        "primary_registry_sha256": primary.registry_sha256,
        "secondary_registry_sha256": secondary.registry_sha256,
        "primary_manifest_sha256": primary.manifest_sha256,
        "secondary_manifest_sha256": secondary.manifest_sha256,
        "semantic_state_sha256": primary.semantic_state_sha256,
        "state_ref_sha256": causal_state_ref_sha256(state_ref),
        "checkpoint_sha256": checkpoint_digest,
        "witness_sha256": witness_digest,
        "raw_history_embedded": False,
        "equivalent_downstream_checkpoint": True,
        "equivalent_downstream_witness": True,
    }
    return DownstreamCausalStateAgreement(
        True,
        PORTABILITY_REASON,
        primary_checkpoint,
        primary_witness,
        receipt,
    )
