"""Provider-neutral upstream rotation-authority portability.

This module compares independently verified rotation-authority observations. It does not
perform cryptographic verification and it never grants authority from provider labels.
Concrete producer/control-plane identities are audit metadata; portable identity is the
causal rotation claim and its contracts.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Any

from liminal.recovery_trust_root_registry import canonical_json_bytes, sha256_hex

ROTATION_RECEIPT_SCHEMA = "liminal-rotation-authority-receipt/v0.1"
ROTATION_AGREEMENT_REASON = "verified_rotation_authority_claims_agree"
_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")


@dataclass(frozen=True)
class RotationAuthorityObservation:
    verified: bool
    rotation_producer_provider: str
    rotation_producer_instance_id: str
    control_plane_provider: str
    control_plane_id: str
    logical_rotation_id: str
    rotation_contract_sha256: str
    authorization_contract_sha256: str
    previous_registry_sha256: str
    current_registry_sha256: str
    previous_manifest_sha256: str
    current_manifest_sha256: str
    from_generation: int
    to_generation: int
    rotation_reason: str


@dataclass(frozen=True)
class RotationAuthorityAgreement:
    verified: bool
    reason: str
    receipt: dict[str, Any] | None = None


def _valid_sha256(value: object) -> bool:
    return isinstance(value, str) and _SHA256_RE.fullmatch(value) is not None


def validate_rotation_observation(value: object) -> bool:
    if not isinstance(value, RotationAuthorityObservation):
        return False
    if not isinstance(value.verified, bool):
        return False
    for field in (
        value.rotation_producer_provider,
        value.rotation_producer_instance_id,
        value.control_plane_provider,
        value.control_plane_id,
        value.logical_rotation_id,
        value.rotation_reason,
    ):
        if not isinstance(field, str) or not field:
            return False
    for digest in (
        value.rotation_contract_sha256,
        value.authorization_contract_sha256,
        value.previous_registry_sha256,
        value.current_registry_sha256,
        value.previous_manifest_sha256,
        value.current_manifest_sha256,
    ):
        if not _valid_sha256(digest):
            return False
    if not isinstance(value.from_generation, int) or isinstance(value.from_generation, bool):
        return False
    if not isinstance(value.to_generation, int) or isinstance(value.to_generation, bool):
        return False
    if value.from_generation < 0 or value.to_generation != value.from_generation + 1:
        return False
    return value.rotation_reason == "registry_rotation_authorized"


def rotation_authority_receipt(observation: RotationAuthorityObservation) -> dict[str, Any]:
    if not validate_rotation_observation(observation) or not observation.verified:
        raise ValueError("rotation_observation_not_verified")
    return {
        "schema": ROTATION_RECEIPT_SCHEMA,
        "verified": True,
        "logical_rotation_id": observation.logical_rotation_id,
        "rotation_contract_sha256": observation.rotation_contract_sha256,
        "authorization_contract_sha256": observation.authorization_contract_sha256,
        "previous_registry_sha256": observation.previous_registry_sha256,
        "current_registry_sha256": observation.current_registry_sha256,
        "previous_manifest_sha256": observation.previous_manifest_sha256,
        "current_manifest_sha256": observation.current_manifest_sha256,
        "from_generation": observation.from_generation,
        "to_generation": observation.to_generation,
        "rotation_reason": observation.rotation_reason,
    }


def rotation_authority_receipt_sha256(observation: RotationAuthorityObservation) -> str:
    return sha256_hex(canonical_json_bytes(rotation_authority_receipt(observation)))


def compare_rotation_authority(
    primary: RotationAuthorityObservation,
    secondary: RotationAuthorityObservation,
) -> RotationAuthorityAgreement:
    """Require independent source/control paths and exact portable rotation semantics."""

    if not validate_rotation_observation(primary) or not validate_rotation_observation(secondary):
        return RotationAuthorityAgreement(False, "rotation_observation_invalid")
    if not primary.verified or not secondary.verified:
        return RotationAuthorityAgreement(False, "rotation_observation_unverified")
    if primary.rotation_producer_provider == secondary.rotation_producer_provider:
        return RotationAuthorityAgreement(False, "rotation_producer_provider_not_independent")
    if primary.control_plane_provider == secondary.control_plane_provider:
        return RotationAuthorityAgreement(False, "rotation_control_plane_not_independent")

    portable_fields = (
        "logical_rotation_id",
        "rotation_contract_sha256",
        "authorization_contract_sha256",
        "previous_registry_sha256",
        "current_registry_sha256",
        "previous_manifest_sha256",
        "current_manifest_sha256",
        "from_generation",
        "to_generation",
        "rotation_reason",
    )
    for field in portable_fields:
        if getattr(primary, field) != getattr(secondary, field):
            return RotationAuthorityAgreement(False, f"rotation_{field}_mismatch")

    primary_receipt = rotation_authority_receipt(primary)
    secondary_receipt = rotation_authority_receipt(secondary)
    if canonical_json_bytes(primary_receipt) != canonical_json_bytes(secondary_receipt):
        return RotationAuthorityAgreement(False, "rotation_receipt_mismatch")
    return RotationAuthorityAgreement(True, ROTATION_AGREEMENT_REASON, primary_receipt)
