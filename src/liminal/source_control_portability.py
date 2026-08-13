"""Fail-closed portability model across source producers and control planes.

This layer compares already-verified observations from materially distinct evidence
producers and control-plane providers. Concrete producer and control-plane identities
remain audit metadata; the portable receipt binds only the logical producer contract,
authorization contract, evidence identity, and resulting trusted transition.

The module performs no cryptographic verification and never maps an external producer
to a legacy concrete signer identity.
"""

from __future__ import annotations

import hashlib
import json
import re
from dataclasses import dataclass
from enum import Enum

_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")
_RECEIPT_SCHEMA = "liminal-source-control-receipt/v0.1"


class SourceControlAgreementReason(str, Enum):
    VERIFIED_SOURCE_CONTROL_CLAIMS_AGREE = "verified_source_control_claims_agree"
    PRODUCER_PROVIDER_NOT_INDEPENDENT = "producer_provider_not_independent"
    CONTROL_PLANE_NOT_INDEPENDENT = "control_plane_not_independent"
    PRIMARY_VERIFICATION_FAILED = "primary_verification_failed"
    SECONDARY_VERIFICATION_FAILED = "secondary_verification_failed"
    SUBJECT_MISMATCH = "subject_mismatch"
    LOGICAL_PRODUCER_MISMATCH = "logical_producer_mismatch"
    PRODUCER_CONTRACT_MISMATCH = "producer_contract_mismatch"
    AUTHORIZATION_CONTRACT_MISMATCH = "authorization_contract_mismatch"
    EVIDENCE_TYPE_MISMATCH = "evidence_type_mismatch"
    GENERATION_MISMATCH = "generation_mismatch"
    WITNESS_REASON_MISMATCH = "witness_reason_mismatch"
    NEXT_WITNESS_MISMATCH = "next_witness_mismatch"
    PORTABLE_RECEIPT_MISMATCH = "portable_receipt_mismatch"


@dataclass(frozen=True)
class SourceControlObservation:
    """One externally established producer/control-plane observation.

    ``producer_provider`` / ``producer_instance_id`` and
    ``control_plane_provider`` / ``control_plane_id`` are concrete audit metadata.
    They prove infrastructure independence but are deliberately excluded from portable
    receipt identity.
    """

    producer_provider: str
    producer_instance_id: str
    control_plane_provider: str
    control_plane_id: str
    subject_sha256: str
    logical_producer_id: str
    producer_contract_sha256: str
    authorization_contract_sha256: str
    evidence_type: str
    generation: int
    witness_reason: str
    next_witness_sha256: str
    verified: bool


@dataclass(frozen=True)
class PortableSourceControlReceipt:
    """Provider-neutral producer/control-plane semantics for one transition."""

    subject_sha256: str
    logical_producer_id: str
    producer_contract_sha256: str
    authorization_contract_sha256: str
    evidence_type: str
    generation: int
    witness_reason: str
    next_witness_sha256: str
    verified: bool
    schema: str = _RECEIPT_SCHEMA


@dataclass(frozen=True)
class SourceControlAgreement:
    agreed: bool
    reason: SourceControlAgreementReason
    portable_receipt_sha256: str | None = None


def _require_non_empty(value: str, *, field: str) -> None:
    if not value:
        raise ValueError(f"{field}_must_be_non_empty")


def _validate_sha256(value: str, *, field: str) -> None:
    if not _SHA256_RE.fullmatch(value):
        raise ValueError(f"{field}_must_be_lowercase_sha256")


def validate_source_control_observation(observation: SourceControlObservation) -> None:
    _require_non_empty(observation.producer_provider, field="producer_provider")
    _require_non_empty(observation.producer_instance_id, field="producer_instance_id")
    _require_non_empty(observation.control_plane_provider, field="control_plane_provider")
    _require_non_empty(observation.control_plane_id, field="control_plane_id")
    _validate_sha256(observation.subject_sha256, field="subject_sha256")
    _require_non_empty(observation.logical_producer_id, field="logical_producer_id")
    _validate_sha256(observation.producer_contract_sha256, field="producer_contract_sha256")
    _validate_sha256(
        observation.authorization_contract_sha256,
        field="authorization_contract_sha256",
    )
    _require_non_empty(observation.evidence_type, field="evidence_type")
    if (
        not isinstance(observation.generation, int)
        or isinstance(observation.generation, bool)
        or observation.generation < 0
    ):
        raise ValueError("generation_must_be_non_negative_integer")
    _require_non_empty(observation.witness_reason, field="witness_reason")
    _validate_sha256(observation.next_witness_sha256, field="next_witness_sha256")
    if not isinstance(observation.verified, bool):
        raise ValueError("verified_must_be_boolean")


def validate_portable_source_control_receipt(receipt: PortableSourceControlReceipt) -> None:
    if receipt.schema != _RECEIPT_SCHEMA:
        raise ValueError("unsupported_portable_source_control_receipt_schema")
    _validate_sha256(receipt.subject_sha256, field="subject_sha256")
    _require_non_empty(receipt.logical_producer_id, field="logical_producer_id")
    _validate_sha256(receipt.producer_contract_sha256, field="producer_contract_sha256")
    _validate_sha256(
        receipt.authorization_contract_sha256,
        field="authorization_contract_sha256",
    )
    _require_non_empty(receipt.evidence_type, field="evidence_type")
    if (
        not isinstance(receipt.generation, int)
        or isinstance(receipt.generation, bool)
        or receipt.generation < 0
    ):
        raise ValueError("generation_must_be_non_negative_integer")
    _require_non_empty(receipt.witness_reason, field="witness_reason")
    _validate_sha256(receipt.next_witness_sha256, field="next_witness_sha256")
    if not isinstance(receipt.verified, bool):
        raise ValueError("verified_must_be_boolean")


def portable_source_control_receipt_from_observation(
    observation: SourceControlObservation,
) -> PortableSourceControlReceipt:
    """Project concrete producer/control-plane metadata into portable semantics."""

    validate_source_control_observation(observation)
    receipt = PortableSourceControlReceipt(
        subject_sha256=observation.subject_sha256,
        logical_producer_id=observation.logical_producer_id,
        producer_contract_sha256=observation.producer_contract_sha256,
        authorization_contract_sha256=observation.authorization_contract_sha256,
        evidence_type=observation.evidence_type,
        generation=observation.generation,
        witness_reason=observation.witness_reason,
        next_witness_sha256=observation.next_witness_sha256,
        verified=observation.verified,
    )
    validate_portable_source_control_receipt(receipt)
    return receipt


def canonical_portable_source_control_receipt_bytes(
    receipt: PortableSourceControlReceipt,
) -> bytes:
    """Serialize portable source/control semantics deterministically."""

    validate_portable_source_control_receipt(receipt)
    payload = {
        "schema": receipt.schema,
        "subject_sha256": receipt.subject_sha256,
        "logical_producer_id": receipt.logical_producer_id,
        "producer_contract_sha256": receipt.producer_contract_sha256,
        "authorization_contract_sha256": receipt.authorization_contract_sha256,
        "evidence_type": receipt.evidence_type,
        "generation": receipt.generation,
        "witness_reason": receipt.witness_reason,
        "next_witness_sha256": receipt.next_witness_sha256,
        "verified": receipt.verified,
    }
    return (json.dumps(payload, sort_keys=True, separators=(",", ":")) + "\n").encode()


def portable_source_control_receipt_sha256(receipt: PortableSourceControlReceipt) -> str:
    return hashlib.sha256(canonical_portable_source_control_receipt_bytes(receipt)).hexdigest()


def compare_independent_source_control_paths(
    primary: SourceControlObservation,
    secondary: SourceControlObservation,
) -> SourceControlAgreement:
    """Require exact portable semantics across distinct producers/control planes."""

    validate_source_control_observation(primary)
    validate_source_control_observation(secondary)

    if primary.producer_provider == secondary.producer_provider:
        return SourceControlAgreement(
            agreed=False,
            reason=SourceControlAgreementReason.PRODUCER_PROVIDER_NOT_INDEPENDENT,
        )
    if primary.control_plane_provider == secondary.control_plane_provider:
        return SourceControlAgreement(
            agreed=False,
            reason=SourceControlAgreementReason.CONTROL_PLANE_NOT_INDEPENDENT,
        )
    if not primary.verified:
        return SourceControlAgreement(
            agreed=False,
            reason=SourceControlAgreementReason.PRIMARY_VERIFICATION_FAILED,
        )
    if not secondary.verified:
        return SourceControlAgreement(
            agreed=False,
            reason=SourceControlAgreementReason.SECONDARY_VERIFICATION_FAILED,
        )

    comparisons = (
        (
            primary.subject_sha256 == secondary.subject_sha256,
            SourceControlAgreementReason.SUBJECT_MISMATCH,
        ),
        (
            primary.logical_producer_id == secondary.logical_producer_id,
            SourceControlAgreementReason.LOGICAL_PRODUCER_MISMATCH,
        ),
        (
            primary.producer_contract_sha256 == secondary.producer_contract_sha256,
            SourceControlAgreementReason.PRODUCER_CONTRACT_MISMATCH,
        ),
        (
            primary.authorization_contract_sha256
            == secondary.authorization_contract_sha256,
            SourceControlAgreementReason.AUTHORIZATION_CONTRACT_MISMATCH,
        ),
        (
            primary.evidence_type == secondary.evidence_type,
            SourceControlAgreementReason.EVIDENCE_TYPE_MISMATCH,
        ),
        (
            primary.generation == secondary.generation,
            SourceControlAgreementReason.GENERATION_MISMATCH,
        ),
        (
            primary.witness_reason == secondary.witness_reason,
            SourceControlAgreementReason.WITNESS_REASON_MISMATCH,
        ),
        (
            primary.next_witness_sha256 == secondary.next_witness_sha256,
            SourceControlAgreementReason.NEXT_WITNESS_MISMATCH,
        ),
    )
    for matches, reason in comparisons:
        if not matches:
            return SourceControlAgreement(agreed=False, reason=reason)

    primary_receipt = portable_source_control_receipt_from_observation(primary)
    secondary_receipt = portable_source_control_receipt_from_observation(secondary)
    primary_sha = portable_source_control_receipt_sha256(primary_receipt)
    secondary_sha = portable_source_control_receipt_sha256(secondary_receipt)
    if primary_sha != secondary_sha:
        return SourceControlAgreement(
            agreed=False,
            reason=SourceControlAgreementReason.PORTABLE_RECEIPT_MISMATCH,
        )

    return SourceControlAgreement(
        agreed=True,
        reason=SourceControlAgreementReason.VERIFIED_SOURCE_CONTROL_CLAIMS_AGREE,
        portable_receipt_sha256=primary_sha,
    )
