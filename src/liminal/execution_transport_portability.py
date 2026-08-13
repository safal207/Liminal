"""Fail-closed portability across execution and evidence-transport providers.

This layer compares already-established observations from materially distinct execution
and transport paths. It performs no remote execution, transport integrity verification,
or cryptographic verification itself. Provider/environment metadata remains audit data;
portable identity is the security semantics that must survive those boundary changes.
"""

from __future__ import annotations

import hashlib
import json
import re
from dataclasses import dataclass
from enum import Enum

_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")
_RECEIPT_SCHEMA = "liminal-execution-transport-receipt/v0.1"


class ExecutionTransportAgreementReason(str, Enum):
    VERIFIED_EXECUTION_TRANSPORT_CLAIMS_AGREE = (
        "verified_execution_transport_claims_agree"
    )
    EXECUTION_PROVIDER_NOT_INDEPENDENT = "execution_provider_not_independent"
    TRANSPORT_PROVIDER_NOT_INDEPENDENT = "transport_provider_not_independent"
    PRIMARY_VERIFICATION_FAILED = "primary_verification_failed"
    SECONDARY_VERIFICATION_FAILED = "secondary_verification_failed"
    SUBJECT_MISMATCH = "subject_mismatch"
    TRUST_RECEIPT_MISMATCH = "trust_receipt_mismatch"
    AUTHORIZATION_POLICY_MISMATCH = "authorization_policy_mismatch"
    WITNESS_REASON_MISMATCH = "witness_reason_mismatch"
    NEXT_WITNESS_MISMATCH = "next_witness_mismatch"
    PORTABLE_RECEIPT_MISMATCH = "portable_receipt_mismatch"


@dataclass(frozen=True)
class ExecutionTransportObservation:
    """One externally established execution + transport path observation."""

    execution_provider: str
    execution_environment_id: str
    transport_provider: str
    transport_channel_id: str
    subject_sha256: str
    portable_trust_receipt_sha256: str
    authorization_policy_sha256: str
    witness_reason: str
    next_witness_sha256: str
    verified: bool


@dataclass(frozen=True)
class PortableExecutionTransportReceipt:
    """Provider-neutral transition semantics that must survive boundary changes."""

    subject_sha256: str
    portable_trust_receipt_sha256: str
    authorization_policy_sha256: str
    witness_reason: str
    next_witness_sha256: str
    verified: bool
    schema: str = _RECEIPT_SCHEMA


@dataclass(frozen=True)
class ExecutionTransportAgreement:
    agreed: bool
    reason: ExecutionTransportAgreementReason
    portable_receipt_sha256: str | None = None


def _require_non_empty(value: str, *, field: str) -> None:
    if not value:
        raise ValueError(f"{field}_must_be_non_empty")


def _validate_sha256(value: str, *, field: str) -> None:
    if not _SHA256_RE.fullmatch(value):
        raise ValueError(f"{field}_must_be_lowercase_sha256")


def validate_execution_transport_observation(
    observation: ExecutionTransportObservation,
) -> None:
    _require_non_empty(observation.execution_provider, field="execution_provider")
    _require_non_empty(
        observation.execution_environment_id,
        field="execution_environment_id",
    )
    _require_non_empty(observation.transport_provider, field="transport_provider")
    _require_non_empty(observation.transport_channel_id, field="transport_channel_id")
    _validate_sha256(observation.subject_sha256, field="subject_sha256")
    _validate_sha256(
        observation.portable_trust_receipt_sha256,
        field="portable_trust_receipt_sha256",
    )
    _validate_sha256(
        observation.authorization_policy_sha256,
        field="authorization_policy_sha256",
    )
    _require_non_empty(observation.witness_reason, field="witness_reason")
    _validate_sha256(observation.next_witness_sha256, field="next_witness_sha256")
    if not isinstance(observation.verified, bool):
        raise ValueError("verified_must_be_boolean")


def validate_portable_execution_transport_receipt(
    receipt: PortableExecutionTransportReceipt,
) -> None:
    if receipt.schema != _RECEIPT_SCHEMA:
        raise ValueError("unsupported_execution_transport_receipt_schema")
    _validate_sha256(receipt.subject_sha256, field="subject_sha256")
    _validate_sha256(
        receipt.portable_trust_receipt_sha256,
        field="portable_trust_receipt_sha256",
    )
    _validate_sha256(
        receipt.authorization_policy_sha256,
        field="authorization_policy_sha256",
    )
    _require_non_empty(receipt.witness_reason, field="witness_reason")
    _validate_sha256(receipt.next_witness_sha256, field="next_witness_sha256")
    if not isinstance(receipt.verified, bool):
        raise ValueError("verified_must_be_boolean")


def portable_execution_transport_receipt_from_observation(
    observation: ExecutionTransportObservation,
) -> PortableExecutionTransportReceipt:
    validate_execution_transport_observation(observation)
    receipt = PortableExecutionTransportReceipt(
        subject_sha256=observation.subject_sha256,
        portable_trust_receipt_sha256=observation.portable_trust_receipt_sha256,
        authorization_policy_sha256=observation.authorization_policy_sha256,
        witness_reason=observation.witness_reason,
        next_witness_sha256=observation.next_witness_sha256,
        verified=observation.verified,
    )
    validate_portable_execution_transport_receipt(receipt)
    return receipt


def canonical_portable_execution_transport_receipt_bytes(
    receipt: PortableExecutionTransportReceipt,
) -> bytes:
    validate_portable_execution_transport_receipt(receipt)
    payload = {
        "schema": receipt.schema,
        "subject_sha256": receipt.subject_sha256,
        "portable_trust_receipt_sha256": receipt.portable_trust_receipt_sha256,
        "authorization_policy_sha256": receipt.authorization_policy_sha256,
        "witness_reason": receipt.witness_reason,
        "next_witness_sha256": receipt.next_witness_sha256,
        "verified": receipt.verified,
    }
    return (json.dumps(payload, sort_keys=True, separators=(",", ":")) + "\n").encode()


def portable_execution_transport_receipt_sha256(
    receipt: PortableExecutionTransportReceipt,
) -> str:
    return hashlib.sha256(
        canonical_portable_execution_transport_receipt_bytes(receipt)
    ).hexdigest()


def compare_independent_execution_transport_paths(
    primary: ExecutionTransportObservation,
    secondary: ExecutionTransportObservation,
) -> ExecutionTransportAgreement:
    """Require exact semantic agreement across distinct execution and transport providers."""

    validate_execution_transport_observation(primary)
    validate_execution_transport_observation(secondary)

    if primary.execution_provider == secondary.execution_provider:
        return ExecutionTransportAgreement(
            agreed=False,
            reason=ExecutionTransportAgreementReason.EXECUTION_PROVIDER_NOT_INDEPENDENT,
        )
    if primary.transport_provider == secondary.transport_provider:
        return ExecutionTransportAgreement(
            agreed=False,
            reason=ExecutionTransportAgreementReason.TRANSPORT_PROVIDER_NOT_INDEPENDENT,
        )
    if not primary.verified:
        return ExecutionTransportAgreement(
            agreed=False,
            reason=ExecutionTransportAgreementReason.PRIMARY_VERIFICATION_FAILED,
        )
    if not secondary.verified:
        return ExecutionTransportAgreement(
            agreed=False,
            reason=ExecutionTransportAgreementReason.SECONDARY_VERIFICATION_FAILED,
        )

    comparisons = (
        (
            primary.subject_sha256 == secondary.subject_sha256,
            ExecutionTransportAgreementReason.SUBJECT_MISMATCH,
        ),
        (
            primary.portable_trust_receipt_sha256
            == secondary.portable_trust_receipt_sha256,
            ExecutionTransportAgreementReason.TRUST_RECEIPT_MISMATCH,
        ),
        (
            primary.authorization_policy_sha256
            == secondary.authorization_policy_sha256,
            ExecutionTransportAgreementReason.AUTHORIZATION_POLICY_MISMATCH,
        ),
        (
            primary.witness_reason == secondary.witness_reason,
            ExecutionTransportAgreementReason.WITNESS_REASON_MISMATCH,
        ),
        (
            primary.next_witness_sha256 == secondary.next_witness_sha256,
            ExecutionTransportAgreementReason.NEXT_WITNESS_MISMATCH,
        ),
    )
    for matches, reason in comparisons:
        if not matches:
            return ExecutionTransportAgreement(agreed=False, reason=reason)

    primary_receipt = portable_execution_transport_receipt_from_observation(primary)
    secondary_receipt = portable_execution_transport_receipt_from_observation(secondary)
    primary_sha = portable_execution_transport_receipt_sha256(primary_receipt)
    secondary_sha = portable_execution_transport_receipt_sha256(secondary_receipt)
    if primary_sha != secondary_sha:
        return ExecutionTransportAgreement(
            agreed=False,
            reason=ExecutionTransportAgreementReason.PORTABLE_RECEIPT_MISMATCH,
        )

    return ExecutionTransportAgreement(
        agreed=True,
        reason=(
            ExecutionTransportAgreementReason.VERIFIED_EXECUTION_TRANSPORT_CLAIMS_AGREE
        ),
        portable_receipt_sha256=primary_sha,
    )
