"""Fail-closed portability model across materially distinct trust providers.

This layer is deliberately above provider-specific verification receipts. Each provider
must independently prove the same portable trust claims, but provider identity,
verification scheme, and trust-root identity remain audit metadata rather than part of
the portable receipt itself.

The module performs no cryptographic verification. It only compares externally
established provider observations and refuses to claim portability when provider or
trust-root independence is missing.
"""

from __future__ import annotations

import hashlib
import json
import re
from dataclasses import dataclass
from enum import Enum

_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")
_GIT_SHA_RE = re.compile(r"^[0-9a-f]{40}$")
_REPOSITORY_RE = re.compile(r"^[^/\s]+/[^/\s]+$")
_RECEIPT_SCHEMA = "liminal-portable-trust-receipt/v0.1"


class TrustProviderAgreementReason(str, Enum):
    VERIFIED_TRUST_CLAIMS_AGREE = "verified_trust_claims_agree"
    TRUST_PROVIDER_NOT_INDEPENDENT = "trust_provider_not_independent"
    TRUST_ROOT_NOT_INDEPENDENT = "trust_root_not_independent"
    PRIMARY_VERIFICATION_FAILED = "primary_verification_failed"
    SECONDARY_VERIFICATION_FAILED = "secondary_verification_failed"
    SUBJECT_MISMATCH = "subject_mismatch"
    AUTHORITY_MISMATCH = "authority_mismatch"
    REPOSITORY_MISMATCH = "repository_mismatch"
    PRODUCER_REVISION_MISMATCH = "producer_revision_mismatch"
    SOURCE_REF_MISMATCH = "source_ref_mismatch"
    EXECUTION_POLICY_MISMATCH = "execution_policy_mismatch"
    AUTHORIZATION_POLICY_MISMATCH = "authorization_policy_mismatch"
    PORTABLE_RECEIPT_MISMATCH = "portable_receipt_mismatch"


@dataclass(frozen=True)
class TrustProviderObservation:
    """One provider-specific verification result plus portable trust claims.

    ``provider`` / ``verification_scheme`` / ``trust_root_id`` identify the concrete
    trust substrate and are retained for independence and audit. They are deliberately
    excluded from portable receipt identity.

    The remaining fields are claims that each provider must establish independently.
    """

    provider: str
    verification_scheme: str
    trust_root_id: str
    subject_sha256: str
    authority_id: str
    repository: str
    producer_revision: str
    source_ref: str
    execution_policy: str
    authorization_policy_sha256: str
    verified: bool


@dataclass(frozen=True)
class PortableTrustReceipt:
    """Provider-neutral security semantics proven by an external trust provider."""

    subject_sha256: str
    authority_id: str
    repository: str
    producer_revision: str
    source_ref: str
    execution_policy: str
    authorization_policy_sha256: str
    verified: bool
    schema: str = _RECEIPT_SCHEMA


@dataclass(frozen=True)
class TrustProviderAgreement:
    agreed: bool
    reason: TrustProviderAgreementReason
    portable_receipt_sha256: str | None = None


def _require_non_empty(value: str, *, field: str) -> None:
    if not value:
        raise ValueError(f"{field}_must_be_non_empty")


def _validate_sha256(value: str, *, field: str) -> None:
    if not _SHA256_RE.fullmatch(value):
        raise ValueError(f"{field}_must_be_lowercase_sha256")


def _validate_git_sha(value: str, *, field: str) -> None:
    if not _GIT_SHA_RE.fullmatch(value):
        raise ValueError(f"{field}_must_be_lowercase_git_sha")


def validate_trust_provider_observation(observation: TrustProviderObservation) -> None:
    _require_non_empty(observation.provider, field="provider")
    _require_non_empty(observation.verification_scheme, field="verification_scheme")
    _require_non_empty(observation.trust_root_id, field="trust_root_id")
    _validate_sha256(observation.subject_sha256, field="subject_sha256")
    _require_non_empty(observation.authority_id, field="authority_id")
    if not _REPOSITORY_RE.fullmatch(observation.repository):
        raise ValueError("repository_must_be_owner_slash_name")
    _validate_git_sha(observation.producer_revision, field="producer_revision")
    if not observation.source_ref.startswith("refs/"):
        raise ValueError("source_ref_must_be_fully_qualified_ref")
    _require_non_empty(observation.execution_policy, field="execution_policy")
    _validate_sha256(
        observation.authorization_policy_sha256,
        field="authorization_policy_sha256",
    )
    if not isinstance(observation.verified, bool):
        raise ValueError("verified_must_be_boolean")


def validate_portable_trust_receipt(receipt: PortableTrustReceipt) -> None:
    if receipt.schema != _RECEIPT_SCHEMA:
        raise ValueError("unsupported_portable_trust_receipt_schema")
    _validate_sha256(receipt.subject_sha256, field="subject_sha256")
    _require_non_empty(receipt.authority_id, field="authority_id")
    if not _REPOSITORY_RE.fullmatch(receipt.repository):
        raise ValueError("repository_must_be_owner_slash_name")
    _validate_git_sha(receipt.producer_revision, field="producer_revision")
    if not receipt.source_ref.startswith("refs/"):
        raise ValueError("source_ref_must_be_fully_qualified_ref")
    _require_non_empty(receipt.execution_policy, field="execution_policy")
    _validate_sha256(
        receipt.authorization_policy_sha256,
        field="authorization_policy_sha256",
    )
    if not isinstance(receipt.verified, bool):
        raise ValueError("verified_must_be_boolean")


def portable_trust_receipt_from_observation(
    observation: TrustProviderObservation,
) -> PortableTrustReceipt:
    """Project one provider-specific observation into provider-neutral semantics."""

    validate_trust_provider_observation(observation)
    receipt = PortableTrustReceipt(
        subject_sha256=observation.subject_sha256,
        authority_id=observation.authority_id,
        repository=observation.repository,
        producer_revision=observation.producer_revision,
        source_ref=observation.source_ref,
        execution_policy=observation.execution_policy,
        authorization_policy_sha256=observation.authorization_policy_sha256,
        verified=observation.verified,
    )
    validate_portable_trust_receipt(receipt)
    return receipt


def canonical_portable_trust_receipt_bytes(receipt: PortableTrustReceipt) -> bytes:
    """Serialize provider-neutral trust semantics deterministically."""

    validate_portable_trust_receipt(receipt)
    payload = {
        "schema": receipt.schema,
        "subject_sha256": receipt.subject_sha256,
        "authority_id": receipt.authority_id,
        "repository": receipt.repository,
        "producer_revision": receipt.producer_revision,
        "source_ref": receipt.source_ref,
        "execution_policy": receipt.execution_policy,
        "authorization_policy_sha256": receipt.authorization_policy_sha256,
        "verified": receipt.verified,
    }
    return (json.dumps(payload, sort_keys=True, separators=(",", ":")) + "\n").encode()


def portable_trust_receipt_sha256(receipt: PortableTrustReceipt) -> str:
    return hashlib.sha256(canonical_portable_trust_receipt_bytes(receipt)).hexdigest()


def compare_independent_trust_providers(
    primary: TrustProviderObservation,
    secondary: TrustProviderObservation,
) -> TrustProviderAgreement:
    """Require exact portable trust agreement across distinct providers and roots."""

    validate_trust_provider_observation(primary)
    validate_trust_provider_observation(secondary)

    if primary.provider == secondary.provider:
        return TrustProviderAgreement(
            agreed=False,
            reason=TrustProviderAgreementReason.TRUST_PROVIDER_NOT_INDEPENDENT,
        )
    if primary.trust_root_id == secondary.trust_root_id:
        return TrustProviderAgreement(
            agreed=False,
            reason=TrustProviderAgreementReason.TRUST_ROOT_NOT_INDEPENDENT,
        )
    if not primary.verified:
        return TrustProviderAgreement(
            agreed=False,
            reason=TrustProviderAgreementReason.PRIMARY_VERIFICATION_FAILED,
        )
    if not secondary.verified:
        return TrustProviderAgreement(
            agreed=False,
            reason=TrustProviderAgreementReason.SECONDARY_VERIFICATION_FAILED,
        )

    comparisons = (
        (
            primary.subject_sha256 == secondary.subject_sha256,
            TrustProviderAgreementReason.SUBJECT_MISMATCH,
        ),
        (
            primary.authority_id == secondary.authority_id,
            TrustProviderAgreementReason.AUTHORITY_MISMATCH,
        ),
        (
            primary.repository == secondary.repository,
            TrustProviderAgreementReason.REPOSITORY_MISMATCH,
        ),
        (
            primary.producer_revision == secondary.producer_revision,
            TrustProviderAgreementReason.PRODUCER_REVISION_MISMATCH,
        ),
        (
            primary.source_ref == secondary.source_ref,
            TrustProviderAgreementReason.SOURCE_REF_MISMATCH,
        ),
        (
            primary.execution_policy == secondary.execution_policy,
            TrustProviderAgreementReason.EXECUTION_POLICY_MISMATCH,
        ),
        (
            primary.authorization_policy_sha256
            == secondary.authorization_policy_sha256,
            TrustProviderAgreementReason.AUTHORIZATION_POLICY_MISMATCH,
        ),
    )
    for matches, reason in comparisons:
        if not matches:
            return TrustProviderAgreement(agreed=False, reason=reason)

    primary_receipt = portable_trust_receipt_from_observation(primary)
    secondary_receipt = portable_trust_receipt_from_observation(secondary)
    primary_sha = portable_trust_receipt_sha256(primary_receipt)
    secondary_sha = portable_trust_receipt_sha256(secondary_receipt)
    if primary_sha != secondary_sha:
        return TrustProviderAgreement(
            agreed=False,
            reason=TrustProviderAgreementReason.PORTABLE_RECEIPT_MISMATCH,
        )

    return TrustProviderAgreement(
        agreed=True,
        reason=TrustProviderAgreementReason.VERIFIED_TRUST_CLAIMS_AGREE,
        portable_receipt_sha256=primary_sha,
    )
