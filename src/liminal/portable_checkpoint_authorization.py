"""Fail-closed adapter from provider-neutral trust receipts to checkpoint evidence.

The existing checkpoint witness intentionally pins a concrete checkpoint producer
workflow. Trust-provider portability must therefore map provider-neutral claims back to
that local authorization policy without allowing a provider to choose its own signer
identity.

This adapter performs no cryptographic verification. It accepts only an already
verified :class:`PortableTrustReceipt`, checks it against explicit portable policy and
the signer pinned in the trusted witness, then emits the legacy
``VerifiedCheckpointEvidence`` consumed by the witness evaluator.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from enum import Enum

from liminal.recovery_trust_consumer_checkpoint_witness import (
    VerifiedCheckpointEvidence,
    validate_witness,
)
from liminal.trust_provider_portability import (
    PortableTrustReceipt,
    validate_portable_trust_receipt,
)

_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")


class PortableCheckpointAuthorizationReason(str, Enum):
    AUTHORIZED = "portable_trust_receipt_authorized"
    TRUSTED_WITNESS_INVALID = "trusted_witness_invalid"
    RECEIPT_UNVERIFIED = "portable_trust_receipt_unverified"
    AUTHORITY_MISMATCH = "portable_authority_mismatch"
    REPOSITORY_MISMATCH = "portable_repository_mismatch"
    PRODUCER_REVISION_MISMATCH = "portable_producer_revision_mismatch"
    SOURCE_REF_MISMATCH = "portable_source_ref_mismatch"
    EXECUTION_POLICY_MISMATCH = "portable_execution_policy_mismatch"
    AUTHORIZATION_POLICY_MISMATCH = "portable_authorization_policy_mismatch"


@dataclass(frozen=True)
class PortableCheckpointAuthorizationPolicy:
    """Provider-neutral claims that local witness policy expects."""

    authority_id: str
    source_ref: str
    execution_policy: str
    authorization_policy_sha256: str


@dataclass(frozen=True)
class PortableCheckpointAuthorization:
    authorized: bool
    reason: PortableCheckpointAuthorizationReason
    checkpoint_evidence: VerifiedCheckpointEvidence | None = None


def validate_portable_checkpoint_authorization_policy(
    policy: PortableCheckpointAuthorizationPolicy,
) -> None:
    if not policy.authority_id:
        raise ValueError("authority_id_must_be_non_empty")
    if not policy.source_ref.startswith("refs/"):
        raise ValueError("source_ref_must_be_fully_qualified_ref")
    if not policy.execution_policy:
        raise ValueError("execution_policy_must_be_non_empty")
    if not _SHA256_RE.fullmatch(policy.authorization_policy_sha256):
        raise ValueError("authorization_policy_sha256_must_be_lowercase_sha256")


def adapt_portable_trust_receipt_to_checkpoint_evidence(
    trusted_witness: object,
    receipt: PortableTrustReceipt,
    policy: PortableCheckpointAuthorizationPolicy,
    *,
    previous_witness: object | None = None,
) -> PortableCheckpointAuthorization:
    """Map verified portable claims into the signer identity pinned by the witness.

    The signer workflow path and signer SHA are never taken from provider-specific
    observation metadata. They come exclusively from the trusted witness. The receipt
    may only prove that its producer revision matches that pinned signer revision.
    """

    validate_portable_trust_receipt(receipt)
    validate_portable_checkpoint_authorization_policy(policy)

    if not validate_witness(trusted_witness, previous_witness):
        return PortableCheckpointAuthorization(
            authorized=False,
            reason=PortableCheckpointAuthorizationReason.TRUSTED_WITNESS_INVALID,
        )
    assert isinstance(trusted_witness, dict)

    if not receipt.verified:
        return PortableCheckpointAuthorization(
            authorized=False,
            reason=PortableCheckpointAuthorizationReason.RECEIPT_UNVERIFIED,
        )
    if receipt.authority_id != policy.authority_id:
        return PortableCheckpointAuthorization(
            authorized=False,
            reason=PortableCheckpointAuthorizationReason.AUTHORITY_MISMATCH,
        )
    if receipt.repository != trusted_witness["repository"]:
        return PortableCheckpointAuthorization(
            authorized=False,
            reason=PortableCheckpointAuthorizationReason.REPOSITORY_MISMATCH,
        )

    expected_signer = trusted_witness["checkpoint_signer"]
    assert isinstance(expected_signer, dict)
    expected_path = expected_signer["workflow_path"]
    expected_revision = expected_signer["workflow_sha"]
    assert isinstance(expected_path, str)
    assert isinstance(expected_revision, str)

    if receipt.producer_revision != expected_revision:
        return PortableCheckpointAuthorization(
            authorized=False,
            reason=PortableCheckpointAuthorizationReason.PRODUCER_REVISION_MISMATCH,
        )
    if receipt.source_ref != policy.source_ref:
        return PortableCheckpointAuthorization(
            authorized=False,
            reason=PortableCheckpointAuthorizationReason.SOURCE_REF_MISMATCH,
        )
    if receipt.execution_policy != policy.execution_policy:
        return PortableCheckpointAuthorization(
            authorized=False,
            reason=PortableCheckpointAuthorizationReason.EXECUTION_POLICY_MISMATCH,
        )
    if receipt.authorization_policy_sha256 != policy.authorization_policy_sha256:
        return PortableCheckpointAuthorization(
            authorized=False,
            reason=PortableCheckpointAuthorizationReason.AUTHORIZATION_POLICY_MISMATCH,
        )

    return PortableCheckpointAuthorization(
        authorized=True,
        reason=PortableCheckpointAuthorizationReason.AUTHORIZED,
        checkpoint_evidence=VerifiedCheckpointEvidence(
            verified=True,
            signer_workflow_path=expected_path,
            signer_workflow_sha=expected_revision,
            subject_sha256=receipt.subject_sha256,
        ),
    )
