"""Fail-closed agreement model for independent verification implementations.

The portability layer compares security-relevant verification observations from two
independent verifier implementations. Verifier implementation identity is retained as
audit metadata, but is deliberately excluded from the normalized verification receipt.

This module performs no cryptographic verification. Each observation must be produced
by an external verifier path that actually checked the stated contract.
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum

from liminal.verification_receipt import (
    NormalizedVerificationReceipt,
    build_normalized_verification_receipt,
    verification_receipt_sha256,
)


class VerifierAgreementReason(str, Enum):
    VERIFIED_SEMANTICS_AGREE = "verified_semantics_agree"
    VERIFIER_IMPLEMENTATION_NOT_INDEPENDENT = "verifier_implementation_not_independent"
    PRIMARY_VERIFICATION_FAILED = "primary_verification_failed"
    SECONDARY_VERIFICATION_FAILED = "secondary_verification_failed"
    VERIFICATION_SCHEME_MISMATCH = "verification_scheme_mismatch"
    SUBJECT_MISMATCH = "subject_mismatch"
    REPOSITORY_MISMATCH = "repository_mismatch"
    SIGNER_WORKFLOW_MISMATCH = "signer_workflow_mismatch"
    SIGNER_DIGEST_MISMATCH = "signer_digest_mismatch"
    SOURCE_REF_MISMATCH = "source_ref_mismatch"
    RUNNER_POLICY_MISMATCH = "runner_policy_mismatch"
    NORMALIZED_RECEIPT_MISMATCH = "normalized_receipt_mismatch"


@dataclass(frozen=True)
class VerifierObservation:
    """One externally produced verification observation.

    ``implementation`` identifies the verifier implementation for independence/audit
    purposes. It is not part of portable normalized receipt identity.
    """

    implementation: str
    verification_scheme: str
    subject_sha256: str
    repository: str
    signer_workflow: str
    signer_digest: str
    source_ref: str
    deny_self_hosted_runners: bool
    verified: bool


@dataclass(frozen=True)
class VerifierAgreement:
    agreed: bool
    reason: VerifierAgreementReason
    receipt_sha256: str | None = None


def _require_non_empty(value: str, *, field: str) -> None:
    if not value:
        raise ValueError(f"{field}_must_be_non_empty")


def validate_verifier_observation(observation: VerifierObservation) -> None:
    _require_non_empty(observation.implementation, field="implementation")
    # Delegate canonical contract validation to the receipt layer. A failed verifier
    # observation is still structurally valid and therefore intentionally recordable.
    build_normalized_verification_receipt(
        verification_scheme=observation.verification_scheme,
        subject_sha256=observation.subject_sha256,
        repository=observation.repository,
        signer_workflow=observation.signer_workflow,
        signer_digest=observation.signer_digest,
        source_ref=observation.source_ref,
        deny_self_hosted_runners=observation.deny_self_hosted_runners,
        verification_succeeded=observation.verified,
    )


def normalized_receipt_from_observation(
    observation: VerifierObservation,
) -> NormalizedVerificationReceipt:
    """Canonicalize one externally verified observation into portable semantics."""

    validate_verifier_observation(observation)
    return build_normalized_verification_receipt(
        verification_scheme=observation.verification_scheme,
        subject_sha256=observation.subject_sha256,
        repository=observation.repository,
        signer_workflow=observation.signer_workflow,
        signer_digest=observation.signer_digest,
        source_ref=observation.source_ref,
        deny_self_hosted_runners=observation.deny_self_hosted_runners,
        verification_succeeded=observation.verified,
    )


def compare_independent_verifiers(
    primary: VerifierObservation,
    secondary: VerifierObservation,
) -> VerifierAgreement:
    """Require exact verified security-semantic agreement across implementations."""

    validate_verifier_observation(primary)
    validate_verifier_observation(secondary)

    if primary.implementation == secondary.implementation:
        return VerifierAgreement(
            agreed=False,
            reason=VerifierAgreementReason.VERIFIER_IMPLEMENTATION_NOT_INDEPENDENT,
        )
    if not primary.verified:
        return VerifierAgreement(
            agreed=False,
            reason=VerifierAgreementReason.PRIMARY_VERIFICATION_FAILED,
        )
    if not secondary.verified:
        return VerifierAgreement(
            agreed=False,
            reason=VerifierAgreementReason.SECONDARY_VERIFICATION_FAILED,
        )

    comparisons = (
        (
            primary.verification_scheme == secondary.verification_scheme,
            VerifierAgreementReason.VERIFICATION_SCHEME_MISMATCH,
        ),
        (
            primary.subject_sha256 == secondary.subject_sha256,
            VerifierAgreementReason.SUBJECT_MISMATCH,
        ),
        (
            primary.repository == secondary.repository,
            VerifierAgreementReason.REPOSITORY_MISMATCH,
        ),
        (
            primary.signer_workflow == secondary.signer_workflow,
            VerifierAgreementReason.SIGNER_WORKFLOW_MISMATCH,
        ),
        (
            primary.signer_digest == secondary.signer_digest,
            VerifierAgreementReason.SIGNER_DIGEST_MISMATCH,
        ),
        (
            primary.source_ref == secondary.source_ref,
            VerifierAgreementReason.SOURCE_REF_MISMATCH,
        ),
        (
            primary.deny_self_hosted_runners
            == secondary.deny_self_hosted_runners,
            VerifierAgreementReason.RUNNER_POLICY_MISMATCH,
        ),
    )
    for matches, reason in comparisons:
        if not matches:
            return VerifierAgreement(agreed=False, reason=reason)

    primary_receipt = normalized_receipt_from_observation(primary)
    secondary_receipt = normalized_receipt_from_observation(secondary)
    primary_sha = verification_receipt_sha256(primary_receipt)
    secondary_sha = verification_receipt_sha256(secondary_receipt)
    if primary_sha != secondary_sha:
        return VerifierAgreement(
            agreed=False,
            reason=VerifierAgreementReason.NORMALIZED_RECEIPT_MISMATCH,
        )

    return VerifierAgreement(
        agreed=True,
        reason=VerifierAgreementReason.VERIFIED_SEMANTICS_AGREE,
        receipt_sha256=primary_sha,
    )
