import pytest

from liminal.independent_verifier_portability import (
    VerifierAgreementReason,
    VerifierObservation,
    compare_independent_verifiers,
    normalized_receipt_from_observation,
)
from liminal.verification_receipt import verification_receipt_sha256


SUBJECT_SHA = "a" * 64
SIGNER_SHA = "b" * 40
SIGNER_WORKFLOW = (
    "safal207/Liminal/.github/workflows/"
    "trusted-recovery-trust-consumer-checkpoint-attested.yml"
)
SOURCE_REF = "refs/heads/agent/recovery-routing-v0-1"


def observation(implementation: str, **overrides) -> VerifierObservation:
    values = dict(
        implementation=implementation,
        verification_scheme="github_attestation",
        subject_sha256=SUBJECT_SHA,
        repository="safal207/Liminal",
        signer_workflow=SIGNER_WORKFLOW,
        signer_digest=SIGNER_SHA,
        source_ref=SOURCE_REF,
        deny_self_hosted_runners=True,
        verified=True,
    )
    values.update(overrides)
    return VerifierObservation(**values)


def test_distinct_verifiers_can_agree_on_one_portable_receipt():
    gh = observation("github-cli/attestation-verify")
    cosign = observation("sigstore/cosign")

    agreement = compare_independent_verifiers(gh, cosign)

    assert agreement.agreed is True
    assert agreement.reason is VerifierAgreementReason.VERIFIED_SEMANTICS_AGREE
    assert agreement.receipt_sha256 == verification_receipt_sha256(
        normalized_receipt_from_observation(gh)
    )
    assert agreement.receipt_sha256 == verification_receipt_sha256(
        normalized_receipt_from_observation(cosign)
    )


def test_same_implementation_is_not_independent():
    first = observation("github-cli/attestation-verify")
    second = observation("github-cli/attestation-verify")

    agreement = compare_independent_verifiers(first, second)

    assert agreement.agreed is False
    assert (
        agreement.reason
        is VerifierAgreementReason.VERIFIER_IMPLEMENTATION_NOT_INDEPENDENT
    )


@pytest.mark.parametrize(
    ("field", "value", "reason"),
    [
        (
            "verification_scheme",
            "other_scheme",
            VerifierAgreementReason.VERIFICATION_SCHEME_MISMATCH,
        ),
        ("subject_sha256", "c" * 64, VerifierAgreementReason.SUBJECT_MISMATCH),
        (
            "repository",
            "safal207/Other",
            VerifierAgreementReason.REPOSITORY_MISMATCH,
        ),
        (
            "signer_workflow",
            "safal207/Liminal/.github/workflows/other.yml",
            VerifierAgreementReason.SIGNER_WORKFLOW_MISMATCH,
        ),
        ("signer_digest", "d" * 40, VerifierAgreementReason.SIGNER_DIGEST_MISMATCH),
        ("source_ref", "refs/heads/other", VerifierAgreementReason.SOURCE_REF_MISMATCH),
        (
            "deny_self_hosted_runners",
            False,
            VerifierAgreementReason.RUNNER_POLICY_MISMATCH,
        ),
    ],
)
def test_any_security_semantic_drift_fails_closed(field, value, reason):
    primary = observation("github-cli/attestation-verify")
    secondary = observation("sigstore/cosign", **{field: value})

    agreement = compare_independent_verifiers(primary, secondary)

    assert agreement.agreed is False
    assert agreement.reason is reason
    assert agreement.receipt_sha256 is None


def test_primary_failure_fails_closed():
    agreement = compare_independent_verifiers(
        observation("github-cli/attestation-verify", verified=False),
        observation("sigstore/cosign"),
    )

    assert agreement.agreed is False
    assert agreement.reason is VerifierAgreementReason.PRIMARY_VERIFICATION_FAILED


def test_secondary_failure_fails_closed():
    agreement = compare_independent_verifiers(
        observation("github-cli/attestation-verify"),
        observation("sigstore/cosign", verified=False),
    )

    assert agreement.agreed is False
    assert agreement.reason is VerifierAgreementReason.SECONDARY_VERIFICATION_FAILED


def test_implementation_identity_is_not_receipt_identity():
    gh = observation("github-cli/attestation-verify")
    cosign = observation("sigstore/cosign")

    gh_receipt = normalized_receipt_from_observation(gh)
    cosign_receipt = normalized_receipt_from_observation(cosign)

    assert gh_receipt == cosign_receipt
    assert verification_receipt_sha256(gh_receipt) == verification_receipt_sha256(
        cosign_receipt
    )


def test_invalid_observation_contract_is_rejected():
    with pytest.raises(ValueError, match="subject_sha256_must_be_lowercase_sha256"):
        compare_independent_verifiers(
            observation("github-cli/attestation-verify", subject_sha256="not-a-digest"),
            observation("sigstore/cosign"),
        )
