from dataclasses import replace

import pytest

from liminal.trust_provider_portability import (
    TrustProviderAgreementReason,
    TrustProviderObservation,
    canonical_portable_trust_receipt_bytes,
    compare_independent_trust_providers,
    portable_trust_receipt_from_observation,
    portable_trust_receipt_sha256,
    validate_trust_provider_observation,
)

SUBJECT_SHA = "7" * 64
POLICY_SHA = "8" * 64
PRODUCER_SHA = "9" * 40


def _primary() -> TrustProviderObservation:
    return TrustProviderObservation(
        provider="github-sigstore-public-good",
        verification_scheme="github_attestation",
        trust_root_id="sigstore-public-good-v1",
        subject_sha256=SUBJECT_SHA,
        authority_id="liminal:checkpoint-producer",
        repository="safal207/Liminal",
        producer_revision=PRODUCER_SHA,
        source_ref="refs/heads/agent/recovery-routing-v0-1",
        execution_policy="trusted-ci-no-self-hosted",
        authorization_policy_sha256=POLICY_SHA,
        verified=True,
    )


def _secondary() -> TrustProviderObservation:
    return TrustProviderObservation(
        provider="offline-ed25519-root",
        verification_scheme="detached_ed25519",
        trust_root_id="liminal-offline-root-v0.1",
        subject_sha256=SUBJECT_SHA,
        authority_id="liminal:checkpoint-producer",
        repository="safal207/Liminal",
        producer_revision=PRODUCER_SHA,
        source_ref="refs/heads/agent/recovery-routing-v0-1",
        execution_policy="trusted-ci-no-self-hosted",
        authorization_policy_sha256=POLICY_SHA,
        verified=True,
    )


def test_distinct_providers_and_roots_can_converge_on_one_portable_receipt() -> None:
    primary = _primary()
    secondary = _secondary()

    agreement = compare_independent_trust_providers(primary, secondary)

    assert agreement.agreed is True
    assert (
        agreement.reason
        is TrustProviderAgreementReason.VERIFIED_TRUST_CLAIMS_AGREE
    )
    assert agreement.portable_receipt_sha256 is not None

    primary_receipt = portable_trust_receipt_from_observation(primary)
    secondary_receipt = portable_trust_receipt_from_observation(secondary)
    assert primary_receipt == secondary_receipt
    assert (
        portable_trust_receipt_sha256(primary_receipt)
        == portable_trust_receipt_sha256(secondary_receipt)
        == agreement.portable_receipt_sha256
    )


def test_provider_and_scheme_are_audit_metadata_not_portable_identity() -> None:
    primary_receipt = portable_trust_receipt_from_observation(_primary())
    secondary_receipt = portable_trust_receipt_from_observation(_secondary())

    assert canonical_portable_trust_receipt_bytes(primary_receipt) == (
        canonical_portable_trust_receipt_bytes(secondary_receipt)
    )


def test_same_provider_is_not_independent() -> None:
    primary = _primary()
    secondary = replace(_secondary(), provider=primary.provider)

    agreement = compare_independent_trust_providers(primary, secondary)

    assert agreement.agreed is False
    assert (
        agreement.reason
        is TrustProviderAgreementReason.TRUST_PROVIDER_NOT_INDEPENDENT
    )
    assert agreement.portable_receipt_sha256 is None


def test_same_trust_root_is_not_independent() -> None:
    primary = _primary()
    secondary = replace(_secondary(), trust_root_id=primary.trust_root_id)

    agreement = compare_independent_trust_providers(primary, secondary)

    assert agreement.agreed is False
    assert agreement.reason is TrustProviderAgreementReason.TRUST_ROOT_NOT_INDEPENDENT


def test_failed_secondary_provider_fails_closed() -> None:
    agreement = compare_independent_trust_providers(
        _primary(), replace(_secondary(), verified=False)
    )

    assert agreement.agreed is False
    assert (
        agreement.reason
        is TrustProviderAgreementReason.SECONDARY_VERIFICATION_FAILED
    )


@pytest.mark.parametrize(
    ("field", "value", "reason"),
    [
        ("subject_sha256", "a" * 64, TrustProviderAgreementReason.SUBJECT_MISMATCH),
        ("authority_id", "liminal:other", TrustProviderAgreementReason.AUTHORITY_MISMATCH),
        ("repository", "other/repo", TrustProviderAgreementReason.REPOSITORY_MISMATCH),
        (
            "producer_revision",
            "b" * 40,
            TrustProviderAgreementReason.PRODUCER_REVISION_MISMATCH,
        ),
        ("source_ref", "refs/heads/other", TrustProviderAgreementReason.SOURCE_REF_MISMATCH),
        (
            "execution_policy",
            "unrestricted",
            TrustProviderAgreementReason.EXECUTION_POLICY_MISMATCH,
        ),
        (
            "authorization_policy_sha256",
            "c" * 64,
            TrustProviderAgreementReason.AUTHORIZATION_POLICY_MISMATCH,
        ),
    ],
)
def test_security_semantic_drift_is_never_normalized_away(
    field: str, value: str, reason: TrustProviderAgreementReason
) -> None:
    agreement = compare_independent_trust_providers(
        _primary(), replace(_secondary(), **{field: value})
    )

    assert agreement.agreed is False
    assert agreement.reason is reason
    assert agreement.portable_receipt_sha256 is None


def test_invalid_subject_digest_is_rejected() -> None:
    observation = replace(_primary(), subject_sha256="not-a-digest")

    with pytest.raises(ValueError, match="subject_sha256_must_be_lowercase_sha256"):
        validate_trust_provider_observation(observation)


def test_invalid_producer_revision_is_rejected() -> None:
    observation = replace(_primary(), producer_revision="deadbeef")

    with pytest.raises(ValueError, match="producer_revision_must_be_lowercase_git_sha"):
        validate_trust_provider_observation(observation)


def test_invalid_policy_digest_is_rejected() -> None:
    observation = replace(_primary(), authorization_policy_sha256="A" * 64)

    with pytest.raises(
        ValueError, match="authorization_policy_sha256_must_be_lowercase_sha256"
    ):
        validate_trust_provider_observation(observation)


def test_portable_receipt_serialization_is_deterministic() -> None:
    receipt = portable_trust_receipt_from_observation(_primary())

    first = canonical_portable_trust_receipt_bytes(receipt)
    second = canonical_portable_trust_receipt_bytes(receipt)

    assert first == second
    assert first.endswith(b"\n")
    assert portable_trust_receipt_sha256(receipt) == portable_trust_receipt_sha256(
        receipt
    )
