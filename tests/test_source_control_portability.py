from __future__ import annotations

import dataclasses

import pytest

from liminal.source_control_portability import (
    SourceControlAgreementReason,
    SourceControlObservation,
    compare_independent_source_control_paths,
    portable_source_control_receipt_from_observation,
    portable_source_control_receipt_sha256,
    validate_source_control_observation,
)

SUBJECT = "74" * 32
PRODUCER_CONTRACT = "11" * 32
AUTHORIZATION_CONTRACT = "22" * 32
NEXT_WITNESS = "cc" * 32


def _observation(**overrides: object) -> SourceControlObservation:
    values: dict[str, object] = {
        "producer_provider": "github-actions-checkpoint-producer",
        "producer_instance_id": "github-workflow:f31b56a5",
        "control_plane_provider": "github-repository-policy",
        "control_plane_id": "safal207/Liminal:agent/recovery-routing-v0-1",
        "subject_sha256": SUBJECT,
        "logical_producer_id": "liminal:trusted-recovery-checkpoint-producer",
        "producer_contract_sha256": PRODUCER_CONTRACT,
        "authorization_contract_sha256": AUTHORIZATION_CONTRACT,
        "evidence_type": "trusted-recovery-consumer-checkpoint",
        "generation": 1,
        "witness_reason": "checkpoint_witness_advanced",
        "next_witness_sha256": NEXT_WITNESS,
        "verified": True,
    }
    values.update(overrides)
    return SourceControlObservation(**values)  # type: ignore[arg-type]


def _secondary(**overrides: object) -> SourceControlObservation:
    values: dict[str, object] = {
        "producer_provider": "openai-workspace-checkpoint-producer",
        "producer_instance_id": "openai-workspace:external-producer-v0.1",
        "control_plane_provider": "portable-signed-policy-bundle",
        "control_plane_id": "ed25519-root:external-control-v0.1",
    }
    values.update(overrides)
    return _observation(**values)


def test_independent_producer_and_control_plane_agree_on_portable_transition() -> None:
    primary = _observation()
    secondary = _secondary()

    agreement = compare_independent_source_control_paths(primary, secondary)

    assert agreement.agreed is True
    assert (
        agreement.reason
        is SourceControlAgreementReason.VERIFIED_SOURCE_CONTROL_CLAIMS_AGREE
    )
    assert agreement.portable_receipt_sha256 == portable_source_control_receipt_sha256(
        portable_source_control_receipt_from_observation(primary)
    )


def test_same_producer_provider_fails_closed() -> None:
    agreement = compare_independent_source_control_paths(
        _observation(),
        _secondary(producer_provider="github-actions-checkpoint-producer"),
    )

    assert agreement.agreed is False
    assert (
        agreement.reason
        is SourceControlAgreementReason.PRODUCER_PROVIDER_NOT_INDEPENDENT
    )


def test_same_control_plane_fails_closed() -> None:
    agreement = compare_independent_source_control_paths(
        _observation(),
        _secondary(control_plane_provider="github-repository-policy"),
    )

    assert agreement.agreed is False
    assert agreement.reason is SourceControlAgreementReason.CONTROL_PLANE_NOT_INDEPENDENT


def test_unverified_external_path_fails_closed() -> None:
    agreement = compare_independent_source_control_paths(
        _observation(),
        _secondary(verified=False),
    )

    assert agreement.agreed is False
    assert agreement.reason is SourceControlAgreementReason.SECONDARY_VERIFICATION_FAILED


@pytest.mark.parametrize(
    ("field", "value", "reason"),
    [
        ("subject_sha256", "33" * 32, SourceControlAgreementReason.SUBJECT_MISMATCH),
        (
            "logical_producer_id",
            "liminal:other-producer",
            SourceControlAgreementReason.LOGICAL_PRODUCER_MISMATCH,
        ),
        (
            "producer_contract_sha256",
            "44" * 32,
            SourceControlAgreementReason.PRODUCER_CONTRACT_MISMATCH,
        ),
        (
            "authorization_contract_sha256",
            "55" * 32,
            SourceControlAgreementReason.AUTHORIZATION_CONTRACT_MISMATCH,
        ),
        (
            "evidence_type",
            "other-evidence",
            SourceControlAgreementReason.EVIDENCE_TYPE_MISMATCH,
        ),
        ("generation", 2, SourceControlAgreementReason.GENERATION_MISMATCH),
        (
            "witness_reason",
            "checkpoint_already_witnessed",
            SourceControlAgreementReason.WITNESS_REASON_MISMATCH,
        ),
        (
            "next_witness_sha256",
            "66" * 32,
            SourceControlAgreementReason.NEXT_WITNESS_MISMATCH,
        ),
    ],
)
def test_portable_semantic_drift_fails_closed(
    field: str,
    value: object,
    reason: SourceControlAgreementReason,
) -> None:
    agreement = compare_independent_source_control_paths(
        _observation(),
        _secondary(**{field: value}),
    )

    assert agreement.agreed is False
    assert agreement.reason is reason


def test_provider_metadata_does_not_change_portable_receipt_identity() -> None:
    primary = _observation()
    secondary = _secondary(
        producer_instance_id="totally-different-producer-instance",
        control_plane_id="totally-different-control-plane-instance",
    )

    assert portable_source_control_receipt_sha256(
        portable_source_control_receipt_from_observation(primary)
    ) == portable_source_control_receipt_sha256(
        portable_source_control_receipt_from_observation(secondary)
    )


def test_invalid_generation_is_rejected() -> None:
    invalid = dataclasses.replace(_observation(), generation=-1)

    with pytest.raises(ValueError, match="generation_must_be_non_negative_integer"):
        validate_source_control_observation(invalid)


def test_invalid_contract_digest_is_rejected() -> None:
    invalid = dataclasses.replace(_observation(), producer_contract_sha256="not-a-digest")

    with pytest.raises(ValueError, match="producer_contract_sha256_must_be_lowercase_sha256"):
        validate_source_control_observation(invalid)
