from __future__ import annotations

from dataclasses import replace

import pytest

from liminal.execution_transport_portability import (
    ExecutionTransportAgreementReason,
    ExecutionTransportObservation,
    PortableExecutionTransportReceipt,
    canonical_portable_execution_transport_receipt_bytes,
    compare_independent_execution_transport_paths,
    portable_execution_transport_receipt_from_observation,
    portable_execution_transport_receipt_sha256,
    validate_execution_transport_observation,
    validate_portable_execution_transport_receipt,
)

SUBJECT_SHA = "74" * 32
TRUST_RECEIPT_SHA = "22" * 32
POLICY_SHA = "ab" * 32
NEXT_WITNESS_SHA = "cc" * 32


def _observation(**overrides: object) -> ExecutionTransportObservation:
    values: dict[str, object] = {
        "execution_provider": "github-actions-hosted",
        "execution_environment_id": "github-hosted:ubuntu-24.04",
        "transport_provider": "github-actions-artifact",
        "transport_channel_id": "github-artifact:checkpoint-evidence",
        "subject_sha256": SUBJECT_SHA,
        "portable_trust_receipt_sha256": TRUST_RECEIPT_SHA,
        "authorization_policy_sha256": POLICY_SHA,
        "witness_reason": "checkpoint_witness_advanced",
        "next_witness_sha256": NEXT_WITNESS_SHA,
        "verified": True,
    }
    values.update(overrides)
    return ExecutionTransportObservation(**values)  # type: ignore[arg-type]


def _independent() -> ExecutionTransportObservation:
    return _observation(
        execution_provider="openai-workspace-container",
        execution_environment_id="chatgpt-container:linux",
        transport_provider="google-drive",
        transport_channel_id="drive:file-content",
    )


def test_independent_execution_and_transport_paths_converge() -> None:
    primary = _observation()
    secondary = _independent()

    agreement = compare_independent_execution_transport_paths(primary, secondary)

    assert agreement.agreed is True
    assert (
        agreement.reason
        is ExecutionTransportAgreementReason.VERIFIED_EXECUTION_TRANSPORT_CLAIMS_AGREE
    )
    assert agreement.portable_receipt_sha256 == portable_execution_transport_receipt_sha256(
        portable_execution_transport_receipt_from_observation(primary)
    )
    assert portable_execution_transport_receipt_from_observation(
        primary
    ) == portable_execution_transport_receipt_from_observation(secondary)


def test_same_execution_provider_fails_closed_even_if_environment_differs() -> None:
    secondary = _observation(
        execution_environment_id="github-self-hosted:runner-2",
        transport_provider="google-drive",
        transport_channel_id="drive:file-content",
    )

    agreement = compare_independent_execution_transport_paths(
        _observation(),
        secondary,
    )

    assert agreement.agreed is False
    assert (
        agreement.reason
        is ExecutionTransportAgreementReason.EXECUTION_PROVIDER_NOT_INDEPENDENT
    )


def test_same_transport_provider_fails_closed_even_if_channel_differs() -> None:
    secondary = _observation(
        execution_provider="openai-workspace-container",
        execution_environment_id="chatgpt-container:linux",
        transport_channel_id="github-artifact:different-download-url",
    )

    agreement = compare_independent_execution_transport_paths(
        _observation(),
        secondary,
    )

    assert agreement.agreed is False
    assert (
        agreement.reason
        is ExecutionTransportAgreementReason.TRANSPORT_PROVIDER_NOT_INDEPENDENT
    )


@pytest.mark.parametrize(
    ("field", "value", "reason"),
    [
        ("subject_sha256", "11" * 32, ExecutionTransportAgreementReason.SUBJECT_MISMATCH),
        (
            "portable_trust_receipt_sha256",
            "33" * 32,
            ExecutionTransportAgreementReason.TRUST_RECEIPT_MISMATCH,
        ),
        (
            "authorization_policy_sha256",
            "44" * 32,
            ExecutionTransportAgreementReason.AUTHORIZATION_POLICY_MISMATCH,
        ),
        (
            "witness_reason",
            "checkpoint_witness_rejected",
            ExecutionTransportAgreementReason.WITNESS_REASON_MISMATCH,
        ),
        (
            "next_witness_sha256",
            "55" * 32,
            ExecutionTransportAgreementReason.NEXT_WITNESS_MISMATCH,
        ),
    ],
)
def test_semantic_drift_fails_closed(
    field: str,
    value: object,
    reason: ExecutionTransportAgreementReason,
) -> None:
    secondary = replace(_independent(), **{field: value})

    agreement = compare_independent_execution_transport_paths(
        _observation(),
        secondary,
    )

    assert agreement.agreed is False
    assert agreement.reason is reason
    assert agreement.portable_receipt_sha256 is None


def test_failed_secondary_path_never_converges() -> None:
    agreement = compare_independent_execution_transport_paths(
        _observation(),
        replace(_independent(), verified=False),
    )

    assert agreement.agreed is False
    assert (
        agreement.reason
        is ExecutionTransportAgreementReason.SECONDARY_VERIFICATION_FAILED
    )


def test_provider_and_channel_metadata_do_not_change_portable_identity() -> None:
    primary_receipt = portable_execution_transport_receipt_from_observation(_observation())
    secondary_receipt = portable_execution_transport_receipt_from_observation(_independent())

    assert primary_receipt == secondary_receipt
    assert canonical_portable_execution_transport_receipt_bytes(
        primary_receipt
    ) == canonical_portable_execution_transport_receipt_bytes(secondary_receipt)


def test_validation_rejects_empty_provider_metadata() -> None:
    with pytest.raises(ValueError, match="execution_provider_must_be_non_empty"):
        validate_execution_transport_observation(
            replace(_observation(), execution_provider="")
        )

    with pytest.raises(ValueError, match="transport_channel_id_must_be_non_empty"):
        validate_execution_transport_observation(
            replace(_observation(), transport_channel_id="")
        )


def test_validation_rejects_invalid_hashes_and_schema() -> None:
    with pytest.raises(ValueError, match="subject_sha256_must_be_lowercase_sha256"):
        validate_execution_transport_observation(
            replace(_observation(), subject_sha256="ABC")
        )

    receipt = portable_execution_transport_receipt_from_observation(_observation())
    invalid = PortableExecutionTransportReceipt(
        subject_sha256=receipt.subject_sha256,
        portable_trust_receipt_sha256=receipt.portable_trust_receipt_sha256,
        authorization_policy_sha256=receipt.authorization_policy_sha256,
        witness_reason=receipt.witness_reason,
        next_witness_sha256=receipt.next_witness_sha256,
        verified=receipt.verified,
        schema="unknown/v9",
    )
    with pytest.raises(
        ValueError,
        match="unsupported_execution_transport_receipt_schema",
    ):
        validate_portable_execution_transport_receipt(invalid)
