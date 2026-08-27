from dataclasses import replace

from liminal.portable_checkpoint_authorization import (
    PortableCheckpointAuthorizationPolicy,
    PortableCheckpointAuthorizationReason,
    adapt_portable_trust_receipt_to_checkpoint_evidence,
)
from liminal.trust_provider_portability import (
    TrustProviderObservation,
    portable_trust_receipt_from_observation,
)

PRODUCER_SHA = "f31b56a5e21a668bcb98791b05542652760dcc27"
SUBJECT_SHA = "7" * 64
POLICY_SHA = "8" * 64
SOURCE_REF = "refs/heads/agent/recovery-routing-v0-1"


def _witness() -> dict[str, object]:
    return {
        "schema_version": "liminal.recovery-trust-consumer-checkpoint-witness.v0.1",
        "repository": "safal207/Liminal",
        "observed_generation": 0,
        "checkpoint_sha256": "1" * 64,
        "accepted_registry_sha256": "2" * 64,
        "accepted_manifest_sha256": "3" * 64,
        "previous_witness_sha256": None,
        "checkpoint_signer": {
            "workflow_path": (
                ".github/workflows/"
                "trusted-recovery-trust-consumer-checkpoint-attested.yml"
            ),
            "workflow_sha": PRODUCER_SHA,
        },
    }


def _observation(*, verified: bool = True) -> TrustProviderObservation:
    return TrustProviderObservation(
        provider="offline-ed25519-root",
        verification_scheme="detached_ed25519",
        trust_root_id="ed25519-sha256:example",
        subject_sha256=SUBJECT_SHA,
        authority_id="liminal:checkpoint-producer",
        repository="safal207/Liminal",
        producer_revision=PRODUCER_SHA,
        source_ref=SOURCE_REF,
        execution_policy="trusted-ci-no-self-hosted",
        authorization_policy_sha256=POLICY_SHA,
        verified=verified,
    )


def _policy() -> PortableCheckpointAuthorizationPolicy:
    return PortableCheckpointAuthorizationPolicy(
        authority_id="liminal:checkpoint-producer",
        source_ref=SOURCE_REF,
        execution_policy="trusted-ci-no-self-hosted",
        authorization_policy_sha256=POLICY_SHA,
    )


def test_portable_receipt_maps_only_to_signer_pinned_by_witness() -> None:
    receipt = portable_trust_receipt_from_observation(_observation())

    result = adapt_portable_trust_receipt_to_checkpoint_evidence(
        _witness(), receipt, _policy()
    )

    assert result.authorized is True
    assert result.reason is PortableCheckpointAuthorizationReason.AUTHORIZED
    assert result.checkpoint_evidence is not None
    assert result.checkpoint_evidence.verified is True
    assert result.checkpoint_evidence.subject_sha256 == SUBJECT_SHA
    assert result.checkpoint_evidence.signer_workflow_sha == PRODUCER_SHA
    assert result.checkpoint_evidence.signer_workflow_path == (
        ".github/workflows/trusted-recovery-trust-consumer-checkpoint-attested.yml"
    )


def test_unverified_portable_receipt_cannot_be_adapted() -> None:
    receipt = portable_trust_receipt_from_observation(_observation(verified=False))

    result = adapt_portable_trust_receipt_to_checkpoint_evidence(
        _witness(), receipt, _policy()
    )

    assert result.authorized is False
    assert result.reason is PortableCheckpointAuthorizationReason.RECEIPT_UNVERIFIED
    assert result.checkpoint_evidence is None


def test_provider_claim_cannot_override_witness_producer_revision() -> None:
    observation = replace(_observation(), producer_revision="a" * 40)
    receipt = portable_trust_receipt_from_observation(observation)

    result = adapt_portable_trust_receipt_to_checkpoint_evidence(
        _witness(), receipt, _policy()
    )

    assert result.authorized is False
    assert (
        result.reason
        is PortableCheckpointAuthorizationReason.PRODUCER_REVISION_MISMATCH
    )


def test_authority_mismatch_fails_closed() -> None:
    receipt = portable_trust_receipt_from_observation(
        replace(_observation(), authority_id="liminal:other-authority")
    )

    result = adapt_portable_trust_receipt_to_checkpoint_evidence(
        _witness(), receipt, _policy()
    )

    assert result.authorized is False
    assert result.reason is PortableCheckpointAuthorizationReason.AUTHORITY_MISMATCH


def test_execution_policy_mismatch_fails_closed() -> None:
    receipt = portable_trust_receipt_from_observation(
        replace(_observation(), execution_policy="unrestricted")
    )

    result = adapt_portable_trust_receipt_to_checkpoint_evidence(
        _witness(), receipt, _policy()
    )

    assert result.authorized is False
    assert (
        result.reason
        is PortableCheckpointAuthorizationReason.EXECUTION_POLICY_MISMATCH
    )


def test_authorization_policy_digest_mismatch_fails_closed() -> None:
    receipt = portable_trust_receipt_from_observation(
        replace(_observation(), authorization_policy_sha256="9" * 64)
    )

    result = adapt_portable_trust_receipt_to_checkpoint_evidence(
        _witness(), receipt, _policy()
    )

    assert result.authorized is False
    assert (
        result.reason
        is PortableCheckpointAuthorizationReason.AUTHORIZATION_POLICY_MISMATCH
    )


def test_invalid_trusted_witness_fails_closed() -> None:
    receipt = portable_trust_receipt_from_observation(_observation())
    witness = _witness()
    witness["repository"] = "other/repo"

    result = adapt_portable_trust_receipt_to_checkpoint_evidence(
        witness, receipt, _policy()
    )

    assert result.authorized is False
    assert result.reason is PortableCheckpointAuthorizationReason.TRUSTED_WITNESS_INVALID
