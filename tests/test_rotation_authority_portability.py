from __future__ import annotations

from dataclasses import replace

import pytest

from liminal.rotation_authority_portability import (
    RotationAuthorityObservation,
    compare_rotation_authority,
    rotation_authority_receipt_sha256,
    validate_rotation_observation,
)


D = "a" * 64


def _observation(**changes: object) -> RotationAuthorityObservation:
    base = RotationAuthorityObservation(
        verified=True,
        rotation_producer_provider="github-actions-rotation-producer",
        rotation_producer_instance_id="e2cb6a014236bc561d03c405f4986146026041fa",
        control_plane_provider="github-repository-rotation-policy",
        control_plane_id="legacy-rotation-control-plane",
        logical_rotation_id="liminal.trusted-recovery.registry-rotation",
        rotation_contract_sha256="1" * 64,
        authorization_contract_sha256="2" * 64,
        previous_registry_sha256="3" * 64,
        current_registry_sha256="4" * 64,
        previous_manifest_sha256="5" * 64,
        current_manifest_sha256="6" * 64,
        from_generation=0,
        to_generation=1,
        rotation_reason="registry_rotation_authorized",
    )
    return replace(base, **changes)


def _secondary(**changes: object) -> RotationAuthorityObservation:
    return _observation(
        rotation_producer_provider="openai-workspace-standalone-rotation-producer",
        rotation_producer_instance_id="offline-rotation-v0.1",
        control_plane_provider="offline-ed25519-rotation-control-plane",
        control_plane_id="offline-rotation-policy-v0.1",
        **changes,
    )


def test_verified_independent_paths_agree() -> None:
    decision = compare_rotation_authority(_observation(), _secondary())
    assert decision.verified is True
    assert decision.reason == "verified_rotation_authority_claims_agree"
    assert decision.receipt is not None
    assert rotation_authority_receipt_sha256(_observation()) == rotation_authority_receipt_sha256(
        _secondary()
    )


@pytest.mark.parametrize(
    ("field", "value", "reason"),
    [
        ("logical_rotation_id", "other", "rotation_logical_rotation_id_mismatch"),
        ("rotation_contract_sha256", "7" * 64, "rotation_rotation_contract_sha256_mismatch"),
        (
            "authorization_contract_sha256",
            "7" * 64,
            "rotation_authorization_contract_sha256_mismatch",
        ),
        ("previous_registry_sha256", "7" * 64, "rotation_previous_registry_sha256_mismatch"),
        ("current_registry_sha256", "7" * 64, "rotation_current_registry_sha256_mismatch"),
        ("previous_manifest_sha256", "7" * 64, "rotation_previous_manifest_sha256_mismatch"),
        ("current_manifest_sha256", "7" * 64, "rotation_current_manifest_sha256_mismatch"),
        ("rotation_reason", "other", "rotation_observation_invalid"),
    ],
)
def test_semantic_drift_fails_closed(field: str, value: object, reason: str) -> None:
    decision = compare_rotation_authority(_observation(), _secondary(**{field: value}))
    assert decision.verified is False
    assert decision.reason == reason


def test_same_rotation_producer_provider_is_not_independent() -> None:
    secondary = _secondary(rotation_producer_provider="github-actions-rotation-producer")
    decision = compare_rotation_authority(_observation(), secondary)
    assert decision.reason == "rotation_producer_provider_not_independent"


def test_same_control_plane_is_not_independent() -> None:
    secondary = _secondary(control_plane_provider="github-repository-rotation-policy")
    decision = compare_rotation_authority(_observation(), secondary)
    assert decision.reason == "rotation_control_plane_not_independent"


def test_unverified_path_fails_closed() -> None:
    decision = compare_rotation_authority(_observation(), _secondary(verified=False))
    assert decision.reason == "rotation_observation_unverified"


def test_generation_gap_is_invalid() -> None:
    observation = _observation(to_generation=2)
    assert validate_rotation_observation(observation) is False


def test_bad_digest_is_invalid() -> None:
    observation = _observation(current_registry_sha256="not-a-digest")
    assert validate_rotation_observation(observation) is False
