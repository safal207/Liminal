from __future__ import annotations

import copy
import json
from pathlib import Path

import pytest

from liminal.source_control_external_proof import verify_external_source_control_bundle

ROOT = Path(__file__).resolve().parents[1]
BUNDLE_PATH = (
    ROOT
    / "trust"
    / "experimental"
    / "source-control-portability-v0.1"
    / "external-proof-bundle.json"
)
PRODUCER_ROOT = (
    "ed25519-sha256:4313ea228dbb2d29a429b9da9b25b30fb8e42bf6207c323c36c5b3f1863f627f"
)
CONTROL_ROOT = (
    "ed25519-sha256:153efe437acb8e5796def201b31f3f37c62c2123f50ddc5901ee1c268b749224"
)


def _load() -> dict[str, object]:
    payload = json.loads(BUNDLE_PATH.read_text())
    assert isinstance(payload, dict)
    return payload


def _verify(bundle: object):
    return verify_external_source_control_bundle(
        bundle,
        expected_producer_root_id=PRODUCER_ROOT,
        expected_control_plane_root_id=CONTROL_ROOT,
    )


def test_external_bundle_verifies_all_signatures_and_bindings() -> None:
    proof = _verify(_load())

    assert proof.verified is True
    assert proof.producer_root_id == PRODUCER_ROOT
    assert proof.control_plane_root_id == CONTROL_ROOT
    assert proof.subject_sha256 == (
        "74096c48cd730c55dd2f486f1af4b211b4f7f1ce38613134be645055ff1f946a"
    )
    assert proof.producer_contract_sha256 == (
        "72bba8eddc81e88c2e9ad24e266713e9534f6c332fec7ad5ecaa264f922b7ca3"
    )
    assert proof.authorization_contract_sha256 == (
        "576da1fa0c5cd70313ad1d89de88f4a7048e13fa5d0ce05c833f7bef4233a553"
    )
    assert proof.predecessor_witness_sha256 == (
        "af12743396296c788223d3087f427b1f93d3086a5aeb9b7c8c0f38d49347e9f9"
    )


def test_bundle_cannot_supply_its_own_trust_root() -> None:
    with pytest.raises(ValueError, match="producer_root_not_pinned"):
        verify_external_source_control_bundle(
            _load(),
            expected_producer_root_id="ed25519-sha256:" + "0" * 64,
            expected_control_plane_root_id=CONTROL_ROOT,
        )

    with pytest.raises(ValueError, match="control_plane_root_not_pinned"):
        verify_external_source_control_bundle(
            _load(),
            expected_producer_root_id=PRODUCER_ROOT,
            expected_control_plane_root_id="ed25519-sha256:" + "0" * 64,
        )


def test_tampered_checkpoint_fails_subject_binding() -> None:
    bundle = copy.deepcopy(_load())
    checkpoint = bundle["checkpoint_generation_1"]
    assert isinstance(checkpoint, dict)
    checkpoint["accepted_registry_sha256"] = "0" * 64

    with pytest.raises(ValueError, match="producer_claim_subject_sha256_mismatch"):
        _verify(bundle)


def test_tampered_producer_claim_signature_fails_closed() -> None:
    bundle = copy.deepcopy(_load())
    bundle["producer_claim_signature_b64"] = "A" * 88

    with pytest.raises(ValueError, match="producer_claim_signature_verification_failed"):
        _verify(bundle)


def test_tampered_control_plane_contract_fails_signature() -> None:
    bundle = copy.deepcopy(_load())
    contract = bundle["authorization_contract"]
    assert isinstance(contract, dict)
    contract["decision"] = "unauthorized"

    with pytest.raises(ValueError, match="authorization_contract_signature_verification_failed"):
        _verify(bundle)


def test_tampered_migration_predecessor_fails_signature() -> None:
    bundle = copy.deepcopy(_load())
    migration = bundle["authority_migration"]
    assert isinstance(migration, dict)
    migration["from_witness_sha256"] = "0" * 64

    with pytest.raises(ValueError, match="authority_migration_signature_verification_failed"):
        _verify(bundle)


def test_root_fingerprint_cannot_be_relabelled() -> None:
    bundle = copy.deepcopy(_load())
    bundle["producer_root_id"] = "ed25519-sha256:" + "0" * 64

    with pytest.raises(ValueError, match="producer_root_fingerprint_mismatch"):
        _verify(bundle)
