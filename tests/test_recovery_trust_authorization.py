from __future__ import annotations

import hashlib
import json
from pathlib import Path

import pytest

from liminal.recovery_proof_bundle import build_recovery_proof_bundle
from liminal.recovery_trust_authorization import (
    TRUST_AUTHORIZATION_SCHEMA_VERSION,
    build_recovery_trust_authorization_receipt,
)


BUILDER_SHA = "02beb48b9c8a61d67c585573aac6c5781c000e89"
VERIFIER_SHA = "1" * 40
BUILDER_PATH = ".github/workflows/trusted-recovery-proof-builder.yml"
VERIFIER_PATH = ".github/workflows/trusted-recovery-proof-verifier.yml"


def _write_bundle(tmp_path: Path) -> Path:
    for name in (
        "builder-environment.json",
        "decision-receipt.json",
        "public-key.json",
        "summary.json",
    ):
        (tmp_path / name).write_text("{}\n", encoding="utf-8")
    (tmp_path / "recovery-evidence.jsonl").write_text('{"attempt":1}\n', encoding="utf-8")
    return build_recovery_proof_bundle(tmp_path).path


def _identity_payload(*, signer_digest: str = BUILDER_SHA, signer_uri: str | None = None) -> dict[str, object]:
    expected_uri = f"https://github.com/safal207/Liminal/{BUILDER_PATH}@{BUILDER_SHA}"
    return {
        "schema_version": "liminal.github-attestation-identity-authorization.v0.2",
        "authorized": True,
        "reason": "github_attestation_identity_authorized",
        "claims": {
            "repository_uri": "https://github.com/safal207/Liminal",
            "repository_id": "1005410203",
            "source_ref": "refs/heads/agent/recovery-routing-v0-1",
            "signer_uri": expected_uri if signer_uri is None else signer_uri,
            "signer_digest": signer_digest,
            "deployment_environment": "live-provider-trace",
            "runner_environment": "github-hosted",
            "oidc_issuer": "https://token.actions.githubusercontent.com",
        },
        "policy": {},
    }


def _environment_payload(*, authorized: bool = True) -> dict[str, object]:
    return {
        "schema_version": "liminal.builder-environment-authorization.v0.1",
        "authorized": authorized,
        "reason": "builder_environment_authorized" if authorized else "builder_environment_not_authorized",
        "policy": {},
        "receipt": {
            "schema_version": "liminal.builder-environment-receipt.v0.1",
            "builder": {
                "repository": "safal207/Liminal",
                "workflow_path": BUILDER_PATH,
                "workflow_sha": BUILDER_SHA,
                "workflow_file_sha256": "a" * 64,
            },
            "inputs": {},
            "runtime": {},
            "actions": [],
        },
    }


def _write_json(path: Path, payload: dict[str, object]) -> None:
    path.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def test_build_trust_authorization_cross_links_builder_and_subject(tmp_path: Path) -> None:
    bundle = _write_bundle(tmp_path)
    identity_path = tmp_path / "identity.json"
    environment_path = tmp_path / "environment.json"
    _write_json(identity_path, _identity_payload())
    _write_json(environment_path, _environment_payload())

    receipt = build_recovery_trust_authorization_receipt(
        proof_bundle_path=bundle,
        identity_authorization_path=identity_path,
        environment_authorization_path=environment_path,
        verifier_repository="safal207/Liminal",
        verifier_workflow_path=VERIFIER_PATH,
        verifier_workflow_sha=VERIFIER_SHA,
    )

    payload = receipt.as_dict()
    assert payload["schema_version"] == TRUST_AUTHORIZATION_SCHEMA_VERSION
    assert payload["authorized"] is True
    assert payload["reason"] == "recovery_trust_authorized"
    assert payload["subject"] == {
        "name": "recovery-proof-bundle.zip",
        "sha256": hashlib.sha256(bundle.read_bytes()).hexdigest(),
    }
    assert payload["builder"] == {
        "repository": "safal207/Liminal",
        "workflow_path": BUILDER_PATH,
        "workflow_sha": BUILDER_SHA,
    }
    assert payload["verifier"] == {
        "repository": "safal207/Liminal",
        "workflow_path": VERIFIER_PATH,
        "workflow_sha": VERIFIER_SHA,
    }
    assert receipt.canonical_bytes() == (
        json.dumps(payload, sort_keys=True, separators=(",", ":")) + "\n"
    ).encode("utf-8")


def test_trust_authorization_rejects_identity_not_authorized(tmp_path: Path) -> None:
    bundle = _write_bundle(tmp_path)
    identity = _identity_payload()
    identity["authorized"] = False
    identity_path = tmp_path / "identity.json"
    environment_path = tmp_path / "environment.json"
    _write_json(identity_path, identity)
    _write_json(environment_path, _environment_payload())

    with pytest.raises(ValueError, match="recovery_trust_identity_not_authorized"):
        build_recovery_trust_authorization_receipt(
            proof_bundle_path=bundle,
            identity_authorization_path=identity_path,
            environment_authorization_path=environment_path,
            verifier_repository="safal207/Liminal",
            verifier_workflow_path=VERIFIER_PATH,
            verifier_workflow_sha=VERIFIER_SHA,
        )


def test_trust_authorization_rejects_environment_not_authorized(tmp_path: Path) -> None:
    bundle = _write_bundle(tmp_path)
    identity_path = tmp_path / "identity.json"
    environment_path = tmp_path / "environment.json"
    _write_json(identity_path, _identity_payload())
    _write_json(environment_path, _environment_payload(authorized=False))

    with pytest.raises(ValueError, match="recovery_trust_environment_not_authorized"):
        build_recovery_trust_authorization_receipt(
            proof_bundle_path=bundle,
            identity_authorization_path=identity_path,
            environment_authorization_path=environment_path,
            verifier_repository="safal207/Liminal",
            verifier_workflow_path=VERIFIER_PATH,
            verifier_workflow_sha=VERIFIER_SHA,
        )


def test_trust_authorization_rejects_builder_digest_cross_link_mismatch(tmp_path: Path) -> None:
    bundle = _write_bundle(tmp_path)
    identity_path = tmp_path / "identity.json"
    environment_path = tmp_path / "environment.json"
    _write_json(identity_path, _identity_payload(signer_digest="2" * 40))
    _write_json(environment_path, _environment_payload())

    with pytest.raises(ValueError, match="recovery_trust_builder_digest_cross_link_mismatch"):
        build_recovery_trust_authorization_receipt(
            proof_bundle_path=bundle,
            identity_authorization_path=identity_path,
            environment_authorization_path=environment_path,
            verifier_repository="safal207/Liminal",
            verifier_workflow_path=VERIFIER_PATH,
            verifier_workflow_sha=VERIFIER_SHA,
        )


def test_trust_authorization_rejects_builder_uri_cross_link_mismatch(tmp_path: Path) -> None:
    bundle = _write_bundle(tmp_path)
    identity_path = tmp_path / "identity.json"
    environment_path = tmp_path / "environment.json"
    _write_json(
        identity_path,
        _identity_payload(signer_uri=f"https://github.com/safal207/Liminal/{BUILDER_PATH}@{'3' * 40}"),
    )
    _write_json(environment_path, _environment_payload())

    with pytest.raises(ValueError, match="recovery_trust_builder_uri_cross_link_mismatch"):
        build_recovery_trust_authorization_receipt(
            proof_bundle_path=bundle,
            identity_authorization_path=identity_path,
            environment_authorization_path=environment_path,
            verifier_repository="safal207/Liminal",
            verifier_workflow_path=VERIFIER_PATH,
            verifier_workflow_sha=VERIFIER_SHA,
        )
