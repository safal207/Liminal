"""Canonical authorization receipt joining builder identity and environment trust."""

from __future__ import annotations

import hashlib
import json
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Any

from .recovery_proof_bundle import PROOF_BUNDLE_NAME, verify_recovery_proof_bundle


TRUST_AUTHORIZATION_SCHEMA_VERSION = "liminal.recovery-trust-authorization.v0.1"
IDENTITY_AUTHORIZATION_SCHEMA_VERSION = "liminal.github-attestation-identity-authorization.v0.2"
ENVIRONMENT_AUTHORIZATION_SCHEMA_VERSION = "liminal.builder-environment-authorization.v0.1"
_SHA1_RE = re.compile(r"^[0-9a-f]{40}$")


def _sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def _canonical_json(value: object) -> bytes:
    return (json.dumps(value, sort_keys=True, separators=(",", ":")) + "\n").encode("utf-8")


def _commit_sha(value: str, *, reason: str) -> str:
    normalized = value.strip().lower()
    if not _SHA1_RE.fullmatch(normalized):
        raise ValueError(reason)
    return normalized


def _relative_workflow_path(value: str) -> str:
    path = Path(value)
    if (
        not value
        or path.is_absolute()
        or ".." in path.parts
        or not value.startswith(".github/workflows/")
        or path.suffix not in {".yml", ".yaml"}
    ):
        raise ValueError("recovery_trust_verifier_workflow_path_invalid")
    return path.as_posix()


def _load_json(path: str | Path) -> tuple[bytes, Any]:
    raw = Path(path).read_bytes()
    return raw, json.loads(raw)


def _required_dict(mapping: dict[str, Any], key: str, *, reason: str) -> dict[str, Any]:
    value = mapping.get(key)
    if not isinstance(value, dict):
        raise ValueError(reason)
    return value


def _required_string(mapping: dict[str, Any], key: str, *, reason: str) -> str:
    value = mapping.get(key)
    if not isinstance(value, str) or not value:
        raise ValueError(reason)
    return value


@dataclass(frozen=True)
class RecoveryTrustAuthorizationReceipt:
    proof_bundle_sha256: str
    identity_authorization_sha256: str
    environment_authorization_sha256: str
    builder_repository: str
    builder_workflow_path: str
    builder_workflow_sha: str
    source_ref: str
    verifier_repository: str
    verifier_workflow_path: str
    verifier_workflow_sha: str
    schema_version: str = TRUST_AUTHORIZATION_SCHEMA_VERSION

    def __post_init__(self) -> None:
        if self.schema_version != TRUST_AUTHORIZATION_SCHEMA_VERSION:
            raise ValueError("recovery_trust_schema_version_invalid")
        for value in (
            self.proof_bundle_sha256,
            self.identity_authorization_sha256,
            self.environment_authorization_sha256,
        ):
            if len(value) != 64 or any(character not in "0123456789abcdef" for character in value):
                raise ValueError("recovery_trust_sha256_invalid")
        if not self.builder_repository or "/" not in self.builder_repository:
            raise ValueError("recovery_trust_builder_repository_invalid")
        if not self.verifier_repository or "/" not in self.verifier_repository:
            raise ValueError("recovery_trust_verifier_repository_invalid")
        object.__setattr__(self, "builder_workflow_path", _relative_workflow_path(self.builder_workflow_path))
        object.__setattr__(self, "verifier_workflow_path", _relative_workflow_path(self.verifier_workflow_path))
        object.__setattr__(
            self,
            "builder_workflow_sha",
            _commit_sha(self.builder_workflow_sha, reason="recovery_trust_builder_workflow_sha_invalid"),
        )
        object.__setattr__(
            self,
            "verifier_workflow_sha",
            _commit_sha(self.verifier_workflow_sha, reason="recovery_trust_verifier_workflow_sha_invalid"),
        )
        if not self.source_ref.startswith("refs/"):
            raise ValueError("recovery_trust_source_ref_invalid")

    def as_dict(self) -> dict[str, object]:
        return {
            "schema_version": self.schema_version,
            "authorized": True,
            "reason": "recovery_trust_authorized",
            "subject": {
                "name": PROOF_BUNDLE_NAME,
                "sha256": self.proof_bundle_sha256,
            },
            "builder": {
                "repository": self.builder_repository,
                "workflow_path": self.builder_workflow_path,
                "workflow_sha": self.builder_workflow_sha,
            },
            "source_ref": self.source_ref,
            "verifier": {
                "repository": self.verifier_repository,
                "workflow_path": self.verifier_workflow_path,
                "workflow_sha": self.verifier_workflow_sha,
            },
            "authorization_evidence": {
                "identity_sha256": self.identity_authorization_sha256,
                "environment_sha256": self.environment_authorization_sha256,
            },
        }

    def canonical_bytes(self) -> bytes:
        return _canonical_json(self.as_dict())


def build_recovery_trust_authorization_receipt(
    *,
    proof_bundle_path: str | Path,
    identity_authorization_path: str | Path,
    environment_authorization_path: str | Path,
    verifier_repository: str,
    verifier_workflow_path: str,
    verifier_workflow_sha: str,
) -> RecoveryTrustAuthorizationReceipt:
    """Join already-authorized identity/environment evidence into one receipt."""

    bundle_path = Path(proof_bundle_path)
    if not verify_recovery_proof_bundle(bundle_path):
        raise ValueError("recovery_trust_proof_bundle_invalid")

    identity_raw, identity = _load_json(identity_authorization_path)
    environment_raw, environment = _load_json(environment_authorization_path)
    if not isinstance(identity, dict) or not isinstance(environment, dict):
        raise ValueError("recovery_trust_authorization_json_invalid")
    if identity.get("schema_version") != IDENTITY_AUTHORIZATION_SCHEMA_VERSION or identity.get("authorized") is not True:
        raise ValueError("recovery_trust_identity_not_authorized")
    if environment.get("schema_version") != ENVIRONMENT_AUTHORIZATION_SCHEMA_VERSION or environment.get("authorized") is not True:
        raise ValueError("recovery_trust_environment_not_authorized")

    claims = _required_dict(identity, "claims", reason="recovery_trust_identity_claims_invalid")
    environment_receipt = _required_dict(
        environment,
        "receipt",
        reason="recovery_trust_environment_receipt_invalid",
    )
    builder = _required_dict(
        environment_receipt,
        "builder",
        reason="recovery_trust_environment_builder_invalid",
    )

    builder_repository = _required_string(
        builder,
        "repository",
        reason="recovery_trust_builder_repository_invalid",
    )
    builder_workflow_path = _required_string(
        builder,
        "workflow_path",
        reason="recovery_trust_builder_workflow_path_invalid",
    )
    builder_workflow_sha = _commit_sha(
        _required_string(builder, "workflow_sha", reason="recovery_trust_builder_workflow_sha_invalid"),
        reason="recovery_trust_builder_workflow_sha_invalid",
    )
    signer_digest = _required_string(
        claims,
        "signer_digest",
        reason="recovery_trust_identity_signer_digest_invalid",
    )
    signer_uri = _required_string(claims, "signer_uri", reason="recovery_trust_identity_signer_uri_invalid")
    source_ref = _required_string(claims, "source_ref", reason="recovery_trust_identity_source_ref_invalid")

    expected_signer_uri = (
        f"https://github.com/{builder_repository}/{builder_workflow_path}@{builder_workflow_sha}"
    )
    if signer_digest != builder_workflow_sha:
        raise ValueError("recovery_trust_builder_digest_cross_link_mismatch")
    if signer_uri != expected_signer_uri:
        raise ValueError("recovery_trust_builder_uri_cross_link_mismatch")

    return RecoveryTrustAuthorizationReceipt(
        proof_bundle_sha256=_sha256(bundle_path.read_bytes()),
        identity_authorization_sha256=_sha256(identity_raw),
        environment_authorization_sha256=_sha256(environment_raw),
        builder_repository=builder_repository,
        builder_workflow_path=builder_workflow_path,
        builder_workflow_sha=builder_workflow_sha,
        source_ref=source_ref,
        verifier_repository=verifier_repository,
        verifier_workflow_path=verifier_workflow_path,
        verifier_workflow_sha=verifier_workflow_sha,
    )


def write_recovery_trust_authorization_receipt(
    receipt: RecoveryTrustAuthorizationReceipt,
    path: str | Path,
) -> None:
    target = Path(path)
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_bytes(receipt.canonical_bytes())
