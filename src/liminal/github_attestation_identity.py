"""Authorization policy for already-verified GitHub artifact attestations.

This module intentionally does *not* implement Sigstore cryptographic
verification. Its input must be JSON emitted by a successful
``gh attestation verify --format json`` invocation. It adds a second,
fail-closed authorization layer over that verified material.

The policy pins the repository (including immutable repository id), signer
workflow, source ref, GitHub deployment environment, runner environment, and
OIDC issuer. This keeps cryptographic validity separate from authorization:
a valid attestation from another workflow/environment is still unauthorized.
"""

from __future__ import annotations

import base64
from dataclasses import dataclass
from typing import Any, Mapping, Sequence

from cryptography import x509
from cryptography.x509.oid import ObjectIdentifier


DEPLOYMENT_ENVIRONMENT_OID = ObjectIdentifier("1.3.6.1.4.1.57264.1.23")
GITHUB_ACTIONS_OIDC_ISSUER = "https://token.actions.githubusercontent.com"
GITHUB_HOSTED_RUNNER = "github-hosted"


@dataclass(frozen=True)
class GitHubAttestationIdentityClaims:
    repository_uri: str
    repository_id: str
    source_ref: str
    signer_uri: str
    signer_digest: str
    deployment_environment: str
    runner_environment: str
    oidc_issuer: str
    subject_alternative_name: str


@dataclass(frozen=True)
class GitHubAttestationIdentityPolicy:
    repository: str
    repository_id: str
    signer_workflow_path: str
    source_ref: str
    deployment_environment: str
    runner_environment: str = GITHUB_HOSTED_RUNNER
    oidc_issuer: str = GITHUB_ACTIONS_OIDC_ISSUER

    def __post_init__(self) -> None:
        for name, value in (
            ("repository", self.repository),
            ("repository_id", self.repository_id),
            ("signer_workflow_path", self.signer_workflow_path),
            ("source_ref", self.source_ref),
            ("deployment_environment", self.deployment_environment),
            ("runner_environment", self.runner_environment),
            ("oidc_issuer", self.oidc_issuer),
        ):
            if not value:
                raise ValueError(f"github_attestation_policy_{name}_required")
        if self.signer_workflow_path.startswith("/"):
            raise ValueError("github_attestation_policy_signer_workflow_path_must_be_relative")
        if not self.signer_workflow_path.startswith(".github/workflows/"):
            raise ValueError("github_attestation_policy_signer_workflow_path_invalid")
        if not self.source_ref.startswith("refs/"):
            raise ValueError("github_attestation_policy_source_ref_invalid")

    @property
    def repository_uri(self) -> str:
        return f"https://github.com/{self.repository}"

    @property
    def signer_uri(self) -> str:
        return f"{self.repository_uri}/{self.signer_workflow_path}@{self.source_ref}"


@dataclass(frozen=True)
class GitHubAttestationAuthorization:
    authorized: bool
    reason: str
    claims: GitHubAttestationIdentityClaims | None


def _required_string(mapping: Mapping[str, Any], key: str) -> str:
    value = mapping.get(key)
    if not isinstance(value, str) or not value:
        raise ValueError(f"github_attestation_missing_or_invalid_{key}")
    return value


def _decode_der_utf8_string(encoded: bytes) -> str:
    """Decode the DER UTF8String used by Fulcio provider-generic OIDs."""

    if len(encoded) < 2 or encoded[0] != 0x0C:
        raise ValueError("github_attestation_environment_extension_not_utf8string")
    first_length = encoded[1]
    offset = 2
    if first_length & 0x80:
        length_octets = first_length & 0x7F
        if length_octets == 0 or length_octets > 4 or len(encoded) < 2 + length_octets:
            raise ValueError("github_attestation_environment_extension_invalid_length")
        length = int.from_bytes(encoded[offset : offset + length_octets], "big")
        offset += length_octets
    else:
        length = first_length
    payload = encoded[offset:]
    if len(payload) != length:
        raise ValueError("github_attestation_environment_extension_length_mismatch")
    try:
        value = payload.decode("utf-8")
    except UnicodeDecodeError as exc:
        raise ValueError("github_attestation_environment_extension_invalid_utf8") from exc
    if not value:
        raise ValueError("github_attestation_environment_extension_empty")
    return value


def _deployment_environment(raw_certificate_b64: str) -> str:
    try:
        raw = base64.b64decode(raw_certificate_b64, validate=True)
        certificate = x509.load_der_x509_certificate(raw)
        extension = certificate.extensions.get_extension_for_oid(DEPLOYMENT_ENVIRONMENT_OID)
    except (ValueError, x509.ExtensionNotFound) as exc:
        raise ValueError("github_attestation_deployment_environment_unavailable") from exc

    encoded = getattr(extension.value, "value", None)
    if not isinstance(encoded, bytes):
        raise ValueError("github_attestation_deployment_environment_unavailable")
    return _decode_der_utf8_string(encoded)


def claims_from_verified_gh_json(payload: Any) -> GitHubAttestationIdentityClaims:
    """Extract normalized claims from successful ``gh attestation verify`` JSON.

    Exactly one verified attestation is required to avoid ambiguous policy
    evaluation. The deployment environment is read from Fulcio's documented
    provider-generic certificate extension (OID ...57264.1.23), because current
    GitHub CLI parsed certificate output does not expose that field directly.
    """

    if not isinstance(payload, Sequence) or isinstance(payload, (str, bytes)):
        raise ValueError("github_attestation_verification_json_must_be_array")
    if len(payload) != 1:
        raise ValueError("github_attestation_exactly_one_verified_result_required")
    item = payload[0]
    if not isinstance(item, Mapping):
        raise ValueError("github_attestation_verified_result_invalid")

    verification = item.get("verificationResult")
    attestation = item.get("attestation")
    if not isinstance(verification, Mapping) or not isinstance(attestation, Mapping):
        raise ValueError("github_attestation_verified_result_invalid")

    signature = verification.get("signature")
    if not isinstance(signature, Mapping):
        raise ValueError("github_attestation_signature_result_invalid")
    certificate_info = signature.get("certificate")
    if not isinstance(certificate_info, Mapping):
        raise ValueError("github_attestation_certificate_result_invalid")

    bundle = attestation.get("bundle")
    if not isinstance(bundle, Mapping):
        raise ValueError("github_attestation_bundle_invalid")
    material = bundle.get("verificationMaterial")
    if not isinstance(material, Mapping):
        raise ValueError("github_attestation_verification_material_invalid")
    certificate = material.get("certificate")
    if not isinstance(certificate, Mapping):
        raise ValueError("github_attestation_bundle_certificate_invalid")
    raw_certificate_b64 = _required_string(certificate, "rawBytes")

    return GitHubAttestationIdentityClaims(
        repository_uri=_required_string(certificate_info, "sourceRepositoryURI"),
        repository_id=_required_string(certificate_info, "sourceRepositoryIdentifier"),
        source_ref=_required_string(certificate_info, "sourceRepositoryRef"),
        signer_uri=_required_string(certificate_info, "buildSignerURI"),
        signer_digest=_required_string(certificate_info, "buildSignerDigest"),
        deployment_environment=_deployment_environment(raw_certificate_b64),
        runner_environment=_required_string(certificate_info, "runnerEnvironment"),
        oidc_issuer=_required_string(certificate_info, "issuer"),
        subject_alternative_name=_required_string(certificate_info, "subjectAlternativeName"),
    )


def authorize_verified_github_attestation(
    payload: Any,
    *,
    policy: GitHubAttestationIdentityPolicy,
) -> GitHubAttestationAuthorization:
    """Authorize identity claims after external cryptographic verification."""

    try:
        claims = claims_from_verified_gh_json(payload)
    except ValueError as exc:
        return GitHubAttestationAuthorization(False, str(exc), None)

    checks = (
        (claims.repository_uri == policy.repository_uri, "source_repository_not_authorized"),
        (claims.repository_id == policy.repository_id, "source_repository_id_not_authorized"),
        (claims.source_ref == policy.source_ref, "source_ref_not_authorized"),
        (claims.signer_uri == policy.signer_uri, "signer_workflow_not_authorized"),
        (
            claims.subject_alternative_name == policy.signer_uri,
            "signer_subject_alternative_name_not_authorized",
        ),
        (
            claims.deployment_environment == policy.deployment_environment,
            "deployment_environment_not_authorized",
        ),
        (claims.runner_environment == policy.runner_environment, "runner_environment_not_authorized"),
        (claims.oidc_issuer == policy.oidc_issuer, "oidc_issuer_not_authorized"),
    )
    for passed, reason in checks:
        if not passed:
            return GitHubAttestationAuthorization(False, reason, claims)
    return GitHubAttestationAuthorization(True, "github_attestation_identity_authorized", claims)
