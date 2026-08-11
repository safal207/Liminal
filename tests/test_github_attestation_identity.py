from __future__ import annotations

import base64
from datetime import datetime, timedelta, timezone

from cryptography import x509
from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.primitives.asymmetric import ec
from cryptography.x509.oid import NameOID

from liminal.github_attestation_identity import (
    DEPLOYMENT_ENVIRONMENT_OID,
    GitHubAttestationIdentityPolicy,
    authorize_verified_github_attestation,
)


REPOSITORY = "safal207/Liminal"
REPOSITORY_ID = "1005410203"
WORKFLOW = ".github/workflows/live-recovery-decision-proof.yml"
REF = "refs/heads/agent/recovery-routing-v0-1"
ENVIRONMENT = "live-provider-trace"
SIGNER_URI = f"https://github.com/{REPOSITORY}/{WORKFLOW}@{REF}"


def _der_utf8(value: str) -> bytes:
    encoded = value.encode("utf-8")
    assert len(encoded) < 128
    return bytes((0x0C, len(encoded))) + encoded


def _certificate_b64(environment: str | None = ENVIRONMENT) -> str:
    key = ec.generate_private_key(ec.SECP256R1())
    name = x509.Name([x509.NameAttribute(NameOID.COMMON_NAME, "test")])
    builder = (
        x509.CertificateBuilder()
        .subject_name(name)
        .issuer_name(name)
        .public_key(key.public_key())
        .serial_number(1)
        .not_valid_before(datetime.now(timezone.utc) - timedelta(minutes=1))
        .not_valid_after(datetime.now(timezone.utc) + timedelta(minutes=5))
    )
    if environment is not None:
        builder = builder.add_extension(
            x509.UnrecognizedExtension(DEPLOYMENT_ENVIRONMENT_OID, _der_utf8(environment)),
            critical=False,
        )
    certificate = builder.sign(key, hashes.SHA256())
    raw = certificate.public_bytes(serialization.Encoding.DER)
    return base64.b64encode(raw).decode("ascii")


def _payload(
    *,
    repository_uri: str = f"https://github.com/{REPOSITORY}",
    repository_id: str = REPOSITORY_ID,
    source_ref: str = REF,
    signer_uri: str = SIGNER_URI,
    runner_environment: str = "github-hosted",
    issuer: str = "https://token.actions.githubusercontent.com",
    environment: str | None = ENVIRONMENT,
) -> list[dict]:
    return [
        {
            "attestation": {
                "bundle": {
                    "verificationMaterial": {
                        "certificate": {"rawBytes": _certificate_b64(environment)}
                    }
                }
            },
            "verificationResult": {
                "signature": {
                    "certificate": {
                        "sourceRepositoryURI": repository_uri,
                        "sourceRepositoryIdentifier": repository_id,
                        "sourceRepositoryRef": source_ref,
                        "buildSignerURI": signer_uri,
                        "buildSignerDigest": "a" * 40,
                        "runnerEnvironment": runner_environment,
                        "issuer": issuer,
                        "subjectAlternativeName": signer_uri,
                    }
                }
            },
        }
    ]


def _policy() -> GitHubAttestationIdentityPolicy:
    return GitHubAttestationIdentityPolicy(
        repository=REPOSITORY,
        repository_id=REPOSITORY_ID,
        signer_workflow_path=WORKFLOW,
        source_ref=REF,
        deployment_environment=ENVIRONMENT,
    )


def test_authorizes_exact_verified_identity() -> None:
    result = authorize_verified_github_attestation(_payload(), policy=_policy())
    assert result.authorized is True
    assert result.reason == "github_attestation_identity_authorized"
    assert result.claims is not None
    assert result.claims.deployment_environment == ENVIRONMENT


def test_rejects_different_repository() -> None:
    result = authorize_verified_github_attestation(
        _payload(repository_uri="https://github.com/other/Liminal"), policy=_policy()
    )
    assert result.authorized is False
    assert result.reason == "source_repository_not_authorized"


def test_rejects_repository_id_mismatch() -> None:
    result = authorize_verified_github_attestation(_payload(repository_id="999"), policy=_policy())
    assert result.authorized is False
    assert result.reason == "source_repository_id_not_authorized"


def test_rejects_other_ref() -> None:
    result = authorize_verified_github_attestation(
        _payload(source_ref="refs/heads/main"), policy=_policy()
    )
    assert result.authorized is False
    assert result.reason == "source_ref_not_authorized"


def test_rejects_other_signer_workflow() -> None:
    other = f"https://github.com/{REPOSITORY}/.github/workflows/other.yml@{REF}"
    result = authorize_verified_github_attestation(_payload(signer_uri=other), policy=_policy())
    assert result.authorized is False
    assert result.reason == "signer_workflow_not_authorized"


def test_rejects_other_deployment_environment() -> None:
    result = authorize_verified_github_attestation(
        _payload(environment="production"), policy=_policy()
    )
    assert result.authorized is False
    assert result.reason == "deployment_environment_not_authorized"


def test_rejects_self_hosted_runner() -> None:
    result = authorize_verified_github_attestation(
        _payload(runner_environment="self-hosted"), policy=_policy()
    )
    assert result.authorized is False
    assert result.reason == "runner_environment_not_authorized"


def test_rejects_other_oidc_issuer() -> None:
    result = authorize_verified_github_attestation(
        _payload(issuer="https://example.invalid"), policy=_policy()
    )
    assert result.authorized is False
    assert result.reason == "oidc_issuer_not_authorized"


def test_missing_environment_extension_fails_closed() -> None:
    result = authorize_verified_github_attestation(_payload(environment=None), policy=_policy())
    assert result.authorized is False
    assert result.reason == "github_attestation_deployment_environment_unavailable"


def test_ambiguous_multiple_verified_results_fail_closed() -> None:
    payload = _payload() + _payload()
    result = authorize_verified_github_attestation(payload, policy=_policy())
    assert result.authorized is False
    assert result.reason == "github_attestation_exactly_one_verified_result_required"
