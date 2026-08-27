import hashlib

import pytest

from liminal.evidence_bundle import (
    build_verified_evidence_bundle,
    canonical_evidence_bundle_bytes,
    evidence_bundle_sha256,
    parse_evidence_bundle_bytes,
)
from liminal.evidence_manifest import (
    EvidenceManifest,
    EvidenceManifestEntry,
    VerificationExpectation,
)


PRODUCER = "safal207/Liminal"
SIGNER = (
    "safal207/Liminal/.github/workflows/"
    "trusted-recovery-trust-consumer-checkpoint-attested.yml"
)
SIGNER_DIGEST = "a" * 40
MANIFEST_SIGNER = SIGNER
MANIFEST_SIGNER_DIGEST = "b" * 40
EVIDENCE_SHA = "c" * 64
MANIFEST_SHA = "d" * 64
MANIFEST_VERIFY_SHA = "e" * 64
EVIDENCE_VERIFY_SHA = "f" * 64


def manifest() -> EvidenceManifest:
    return EvidenceManifest(
        entries=(
            EvidenceManifestEntry(
                logical_id="trust-consumer-checkpoint:generation-1",
                producer=PRODUCER,
                evidence_type="trusted-recovery-consumer-checkpoint",
                relative_locator="checkpoint-generation-1.json",
                sha256=EVIDENCE_SHA,
                generation=1,
                verification=VerificationExpectation(
                    required=True,
                    verifier="github_attestation",
                    expected_signer=SIGNER,
                    expected_signer_digest=SIGNER_DIGEST,
                ),
            ),
        )
    )


def build(**overrides):
    values = dict(
        manifest_document=manifest(),
        logical_id="trust-consumer-checkpoint:generation-1",
        generation=1,
        manifest_sha256=MANIFEST_SHA,
        manifest_verification_json_sha256=MANIFEST_VERIFY_SHA,
        manifest_signer_workflow=MANIFEST_SIGNER,
        manifest_signer_digest=MANIFEST_SIGNER_DIGEST,
        evidence_sha256=EVIDENCE_SHA,
        evidence_verification_json_sha256=EVIDENCE_VERIFY_SHA,
        evidence_signer_workflow=SIGNER,
        evidence_signer_digest=SIGNER_DIGEST,
        manifest_verification_succeeded=True,
        evidence_verification_succeeded=True,
    )
    values.update(overrides)
    return build_verified_evidence_bundle(**values)


def test_build_verified_bundle_is_path_independent_and_deterministic():
    bundle = build()
    raw = canonical_evidence_bundle_bytes(bundle)

    assert b"checkpoint-generation-1.json" not in raw
    assert b"relative_locator" not in raw
    assert bundle.manifest.sha256 == MANIFEST_SHA
    assert bundle.evidence.sha256 == EVIDENCE_SHA
    assert evidence_bundle_sha256(bundle) == hashlib.sha256(raw).hexdigest()
    assert parse_evidence_bundle_bytes(raw) == bundle


def test_bundle_requires_manifest_verification():
    with pytest.raises(
        ValueError, match="manifest_verification_required_for_evidence_bundle"
    ):
        build(manifest_verification_succeeded=False)


def test_bundle_requires_evidence_verification():
    with pytest.raises(
        ValueError, match="evidence_verification_required_for_evidence_bundle"
    ):
        build(evidence_verification_succeeded=False)


def test_bundle_rejects_manifest_evidence_digest_mismatch():
    with pytest.raises(ValueError, match="evidence_bundle_manifest_digest_mismatch"):
        build(evidence_sha256="1" * 64)


def test_bundle_rejects_evidence_signer_workflow_mismatch():
    with pytest.raises(
        ValueError, match="evidence_bundle_evidence_signer_workflow_mismatch"
    ):
        build(evidence_signer_workflow="safal207/Liminal/.github/workflows/other.yml")


def test_bundle_rejects_evidence_signer_digest_mismatch():
    with pytest.raises(
        ValueError, match="evidence_bundle_evidence_signer_digest_mismatch"
    ):
        build(evidence_signer_digest="2" * 40)


def test_bundle_rejects_non_git_signer_digest_shape():
    with pytest.raises(ValueError, match="manifest_signer_digest_must_be_lowercase_git_sha"):
        build(manifest_signer_digest="3" * 64)


def test_parser_rejects_unsupported_schema():
    raw = canonical_evidence_bundle_bytes(build()).replace(
        b"liminal-evidence-bundle/v0.1", b"liminal-evidence-bundle/v9.9"
    )
    with pytest.raises(ValueError, match="unsupported_evidence_bundle_schema"):
        parse_evidence_bundle_bytes(raw)
