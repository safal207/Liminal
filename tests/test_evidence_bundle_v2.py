import hashlib

import pytest

from liminal.evidence_bundle_v2 import (
    ReceiptBackedEvidenceBundle,
    canonical_receipt_backed_evidence_bundle_bytes,
    build_receipt_backed_evidence_bundle,
    parse_receipt_backed_evidence_bundle_bytes,
    receipt_backed_evidence_bundle_sha256,
)
from liminal.evidence_manifest import (
    EvidenceManifest,
    EvidenceManifestEntry,
    VerificationExpectation,
    canonical_manifest_bytes,
)
from liminal.verification_receipt import build_normalized_verification_receipt


PRODUCER = "safal207/Liminal"
SIGNER = (
    "safal207/Liminal/.github/workflows/"
    "trusted-recovery-trust-consumer-checkpoint-attested.yml"
)
SIGNER_DIGEST = "a" * 40
MANIFEST_SIGNER = SIGNER
MANIFEST_SIGNER_DIGEST = "b" * 40
EVIDENCE_SHA = "c" * 64
SOURCE_REF = "refs/heads/agent/recovery-routing-v0-1"


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


def manifest_receipt(*, verified: bool = True, signer_digest: str = MANIFEST_SIGNER_DIGEST):
    document = manifest()
    manifest_sha = hashlib.sha256(canonical_manifest_bytes(document)).hexdigest()
    return build_normalized_verification_receipt(
        verification_scheme="github_attestation",
        subject_sha256=manifest_sha,
        repository=PRODUCER,
        signer_workflow=MANIFEST_SIGNER,
        signer_digest=signer_digest,
        source_ref=SOURCE_REF,
        deny_self_hosted_runners=True,
        verification_succeeded=verified,
    )


def evidence_receipt(
    *,
    verified: bool = True,
    subject_sha256: str = EVIDENCE_SHA,
    signer_workflow: str = SIGNER,
    signer_digest: str = SIGNER_DIGEST,
    verification_scheme: str = "github_attestation",
):
    return build_normalized_verification_receipt(
        verification_scheme=verification_scheme,
        subject_sha256=subject_sha256,
        repository=PRODUCER,
        signer_workflow=signer_workflow,
        signer_digest=signer_digest,
        source_ref=SOURCE_REF,
        deny_self_hosted_runners=True,
        verification_succeeded=verified,
    )


def build(**overrides) -> ReceiptBackedEvidenceBundle:
    values = dict(
        manifest_document=manifest(),
        logical_id="trust-consumer-checkpoint:generation-1",
        generation=1,
        manifest_receipt=manifest_receipt(),
        evidence_receipt=evidence_receipt(),
    )
    values.update(overrides)
    return build_receipt_backed_evidence_bundle(**values)


def test_v2_bundle_is_deterministic_path_independent_and_round_trips():
    bundle = build()
    raw = canonical_receipt_backed_evidence_bundle_bytes(bundle)

    assert b"verification_json_sha256" not in raw
    assert b"checkpoint-generation-1.json" not in raw
    assert b"relative_locator" not in raw
    assert receipt_backed_evidence_bundle_sha256(bundle) == hashlib.sha256(raw).hexdigest()
    assert parse_receipt_backed_evidence_bundle_bytes(raw) == bundle


def test_raw_verifier_representation_is_not_part_of_v2_bundle_identity():
    raw_verifier_a = b'{"verified":true,"implementation":"a"}\n'
    raw_verifier_b = b'{"implementation":"b","verified":true,"timestamp":"later"}\n'
    assert hashlib.sha256(raw_verifier_a).hexdigest() != hashlib.sha256(
        raw_verifier_b
    ).hexdigest()

    left = build()
    right = build()

    assert canonical_receipt_backed_evidence_bundle_bytes(
        left
    ) == canonical_receipt_backed_evidence_bundle_bytes(right)


def test_bundle_rejects_unverified_manifest_receipt():
    with pytest.raises(
        ValueError,
        match="manifest_verified_receipt_required_for_evidence_bundle",
    ):
        build(manifest_receipt=manifest_receipt(verified=False))


def test_bundle_rejects_unverified_evidence_receipt():
    with pytest.raises(
        ValueError,
        match="evidence_verified_receipt_required_for_evidence_bundle",
    ):
        build(evidence_receipt=evidence_receipt(verified=False))


def test_bundle_rejects_manifest_receipt_subject_mismatch():
    wrong_manifest_receipt = build_normalized_verification_receipt(
        verification_scheme="github_attestation",
        subject_sha256="d" * 64,
        repository=PRODUCER,
        signer_workflow=MANIFEST_SIGNER,
        signer_digest=MANIFEST_SIGNER_DIGEST,
        source_ref=SOURCE_REF,
        deny_self_hosted_runners=True,
        verification_succeeded=True,
    )
    with pytest.raises(
        ValueError,
        match="evidence_bundle_manifest_receipt_subject_mismatch",
    ):
        build(manifest_receipt=wrong_manifest_receipt)


def test_bundle_rejects_evidence_digest_mismatch():
    with pytest.raises(ValueError, match="evidence_bundle_manifest_digest_mismatch"):
        build(evidence_receipt=evidence_receipt(subject_sha256="e" * 64))


def test_bundle_rejects_verification_scheme_mismatch():
    with pytest.raises(
        ValueError,
        match="evidence_bundle_verification_scheme_mismatch",
    ):
        build(evidence_receipt=evidence_receipt(verification_scheme="other_scheme"))


def test_bundle_rejects_signer_workflow_mismatch():
    with pytest.raises(
        ValueError,
        match="evidence_bundle_evidence_signer_workflow_mismatch",
    ):
        build(
            evidence_receipt=evidence_receipt(
                signer_workflow="safal207/Liminal/.github/workflows/other.yml"
            )
        )


def test_bundle_rejects_signer_digest_mismatch():
    with pytest.raises(
        ValueError,
        match="evidence_bundle_evidence_signer_digest_mismatch",
    ):
        build(evidence_receipt=evidence_receipt(signer_digest="f" * 40))


def test_tampered_receipt_digest_fails_closed_on_validation():
    bundle = build()
    tampered = ReceiptBackedEvidenceBundle(
        logical_id=bundle.logical_id,
        generation=bundle.generation,
        producer=bundle.producer,
        evidence_type=bundle.evidence_type,
        manifest=bundle.manifest,
        evidence=type(bundle.evidence)(
            sha256=bundle.evidence.sha256,
            verification_receipt_sha256="0" * 64,
            verification_scheme=bundle.evidence.verification_scheme,
            repository=bundle.evidence.repository,
            signer_workflow=bundle.evidence.signer_workflow,
            signer_digest=bundle.evidence.signer_digest,
            source_ref=bundle.evidence.source_ref,
            deny_self_hosted_runners=bundle.evidence.deny_self_hosted_runners,
        ),
    )

    with pytest.raises(
        ValueError,
        match="evidence_verification_receipt_digest_mismatch",
    ):
        canonical_receipt_backed_evidence_bundle_bytes(tampered)
