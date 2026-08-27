import hashlib

import pytest

from liminal.verification_receipt import (
    build_normalized_verification_receipt,
    canonical_verification_receipt_bytes,
    parse_verification_receipt_bytes,
    verification_receipt_sha256,
)


SUBJECT_SHA = "a" * 64
SIGNER_SHA = "b" * 40
SIGNER_WORKFLOW = (
    "safal207/Liminal/.github/workflows/"
    "trusted-recovery-trust-consumer-checkpoint-attested.yml"
)
SOURCE_REF = "refs/heads/agent/recovery-routing-v0-1"


def build(**overrides):
    values = dict(
        verification_scheme="github_attestation",
        subject_sha256=SUBJECT_SHA,
        repository="safal207/Liminal",
        signer_workflow=SIGNER_WORKFLOW,
        signer_digest=SIGNER_SHA,
        source_ref=SOURCE_REF,
        deny_self_hosted_runners=True,
        verification_succeeded=True,
    )
    values.update(overrides)
    return build_normalized_verification_receipt(**values)


def test_receipt_is_deterministic_and_round_trips():
    receipt = build()
    raw = canonical_verification_receipt_bytes(receipt)

    assert verification_receipt_sha256(receipt) == hashlib.sha256(raw).hexdigest()
    assert parse_verification_receipt_bytes(raw) == receipt


def test_different_raw_verifier_outputs_can_share_one_semantic_receipt():
    raw_verifier_a = b'{"verified":true,"implementation":"gh-cli-a","timestamp":"1"}\n'
    raw_verifier_b = b'{"timestamp":"2","implementation":"other-verifier","verified":true}\n'

    assert hashlib.sha256(raw_verifier_a).hexdigest() != hashlib.sha256(
        raw_verifier_b
    ).hexdigest()

    receipt_a = build()
    receipt_b = build()

    assert canonical_verification_receipt_bytes(receipt_a) == canonical_verification_receipt_bytes(
        receipt_b
    )
    assert verification_receipt_sha256(receipt_a) == verification_receipt_sha256(receipt_b)


def test_physical_path_and_filename_are_not_receipt_identity():
    receipt = build()
    raw = canonical_verification_receipt_bytes(receipt)

    assert b"checkpoint-generation-1.json" not in raw
    assert b"opaque-blob.dat" not in raw
    assert b"topology-a" not in raw
    assert b"topology-b" not in raw


def test_signer_digest_change_changes_receipt_identity():
    baseline = build()
    rotated = build(signer_digest="c" * 40)

    assert verification_receipt_sha256(baseline) != verification_receipt_sha256(rotated)


def test_source_ref_change_changes_receipt_identity():
    baseline = build()
    other_ref = build(source_ref="refs/heads/other")

    assert verification_receipt_sha256(baseline) != verification_receipt_sha256(other_ref)


def test_failed_verification_is_recordable_but_not_equivalent_to_success():
    success = build()
    failed = build(verification_succeeded=False)

    assert success.verified is True
    assert failed.verified is False
    assert verification_receipt_sha256(success) != verification_receipt_sha256(failed)


def test_policy_change_changes_receipt_identity():
    strict = build()
    relaxed = build(deny_self_hosted_runners=False)

    assert verification_receipt_sha256(strict) != verification_receipt_sha256(relaxed)


def test_rejects_non_git_signer_digest():
    with pytest.raises(ValueError, match="signer_digest_must_be_lowercase_git_sha"):
        build(signer_digest="d" * 64)


def test_rejects_non_fully_qualified_source_ref():
    with pytest.raises(ValueError, match="source_ref_must_be_fully_qualified_ref"):
        build(source_ref="agent/recovery-routing-v0-1")


def test_rejects_invalid_repository_shape():
    with pytest.raises(ValueError, match="repository_must_be_owner_slash_name"):
        build(repository="safal207")


def test_parser_rejects_unsupported_schema():
    raw = canonical_verification_receipt_bytes(build()).replace(
        b"liminal-verification-receipt/v0.1",
        b"liminal-verification-receipt/v9.9",
    )

    with pytest.raises(ValueError, match="unsupported_verification_receipt_schema"):
        parse_verification_receipt_bytes(raw)
