import hashlib

from liminal.evidence_manifest import (
    EvidenceManifest,
    EvidenceManifestEntry,
    ManifestCandidate,
    ManifestDisposition,
    ManifestReason,
    VerificationExpectation,
    canonical_manifest_bytes,
    resolve_manifest_evidence,
    validate_manifest,
)


LOGICAL_ID = "trust-consumer-checkpoint:generation-1"
DIGEST = hashlib.sha256(b"checkpoint-generation-1\n").hexdigest()
OTHER_DIGEST = hashlib.sha256(b"different-bytes\n").hexdigest()


def _entry(
    *,
    sha256: str = DIGEST,
    relative_locator: str = "artifacts/checkpoint-generation-1.json",
) -> EvidenceManifestEntry:
    return EvidenceManifestEntry(
        logical_id=LOGICAL_ID,
        producer="trusted-recovery-trust-consumer-checkpoint-attested",
        evidence_type="checkpoint",
        relative_locator=relative_locator,
        sha256=sha256,
        generation=1,
        verification=VerificationExpectation(
            required=True,
            verifier="github_attestation",
            expected_signer=(
                "safal207/Liminal/.github/workflows/"
                "trusted-recovery-trust-consumer-checkpoint-attested.yml"
            ),
            expected_signer_digest=(
                "d0688725bd76fdf7221e84ca7c5bfb51e363ff72"
            ),
        ),
    )


def _manifest(*entries: EvidenceManifestEntry) -> EvidenceManifest:
    return EvidenceManifest(entries=tuple(entries))


def test_manifest_resolves_same_bytes_at_drifted_physical_path() -> None:
    result = resolve_manifest_evidence(
        [_manifest(_entry())],
        logical_id=LOGICAL_ID,
        generation=1,
        candidates=[
            ManifestCandidate(
                path=(
                    "checkpoint-evidence/artifacts/"
                    "trust-consumer-checkpoint-attested/"
                    "checkpoint-generation-1.json"
                ),
                sha256=DIGEST,
                verification_available=True,
            )
        ],
    )

    assert result.disposition is ManifestDisposition.RESOLVED
    assert result.reason == ManifestReason.MANIFEST_EVIDENCE_RESOLVED.value
    assert result.locator is not None
    assert result.locator.logical_id == LOGICAL_ID
    assert result.locator.verification_available is True
    assert result.locator.path.endswith("checkpoint-generation-1.json")


def test_manifest_digest_mismatch_defers_fail_closed() -> None:
    result = resolve_manifest_evidence(
        [_manifest(_entry())],
        logical_id=LOGICAL_ID,
        generation=1,
        candidates=[
            ManifestCandidate(
                path="downloaded/checkpoint-generation-1.json",
                sha256=OTHER_DIGEST,
            )
        ],
    )

    assert result.disposition is ManifestDisposition.DEFER
    assert result.reason == ManifestReason.MANIFEST_DIGEST_MISMATCH.value
    assert result.locator is None


def test_two_physical_paths_with_same_digest_are_ambiguous() -> None:
    result = resolve_manifest_evidence(
        [_manifest(_entry())],
        logical_id=LOGICAL_ID,
        generation=1,
        candidates=[
            ManifestCandidate(path="download/a/checkpoint.json", sha256=DIGEST),
            ManifestCandidate(path="download/b/checkpoint.json", sha256=DIGEST),
        ],
    )

    assert result.disposition is ManifestDisposition.DEFER
    assert result.reason == ManifestReason.AMBIGUOUS_DIGEST_MATCHES.value


def test_distinct_manifest_entries_for_same_identity_are_ambiguous() -> None:
    first = _manifest(_entry())
    second = _manifest(_entry(sha256=OTHER_DIGEST))

    result = resolve_manifest_evidence(
        [first, second],
        logical_id=LOGICAL_ID,
        generation=1,
        candidates=[ManifestCandidate(path="download/checkpoint.json", sha256=DIGEST)],
    )

    assert result.disposition is ManifestDisposition.DEFER
    assert result.reason == ManifestReason.AMBIGUOUS_MANIFEST_ENTRIES.value


def test_conflicting_duplicate_logical_id_generation_inside_manifest_is_rejected() -> None:
    manifest = _manifest(_entry(), _entry(sha256=OTHER_DIGEST))

    try:
        validate_manifest(manifest)
    except ValueError as exc:
        assert str(exc) == "conflicting_manifest_logical_id_generation"
    else:
        raise AssertionError("expected ValueError")


def test_invalid_sha256_is_rejected() -> None:
    manifest = _manifest(_entry(sha256="abc"))

    try:
        validate_manifest(manifest)
    except ValueError as exc:
        assert str(exc) == "manifest_sha256_must_be_lowercase_sha256"
    else:
        raise AssertionError("expected ValueError")


def test_absolute_and_parent_traversal_locators_are_rejected() -> None:
    for locator, expected_error in (
        ("/tmp/checkpoint.json", "manifest_locator_must_be_relative"),
        ("artifact/../checkpoint.json", "manifest_locator_parent_traversal_not_allowed"),
    ):
        try:
            validate_manifest(_manifest(_entry(relative_locator=locator)))
        except ValueError as exc:
            assert str(exc) == expected_error
        else:
            raise AssertionError("expected ValueError")


def test_required_manifest_fields_are_rejected_when_missing() -> None:
    invalid = EvidenceManifestEntry(
        logical_id="",
        producer="producer",
        evidence_type="checkpoint",
        relative_locator="checkpoint.json",
        sha256=DIGEST,
        generation=1,
    )

    try:
        validate_manifest(_manifest(invalid))
    except ValueError as exc:
        assert str(exc) == "manifest_logical_id_must_be_non_empty"
    else:
        raise AssertionError("expected ValueError")


def test_canonical_manifest_bytes_are_order_independent() -> None:
    generation_zero = EvidenceManifestEntry(
        logical_id="trust-consumer-checkpoint:generation-0",
        producer="producer",
        evidence_type="checkpoint",
        relative_locator="checkpoint-generation-0.json",
        sha256=OTHER_DIGEST,
        generation=0,
    )
    generation_one = _entry()

    left = canonical_manifest_bytes(_manifest(generation_one, generation_zero))
    right = canonical_manifest_bytes(_manifest(generation_zero, generation_one))

    assert left == right
    assert left.endswith(b"\n")


def test_missing_manifest_entry_defers() -> None:
    result = resolve_manifest_evidence(
        [_manifest(_entry())],
        logical_id="other:evidence",
        generation=1,
        candidates=[ManifestCandidate(path="download/checkpoint.json", sha256=DIGEST)],
    )

    assert result.disposition is ManifestDisposition.DEFER
    assert result.reason == ManifestReason.MANIFEST_ENTRY_NOT_FOUND.value
