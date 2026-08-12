from liminal.evidence_manifest import (
    EvidenceManifest,
    EvidenceManifestEntry,
    ManifestCandidate,
    VerificationExpectation,
)
from liminal.evidence_topology_portability import (
    TopologyObservation,
    VerifiedBundleInputs,
    evaluate_topology_portability,
)


LOGICAL_ID = "trust-consumer-checkpoint:generation-1"
PRODUCER = "safal207/Liminal"
SIGNER = (
    "safal207/Liminal/.github/workflows/"
    "trusted-recovery-trust-consumer-checkpoint-attested.yml"
)
SIGNER_SHA = "f31b56a5e21a668bcb98791b05542652760dcc27"
EVIDENCE_SHA = "a" * 64
MANIFEST_SHA = "b" * 64
MANIFEST_VERIFY_SHA = "c" * 64
EVIDENCE_VERIFY_SHA = "d" * 64


def manifest() -> EvidenceManifest:
    return EvidenceManifest(
        entries=(
            EvidenceManifestEntry(
                logical_id=LOGICAL_ID,
                producer=PRODUCER,
                evidence_type="trusted-recovery-consumer-checkpoint",
                relative_locator="checkpoint-generation-1.json",
                sha256=EVIDENCE_SHA,
                generation=1,
                verification=VerificationExpectation(
                    required=True,
                    verifier="github_attestation",
                    expected_signer=SIGNER,
                    expected_signer_digest=SIGNER_SHA,
                ),
            ),
        )
    )


def verified() -> VerifiedBundleInputs:
    return VerifiedBundleInputs(
        manifest_sha256=MANIFEST_SHA,
        manifest_verification_json_sha256=MANIFEST_VERIFY_SHA,
        manifest_signer_workflow=SIGNER,
        manifest_signer_digest=SIGNER_SHA,
        evidence_verification_json_sha256=EVIDENCE_VERIFY_SHA,
        evidence_signer_workflow=SIGNER,
        evidence_signer_digest=SIGNER_SHA,
    )


def topology_a() -> TopologyObservation:
    return TopologyObservation(
        name="flat",
        expected_path="flat/checkpoint-generation-1.json",
        allowed_prefixes=("flat/",),
        candidates=(
            ManifestCandidate(
                path="flat/checkpoint-generation-1.json",
                sha256=EVIDENCE_SHA,
                verification_available=True,
            ),
        ),
    )


def topology_b(*, sha256: str = EVIDENCE_SHA) -> TopologyObservation:
    return TopologyObservation(
        name="deep-renamed",
        expected_path="deep/checkpoint-generation-1.json",
        allowed_prefixes=("deep/transport/layers/",),
        candidates=(
            ManifestCandidate(
                path="deep/transport/layers/opaque-blob.dat",
                sha256=sha256,
                verification_available=True,
            ),
        ),
    )


def test_distinct_topologies_produce_same_bundle_and_trust_decision() -> None:
    verdict = evaluate_topology_portability(
        manifest(),
        logical_id=LOGICAL_ID,
        generation=1,
        left=topology_a(),
        right=topology_b(),
        verified=verified(),
    )

    assert verdict.portable is True
    assert verdict.reason == "topology_portability_verified"
    assert verdict.left.resolved_path == "flat/checkpoint-generation-1.json"
    assert verdict.left.resolution_reason == "expected_locator_resolved"
    assert verdict.right.resolved_path == "deep/transport/layers/opaque-blob.dat"
    assert verdict.right.resolution_reason == "reanchor_resolved"
    assert verdict.left.bundle_sha256 == verdict.right.bundle_sha256
    assert verdict.left.authorization_reason == "verified_recovery"
    assert verdict.right.authorization_reason == "verified_recovery"


def test_digest_drift_in_second_topology_fails_closed() -> None:
    verdict = evaluate_topology_portability(
        manifest(),
        logical_id=LOGICAL_ID,
        generation=1,
        left=topology_a(),
        right=topology_b(sha256="0" * 64),
        verified=verified(),
    )

    assert verdict.portable is False
    assert verdict.reason == "right_topology_not_authorized"
    assert verdict.right.resolution_reason == "manifest_digest_mismatch"


def test_ambiguous_second_topology_fails_closed() -> None:
    second = topology_b()
    ambiguous = TopologyObservation(
        name=second.name,
        expected_path=second.expected_path,
        allowed_prefixes=second.allowed_prefixes,
        candidates=second.candidates
        + (
            ManifestCandidate(
                path="deep/transport/layers/duplicate.bin",
                sha256=EVIDENCE_SHA,
                verification_available=True,
            ),
        ),
    )

    verdict = evaluate_topology_portability(
        manifest(),
        logical_id=LOGICAL_ID,
        generation=1,
        left=topology_a(),
        right=ambiguous,
        verified=verified(),
    )

    assert verdict.portable is False
    assert verdict.reason == "right_topology_not_authorized"
    assert verdict.right.resolution_reason == "ambiguous_digest_matches"


def test_verification_failure_prevents_portable_authorization() -> None:
    failed = verified()
    failed = VerifiedBundleInputs(
        **{
            **failed.__dict__,
            "evidence_verification_succeeded": False,
        }
    )

    verdict = evaluate_topology_portability(
        manifest(),
        logical_id=LOGICAL_ID,
        generation=1,
        left=topology_a(),
        right=topology_b(),
        verified=failed,
    )

    assert verdict.portable is False
    assert verdict.reason == "left_topology_not_authorized"
    assert verdict.left.authorization_reason == "verification_failed"
    assert verdict.right.authorization_reason == "verification_failed"


def test_same_physical_path_is_not_accepted_as_portability_proof() -> None:
    left = topology_a()
    right = TopologyObservation(
        name="alias",
        expected_path=left.expected_path,
        allowed_prefixes=left.allowed_prefixes,
        candidates=left.candidates,
    )

    verdict = evaluate_topology_portability(
        manifest(),
        logical_id=LOGICAL_ID,
        generation=1,
        left=left,
        right=right,
        verified=verified(),
    )

    assert verdict.portable is False
    assert verdict.reason == "physical_topologies_not_distinct"


def test_second_topology_outside_allowed_field_fails_closed() -> None:
    outside = TopologyObservation(
        name="outside",
        expected_path="deep/checkpoint-generation-1.json",
        allowed_prefixes=("deep/transport/layers/",),
        candidates=(
            ManifestCandidate(
                path="deep/unbounded/opaque-blob.dat",
                sha256=EVIDENCE_SHA,
                verification_available=True,
            ),
        ),
    )

    verdict = evaluate_topology_portability(
        manifest(),
        logical_id=LOGICAL_ID,
        generation=1,
        left=topology_a(),
        right=outside,
        verified=verified(),
    )

    assert verdict.portable is False
    assert verdict.reason == "right_topology_not_authorized"
    assert verdict.right.resolution_reason == "evidence_not_found"
