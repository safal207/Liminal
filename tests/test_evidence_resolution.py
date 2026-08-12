from liminal.evidence_resolution import (
    EvidenceLocator,
    ResolutionDisposition,
    ResolutionNode,
    ResolutionReason,
    resolve_evidence,
)


CHECKPOINT_ID = "trust-consumer-checkpoint:generation-1"
EXPECTED = "checkpoint-evidence/checkpoint-generation-1.json"
NESTED = (
    "checkpoint-evidence/artifacts/trust-consumer-checkpoint-attested/"
    "checkpoint-generation-1.json"
)
ALLOWED = ("checkpoint-evidence/artifacts/trust-consumer-checkpoint-attested/",)


def test_expected_path_resolves_without_reanchor() -> None:
    result = resolve_evidence(
        ResolutionNode(
            logical_id=CHECKPOINT_ID,
            expected_path=EXPECTED,
            allowed_prefixes=ALLOWED,
            require_verified=True,
        ),
        [
            EvidenceLocator(
                logical_id=CHECKPOINT_ID,
                path=EXPECTED,
                verification_available=True,
            )
        ],
    )

    assert result.disposition is ResolutionDisposition.RESOLVED
    assert result.reason == ResolutionReason.EXPECTED_LOCATOR_RESOLVED.value
    assert result.resolved_path == EXPECTED
    assert result.verified is True
    assert result.reanchor is None


def test_missing_expected_path_reanchors_to_one_admissible_nested_candidate() -> None:
    result = resolve_evidence(
        ResolutionNode(
            logical_id=CHECKPOINT_ID,
            expected_path=EXPECTED,
            allowed_prefixes=ALLOWED,
            require_verified=True,
        ),
        [
            EvidenceLocator(
                logical_id=CHECKPOINT_ID,
                path=NESTED,
                verification_available=True,
            )
        ],
    )

    assert result.disposition is ResolutionDisposition.RESOLVED
    assert result.reason == ResolutionReason.VERIFIED_REANCHOR.value
    assert result.resolved_path == NESTED
    assert result.reanchor is not None
    assert result.reanchor.from_path == EXPECTED
    assert result.reanchor.to_path == NESTED


def test_multiple_admissible_candidates_defer_fail_closed() -> None:
    result = resolve_evidence(
        ResolutionNode(
            logical_id=CHECKPOINT_ID,
            expected_path=EXPECTED,
            allowed_prefixes=ALLOWED,
            require_verified=True,
        ),
        [
            EvidenceLocator(
                logical_id=CHECKPOINT_ID,
                path=NESTED,
                verification_available=True,
            ),
            EvidenceLocator(
                logical_id=CHECKPOINT_ID,
                path=(
                    "checkpoint-evidence/artifacts/"
                    "trust-consumer-checkpoint-attested/copy/"
                    "checkpoint-generation-1.json"
                ),
                verification_available=True,
            ),
        ],
    )

    assert result.disposition is ResolutionDisposition.DEFER
    assert result.reason == ResolutionReason.AMBIGUOUS_EVIDENCE_CANDIDATES.value
    assert result.authorized is False
    assert result.resolved_path is None


def test_required_verification_unavailable_defers_instead_of_reanchoring() -> None:
    result = resolve_evidence(
        ResolutionNode(
            logical_id=CHECKPOINT_ID,
            expected_path=EXPECTED,
            allowed_prefixes=ALLOWED,
            require_verified=True,
        ),
        [
            EvidenceLocator(
                logical_id=CHECKPOINT_ID,
                path=NESTED,
                verification_available=False,
            )
        ],
    )

    assert result.disposition is ResolutionDisposition.DEFER
    assert result.reason == ResolutionReason.VERIFIED_EVIDENCE_REQUIRED.value
    assert result.authorized is False


def test_verified_recovery_ignores_unrelated_logical_evidence() -> None:
    result = resolve_evidence(
        ResolutionNode(
            logical_id=CHECKPOINT_ID,
            expected_path=EXPECTED,
            allowed_prefixes=ALLOWED,
            require_verified=True,
        ),
        [
            EvidenceLocator(
                logical_id="trust-consumer-checkpoint:generation-0",
                path=EXPECTED,
                verification_available=True,
            ),
            EvidenceLocator(
                logical_id=CHECKPOINT_ID,
                path=NESTED,
                verification_available=True,
            ),
        ],
    )

    assert result.authorized is True
    assert result.verified is True
    assert result.reason == ResolutionReason.VERIFIED_REANCHOR.value
    assert result.resolved_path == NESTED
