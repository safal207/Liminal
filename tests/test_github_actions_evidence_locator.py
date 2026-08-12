from liminal.evidence_resolution import (
    ResolutionDisposition,
    ResolutionNode,
    ResolutionReason,
    confirm_verified_recovery,
    resolve_evidence,
)
from liminal.github_actions_evidence_locator import (
    GitHubActionsEvidenceSpec,
    discover_github_actions_evidence_locators,
)


CHECKPOINT_ID = "trust-consumer-checkpoint:generation-1"
EXPECTED = "checkpoint-evidence/checkpoint-generation-1.json"
NESTED = (
    "checkpoint-evidence/artifacts/trust-consumer-checkpoint-attested/"
    "checkpoint-generation-1.json"
)
ALLOWED = ("checkpoint-evidence/artifacts/trust-consumer-checkpoint-attested/",)


def _spec(*, verification_available: bool = True) -> GitHubActionsEvidenceSpec:
    return GitHubActionsEvidenceSpec(
        logical_id=CHECKPOINT_ID,
        expected_path=EXPECTED,
        filename="checkpoint-generation-1.json",
        allowed_prefixes=ALLOWED,
        verification_available=verification_available,
    )


def _node() -> ResolutionNode:
    return ResolutionNode(
        logical_id=CHECKPOINT_ID,
        expected_path=EXPECTED,
        allowed_prefixes=ALLOWED,
        require_verification=True,
    )


def test_discovers_real_nested_checkpoint_topology_and_reanchors() -> None:
    locators = discover_github_actions_evidence_locators(
        _spec(),
        [
            "checkpoint-evidence/checkpoint-attestation-verification.json",
            NESTED,
        ],
    )

    assert [locator.path for locator in locators] == [NESTED]

    resolution = resolve_evidence(_node(), locators)
    assert resolution.disposition is ResolutionDisposition.RESOLVED
    assert resolution.reason == ResolutionReason.REANCHOR_RESOLVED.value
    assert resolution.resolved_path == NESTED

    verified = confirm_verified_recovery(
        resolution,
        verification_succeeded=True,
    )
    assert verified.authorized is True
    assert verified.reason == ResolutionReason.VERIFIED_RECOVERY.value


def test_expected_path_wins_when_present() -> None:
    locators = discover_github_actions_evidence_locators(
        _spec(),
        [EXPECTED, "checkpoint-evidence/unrelated.json"],
    )

    resolution = resolve_evidence(_node(), locators)
    assert resolution.reason == ResolutionReason.EXPECTED_LOCATOR_RESOLVED.value
    assert resolution.resolved_path == EXPECTED
    assert resolution.reanchor is None


def test_multiple_admissible_nested_matches_are_preserved_for_fail_closed_resolution() -> None:
    second = (
        "checkpoint-evidence/artifacts/trust-consumer-checkpoint-attested/copy/"
        "checkpoint-generation-1.json"
    )
    locators = discover_github_actions_evidence_locators(
        _spec(),
        [NESTED, second],
    )

    assert len(locators) == 2
    resolution = resolve_evidence(_node(), locators)
    assert resolution.disposition is ResolutionDisposition.DEFER
    assert resolution.reason == ResolutionReason.AMBIGUOUS_EVIDENCE_CANDIDATES.value


def test_out_of_bound_same_filename_is_not_a_candidate() -> None:
    locators = discover_github_actions_evidence_locators(
        _spec(),
        [
            "checkpoint-evidence/other-producer/checkpoint-generation-1.json",
            NESTED,
        ],
    )

    assert [locator.path for locator in locators] == [NESTED]


def test_duplicate_paths_collapse_deterministically() -> None:
    locators = discover_github_actions_evidence_locators(
        _spec(),
        [NESTED, NESTED, NESTED],
    )

    assert len(locators) == 1
    assert locators[0].path == NESTED


def test_parent_traversal_is_rejected() -> None:
    try:
        discover_github_actions_evidence_locators(
            _spec(),
            ["checkpoint-evidence/../checkpoint-generation-1.json"],
        )
    except ValueError as exc:
        assert str(exc) == "artifact_path_parent_traversal_not_allowed"
    else:
        raise AssertionError("expected ValueError")


def test_required_verification_still_fails_closed_when_adapter_has_no_verification_path() -> None:
    locators = discover_github_actions_evidence_locators(
        _spec(verification_available=False),
        [NESTED],
    )

    resolution = resolve_evidence(_node(), locators)
    assert resolution.disposition is ResolutionDisposition.DEFER
    assert resolution.reason == ResolutionReason.VERIFICATION_PATH_REQUIRED.value
