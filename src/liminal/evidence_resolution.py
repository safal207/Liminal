"""Deterministic evidence resolution and re-anchor primitive.

This module separates a stable logical evidence identity from its physical
artifact location, and keeps locator resolution distinct from trust
verification. Callers discover candidate locators, this resolver selects only
an unambiguous policy-allowed location, and the existing verification layer
then decides whether recovery is actually trusted.

The primitive never grants tool/action authority and never replaces signer,
hash, policy, or registry verification.
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from typing import Iterable


class ResolutionDisposition(str, Enum):
    RESOLVED = "resolved"
    DEFER = "defer"


class ResolutionReason(str, Enum):
    EXPECTED_LOCATOR_RESOLVED = "expected_locator_resolved"
    EVIDENCE_NOT_FOUND = "evidence_not_found"
    AMBIGUOUS_EVIDENCE_CANDIDATES = "ambiguous_evidence_candidates"
    VERIFICATION_PATH_REQUIRED = "verification_path_required"
    REANCHOR_RESOLVED = "reanchor_resolved"
    VERIFICATION_FAILED = "verification_failed"
    VERIFIED_RECOVERY = "verified_recovery"


@dataclass(frozen=True)
class EvidenceLocator:
    """Observed physical location for one stable logical evidence identity."""

    logical_id: str
    path: str
    verification_available: bool = False


@dataclass(frozen=True)
class ResolutionNode:
    """Expected location and admissible recovery field for logical evidence."""

    logical_id: str
    expected_path: str
    allowed_prefixes: tuple[str, ...] = ()
    require_verification: bool = True


@dataclass(frozen=True)
class ReAnchor:
    """A deterministic relocation from the expected path to one candidate."""

    from_path: str
    to_path: str
    reason: str


@dataclass(frozen=True)
class ResolutionOutcome:
    """Locator-resolution result, before cryptographic/policy verification."""

    disposition: ResolutionDisposition
    reason: str
    logical_id: str
    resolved_path: str | None
    verification_required: bool
    verification_available: bool
    reanchor: ReAnchor | None

    @property
    def resolved(self) -> bool:
        return self.disposition is ResolutionDisposition.RESOLVED


@dataclass(frozen=True)
class VerifiedRecovery:
    """Final recovery result after the external verification step."""

    authorized: bool
    reason: str
    logical_id: str
    resolved_path: str | None
    reanchor: ReAnchor | None


def _validate_node(node: ResolutionNode) -> None:
    if not node.logical_id:
        raise ValueError("logical_id_must_be_non_empty")
    if not node.expected_path:
        raise ValueError("expected_path_must_be_non_empty")
    if any(not prefix for prefix in node.allowed_prefixes):
        raise ValueError("allowed_prefix_must_be_non_empty")


def _validate_locator(locator: EvidenceLocator) -> None:
    if not locator.logical_id:
        raise ValueError("locator_logical_id_must_be_non_empty")
    if not locator.path:
        raise ValueError("locator_path_must_be_non_empty")


def _defer(node: ResolutionNode, reason: ResolutionReason) -> ResolutionOutcome:
    return ResolutionOutcome(
        disposition=ResolutionDisposition.DEFER,
        reason=reason.value,
        logical_id=node.logical_id,
        resolved_path=None,
        verification_required=node.require_verification,
        verification_available=False,
        reanchor=None,
    )


def _is_allowed_reanchor(node: ResolutionNode, path: str) -> bool:
    return bool(node.allowed_prefixes) and any(
        path.startswith(prefix) for prefix in node.allowed_prefixes
    )


def resolve_evidence(
    node: ResolutionNode,
    candidates: Iterable[EvidenceLocator],
) -> ResolutionOutcome:
    """Resolve evidence by expected path or one bounded re-anchor candidate.

    Resolution order is intentional:

    1. use the exact expected locator when it exists unambiguously;
    2. otherwise consider only candidates with the same ``logical_id`` whose
       physical paths are inside explicitly allowed prefixes;
    3. require exactly one admissible candidate for re-anchoring;
    4. if verification is required, require a verification path to exist;
    5. return the locator to the normal verification layer.

    This function does not claim that evidence is verified. Multiple plausible
    candidates are never ranked here because that would silently turn artifact
    discovery into a trust decision.
    """

    _validate_node(node)
    observed = tuple(candidates)
    for locator in observed:
        _validate_locator(locator)

    same_identity = tuple(
        sorted(
            (locator for locator in observed if locator.logical_id == node.logical_id),
            key=lambda locator: locator.path,
        )
    )

    expected = tuple(
        locator for locator in same_identity if locator.path == node.expected_path
    )
    if len(expected) > 1:
        return _defer(node, ResolutionReason.AMBIGUOUS_EVIDENCE_CANDIDATES)
    if len(expected) == 1:
        locator = expected[0]
        if node.require_verification and not locator.verification_available:
            return _defer(node, ResolutionReason.VERIFICATION_PATH_REQUIRED)
        return ResolutionOutcome(
            disposition=ResolutionDisposition.RESOLVED,
            reason=ResolutionReason.EXPECTED_LOCATOR_RESOLVED.value,
            logical_id=node.logical_id,
            resolved_path=locator.path,
            verification_required=node.require_verification,
            verification_available=locator.verification_available,
            reanchor=None,
        )

    admissible = tuple(
        locator
        for locator in same_identity
        if _is_allowed_reanchor(node, locator.path)
    )
    if not admissible:
        return _defer(node, ResolutionReason.EVIDENCE_NOT_FOUND)
    if len(admissible) > 1:
        return _defer(node, ResolutionReason.AMBIGUOUS_EVIDENCE_CANDIDATES)

    locator = admissible[0]
    if node.require_verification and not locator.verification_available:
        return _defer(node, ResolutionReason.VERIFICATION_PATH_REQUIRED)

    return ResolutionOutcome(
        disposition=ResolutionDisposition.RESOLVED,
        reason=ResolutionReason.REANCHOR_RESOLVED.value,
        logical_id=node.logical_id,
        resolved_path=locator.path,
        verification_required=node.require_verification,
        verification_available=locator.verification_available,
        reanchor=ReAnchor(
            from_path=node.expected_path,
            to_path=locator.path,
            reason="expected_locator_missing_unique_admissible_candidate",
        ),
    )


def confirm_verified_recovery(
    resolution: ResolutionOutcome,
    *,
    verification_succeeded: bool | None,
) -> VerifiedRecovery:
    """Convert a resolved locator into recovery authority after verification.

    ``verification_succeeded`` must come from the existing signer/hash/policy
    verification layer. A required verification that is absent or false fails
    closed. An unresolved locator can never become authorized here.
    """

    if not resolution.resolved:
        return VerifiedRecovery(
            authorized=False,
            reason=resolution.reason,
            logical_id=resolution.logical_id,
            resolved_path=None,
            reanchor=None,
        )

    if resolution.verification_required:
        if not resolution.verification_available:
            return VerifiedRecovery(
                authorized=False,
                reason=ResolutionReason.VERIFICATION_PATH_REQUIRED.value,
                logical_id=resolution.logical_id,
                resolved_path=None,
                reanchor=resolution.reanchor,
            )
        if verification_succeeded is not True:
            return VerifiedRecovery(
                authorized=False,
                reason=ResolutionReason.VERIFICATION_FAILED.value,
                logical_id=resolution.logical_id,
                resolved_path=None,
                reanchor=resolution.reanchor,
            )
    elif verification_succeeded is False:
        return VerifiedRecovery(
            authorized=False,
            reason=ResolutionReason.VERIFICATION_FAILED.value,
            logical_id=resolution.logical_id,
            resolved_path=None,
            reanchor=resolution.reanchor,
        )

    return VerifiedRecovery(
        authorized=True,
        reason=ResolutionReason.VERIFIED_RECOVERY.value,
        logical_id=resolution.logical_id,
        resolved_path=resolution.resolved_path,
        reanchor=resolution.reanchor,
    )
