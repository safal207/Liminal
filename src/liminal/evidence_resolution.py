"""Deterministic evidence resolution and re-anchor primitive.

This module separates a stable logical evidence identity from its physical
artifact location. It is intentionally provider-agnostic: callers discover
candidate locators, then this resolver chooses only an unambiguous, policy-
allowed location and fails closed when required verification is unavailable.

The primitive does not grant trust or action authority. It only resolves where
already-defined evidence is located so the existing verification layer can
make the trust decision.
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
    VERIFIED_EVIDENCE_REQUIRED = "verified_evidence_required"
    VERIFIED_REANCHOR = "verified_reanchor"
    UNVERIFIED_REANCHOR = "unverified_reanchor"


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
    require_verified: bool = True


@dataclass(frozen=True)
class ReAnchor:
    """A deterministic relocation from the expected path to one candidate."""

    from_path: str
    to_path: str
    reason: str


@dataclass(frozen=True)
class VerifiedRecovery:
    """Fail-closed evidence resolution outcome.

    ``authorized`` means only that the resolver may hand the resolved locator
    to the existing verification layer. It does not authorize an external
    action or bypass signer/hash/policy verification.
    """

    disposition: ResolutionDisposition
    reason: str
    logical_id: str
    resolved_path: str | None
    verified: bool
    reanchor: ReAnchor | None

    @property
    def authorized(self) -> bool:
        return self.disposition is ResolutionDisposition.RESOLVED


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


def _defer(node: ResolutionNode, reason: ResolutionReason) -> VerifiedRecovery:
    return VerifiedRecovery(
        disposition=ResolutionDisposition.DEFER,
        reason=reason.value,
        logical_id=node.logical_id,
        resolved_path=None,
        verified=False,
        reanchor=None,
    )


def _is_allowed_reanchor(node: ResolutionNode, path: str) -> bool:
    return bool(node.allowed_prefixes) and any(
        path.startswith(prefix) for prefix in node.allowed_prefixes
    )


def resolve_evidence(
    node: ResolutionNode,
    candidates: Iterable[EvidenceLocator],
) -> VerifiedRecovery:
    """Resolve evidence by expected path or one bounded re-anchor candidate.

    Resolution order is intentional:

    1. use the exact expected locator when it exists unambiguously;
    2. otherwise consider only candidates with the same ``logical_id`` whose
       physical paths are inside explicitly allowed prefixes;
    3. require exactly one admissible candidate for re-anchoring;
    4. if verification is required, refuse a candidate without verification;
    5. return the locator for the normal verification layer to consume.

    Multiple plausible candidates are never ranked here. That would silently
    turn artifact discovery into a trust decision, so ambiguity always defers.
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
        if node.require_verified and not locator.verification_available:
            return _defer(node, ResolutionReason.VERIFIED_EVIDENCE_REQUIRED)
        return VerifiedRecovery(
            disposition=ResolutionDisposition.RESOLVED,
            reason=ResolutionReason.EXPECTED_LOCATOR_RESOLVED.value,
            logical_id=node.logical_id,
            resolved_path=locator.path,
            verified=locator.verification_available,
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
    if node.require_verified and not locator.verification_available:
        return _defer(node, ResolutionReason.VERIFIED_EVIDENCE_REQUIRED)

    reason = (
        ResolutionReason.VERIFIED_REANCHOR
        if locator.verification_available
        else ResolutionReason.UNVERIFIED_REANCHOR
    )
    return VerifiedRecovery(
        disposition=ResolutionDisposition.RESOLVED,
        reason=reason.value,
        logical_id=node.logical_id,
        resolved_path=locator.path,
        verified=locator.verification_available,
        reanchor=ReAnchor(
            from_path=node.expected_path,
            to_path=locator.path,
            reason="expected_locator_missing_unique_admissible_candidate",
        ),
    )
