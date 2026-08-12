"""Topology portability checks for manifest-backed recovery evidence.

The portability contract proves that two different physical artifact topologies can
resolve the same logical evidence generation to the same verified, path-independent
Evidence Bundle and the same recovery authorization result.

This module does not verify cryptographic attestations itself. Callers must provide
verification results produced by the existing signer/attestation layer.
"""

from __future__ import annotations

from dataclasses import dataclass

from liminal.evidence_bundle import (
    build_verified_evidence_bundle,
    evidence_bundle_sha256,
)
from liminal.evidence_manifest import (
    EvidenceManifest,
    ManifestCandidate,
    ManifestDisposition,
    resolve_manifest_evidence,
)
from liminal.evidence_resolution import (
    ResolutionNode,
    confirm_verified_recovery,
    resolve_evidence,
)


@dataclass(frozen=True)
class VerifiedBundleInputs:
    """Externally verified inputs shared by equivalent physical topologies."""

    manifest_sha256: str
    manifest_verification_json_sha256: str
    manifest_signer_workflow: str
    manifest_signer_digest: str
    evidence_verification_json_sha256: str
    evidence_signer_workflow: str
    evidence_signer_digest: str
    manifest_verification_succeeded: bool = True
    evidence_verification_succeeded: bool = True


@dataclass(frozen=True)
class TopologyObservation:
    """One bounded physical topology for the same logical evidence identity."""

    name: str
    expected_path: str
    allowed_prefixes: tuple[str, ...]
    candidates: tuple[ManifestCandidate, ...]


@dataclass(frozen=True)
class TopologyProjection:
    """Path-specific resolution projected into a path-independent trust result."""

    name: str
    resolved: bool
    resolution_reason: str
    resolved_path: str | None
    authorized: bool
    authorization_reason: str
    bundle_sha256: str | None


@dataclass(frozen=True)
class TopologyPortabilityVerdict:
    """Comparison result for two distinct physical artifact topologies."""

    portable: bool
    reason: str
    left: TopologyProjection
    right: TopologyProjection


def _unresolved_projection(
    topology: TopologyObservation,
    *,
    reason: str,
) -> TopologyProjection:
    return TopologyProjection(
        name=topology.name,
        resolved=False,
        resolution_reason=reason,
        resolved_path=None,
        authorized=False,
        authorization_reason=reason,
        bundle_sha256=None,
    )


def _project(
    manifest: EvidenceManifest,
    *,
    logical_id: str,
    generation: int,
    topology: TopologyObservation,
    verified: VerifiedBundleInputs,
) -> TopologyProjection:
    manifest_resolution = resolve_manifest_evidence(
        [manifest],
        logical_id=logical_id,
        generation=generation,
        candidates=topology.candidates,
    )
    if manifest_resolution.disposition is not ManifestDisposition.RESOLVED:
        return _unresolved_projection(topology, reason=manifest_resolution.reason)
    assert manifest_resolution.entry is not None
    assert manifest_resolution.locator is not None

    resolution = resolve_evidence(
        ResolutionNode(
            logical_id=logical_id,
            expected_path=topology.expected_path,
            allowed_prefixes=topology.allowed_prefixes,
            require_verification=True,
        ),
        [manifest_resolution.locator],
    )
    recovery = confirm_verified_recovery(
        resolution,
        verification_succeeded=verified.evidence_verification_succeeded,
    )
    if not recovery.authorized:
        return TopologyProjection(
            name=topology.name,
            resolved=resolution.resolved,
            resolution_reason=resolution.reason,
            resolved_path=resolution.resolved_path,
            authorized=False,
            authorization_reason=recovery.reason,
            bundle_sha256=None,
        )

    bundle = build_verified_evidence_bundle(
        manifest,
        logical_id=logical_id,
        generation=generation,
        manifest_sha256=verified.manifest_sha256,
        manifest_verification_json_sha256=(
            verified.manifest_verification_json_sha256
        ),
        manifest_signer_workflow=verified.manifest_signer_workflow,
        manifest_signer_digest=verified.manifest_signer_digest,
        evidence_sha256=manifest_resolution.entry.sha256,
        evidence_verification_json_sha256=(
            verified.evidence_verification_json_sha256
        ),
        evidence_signer_workflow=verified.evidence_signer_workflow,
        evidence_signer_digest=verified.evidence_signer_digest,
        manifest_verification_succeeded=verified.manifest_verification_succeeded,
        evidence_verification_succeeded=verified.evidence_verification_succeeded,
    )
    return TopologyProjection(
        name=topology.name,
        resolved=True,
        resolution_reason=resolution.reason,
        resolved_path=resolution.resolved_path,
        authorized=True,
        authorization_reason=recovery.reason,
        bundle_sha256=evidence_bundle_sha256(bundle),
    )


def evaluate_topology_portability(
    manifest: EvidenceManifest,
    *,
    logical_id: str,
    generation: int,
    left: TopologyObservation,
    right: TopologyObservation,
    verified: VerifiedBundleInputs,
    require_distinct_paths: bool = True,
) -> TopologyPortabilityVerdict:
    """Compare two bounded topologies for trust-equivalent recovery.

    A portable result requires both topologies to resolve and authorize, both to
    produce the same canonical Evidence Bundle SHA-256, and (by default) the
    physical resolved paths to be genuinely different.
    """

    if not left.name or not right.name:
        raise ValueError("topology_name_must_be_non_empty")
    if left.name == right.name:
        raise ValueError("topology_names_must_be_distinct")

    left_result = _project(
        manifest,
        logical_id=logical_id,
        generation=generation,
        topology=left,
        verified=verified,
    )
    right_result = _project(
        manifest,
        logical_id=logical_id,
        generation=generation,
        topology=right,
        verified=verified,
    )

    if not left_result.authorized:
        return TopologyPortabilityVerdict(
            portable=False,
            reason="left_topology_not_authorized",
            left=left_result,
            right=right_result,
        )
    if not right_result.authorized:
        return TopologyPortabilityVerdict(
            portable=False,
            reason="right_topology_not_authorized",
            left=left_result,
            right=right_result,
        )
    if require_distinct_paths and left_result.resolved_path == right_result.resolved_path:
        return TopologyPortabilityVerdict(
            portable=False,
            reason="physical_topologies_not_distinct",
            left=left_result,
            right=right_result,
        )
    if left_result.bundle_sha256 != right_result.bundle_sha256:
        return TopologyPortabilityVerdict(
            portable=False,
            reason="evidence_bundle_digest_mismatch",
            left=left_result,
            right=right_result,
        )
    if left_result.authorization_reason != right_result.authorization_reason:
        return TopologyPortabilityVerdict(
            portable=False,
            reason="trust_decision_mismatch",
            left=left_result,
            right=right_result,
        )

    return TopologyPortabilityVerdict(
        portable=True,
        reason="topology_portability_verified",
        left=left_result,
        right=right_result,
    )
