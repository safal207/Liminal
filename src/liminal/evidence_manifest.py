"""Manifest-backed evidence identity for deterministic recovery.

The manifest binds a stable logical evidence identity and generation to content
bytes (SHA-256) while keeping the downloaded physical path outside the trust
identity. Bounded topology discovery supplies physical candidates; this module
matches those candidates by digest and fails closed on ambiguity.

The manifest never replaces signer, policy, registry, or attestation checks.
"""

from __future__ import annotations

import json
import re
from dataclasses import dataclass
from enum import Enum
from pathlib import PurePosixPath
from typing import Iterable

from liminal.evidence_resolution import EvidenceLocator

_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")


class ManifestDisposition(str, Enum):
    RESOLVED = "resolved"
    DEFER = "defer"


class ManifestReason(str, Enum):
    MANIFEST_EVIDENCE_RESOLVED = "manifest_evidence_resolved"
    MANIFEST_ENTRY_NOT_FOUND = "manifest_entry_not_found"
    AMBIGUOUS_MANIFEST_ENTRIES = "ambiguous_manifest_entries"
    EVIDENCE_CANDIDATE_NOT_FOUND = "evidence_candidate_not_found"
    MANIFEST_DIGEST_MISMATCH = "manifest_digest_mismatch"
    AMBIGUOUS_DIGEST_MATCHES = "ambiguous_digest_matches"


@dataclass(frozen=True)
class VerificationExpectation:
    """Verification contract expected after manifest resolution."""

    required: bool = True
    verifier: str = "external"
    expected_signer: str | None = None
    expected_signer_digest: str | None = None


@dataclass(frozen=True)
class EvidenceManifestEntry:
    """Stable manifest identity for one evidence generation."""

    logical_id: str
    producer: str
    evidence_type: str
    relative_locator: str
    sha256: str
    generation: int
    verification: VerificationExpectation = VerificationExpectation()


@dataclass(frozen=True)
class EvidenceManifest:
    """Canonical evidence manifest document."""

    entries: tuple[EvidenceManifestEntry, ...]
    schema: str = "liminal-evidence-manifest/v0.1"


@dataclass(frozen=True)
class ManifestCandidate:
    """Observed physical candidate from a bounded topology field."""

    path: str
    sha256: str
    verification_available: bool = True


@dataclass(frozen=True)
class ManifestResolution:
    """Manifest-to-physical-locator binding before external verification."""

    disposition: ManifestDisposition
    reason: str
    logical_id: str
    generation: int
    entry: EvidenceManifestEntry | None
    locator: EvidenceLocator | None

    @property
    def resolved(self) -> bool:
        return self.disposition is ManifestDisposition.RESOLVED


def _normalize_relative_path(path: str) -> str:
    if not path:
        raise ValueError("manifest_locator_must_be_non_empty")
    candidate = PurePosixPath(path.replace("\\", "/"))
    if candidate.is_absolute():
        raise ValueError("manifest_locator_must_be_relative")
    if ".." in candidate.parts:
        raise ValueError("manifest_locator_parent_traversal_not_allowed")
    normalized = candidate.as_posix()
    if normalized in {"", "."}:
        raise ValueError("manifest_locator_must_be_non_empty")
    return normalized


def _validate_sha256(value: str, *, field: str) -> None:
    if not _SHA256_RE.fullmatch(value):
        raise ValueError(f"{field}_must_be_lowercase_sha256")


def _validate_verification(expectation: VerificationExpectation) -> None:
    if expectation.required and not expectation.verifier:
        raise ValueError("verification_verifier_must_be_non_empty_when_required")
    if expectation.expected_signer == "":
        raise ValueError("verification_expected_signer_must_be_non_empty")
    if expectation.expected_signer_digest == "":
        raise ValueError("verification_expected_signer_digest_must_be_non_empty")


def validate_manifest_entry(entry: EvidenceManifestEntry) -> None:
    if not entry.logical_id:
        raise ValueError("manifest_logical_id_must_be_non_empty")
    if not entry.producer:
        raise ValueError("manifest_producer_must_be_non_empty")
    if not entry.evidence_type:
        raise ValueError("manifest_evidence_type_must_be_non_empty")
    _normalize_relative_path(entry.relative_locator)
    _validate_sha256(entry.sha256, field="manifest_sha256")
    if entry.generation < 0:
        raise ValueError("manifest_generation_must_be_non_negative")
    _validate_verification(entry.verification)


def validate_manifest(manifest: EvidenceManifest) -> None:
    if manifest.schema != "liminal-evidence-manifest/v0.1":
        raise ValueError("unsupported_evidence_manifest_schema")
    if not manifest.entries:
        raise ValueError("manifest_entries_must_be_non_empty")

    seen: dict[tuple[str, int], EvidenceManifestEntry] = {}
    for entry in manifest.entries:
        validate_manifest_entry(entry)
        key = (entry.logical_id, entry.generation)
        existing = seen.get(key)
        if existing is not None and existing != entry:
            raise ValueError("conflicting_manifest_logical_id_generation")
        seen[key] = entry


def _validate_candidate(candidate: ManifestCandidate) -> None:
    _normalize_relative_path(candidate.path)
    _validate_sha256(candidate.sha256, field="candidate_sha256")


def canonical_manifest_bytes(manifest: EvidenceManifest) -> bytes:
    """Serialize a validated manifest deterministically."""

    validate_manifest(manifest)
    payload = {
        "schema": manifest.schema,
        "entries": [
            {
                "logical_id": entry.logical_id,
                "producer": entry.producer,
                "evidence_type": entry.evidence_type,
                "relative_locator": _normalize_relative_path(entry.relative_locator),
                "sha256": entry.sha256,
                "generation": entry.generation,
                "verification": {
                    "required": entry.verification.required,
                    "verifier": entry.verification.verifier,
                    "expected_signer": entry.verification.expected_signer,
                    "expected_signer_digest": entry.verification.expected_signer_digest,
                },
            }
            for entry in sorted(
                manifest.entries,
                key=lambda item: (item.logical_id, item.generation, item.sha256),
            )
        ],
    }
    return (json.dumps(payload, sort_keys=True, separators=(",", ":")) + "\n").encode()


def _defer(
    *,
    reason: ManifestReason,
    logical_id: str,
    generation: int,
    entry: EvidenceManifestEntry | None = None,
) -> ManifestResolution:
    return ManifestResolution(
        disposition=ManifestDisposition.DEFER,
        reason=reason.value,
        logical_id=logical_id,
        generation=generation,
        entry=entry,
        locator=None,
    )


def resolve_manifest_evidence(
    manifests: Iterable[EvidenceManifest],
    *,
    logical_id: str,
    generation: int,
    candidates: Iterable[ManifestCandidate],
) -> ManifestResolution:
    """Resolve one logical evidence generation by manifest SHA-256.

    Physical candidates must already come from a bounded topology observation.
    The manifest selects by stable logical identity and digest, never by basename.
    Multiple distinct manifest entries or physical digest matches fail closed.
    """

    if not logical_id:
        raise ValueError("logical_id_must_be_non_empty")
    if generation < 0:
        raise ValueError("generation_must_be_non_negative")

    matching_entries: list[EvidenceManifestEntry] = []
    for manifest in manifests:
        validate_manifest(manifest)
        matching_entries.extend(
            entry
            for entry in manifest.entries
            if entry.logical_id == logical_id and entry.generation == generation
        )

    unique_entries = tuple(
        dict.fromkeys(matching_entries)
    )
    if not unique_entries:
        return _defer(
            reason=ManifestReason.MANIFEST_ENTRY_NOT_FOUND,
            logical_id=logical_id,
            generation=generation,
        )
    if len(unique_entries) > 1:
        return _defer(
            reason=ManifestReason.AMBIGUOUS_MANIFEST_ENTRIES,
            logical_id=logical_id,
            generation=generation,
        )

    entry = unique_entries[0]
    observed = tuple(candidates)
    for candidate in observed:
        _validate_candidate(candidate)
    if not observed:
        return _defer(
            reason=ManifestReason.EVIDENCE_CANDIDATE_NOT_FOUND,
            logical_id=logical_id,
            generation=generation,
            entry=entry,
        )

    digest_matches = tuple(
        candidate for candidate in observed if candidate.sha256 == entry.sha256
    )
    if not digest_matches:
        return _defer(
            reason=ManifestReason.MANIFEST_DIGEST_MISMATCH,
            logical_id=logical_id,
            generation=generation,
            entry=entry,
        )

    unique_paths = {
        candidate.path: candidate for candidate in digest_matches
    }
    if len(unique_paths) > 1:
        return _defer(
            reason=ManifestReason.AMBIGUOUS_DIGEST_MATCHES,
            logical_id=logical_id,
            generation=generation,
            entry=entry,
        )

    candidate = next(iter(unique_paths.values()))
    return ManifestResolution(
        disposition=ManifestDisposition.RESOLVED,
        reason=ManifestReason.MANIFEST_EVIDENCE_RESOLVED.value,
        logical_id=logical_id,
        generation=generation,
        entry=entry,
        locator=EvidenceLocator(
            logical_id=logical_id,
            path=candidate.path,
            verification_available=candidate.verification_available,
        ),
    )
