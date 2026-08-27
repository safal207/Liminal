"""GitHub Actions artifact-topology adapter for evidence resolution.

The adapter converts a bounded set of downloaded artifact-relative paths into
``EvidenceLocator`` candidates. It performs discovery only; it does not select
a trust winner and it does not verify signer, hash, policy, or registry state.

Keeping this adapter separate from ``evidence_resolution`` preserves the split:

    artifact topology -> candidate locators -> bounded resolution -> verification
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import PurePosixPath
from typing import Iterable

from liminal.evidence_resolution import EvidenceLocator


@dataclass(frozen=True)
class GitHubActionsEvidenceSpec:
    """Bounded discovery specification for one logical evidence record."""

    logical_id: str
    expected_path: str
    filename: str
    allowed_prefixes: tuple[str, ...] = ()
    verification_available: bool = True


def _normalize_relative_path(path: str) -> str:
    if not path:
        raise ValueError("artifact_path_must_be_non_empty")

    candidate = PurePosixPath(path.replace("\\", "/"))
    if candidate.is_absolute():
        raise ValueError("artifact_path_must_be_relative")
    if ".." in candidate.parts:
        raise ValueError("artifact_path_parent_traversal_not_allowed")

    normalized = candidate.as_posix()
    if normalized in {"", "."}:
        raise ValueError("artifact_path_must_be_non_empty")
    return normalized


def _normalize_prefix(prefix: str) -> str:
    normalized = _normalize_relative_path(prefix)
    return normalized.rstrip("/") + "/"


def _validate_spec(spec: GitHubActionsEvidenceSpec) -> None:
    if not spec.logical_id:
        raise ValueError("logical_id_must_be_non_empty")
    if not spec.filename or "/" in spec.filename or "\\" in spec.filename:
        raise ValueError("filename_must_be_a_basename")
    _normalize_relative_path(spec.expected_path)
    for prefix in spec.allowed_prefixes:
        _normalize_prefix(prefix)


def _is_allowed_path(
    path: str,
    *,
    expected_path: str,
    allowed_prefixes: tuple[str, ...],
) -> bool:
    if path == expected_path:
        return True
    return any(path.startswith(prefix) for prefix in allowed_prefixes)


def discover_github_actions_evidence_locators(
    spec: GitHubActionsEvidenceSpec,
    artifact_paths: Iterable[str],
) -> tuple[EvidenceLocator, ...]:
    """Create deterministic bounded locator candidates from artifact topology.

    Only files whose basename exactly matches ``spec.filename`` and whose path
    is either the expected location or inside an explicitly allowed prefix are
    returned. Duplicate physical paths are collapsed deterministically.

    The function intentionally preserves multiple distinct admissible matches;
    ``resolve_evidence`` will fail closed on that ambiguity instead of this
    adapter silently choosing one.
    """

    _validate_spec(spec)
    expected_path = _normalize_relative_path(spec.expected_path)
    allowed_prefixes = tuple(_normalize_prefix(prefix) for prefix in spec.allowed_prefixes)

    discovered: set[str] = set()
    for raw_path in artifact_paths:
        path = _normalize_relative_path(raw_path)
        if PurePosixPath(path).name != spec.filename:
            continue
        if not _is_allowed_path(
            path,
            expected_path=expected_path,
            allowed_prefixes=allowed_prefixes,
        ):
            continue
        discovered.add(path)

    return tuple(
        EvidenceLocator(
            logical_id=spec.logical_id,
            path=path,
            verification_available=spec.verification_available,
        )
        for path in sorted(discovered)
    )
