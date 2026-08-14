"""Genesis / Historical Trust-Base Portability v0.1.

This layer is deliberately above cryptographic/provider verification. Each path must
arrive as an independently verified history. Only after both registries validate do we
reject shared ancestry and compare a normalized terminal trust state.
"""

from __future__ import annotations

import re
from collections.abc import Iterable, Mapping
from dataclasses import dataclass
from typing import Any

from liminal.recovery_trust_root_registry import (
    canonical_json_bytes,
    sha256_hex,
    validate_registry,
)

TRUST_STATE_SCHEMA = "liminal-semantic-trust-state/v0.1"
PORTABILITY_RECEIPT_SCHEMA = "liminal-historical-trust-base-portability-receipt/v0.1"
PORTABILITY_REASON = "historical_trust_base_portability_verified"
_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")


@dataclass(frozen=True)
class TerminalTrustControls:
    """Portable controls not represented directly by the legacy manifest schema."""

    trust_domain: str
    authority_ids: tuple[str, ...]
    threshold: int
    rotation_contract_sha256: str
    authorization_contract_sha256: str


@dataclass(frozen=True)
class HistoricalTrustPath:
    """One independently verified genesis-to-terminal trust history."""

    verified: bool
    genesis_authority_id: str
    registry: Mapping[str, Any]
    manifests: Mapping[str, Mapping[str, Any]]
    controls: TerminalTrustControls


@dataclass(frozen=True)
class HistoricalTrustBaseAgreement:
    verified: bool
    reason: str
    receipt: dict[str, Any] | None = None


def _valid_digest(value: object) -> bool:
    return isinstance(value, str) and _SHA256_RE.fullmatch(value) is not None


def _history_manifest_digests(path: HistoricalTrustPath) -> tuple[str, ...]:
    return tuple(str(entry["manifest_sha256"]) for entry in path.registry["history"])


def _active_manifest(path: HistoricalTrustPath) -> Mapping[str, Any]:
    active_entry = path.registry["history"][-1]
    return path.manifests[str(active_entry["manifest_path"])]


def _all_strings(value: object) -> Iterable[str]:
    if isinstance(value, str):
        yield value
    elif isinstance(value, Mapping):
        for item in value.values():
            yield from _all_strings(item)
    elif isinstance(value, (list, tuple)):
        for item in value:
            yield from _all_strings(item)


def validate_terminal_controls(value: object) -> bool:
    if not isinstance(value, TerminalTrustControls):
        return False
    if not isinstance(value.trust_domain, str) or not value.trust_domain:
        return False
    if not isinstance(value.authority_ids, tuple) or not value.authority_ids:
        return False
    if any(not isinstance(authority, str) or not authority for authority in value.authority_ids):
        return False
    if len(set(value.authority_ids)) != len(value.authority_ids):
        return False
    if not isinstance(value.threshold, int) or isinstance(value.threshold, bool):
        return False
    if value.threshold < 1 or value.threshold > len(value.authority_ids):
        return False
    return _valid_digest(value.rotation_contract_sha256) and _valid_digest(
        value.authorization_contract_sha256
    )


def validate_historical_path(path: object) -> bool:
    if not isinstance(path, HistoricalTrustPath):
        return False
    if not isinstance(path.verified, bool):
        return False
    if not isinstance(path.genesis_authority_id, str) or not path.genesis_authority_id:
        return False
    if not validate_terminal_controls(path.controls):
        return False
    if not isinstance(path.registry, Mapping) or not isinstance(path.manifests, Mapping):
        return False
    return validate_registry(dict(path.registry), path.manifests)


def semantic_trust_state(path: HistoricalTrustPath) -> dict[str, Any]:
    """Normalize terminal trust semantics while excluding historical provenance.

    Excluded on purpose: generation number, previous-manifest pointer, registry paths,
    and policy source commits. Those describe how a state was reached, not the state
    currently authorized. Current workflow identities/code blobs and policy digests are
    retained because they are authorization semantics.
    """

    if not validate_historical_path(path):
        raise ValueError("historical_path_invalid")

    manifest = _active_manifest(path)
    roots = manifest["roots"]
    material = manifest["policy_material"]
    return {
        "schema": TRUST_STATE_SCHEMA,
        "trust_domain": path.controls.trust_domain,
        "authorities": {
            "ids": sorted(path.controls.authority_ids),
            "threshold": path.controls.threshold,
        },
        "contracts": {
            "rotation_sha256": path.controls.rotation_contract_sha256,
            "authorization_sha256": path.controls.authorization_contract_sha256,
        },
        "roots": {
            name: {
                "workflow_path": roots[name]["workflow_path"],
                "workflow_sha": roots[name]["workflow_sha"],
                "git_blob_sha": roots[name]["git_blob_sha"],
            }
            for name in sorted(roots)
        },
        "policy_material": {
            name: {
                "path": material[name]["path"],
                "sha256": material[name]["sha256"],
            }
            for name in sorted(material)
        },
        "authorization_scope": manifest["authorization_scope"],
    }


def trust_state_digest(path: HistoricalTrustPath) -> str:
    return sha256_hex(canonical_json_bytes(semantic_trust_state(path)))


def compare_historical_trust_bases(
    primary: HistoricalTrustPath,
    secondary: HistoricalTrustPath,
) -> HistoricalTrustBaseAgreement:
    """Fail closed unless two independent histories converge on one trust state."""

    if not validate_historical_path(primary) or not validate_historical_path(secondary):
        return HistoricalTrustBaseAgreement(False, "historical_path_invalid")
    if not primary.verified or not secondary.verified:
        return HistoricalTrustBaseAgreement(False, "historical_path_unverified")
    if primary.genesis_authority_id == secondary.genesis_authority_id:
        return HistoricalTrustBaseAgreement(False, "genesis_authority_not_independent")

    primary_history = _history_manifest_digests(primary)
    secondary_history = _history_manifest_digests(secondary)
    if primary_history[0] == secondary_history[0]:
        return HistoricalTrustBaseAgreement(False, "genesis_manifest_not_independent")
    if set(primary_history) & set(secondary_history):
        return HistoricalTrustBaseAgreement(False, "shared_manifest_digest")

    primary_registry_sha256 = sha256_hex(canonical_json_bytes(primary.registry))
    secondary_registry_sha256 = sha256_hex(canonical_json_bytes(secondary.registry))
    if primary_registry_sha256 == secondary_registry_sha256:
        return HistoricalTrustBaseAgreement(False, "registry_not_independent")

    # Path B must not smuggle any Path A manifest/registry identity through an alias,
    # nested metadata field, or unused evidence object.
    forbidden_primary_identities = set(primary_history) | {primary_registry_sha256}
    for manifest in secondary.manifests.values():
        if forbidden_primary_identities.intersection(_all_strings(manifest)):
            return HistoricalTrustBaseAgreement(False, "cross_root_dependency")

    primary_state_sha256 = trust_state_digest(primary)
    secondary_state_sha256 = trust_state_digest(secondary)
    if primary_state_sha256 != secondary_state_sha256:
        return HistoricalTrustBaseAgreement(False, "terminal_trust_state_mismatch")

    receipt = {
        "schema": PORTABILITY_RECEIPT_SCHEMA,
        "verified": True,
        "reason": PORTABILITY_REASON,
        "genesis_a_manifest_sha256": primary_history[0],
        "genesis_b_manifest_sha256": secondary_history[0],
        "registry_a_sha256": primary_registry_sha256,
        "registry_b_sha256": secondary_registry_sha256,
        "genesis_a_authority_id": primary.genesis_authority_id,
        "genesis_b_authority_id": secondary.genesis_authority_id,
        "history_a_tip_sha256": primary_history[-1],
        "history_b_tip_sha256": secondary_history[-1],
        "trust_state_digest_a": primary_state_sha256,
        "trust_state_digest_b": secondary_state_sha256,
        "histories_independently_valid": True,
        "cross_root_dependency": False,
        "equivalent_terminal_state": True,
    }
    return HistoricalTrustBaseAgreement(True, PORTABILITY_REASON, receipt)
