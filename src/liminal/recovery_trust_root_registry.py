"""Canonical trust-root manifests and hash-chained rotation registry."""

from __future__ import annotations

import hashlib
import json
import re
from collections.abc import Mapping
from dataclasses import dataclass
from typing import Any


MANIFEST_SCHEMA_VERSION = "liminal.recovery-trust-root-manifest.v0.1"
REGISTRY_SCHEMA_VERSION = "liminal.recovery-trust-root-registry.v0.1"
_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")
_GIT_SHA_RE = re.compile(r"^[0-9a-f]{40}$")


def canonical_json_bytes(value: object) -> bytes:
    return (json.dumps(value, sort_keys=True, separators=(",", ":")) + "\n").encode("utf-8")


def sha256_hex(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


@dataclass(frozen=True)
class TrustRootManifest:
    payload: dict[str, Any]

    @property
    def digest(self) -> str:
        return sha256_hex(canonical_json_bytes(self.payload))


@dataclass(frozen=True)
class TrustRootRegistry:
    payload: dict[str, Any]

    @property
    def digest(self) -> str:
        return sha256_hex(canonical_json_bytes(self.payload))


@dataclass(frozen=True)
class RegistryRotationDecision:
    """Machine-readable authorization decision for one registry rotation."""

    authorized: bool
    reason: str


def _exact_keys(value: Mapping[str, Any], expected: set[str]) -> bool:
    return set(value) == expected


def _validate_manifest_body(payload: object) -> bool:
    if not isinstance(payload, dict) or not _exact_keys(
        payload,
        {
            "schema_version",
            "generation",
            "previous_manifest_sha256",
            "repository",
            "roots",
            "policy_material",
            "authorization_scope",
        },
    ):
        return False
    if payload.get("schema_version") != MANIFEST_SCHEMA_VERSION:
        return False
    generation = payload.get("generation")
    if not isinstance(generation, int) or isinstance(generation, bool) or generation < 0:
        return False
    if payload.get("repository") != "safal207/Liminal":
        return False

    roots = payload.get("roots")
    if not isinstance(roots, dict) or not _exact_keys(roots, {"builder", "verifier"}):
        return False
    for root_name in ("builder", "verifier"):
        root = roots.get(root_name)
        if not isinstance(root, dict) or not _exact_keys(
            root, {"workflow_path", "workflow_sha", "git_blob_sha"}
        ):
            return False
        if not isinstance(root.get("workflow_path"), str) or not root["workflow_path"].startswith(
            ".github/workflows/"
        ):
            return False
        if not isinstance(root.get("workflow_sha"), str) or _GIT_SHA_RE.fullmatch(root["workflow_sha"]) is None:
            return False
        if not isinstance(root.get("git_blob_sha"), str) or _GIT_SHA_RE.fullmatch(root["git_blob_sha"]) is None:
            return False

    material = payload.get("policy_material")
    if not isinstance(material, dict) or not _exact_keys(
        material, {"builder_environment_policy", "verifier_dependency_lock"}
    ):
        return False
    for item in material.values():
        if not isinstance(item, dict) or not _exact_keys(item, {"path", "sha256", "source_sha"}):
            return False
        if not isinstance(item.get("path"), str) or not item["path"]:
            return False
        if not isinstance(item.get("sha256"), str) or _SHA256_RE.fullmatch(item["sha256"]) is None:
            return False
        if not isinstance(item.get("source_sha"), str) or _GIT_SHA_RE.fullmatch(item["source_sha"]) is None:
            return False

    scope = payload.get("authorization_scope")
    if not isinstance(scope, dict) or not _exact_keys(
        scope, {"repository_id", "source_ref", "deployment_environment", "runner_environment"}
    ):
        return False
    return (
        scope.get("repository_id") == "1005410203"
        and scope.get("source_ref") == "refs/heads/agent/recovery-routing-v0-1"
        and scope.get("deployment_environment") == "live-provider-trace"
        and scope.get("runner_environment") == "github-hosted"
    )


def validate_manifest(payload: object, previous_manifest: object | None = None) -> bool:
    if not _validate_manifest_body(payload):
        return False
    assert isinstance(payload, dict)

    generation = payload["generation"]
    previous_digest = payload.get("previous_manifest_sha256")
    if generation == 0:
        return previous_digest is None and previous_manifest is None

    if not isinstance(previous_digest, str) or _SHA256_RE.fullmatch(previous_digest) is None:
        return False
    if not isinstance(previous_manifest, dict) or not _validate_manifest_body(previous_manifest):
        return False
    if previous_manifest.get("generation") != generation - 1:
        return False
    return sha256_hex(canonical_json_bytes(previous_manifest)) == previous_digest


def validate_registry(payload: object, manifests: Mapping[str, object]) -> bool:
    if not isinstance(payload, dict) or not _exact_keys(
        payload, {"schema_version", "active_generation", "active_manifest_sha256", "history"}
    ):
        return False
    if payload.get("schema_version") != REGISTRY_SCHEMA_VERSION:
        return False
    active_generation = payload.get("active_generation")
    active_digest = payload.get("active_manifest_sha256")
    history = payload.get("history")
    if not isinstance(active_generation, int) or isinstance(active_generation, bool) or active_generation < 0:
        return False
    if not isinstance(active_digest, str) or _SHA256_RE.fullmatch(active_digest) is None:
        return False
    if not isinstance(history, list) or len(history) != active_generation + 1:
        return False

    previous_manifest: object | None = None
    previous_digest: str | None = None
    for generation, entry in enumerate(history):
        if not isinstance(entry, dict) or not _exact_keys(
            entry, {"generation", "manifest_path", "manifest_sha256"}
        ):
            return False
        if entry.get("generation") != generation:
            return False
        path = entry.get("manifest_path")
        digest = entry.get("manifest_sha256")
        if not isinstance(path, str) or not isinstance(digest, str) or _SHA256_RE.fullmatch(digest) is None:
            return False
        manifest = manifests.get(path)
        if not isinstance(manifest, dict):
            return False
        if sha256_hex(canonical_json_bytes(manifest)) != digest:
            return False
        if not validate_manifest(manifest, previous_manifest):
            return False
        if generation > 0 and manifest.get("previous_manifest_sha256") != previous_digest:
            return False
        previous_manifest = manifest
        previous_digest = digest

    return history[-1]["manifest_sha256"] == active_digest


def evaluate_registry_rotation(
    previous_registry: object,
    current_registry: object,
    manifests: Mapping[str, object],
) -> RegistryRotationDecision:
    """Evaluate one append-only rotation and return a stable reason code."""

    if not validate_registry(previous_registry, manifests):
        return RegistryRotationDecision(False, "previous_registry_invalid")
    if not validate_registry(current_registry, manifests):
        return RegistryRotationDecision(False, "current_registry_invalid")
    assert isinstance(previous_registry, dict)
    assert isinstance(current_registry, dict)

    previous_generation = previous_registry["active_generation"]
    current_generation = current_registry["active_generation"]
    if current_generation != previous_generation + 1:
        return RegistryRotationDecision(False, "rotation_generation_not_incremented")

    previous_history = previous_registry["history"]
    current_history = current_registry["history"]
    if current_history[:-1] != previous_history:
        return RegistryRotationDecision(False, "registry_history_not_append_only")

    previous_entry = previous_history[-1]
    current_entry = current_history[-1]
    previous_manifest = manifests.get(previous_entry["manifest_path"])
    current_manifest = manifests.get(current_entry["manifest_path"])
    if not isinstance(previous_manifest, dict) or not isinstance(current_manifest, dict):
        return RegistryRotationDecision(False, "rotation_manifest_missing")
    if current_manifest.get("previous_manifest_sha256") != previous_registry["active_manifest_sha256"]:
        return RegistryRotationDecision(False, "previous_manifest_digest_mismatch")

    for root_name in ("builder", "verifier"):
        previous_root = previous_manifest["roots"][root_name]["workflow_sha"]
        current_root = current_manifest["roots"][root_name]["workflow_sha"]
        historical_roots = {
            manifests[entry["manifest_path"]]["roots"][root_name]["workflow_sha"]
            for entry in previous_history[:-1]
            if isinstance(manifests.get(entry["manifest_path"]), dict)
        }
        if current_root != previous_root and current_root in historical_roots:
            return RegistryRotationDecision(False, f"{root_name}_root_downgrade")

    for material_name in ("builder_environment_policy", "verifier_dependency_lock"):
        previous_material = previous_manifest["policy_material"][material_name]["sha256"]
        current_material = current_manifest["policy_material"][material_name]["sha256"]
        historical_material = {
            manifests[entry["manifest_path"]]["policy_material"][material_name]["sha256"]
            for entry in previous_history[:-1]
            if isinstance(manifests.get(entry["manifest_path"]), dict)
        }
        if current_material != previous_material and current_material in historical_material:
            return RegistryRotationDecision(False, f"{material_name}_downgrade")

    return RegistryRotationDecision(True, "registry_rotation_authorized")


def validate_registry_rotation(
    previous_registry: object,
    current_registry: object,
    manifests: Mapping[str, object],
) -> bool:
    """Return whether one append-only rotation is authorized."""

    return evaluate_registry_rotation(previous_registry, current_registry, manifests).authorized
