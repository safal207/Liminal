from __future__ import annotations

import copy
import json
from pathlib import Path

from liminal.recovery_trust_root_registry import (
    canonical_json_bytes,
    sha256_hex,
    validate_manifest,
    validate_registry,
)


ROOT = Path(__file__).resolve().parents[1]
MANIFEST_PATH = "policies/recovery-trust-root-manifest-v0.1.json"
REGISTRY_PATH = ROOT / "policies" / "recovery-trust-root-registry-v0.1.json"
MANIFEST_FILE = ROOT / MANIFEST_PATH
EXPECTED_MANIFEST_SHA256 = "bd8aaa6162d0f7e9627e10ee6d495810820fd6fd8cd07d9d48e5d585786537b5"
EXPECTED_REGISTRY_SHA256 = "bd43cb039d29245f3d7eb8b78a7a5fcde14d7bf638c4dfe98bb300b00f8670e1"


def _load(path: Path) -> dict[str, object]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    assert isinstance(payload, dict)
    return payload


def test_genesis_manifest_and_registry_are_canonical_and_valid() -> None:
    manifest = _load(MANIFEST_FILE)
    registry = _load(REGISTRY_PATH)

    assert validate_manifest(manifest)
    assert sha256_hex(canonical_json_bytes(manifest)) == EXPECTED_MANIFEST_SHA256
    assert validate_registry(registry, {MANIFEST_PATH: manifest})
    assert sha256_hex(canonical_json_bytes(registry)) == EXPECTED_REGISTRY_SHA256


def test_genesis_manifest_pins_current_trusted_roots_and_policy_material() -> None:
    manifest = _load(MANIFEST_FILE)
    roots = manifest["roots"]
    material = manifest["policy_material"]

    assert isinstance(roots, dict)
    assert isinstance(material, dict)
    assert roots["builder"]["workflow_sha"] == "02beb48b9c8a61d67c585573aac6c5781c000e89"
    assert roots["verifier"]["workflow_sha"] == "0aa3dce24f9aeb0c90f955fa5f68d12685e5654a"
    assert (
        material["builder_environment_policy"]["sha256"]
        == "8bafd0ca5cd8705c16a44789a9e11b5bc54361261a5ff50c6f28a5d5f9c83b02"
    )
    assert (
        material["verifier_dependency_lock"]["sha256"]
        == "70c7c84e04443d173e667d965fcbbc05bb777a2772948180eca8c49a6c088950"
    )


def test_rotation_requires_exact_previous_manifest_digest() -> None:
    previous = _load(MANIFEST_FILE)
    current = copy.deepcopy(previous)
    current["generation"] = 1
    current["previous_manifest_sha256"] = sha256_hex(canonical_json_bytes(previous))

    assert validate_manifest(current, previous)

    current["previous_manifest_sha256"] = "0" * 64
    assert not validate_manifest(current, previous)


def test_registry_rejects_active_digest_or_history_tamper() -> None:
    manifest = _load(MANIFEST_FILE)
    registry = _load(REGISTRY_PATH)

    tampered = copy.deepcopy(registry)
    tampered["active_manifest_sha256"] = "f" * 64
    assert not validate_registry(tampered, {MANIFEST_PATH: manifest})

    tampered = copy.deepcopy(registry)
    tampered["history"][0]["manifest_sha256"] = "f" * 64
    assert not validate_registry(tampered, {MANIFEST_PATH: manifest})
