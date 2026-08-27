"""Authorization policy for environment receipts embedded in attested proof bundles.

Cryptographic provenance and environment authorization are deliberately separate.
Callers must first verify the outer GitHub/Sigstore attestation. This module then
verifies the canonical proof bundle and authorizes the embedded builder environment
against an explicit, fail-closed policy.
"""

from __future__ import annotations

import json
import re
import zipfile
from dataclasses import dataclass
from pathlib import Path

from liminal.builder_environment_receipt import (
    BuilderActionPin,
    BuilderEnvironmentReceipt,
    parse_builder_environment_receipt,
)
from liminal.recovery_proof_bundle import verify_recovery_proof_bundle


BUILDER_ENVIRONMENT_AUTHORIZATION_SCHEMA_VERSION = (
    "liminal.builder-environment-authorization.v0.1"
)
BUILDER_ENVIRONMENT_MEMBER = "builder-environment.json"
_SHA1_RE = re.compile(r"^[0-9a-f]{40}$")
_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")


def _sha1(value: str, *, reason: str) -> str:
    normalized = value.strip().lower()
    if not _SHA1_RE.fullmatch(normalized):
        raise ValueError(reason)
    return normalized


def _sha256(value: str, *, reason: str) -> str:
    normalized = value.strip().lower()
    if not _SHA256_RE.fullmatch(normalized):
        raise ValueError(reason)
    return normalized


def _relative_path(value: str, *, reason: str) -> str:
    path = Path(value)
    if not value or path.is_absolute() or ".." in path.parts:
        raise ValueError(reason)
    return path.as_posix()


@dataclass(frozen=True)
class BuilderEnvironmentPolicy:
    builder_repository: str
    builder_workflow_path: str
    builder_workflow_sha: str
    builder_workflow_file_sha256: str
    dependency_lock_path: str
    dependency_lock_sha256: str
    proof_script_path: str
    proof_script_sha256: str
    python_implementation: str
    python_version: str
    pip_version: str
    runner_os: str
    runner_arch: str
    runner_image_os: str
    action_pins: tuple[BuilderActionPin, ...]
    runner_image_version: str | None = None

    def __post_init__(self) -> None:
        repository = self.builder_repository.strip()
        if not repository or "/" not in repository:
            raise ValueError("builder_environment_policy_repository_invalid")
        object.__setattr__(self, "builder_repository", repository)
        object.__setattr__(
            self,
            "builder_workflow_path",
            _relative_path(
                self.builder_workflow_path,
                reason="builder_environment_policy_workflow_path_invalid",
            ),
        )
        object.__setattr__(
            self,
            "builder_workflow_sha",
            _sha1(
                self.builder_workflow_sha,
                reason="builder_environment_policy_workflow_sha_invalid",
            ),
        )
        object.__setattr__(
            self,
            "builder_workflow_file_sha256",
            _sha256(
                self.builder_workflow_file_sha256,
                reason="builder_environment_policy_workflow_file_sha256_invalid",
            ),
        )
        object.__setattr__(
            self,
            "dependency_lock_path",
            _relative_path(
                self.dependency_lock_path,
                reason="builder_environment_policy_dependency_lock_path_invalid",
            ),
        )
        object.__setattr__(
            self,
            "dependency_lock_sha256",
            _sha256(
                self.dependency_lock_sha256,
                reason="builder_environment_policy_dependency_lock_sha256_invalid",
            ),
        )
        object.__setattr__(
            self,
            "proof_script_path",
            _relative_path(
                self.proof_script_path,
                reason="builder_environment_policy_proof_script_path_invalid",
            ),
        )
        object.__setattr__(
            self,
            "proof_script_sha256",
            _sha256(
                self.proof_script_sha256,
                reason="builder_environment_policy_proof_script_sha256_invalid",
            ),
        )
        for name, value in (
            ("python_implementation", self.python_implementation),
            ("python_version", self.python_version),
            ("pip_version", self.pip_version),
            ("runner_os", self.runner_os),
            ("runner_arch", self.runner_arch),
            ("runner_image_os", self.runner_image_os),
        ):
            if not value.strip():
                raise ValueError(f"builder_environment_policy_{name}_required")
        if self.runner_image_version is not None and not self.runner_image_version.strip():
            raise ValueError("builder_environment_policy_runner_image_version_invalid")
        actions = tuple(sorted(self.action_pins, key=lambda item: item.action))
        if not actions or len({item.action for item in actions}) != len(actions):
            raise ValueError("builder_environment_policy_actions_invalid")
        object.__setattr__(self, "action_pins", actions)

    def as_dict(self) -> dict[str, object]:
        return {
            "builder": {
                "repository": self.builder_repository,
                "workflow_path": self.builder_workflow_path,
                "workflow_sha": self.builder_workflow_sha,
                "workflow_file_sha256": self.builder_workflow_file_sha256,
            },
            "inputs": {
                "dependency_lock": {
                    "path": self.dependency_lock_path,
                    "sha256": self.dependency_lock_sha256,
                },
                "proof_script": {
                    "path": self.proof_script_path,
                    "sha256": self.proof_script_sha256,
                },
            },
            "runtime": {
                "python_implementation": self.python_implementation,
                "python_version": self.python_version,
                "pip_version": self.pip_version,
                "runner_os": self.runner_os,
                "runner_arch": self.runner_arch,
                "runner_image_os": self.runner_image_os,
                "runner_image_version": self.runner_image_version,
            },
            "actions": [item.as_dict() for item in self.action_pins],
        }


@dataclass(frozen=True)
class BuilderEnvironmentAuthorization:
    authorized: bool
    reason: str
    receipt: BuilderEnvironmentReceipt | None


def policy_from_dict(value: object) -> BuilderEnvironmentPolicy:
    if not isinstance(value, dict):
        raise ValueError("builder_environment_policy_invalid")
    builder = value.get("builder")
    inputs = value.get("inputs")
    runtime = value.get("runtime")
    actions = value.get("actions")
    if not isinstance(builder, dict) or not isinstance(inputs, dict) or not isinstance(runtime, dict):
        raise ValueError("builder_environment_policy_invalid")
    lock = inputs.get("dependency_lock")
    script = inputs.get("proof_script")
    if not isinstance(lock, dict) or not isinstance(script, dict) or not isinstance(actions, list):
        raise ValueError("builder_environment_policy_invalid")
    runner_image_version = runtime.get("runner_image_version")
    return BuilderEnvironmentPolicy(
        builder_repository=str(builder.get("repository", "")),
        builder_workflow_path=str(builder.get("workflow_path", "")),
        builder_workflow_sha=str(builder.get("workflow_sha", "")),
        builder_workflow_file_sha256=str(builder.get("workflow_file_sha256", "")),
        dependency_lock_path=str(lock.get("path", "")),
        dependency_lock_sha256=str(lock.get("sha256", "")),
        proof_script_path=str(script.get("path", "")),
        proof_script_sha256=str(script.get("sha256", "")),
        python_implementation=str(runtime.get("python_implementation", "")),
        python_version=str(runtime.get("python_version", "")),
        pip_version=str(runtime.get("pip_version", "")),
        runner_os=str(runtime.get("runner_os", "")),
        runner_arch=str(runtime.get("runner_arch", "")),
        runner_image_os=str(runtime.get("runner_image_os", "")),
        runner_image_version=(
            None if runner_image_version is None else str(runner_image_version)
        ),
        action_pins=tuple(
            BuilderActionPin(action=str(item.get("action", "")), sha=str(item.get("sha", "")))
            for item in actions
            if isinstance(item, dict)
        ),
    )


def load_builder_environment_policy(path: str | Path) -> BuilderEnvironmentPolicy:
    return policy_from_dict(json.loads(Path(path).read_text(encoding="utf-8")))


def _actions(receipt: BuilderEnvironmentReceipt) -> tuple[tuple[str, str], ...]:
    return tuple((item.action, item.sha) for item in receipt.action_pins)


def authorize_builder_environment_receipt(
    receipt: BuilderEnvironmentReceipt,
    *,
    policy: BuilderEnvironmentPolicy,
) -> BuilderEnvironmentAuthorization:
    checks = (
        (
            receipt.builder_repository == policy.builder_repository,
            "builder_repository_not_authorized",
        ),
        (
            receipt.builder_workflow_path == policy.builder_workflow_path,
            "builder_workflow_path_not_authorized",
        ),
        (
            receipt.builder_workflow_sha == policy.builder_workflow_sha,
            "builder_workflow_sha_not_authorized",
        ),
        (
            receipt.builder_workflow_file_sha256 == policy.builder_workflow_file_sha256,
            "builder_workflow_file_not_authorized",
        ),
        (
            receipt.dependency_lock_path == policy.dependency_lock_path,
            "dependency_lock_path_not_authorized",
        ),
        (
            receipt.dependency_lock_sha256 == policy.dependency_lock_sha256,
            "dependency_lock_not_authorized",
        ),
        (
            receipt.proof_script_path == policy.proof_script_path,
            "proof_script_path_not_authorized",
        ),
        (
            receipt.proof_script_sha256 == policy.proof_script_sha256,
            "proof_script_not_authorized",
        ),
        (
            receipt.python_implementation == policy.python_implementation,
            "python_implementation_not_authorized",
        ),
        (receipt.python_version == policy.python_version, "python_version_not_authorized"),
        (receipt.pip_version == policy.pip_version, "pip_version_not_authorized"),
        (receipt.runner_os == policy.runner_os, "runner_os_not_authorized"),
        (receipt.runner_arch == policy.runner_arch, "runner_arch_not_authorized"),
        (
            receipt.runner_image_os == policy.runner_image_os,
            "runner_image_family_not_authorized",
        ),
        (
            policy.runner_image_version is None
            or receipt.runner_image_version == policy.runner_image_version,
            "runner_image_version_not_authorized",
        ),
        (_actions(receipt) == _actions_from_policy(policy), "action_pins_not_authorized"),
    )
    for passed, reason in checks:
        if not passed:
            return BuilderEnvironmentAuthorization(False, reason, receipt)
    return BuilderEnvironmentAuthorization(
        True,
        "builder_environment_authorized",
        receipt,
    )


def _actions_from_policy(policy: BuilderEnvironmentPolicy) -> tuple[tuple[str, str], ...]:
    return tuple((item.action, item.sha) for item in policy.action_pins)


def authorize_recovery_proof_builder_environment(
    bundle_path: str | Path,
    *,
    policy: BuilderEnvironmentPolicy,
) -> BuilderEnvironmentAuthorization:
    """Authorize only the environment receipt embedded in a verified proof bundle."""

    path = Path(bundle_path)
    if not verify_recovery_proof_bundle(path):
        return BuilderEnvironmentAuthorization(False, "recovery_proof_bundle_not_verified", None)
    try:
        with zipfile.ZipFile(path, mode="r") as archive:
            raw = archive.read(BUILDER_ENVIRONMENT_MEMBER)
        receipt = parse_builder_environment_receipt(raw)
    except (OSError, KeyError, json.JSONDecodeError, ValueError, zipfile.BadZipFile) as exc:
        return BuilderEnvironmentAuthorization(False, str(exc), None)
    return authorize_builder_environment_receipt(receipt, policy=policy)
