"""Machine-readable environment evidence for trusted recovery proof builders."""

from __future__ import annotations

import hashlib
import importlib.metadata
import json
import platform
import re
from dataclasses import dataclass
from pathlib import Path


BUILDER_ENVIRONMENT_SCHEMA_VERSION = "liminal.builder-environment-receipt.v0.1"
_SHA1_RE = re.compile(r"^[0-9a-f]{40}$")
_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")


def _sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def _canonical_json(value: object) -> bytes:
    return (json.dumps(value, sort_keys=True, separators=(",", ":")) + "\n").encode("utf-8")


def _relative_path(value: str) -> str:
    path = Path(value)
    if not value or path.is_absolute() or ".." in path.parts:
        raise ValueError("builder_environment_path_invalid")
    return path.as_posix()


def _sha1(value: str, *, reason: str) -> str:
    normalized = value.strip().lower()
    if not _SHA1_RE.fullmatch(normalized):
        raise ValueError(reason)
    return normalized


@dataclass(frozen=True)
class BuilderActionPin:
    action: str
    sha: str

    def __post_init__(self) -> None:
        action = self.action.strip()
        if not action or "@" in action:
            raise ValueError("builder_environment_action_invalid")
        object.__setattr__(self, "action", action)
        object.__setattr__(self, "sha", _sha1(self.sha, reason="builder_environment_action_sha_invalid"))

    def as_dict(self) -> dict[str, str]:
        return {"action": self.action, "sha": self.sha}


@dataclass(frozen=True)
class BuilderEnvironmentReceipt:
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
    runner_image_version: str
    action_pins: tuple[BuilderActionPin, ...]
    schema_version: str = BUILDER_ENVIRONMENT_SCHEMA_VERSION

    def __post_init__(self) -> None:
        if self.schema_version != BUILDER_ENVIRONMENT_SCHEMA_VERSION:
            raise ValueError("builder_environment_schema_version_invalid")
        repository = self.builder_repository.strip()
        if not repository or "/" not in repository:
            raise ValueError("builder_environment_repository_invalid")
        object.__setattr__(self, "builder_repository", repository)
        object.__setattr__(self, "builder_workflow_path", _relative_path(self.builder_workflow_path))
        object.__setattr__(
            self,
            "builder_workflow_sha",
            _sha1(self.builder_workflow_sha, reason="builder_environment_workflow_sha_invalid"),
        )
        object.__setattr__(self, "dependency_lock_path", _relative_path(self.dependency_lock_path))
        object.__setattr__(self, "proof_script_path", _relative_path(self.proof_script_path))
        for value in (
            self.builder_workflow_file_sha256,
            self.dependency_lock_sha256,
            self.proof_script_sha256,
        ):
            if not _SHA256_RE.fullmatch(value):
                raise ValueError("builder_environment_file_sha256_invalid")
        for value in (
            self.python_implementation,
            self.python_version,
            self.pip_version,
            self.runner_os,
            self.runner_arch,
            self.runner_image_os,
            self.runner_image_version,
        ):
            if not value.strip():
                raise ValueError("builder_environment_field_empty")
        actions = tuple(sorted(self.action_pins, key=lambda item: item.action))
        if not actions or len({item.action for item in actions}) != len(actions):
            raise ValueError("builder_environment_actions_invalid")
        object.__setattr__(self, "action_pins", actions)

    def as_dict(self) -> dict[str, object]:
        return {
            "schema_version": self.schema_version,
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

    def canonical_bytes(self) -> bytes:
        return _canonical_json(self.as_dict())


def build_builder_environment_receipt(
    *,
    repository_root: str | Path,
    builder_repository: str,
    builder_workflow_path: str,
    builder_workflow_sha: str,
    dependency_lock_path: str,
    proof_script_path: str,
    runner_os: str,
    runner_arch: str,
    runner_image_os: str,
    runner_image_version: str,
    action_pins: tuple[BuilderActionPin, ...],
) -> BuilderEnvironmentReceipt:
    root = Path(repository_root)
    workflow_path = _relative_path(builder_workflow_path)
    lock_path = _relative_path(dependency_lock_path)
    script_path = _relative_path(proof_script_path)
    return BuilderEnvironmentReceipt(
        builder_repository=builder_repository,
        builder_workflow_path=workflow_path,
        builder_workflow_sha=builder_workflow_sha,
        builder_workflow_file_sha256=_sha256((root / workflow_path).read_bytes()),
        dependency_lock_path=lock_path,
        dependency_lock_sha256=_sha256((root / lock_path).read_bytes()),
        proof_script_path=script_path,
        proof_script_sha256=_sha256((root / script_path).read_bytes()),
        python_implementation=platform.python_implementation(),
        python_version=platform.python_version(),
        pip_version=importlib.metadata.version("pip"),
        runner_os=runner_os,
        runner_arch=runner_arch,
        runner_image_os=runner_image_os,
        runner_image_version=runner_image_version,
        action_pins=action_pins,
    )


def write_builder_environment_receipt(receipt: BuilderEnvironmentReceipt, path: str | Path) -> None:
    target = Path(path)
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_bytes(receipt.canonical_bytes())


def _receipt_from_dict(value: object) -> BuilderEnvironmentReceipt:
    if not isinstance(value, dict):
        raise ValueError("builder_environment_receipt_invalid")
    builder = value.get("builder")
    inputs = value.get("inputs")
    runtime = value.get("runtime")
    actions = value.get("actions")
    if not isinstance(builder, dict) or not isinstance(inputs, dict) or not isinstance(runtime, dict):
        raise ValueError("builder_environment_receipt_invalid")
    lock = inputs.get("dependency_lock")
    script = inputs.get("proof_script")
    if not isinstance(lock, dict) or not isinstance(script, dict) or not isinstance(actions, list):
        raise ValueError("builder_environment_receipt_invalid")
    return BuilderEnvironmentReceipt(
        schema_version=str(value.get("schema_version", "")),
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
        runner_image_version=str(runtime.get("runner_image_version", "")),
        action_pins=tuple(
            BuilderActionPin(action=str(item.get("action", "")), sha=str(item.get("sha", "")))
            for item in actions
            if isinstance(item, dict)
        ),
    )


def parse_builder_environment_receipt(raw: bytes) -> BuilderEnvironmentReceipt:
    """Parse canonical receipt bytes and reject non-canonical JSON."""

    receipt = _receipt_from_dict(json.loads(raw))
    if raw != receipt.canonical_bytes():
        raise ValueError("builder_environment_receipt_not_canonical")
    return receipt


def verify_builder_environment_receipt(
    path: str | Path,
    *,
    repository_root: str | Path,
    expected_builder_repository: str | None = None,
    expected_builder_workflow_sha: str | None = None,
) -> bool:
    try:
        receipt = parse_builder_environment_receipt(Path(path).read_bytes())
        if expected_builder_repository is not None and receipt.builder_repository != expected_builder_repository:
            return False
        if expected_builder_workflow_sha is not None:
            expected_sha = _sha1(
                expected_builder_workflow_sha,
                reason="builder_environment_expected_workflow_sha_invalid",
            )
            if receipt.builder_workflow_sha != expected_sha:
                return False
        root = Path(repository_root)
        expected_files = (
            (receipt.builder_workflow_path, receipt.builder_workflow_file_sha256),
            (receipt.dependency_lock_path, receipt.dependency_lock_sha256),
            (receipt.proof_script_path, receipt.proof_script_sha256),
        )
        for relative, expected_hash in expected_files:
            candidate = root / relative
            if not candidate.is_file() or _sha256(candidate.read_bytes()) != expected_hash:
                return False
        return True
    except (OSError, json.JSONDecodeError, ValueError, importlib.metadata.PackageNotFoundError):
        return False
