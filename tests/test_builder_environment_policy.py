from __future__ import annotations

import json
import zipfile
from pathlib import Path

from liminal.builder_environment_policy import (
    BuilderEnvironmentPolicy,
    authorize_recovery_proof_builder_environment,
)
from liminal.builder_environment_receipt import (
    BuilderActionPin,
    BuilderEnvironmentReceipt,
    write_builder_environment_receipt,
)
from liminal.recovery_proof_bundle import build_recovery_proof_bundle


WORKFLOW_SHA = "1" * 40
WORKFLOW_FILE_SHA256 = "2" * 64
LOCK_SHA256 = "3" * 64
PROOF_SCRIPT_SHA256 = "4" * 64
CHECKOUT_SHA = "5" * 40
ATTEST_SHA = "6" * 40


def _receipt(**overrides: object) -> BuilderEnvironmentReceipt:
    values: dict[str, object] = {
        "builder_repository": "owner/repo",
        "builder_workflow_path": ".github/workflows/builder.yml",
        "builder_workflow_sha": WORKFLOW_SHA,
        "builder_workflow_file_sha256": WORKFLOW_FILE_SHA256,
        "dependency_lock_path": "requirements/builder.lock",
        "dependency_lock_sha256": LOCK_SHA256,
        "proof_script_path": "scripts/proof.py",
        "proof_script_sha256": PROOF_SCRIPT_SHA256,
        "python_implementation": "CPython",
        "python_version": "3.11.15",
        "pip_version": "26.1.2",
        "runner_os": "Linux",
        "runner_arch": "X64",
        "runner_image_os": "ubuntu24",
        "runner_image_version": "20260720.247.2",
        "action_pins": (
            BuilderActionPin("actions/attest", ATTEST_SHA),
            BuilderActionPin("actions/checkout", CHECKOUT_SHA),
        ),
    }
    values.update(overrides)
    return BuilderEnvironmentReceipt(**values)  # type: ignore[arg-type]


def _policy(**overrides: object) -> BuilderEnvironmentPolicy:
    receipt = _receipt()
    values: dict[str, object] = {
        "builder_repository": receipt.builder_repository,
        "builder_workflow_path": receipt.builder_workflow_path,
        "builder_workflow_sha": receipt.builder_workflow_sha,
        "builder_workflow_file_sha256": receipt.builder_workflow_file_sha256,
        "dependency_lock_path": receipt.dependency_lock_path,
        "dependency_lock_sha256": receipt.dependency_lock_sha256,
        "proof_script_path": receipt.proof_script_path,
        "proof_script_sha256": receipt.proof_script_sha256,
        "python_implementation": receipt.python_implementation,
        "python_version": receipt.python_version,
        "pip_version": receipt.pip_version,
        "runner_os": receipt.runner_os,
        "runner_arch": receipt.runner_arch,
        "runner_image_os": receipt.runner_image_os,
        "runner_image_version": None,
        "action_pins": receipt.action_pins,
    }
    values.update(overrides)
    return BuilderEnvironmentPolicy(**values)  # type: ignore[arg-type]


def _bundle(tmp_path: Path, receipt: BuilderEnvironmentReceipt | None = None) -> Path:
    root = tmp_path / "proof"
    root.mkdir(parents=True, exist_ok=True)
    write_builder_environment_receipt(receipt or _receipt(), root / "builder-environment.json")
    (root / "decision-receipt.json").write_text('{"receipt":"ok"}\n', encoding="utf-8")
    (root / "public-key.json").write_text('{"key":"public"}\n', encoding="utf-8")
    (root / "recovery-evidence.jsonl").write_text('{"attempt":1}\n', encoding="utf-8")
    (root / "summary.json").write_text('{"summary":"ok"}\n', encoding="utf-8")
    return build_recovery_proof_bundle(root).path


def test_authorizes_exact_environment_from_verified_bundle(tmp_path: Path) -> None:
    result = authorize_recovery_proof_builder_environment(_bundle(tmp_path), policy=_policy())

    assert result.authorized
    assert result.reason == "builder_environment_authorized"
    assert result.receipt is not None
    assert result.receipt.runner_image_version == "20260720.247.2"


def test_runner_image_patch_version_is_audited_but_not_required_by_family_policy(
    tmp_path: Path,
) -> None:
    bundle = _bundle(tmp_path, _receipt(runner_image_version="future-patch"))
    result = authorize_recovery_proof_builder_environment(bundle, policy=_policy())

    assert result.authorized
    assert result.receipt is not None
    assert result.receipt.runner_image_version == "future-patch"


def test_rejects_wrong_dependency_lock(tmp_path: Path) -> None:
    result = authorize_recovery_proof_builder_environment(
        _bundle(tmp_path, _receipt(dependency_lock_sha256="9" * 64)),
        policy=_policy(),
    )

    assert not result.authorized
    assert result.reason == "dependency_lock_not_authorized"


def test_rejects_wrong_python_version(tmp_path: Path) -> None:
    result = authorize_recovery_proof_builder_environment(
        _bundle(tmp_path, _receipt(python_version="3.12.0")),
        policy=_policy(),
    )

    assert not result.authorized
    assert result.reason == "python_version_not_authorized"


def test_rejects_wrong_action_pin(tmp_path: Path) -> None:
    result = authorize_recovery_proof_builder_environment(
        _bundle(
            tmp_path,
            _receipt(
                action_pins=(
                    BuilderActionPin("actions/attest", "7" * 40),
                    BuilderActionPin("actions/checkout", CHECKOUT_SHA),
                )
            ),
        ),
        policy=_policy(),
    )

    assert not result.authorized
    assert result.reason == "action_pins_not_authorized"


def test_rejects_wrong_runner_family(tmp_path: Path) -> None:
    result = authorize_recovery_proof_builder_environment(
        _bundle(tmp_path, _receipt(runner_image_os="ubuntu26")),
        policy=_policy(),
    )

    assert not result.authorized
    assert result.reason == "runner_image_family_not_authorized"


def test_rejects_tampered_bundle_before_environment_policy(tmp_path: Path) -> None:
    bundle = _bundle(tmp_path)
    tampered = tmp_path / "tampered.zip"
    with zipfile.ZipFile(bundle, mode="r") as source, zipfile.ZipFile(
        tampered, mode="w", compression=zipfile.ZIP_STORED
    ) as target:
        for name in source.namelist():
            payload = source.read(name)
            if name == "summary.json":
                payload = json.dumps({"summary": "tampered"}).encode("utf-8")
            target.writestr(name, payload)

    result = authorize_recovery_proof_builder_environment(tampered, policy=_policy())

    assert not result.authorized
    assert result.reason == "recovery_proof_bundle_not_verified"
