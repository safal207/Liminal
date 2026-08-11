from __future__ import annotations

import json
from pathlib import Path

import pytest

from liminal.builder_environment_receipt import (
    BUILDER_ENVIRONMENT_SCHEMA_VERSION,
    BuilderActionPin,
    build_builder_environment_receipt,
    verify_builder_environment_receipt,
    write_builder_environment_receipt,
)


WORKFLOW_SHA = "a" * 40


def _repository(root: Path) -> None:
    (root / ".github/workflows").mkdir(parents=True)
    (root / "requirements").mkdir(parents=True)
    (root / "scripts").mkdir(parents=True)
    (root / ".github/workflows/trusted-recovery-proof-builder.yml").write_text(
        "name: trusted\n", encoding="utf-8"
    )
    (root / "requirements/trusted-recovery-proof.lock").write_text(
        "example==1.0 \\\n    --hash=sha256:" + "b" * 64 + "\n",
        encoding="utf-8",
    )
    (root / "scripts/run-live-recovery-decision-proof.py").write_text(
        "print('proof')\n", encoding="utf-8"
    )


def _receipt(root: Path):
    return build_builder_environment_receipt(
        repository_root=root,
        builder_repository="safal207/Liminal",
        builder_workflow_path=".github/workflows/trusted-recovery-proof-builder.yml",
        builder_workflow_sha=WORKFLOW_SHA,
        dependency_lock_path="requirements/trusted-recovery-proof.lock",
        proof_script_path="scripts/run-live-recovery-decision-proof.py",
        runner_os="Linux",
        runner_arch="X64",
        runner_image_os="ubuntu24",
        runner_image_version="20260720.247.2",
        action_pins=(
            BuilderActionPin(action="actions/setup-python", sha="c" * 40),
            BuilderActionPin(action="actions/checkout", sha="d" * 40),
        ),
    )


def test_builder_environment_receipt_is_canonical_and_self_verifying(tmp_path: Path) -> None:
    _repository(tmp_path)
    receipt = _receipt(tmp_path)
    target = tmp_path / "builder-environment.json"
    write_builder_environment_receipt(receipt, target)

    assert receipt.schema_version == BUILDER_ENVIRONMENT_SCHEMA_VERSION
    assert verify_builder_environment_receipt(
        target,
        repository_root=tmp_path,
        expected_builder_repository="safal207/Liminal",
        expected_builder_workflow_sha=WORKFLOW_SHA,
    )
    parsed = json.loads(target.read_text(encoding="utf-8"))
    assert [item["action"] for item in parsed["actions"]] == [
        "actions/checkout",
        "actions/setup-python",
    ]


def test_builder_environment_receipt_rejects_tampered_lock(tmp_path: Path) -> None:
    _repository(tmp_path)
    target = tmp_path / "builder-environment.json"
    write_builder_environment_receipt(_receipt(tmp_path), target)
    (tmp_path / "requirements/trusted-recovery-proof.lock").write_text(
        "tampered\n", encoding="utf-8"
    )

    assert not verify_builder_environment_receipt(target, repository_root=tmp_path)


def test_builder_environment_receipt_rejects_wrong_expected_builder(tmp_path: Path) -> None:
    _repository(tmp_path)
    target = tmp_path / "builder-environment.json"
    write_builder_environment_receipt(_receipt(tmp_path), target)

    assert not verify_builder_environment_receipt(
        target,
        repository_root=tmp_path,
        expected_builder_workflow_sha="e" * 40,
    )


def test_builder_environment_receipt_rejects_noncanonical_json(tmp_path: Path) -> None:
    _repository(tmp_path)
    target = tmp_path / "builder-environment.json"
    receipt = _receipt(tmp_path)
    target.write_text(json.dumps(receipt.as_dict(), indent=2) + "\n", encoding="utf-8")

    assert not verify_builder_environment_receipt(target, repository_root=tmp_path)


def test_builder_action_pin_requires_immutable_sha() -> None:
    with pytest.raises(ValueError, match="builder_environment_action_sha_invalid"):
        BuilderActionPin(action="actions/checkout", sha="v4")
