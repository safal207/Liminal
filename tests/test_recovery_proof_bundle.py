from __future__ import annotations

import zipfile
from pathlib import Path

import pytest

from liminal.recovery_proof_bundle import (
    PROOF_BUNDLE_NAME,
    PROOF_MANIFEST_NAME,
    PROOF_MEMBERS,
    build_recovery_proof_bundle,
    verify_recovery_proof_bundle,
)


def _write_members(root: Path) -> None:
    root.mkdir(parents=True, exist_ok=True)
    payloads = {
        "builder-environment.json": b'{"builder":"ok"}\n',
        "decision-receipt.json": b'{"receipt":"ok"}\n',
        "public-key.json": b'{"key":"public"}\n',
        "recovery-evidence.jsonl": b'{"attempt":1}\n{"attempt":2}\n',
        "summary.json": b'{"summary":"ok"}\n',
    }
    for name, payload in payloads.items():
        (root / name).write_bytes(payload)


def test_bundle_is_byte_deterministic_and_self_verifying(tmp_path: Path) -> None:
    first = tmp_path / "first"
    second = tmp_path / "second"
    _write_members(first)
    _write_members(second)

    bundle_a = build_recovery_proof_bundle(first)
    bundle_b = build_recovery_proof_bundle(second)

    assert bundle_a.sha256 == bundle_b.sha256
    assert bundle_a.manifest_sha256 == bundle_b.manifest_sha256
    assert bundle_a.path.read_bytes() == bundle_b.path.read_bytes()
    assert verify_recovery_proof_bundle(bundle_a.path)
    assert verify_recovery_proof_bundle(bundle_b.path)

    with zipfile.ZipFile(bundle_a.path) as archive:
        assert set(archive.namelist()) == set((*PROOF_MEMBERS, PROOF_MANIFEST_NAME))


def test_bundle_verifier_rejects_tampered_member(tmp_path: Path) -> None:
    root = tmp_path / "proof"
    _write_members(root)
    bundle = build_recovery_proof_bundle(root)

    tampered = tmp_path / "tampered.zip"
    with zipfile.ZipFile(bundle.path, mode="r") as source, zipfile.ZipFile(
        tampered, mode="w", compression=zipfile.ZIP_STORED
    ) as target:
        for name in source.namelist():
            payload = source.read(name)
            if name == "builder-environment.json":
                payload = b'{"builder":"tampered"}\n'
            target.writestr(name, payload)

    assert not verify_recovery_proof_bundle(tampered)


def test_bundle_requires_every_evidence_member(tmp_path: Path) -> None:
    root = tmp_path / "proof"
    _write_members(root)
    (root / "builder-environment.json").unlink()

    with pytest.raises(
        FileNotFoundError, match="recovery_proof_member_missing:builder-environment.json"
    ):
        build_recovery_proof_bundle(root)


def test_default_bundle_name_is_stable(tmp_path: Path) -> None:
    root = tmp_path / "proof"
    _write_members(root)
    bundle = build_recovery_proof_bundle(root)
    assert bundle.path.name == PROOF_BUNDLE_NAME
