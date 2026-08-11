from __future__ import annotations

import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
BUILDER_LOCK = ROOT / "requirements" / "trusted-recovery-proof.lock"
VERIFIER_LOCK = ROOT / "requirements" / "trusted-attestation-verifier.lock"
BUILDER_WORKFLOW = ROOT / ".github" / "workflows" / "trusted-recovery-proof-builder.yml"
WRAPPER_WORKFLOW = ROOT / ".github" / "workflows" / "trusted-recovery-decision-proof.yml"
TRUSTED_BUILDER_SHA = "ef9a0707301166a6be0561c7ddff308e661b7812"
SHA256_RE = re.compile(r"^--hash=sha256:([0-9a-f]{64})$")


def _locked_requirements(path: Path) -> dict[str, tuple[str, str]]:
    lines = [
        line.strip()
        for line in path.read_text(encoding="utf-8").splitlines()
        if line.strip() and not line.lstrip().startswith("#")
    ]
    assert len(lines) % 2 == 0

    locked: dict[str, tuple[str, str]] = {}
    for index in range(0, len(lines), 2):
        requirement_line = lines[index]
        hash_line = lines[index + 1]
        assert requirement_line.endswith("\\")
        requirement = requirement_line[:-1].strip()
        name, version = requirement.split("==", 1)
        match = SHA256_RE.fullmatch(hash_line)
        assert match is not None
        assert name not in locked
        locked[name] = (version, match.group(1))
    return locked


def test_trusted_builder_dependency_closure_is_fully_hash_locked() -> None:
    locked = _locked_requirements(BUILDER_LOCK)
    assert len(locked) == 20
    assert locked["openai"][0] == "2.53.0"
    assert locked["httpx"][0] == "0.28.1"
    assert locked["structlog"][0] == "26.1.0"
    assert locked["cryptography"][0] == "50.0.0"


def test_trusted_verifier_dependency_closure_is_fully_hash_locked() -> None:
    locked = _locked_requirements(VERIFIER_LOCK)
    assert set(locked) == {"cffi", "cryptography", "pycparser"}
    assert locked["cryptography"][0] == "50.0.0"


def test_trusted_builder_uses_pinned_platform_python_and_hash_mode() -> None:
    workflow = BUILDER_WORKFLOW.read_text(encoding="utf-8")
    assert "runs-on: ubuntu-24.04" in workflow
    assert 'python-version: "3.11.15"' in workflow
    assert "--require-hashes" in workflow
    assert "--only-binary=:all:" in workflow
    assert "-r requirements/trusted-recovery-proof.lock" in workflow
    assert "python -m pip check" in workflow


def test_trusted_builder_emits_environment_receipt_before_provider_call() -> None:
    workflow = BUILDER_WORKFLOW.read_text(encoding="utf-8")
    assert "Trusted Recovery Proof Builder v0.3" in workflow
    assert "LIMINAL_BUILDER_REPOSITORY: ${{ job.workflow_repository }}" in workflow
    assert "LIMINAL_BUILDER_WORKFLOW_SHA: ${{ job.workflow_sha }}" in workflow
    assert "write-builder-environment-receipt.py" in workflow
    assert "builder-environment.json" in workflow
    assert "actions/attest=1e69f48acb82d1966a394da916b4c1698aa569d6" in workflow
    assert workflow.index("Emit verified builder environment receipt") < workflow.index(
        "Run trusted live Gonka recovery decision proof"
    )


def test_wrapper_pins_v0_3_builder_and_hash_locked_verifier() -> None:
    workflow = WRAPPER_WORKFLOW.read_text(encoding="utf-8")
    assert f"trusted-recovery-proof-builder.yml@{TRUSTED_BUILDER_SHA}" in workflow
    assert f"--signer-digest {TRUSTED_BUILDER_SHA}" in workflow
    assert f"--signer-ref {TRUSTED_BUILDER_SHA}" in workflow
    assert "runs-on: ubuntu-24.04" in workflow
    assert 'python-version: "3.11.15"' in workflow
    assert "--require-hashes" in workflow
    assert "-r requirements/trusted-attestation-verifier.lock" in workflow
