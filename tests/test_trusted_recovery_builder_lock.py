from __future__ import annotations

import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
LOCK = ROOT / "requirements" / "trusted-recovery-proof.lock"
WORKFLOW = ROOT / ".github" / "workflows" / "trusted-recovery-proof-builder.yml"
SHA256_RE = re.compile(r"^--hash=sha256:([0-9a-f]{64})$")


def _locked_requirements() -> dict[str, tuple[str, str]]:
    lines = [
        line.strip()
        for line in LOCK.read_text(encoding="utf-8").splitlines()
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
    locked = _locked_requirements()
    assert len(locked) == 20
    assert locked["openai"][0] == "2.53.0"
    assert locked["httpx"][0] == "0.28.1"
    assert locked["structlog"][0] == "26.1.0"
    assert locked["cryptography"][0] == "50.0.0"


def test_trusted_builder_uses_pinned_platform_python_and_hash_mode() -> None:
    workflow = WORKFLOW.read_text(encoding="utf-8")
    assert "runs-on: ubuntu-24.04" in workflow
    assert 'python-version: "3.11.15"' in workflow
    assert "--require-hashes" in workflow
    assert "--only-binary=:all:" in workflow
    assert "-r requirements/trusted-recovery-proof.lock" in workflow
    assert "python -m pip check" in workflow
