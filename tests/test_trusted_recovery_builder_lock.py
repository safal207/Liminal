from __future__ import annotations

import json
import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
BUILDER_LOCK = ROOT / "requirements" / "trusted-recovery-proof.lock"
VERIFIER_LOCK = ROOT / "requirements" / "trusted-attestation-verifier.lock"
BUILDER_WORKFLOW = ROOT / ".github" / "workflows" / "trusted-recovery-proof-builder.yml"
VERIFIER_WORKFLOW = ROOT / ".github" / "workflows" / "trusted-recovery-proof-verifier.yml"
WRAPPER_WORKFLOW = ROOT / ".github" / "workflows" / "trusted-recovery-decision-proof.yml"
ENVIRONMENT_POLICY = ROOT / "policies" / "trusted-recovery-proof-builder-v0.3.json"
TRUSTED_BUILDER_SHA = "02beb48b9c8a61d67c585573aac6c5781c000e89"
TRUSTED_VERIFIER_SHA = "0aa3dce24f9aeb0c90f955fa5f68d12685e5654a"
TRUSTED_BUILDER_FILE_SHA256 = "2960a30a4dfbb2c57dba862b6050474f0ea82c101616f7e54277970db1c6878a"
TRUSTED_LOCK_SHA256 = "436c88acf5f98d4521b681c133bb68e7598148d27102d070478c0b53f1c6b2d0"
TRUSTED_PROOF_SCRIPT_SHA256 = "37ff22aec93b7b2d94c71d14202fc0c579e9492e3723cf6e72197db9c65b4cde"
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


def test_candidate_verifier_is_self_pinned_and_attests_authorization_receipt() -> None:
    workflow = VERIFIER_WORKFLOW.read_text(encoding="utf-8")
    assert "Trusted Recovery Proof Verifier v0.1" in workflow
    assert "ref: ${{ job.workflow_sha }}" in workflow
    assert "LIMINAL_VERIFIER_REPOSITORY: ${{ job.workflow_repository }}" in workflow
    assert "LIMINAL_VERIFIER_WORKFLOW_SHA: ${{ job.workflow_sha }}" in workflow
    assert f"--signer-digest {TRUSTED_BUILDER_SHA}" in workflow
    assert f"--signer-ref {TRUSTED_BUILDER_SHA}" in workflow
    assert "--require-hashes" in workflow
    assert "-r requirements/trusted-attestation-verifier.lock" in workflow
    assert "check-builder-environment-authorization.py" in workflow
    assert "write-recovery-trust-authorization.py" in workflow
    assert "recovery-trust-authorization.json" in workflow
    assert "actions/attest@1e69f48acb82d1966a394da916b4c1698aa569d6" in workflow
    assert workflow.index("Emit recovery trust authorization receipt") < workflow.index(
        "Attest recovery trust authorization receipt"
    )


def test_wrapper_pins_builder_and_immutable_verifier_policy_root() -> None:
    workflow = WRAPPER_WORKFLOW.read_text(encoding="utf-8")
    assert f"trusted-recovery-proof-builder.yml@{TRUSTED_BUILDER_SHA}" in workflow
    assert f"trusted-recovery-proof-verifier.yml@{TRUSTED_VERIFIER_SHA}" in workflow
    assert "source_ref: ${{ github.ref }}" in workflow
    assert "verify-and-attest-policy-root:" in workflow
    assert "check-builder-environment-authorization.py" not in workflow
    assert "check-github-attestation-identity.py" not in workflow


def test_environment_policy_pins_trusted_v0_3_critical_inputs() -> None:
    policy = json.loads(ENVIRONMENT_POLICY.read_text(encoding="utf-8"))

    assert policy["builder"]["workflow_sha"] == TRUSTED_BUILDER_SHA
    assert policy["builder"]["workflow_file_sha256"] == TRUSTED_BUILDER_FILE_SHA256
    assert policy["inputs"]["dependency_lock"]["sha256"] == TRUSTED_LOCK_SHA256
    assert policy["inputs"]["proof_script"]["sha256"] == TRUSTED_PROOF_SCRIPT_SHA256
    assert policy["runtime"]["python_implementation"] == "CPython"
    assert policy["runtime"]["python_version"] == "3.11.15"
    assert policy["runtime"]["pip_version"] == "26.1.2"
    assert policy["runtime"]["runner_os"] == "Linux"
    assert policy["runtime"]["runner_arch"] == "X64"
    assert policy["runtime"]["runner_image_os"] == "ubuntu24"
    assert policy["runtime"]["runner_image_version"] is None
    assert {item["action"] for item in policy["actions"]} == {
        "actions/attest",
        "actions/checkout",
        "actions/setup-python",
        "actions/upload-artifact",
    }
