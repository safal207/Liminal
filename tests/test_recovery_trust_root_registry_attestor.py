from __future__ import annotations

from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
ATTESTOR = ROOT / ".github" / "workflows" / "trusted-recovery-trust-root-registry-attestor.yml"


def test_registry_attestor_is_self_pinned_and_checks_historical_material() -> None:
    workflow = ATTESTOR.read_text(encoding="utf-8")

    assert "Trusted Recovery Trust Root Registry Attestor v0.1" in workflow
    assert "ref: ${{ job.workflow_sha }}" in workflow
    assert "fetch-depth: 0" in workflow
    assert "LIMINAL_REGISTRY_ATTESTOR_WORKFLOW_SHA: ${{ job.workflow_sha }}" in workflow
    assert 'python-version: "3.11.15"' in workflow
    assert "check-recovery-trust-root-registry.py" in workflow
    assert "policies/recovery-trust-root-registry-v0.1.json" in workflow
    assert "recovery-trust-root-registry.canonical.json" in workflow
    assert "recovery-trust-root-registry-verification.json" in workflow


def test_registry_attestor_attests_canonical_registry_and_verification_receipt() -> None:
    workflow = ATTESTOR.read_text(encoding="utf-8")

    assert workflow.count("actions/attest@1e69f48acb82d1966a394da916b4c1698aa569d6") == 2
    assert (
        "subject-path: artifacts/trust-root/recovery-trust-root-registry.canonical.json"
        in workflow
    )
    assert "subject-path: policies/recovery-trust-root-registry-v0.1.json" not in workflow
    assert (
        "subject-path: artifacts/trust-root/recovery-trust-root-registry-verification.json"
        in workflow
    )
    assert "actions/checkout@11d5960a326750d5838078e36cf38b85af677262" in workflow
    assert "actions/setup-python@a26af69be951a213d495a4c3e4e4022e16d87065" in workflow
    assert "actions/upload-artifact@ea165f8d65b6e75b540449e92b4886f43607fa02" in workflow
