from __future__ import annotations

from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github" / "workflows" / "trusted-recovery-trust-root-rotation-drill.yml"
SCRIPT = ROOT / "scripts" / "run-recovery-trust-root-rotation-drill.py"


def test_rotation_drill_is_immutable_and_provider_free() -> None:
    workflow = WORKFLOW.read_text(encoding="utf-8")
    script = SCRIPT.read_text(encoding="utf-8")

    assert "ref: ${{ job.workflow_sha }}" in workflow
    assert "fetch-depth: 0" in workflow
    assert 'python-version: "3.11.15"' in workflow
    assert "ca69a7d342a4e00dfc2d11b13016a8eb7733da6b" in workflow
    assert "verifier_root_downgrade" in workflow
    assert "actions/attest@1e69f48acb82d1966a394da916b4c1698aa569d6" in workflow
    assert "GONKA" not in workflow.upper()
    assert "OPENAI" not in workflow.upper()
    assert "external_provider_calls" in script


def test_rotation_drill_keeps_active_registry_unchanged() -> None:
    workflow = WORKFLOW.read_text(encoding="utf-8")
    script = SCRIPT.read_text(encoding="utf-8")

    assert "recovery-trust-root-registry-v0.1.json" in workflow
    assert "generation-1-registry.json" in script
    assert "downgrade-generation-2-registry.json" in script
    assert "write_text" not in script
    assert "write_bytes(canonical_json_bytes(payload))" in script
