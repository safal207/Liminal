from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/trusted-recovery-trust-consumer-checkpoint-witness.yml"
CHECKPOINT_ROOT = "d0688725bd76fdf7221e84ca7c5bfb51e363ff72"


def test_checkpoint_witness_workflow_keeps_immutable_trust_gates() -> None:
    text = WORKFLOW.read_text()

    assert (
        "uses: safal207/Liminal/.github/workflows/"
        "trusted-recovery-trust-consumer-checkpoint-attested.yml@" + CHECKPOINT_ROOT
    ) in text
    assert "ref: ${{ job.workflow_sha }}" in text
    assert 'python-version: "3.11.15"' in text
    assert "gh attestation verify" in text
    assert "checkpoint-evidence/checkpoint-generation-1.json" in text
    assert (
        "--signer-workflow safal207/Liminal/.github/workflows/"
        "trusted-recovery-trust-consumer-checkpoint-attested.yml"
    ) in text
    assert f"--signer-digest {CHECKPOINT_ROOT}" in text
    assert "--deny-self-hosted-runners" in text
    assert "checkpoint_attestation\"][\"cryptographically_verified\"] is True" in text
    assert "local_generation_1_checkpoint_available\"] is False" in text
    assert "stale_checkpoint_replay\"][\"reason\"] == \"stale_checkpoint\"" in text
    assert "subject-path: artifacts/trust-consumer-checkpoint-witness/witness-generation-1.json" in text
    assert (
        "subject-path: artifacts/trust-consumer-checkpoint-witness/"
        "checkpoint-witness-drill-result.json"
    ) in text
