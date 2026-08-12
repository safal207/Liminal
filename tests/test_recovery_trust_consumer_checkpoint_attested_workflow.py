from pathlib import Path


WORKFLOW_PATH = Path(
    ".github/workflows/trusted-recovery-trust-consumer-checkpoint-attested.yml"
)
ROTATION_ROOT = "e2cb6a014236bc561d03c405f4986146026041fa"


def test_attested_checkpoint_workflow_pins_rotation_root() -> None:
    workflow = WORKFLOW_PATH.read_text(encoding="utf-8")
    assert (
        "uses: safal207/Liminal/.github/workflows/"
        f"trusted-recovery-trust-root-rotation-drill.yml@{ROTATION_ROOT}"
    ) in workflow
    assert f"--signer-digest {ROTATION_ROOT}" in workflow
    assert "--deny-self-hosted-runners" in workflow


def test_attested_checkpoint_workflow_attests_checkpoint_and_result() -> None:
    workflow = WORKFLOW_PATH.read_text(encoding="utf-8")
    assert (
        "subject-path: artifacts/trust-consumer-checkpoint-attested/"
        "checkpoint-generation-1.json"
    ) in workflow
    assert (
        "subject-path: artifacts/trust-consumer-checkpoint-attested/"
        "consumer-checkpoint-attested-result.json"
    ) in workflow
    assert "actions/attest@1e69f48acb82d1966a394da916b4c1698aa569d6" in workflow
