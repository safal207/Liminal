from __future__ import annotations

import copy
import json
from pathlib import Path

from liminal.recovery_trust_consumer_checkpoint_attested import checkpoint_sha256
from liminal.recovery_trust_consumer_checkpoint_witness import (
    VerifiedCheckpointEvidence,
    evaluate_checkpoint_candidate,
    validate_witness,
)


ROOT = Path(__file__).resolve().parents[1]
CHECKPOINT_PATH = ROOT / "policies/recovery-trust-consumer-checkpoint-v0.2.json"
WITNESS_V2_PATH = ROOT / "policies/recovery-trust-consumer-checkpoint-witness-v0.2.json"
NEW_SIGNER = "f31b56a5e21a668bcb98791b05542652760dcc27"
OLD_SIGNER = "d0688725bd76fdf7221e84ca7c5bfb51e363ff72"
SIGNER_PATH = ".github/workflows/trusted-recovery-trust-consumer-checkpoint-attested.yml"


def _load(path: Path) -> dict[str, object]:
    payload = json.loads(path.read_text())
    assert isinstance(payload, dict)
    return payload


def _checkpoint_1(checkpoint_0: dict[str, object]) -> dict[str, object]:
    checkpoint_1 = copy.deepcopy(checkpoint_0)
    checkpoint_1["accepted_generation"] = 1
    checkpoint_1["accepted_registry_sha256"] = (
        "5441072b0e550995a9ad0b27b4f3af7c7b5bf531f59e27c870ab1a8cf61789a1"
    )
    checkpoint_1["accepted_manifest_sha256"] = (
        "b9cb0b37da2d74ece6c1cf780b06b17fbbb96f02e073ac64fb26be49cae24277"
    )
    checkpoint_1["previous_checkpoint_sha256"] = checkpoint_sha256(checkpoint_0)
    checkpoint_1["accepted_evidence"] = {
        "kind": "rotation_authorization",
        "subject_sha256": "17d97510206db3323c2a1642675659670563ff3401beef63de15a0ee1df31bd7",
        "signer_workflow_path": ".github/workflows/trusted-recovery-trust-root-rotation-drill.yml",
        "signer_workflow_sha": "e2cb6a014236bc561d03c405f4986146026041fa",
    }
    return checkpoint_1


def test_v02_root_records_checkpoint_signer_transition_lineage() -> None:
    witness = _load(WITNESS_V2_PATH)

    assert validate_witness(witness)
    assert witness["checkpoint_signer"] == {
        "workflow_path": SIGNER_PATH,
        "workflow_sha": NEW_SIGNER,
    }
    transition = witness["checkpoint_signer_transition"]
    assert isinstance(transition, dict)
    assert transition["previous_signer"] == {
        "workflow_path": SIGNER_PATH,
        "workflow_sha": OLD_SIGNER,
    }
    assert transition["reason"] == "manifest_backed_checkpoint_producer_rotation"
    assert transition["previous_witness_workflow_sha"] == (
        "3f0af42a680f42923cb18591ba127206b2292599"
    )


def test_v02_rejects_old_checkpoint_producer_signer_and_accepts_new_signer() -> None:
    checkpoint_0 = _load(CHECKPOINT_PATH)
    checkpoint_1 = _checkpoint_1(checkpoint_0)
    witness = _load(WITNESS_V2_PATH)

    old = evaluate_checkpoint_candidate(
        witness,
        checkpoint_1,
        previous_checkpoint=checkpoint_0,
        checkpoint_evidence=VerifiedCheckpointEvidence(
            verified=True,
            signer_workflow_path=SIGNER_PATH,
            signer_workflow_sha=OLD_SIGNER,
            subject_sha256=checkpoint_sha256(checkpoint_1),
        ),
    )
    assert old.authorized is False
    assert old.reason == "checkpoint_signer_mismatch"

    current = evaluate_checkpoint_candidate(
        witness,
        checkpoint_1,
        previous_checkpoint=checkpoint_0,
        checkpoint_evidence=VerifiedCheckpointEvidence(
            verified=True,
            signer_workflow_path=SIGNER_PATH,
            signer_workflow_sha=NEW_SIGNER,
            subject_sha256=checkpoint_sha256(checkpoint_1),
        ),
    )
    assert current.authorized is True
    assert current.reason == "checkpoint_witness_advanced"
    assert current.next_witness is not None
    assert current.next_witness["checkpoint_signer_transition"] == witness[
        "checkpoint_signer_transition"
    ]
    assert validate_witness(current.next_witness, witness)


def test_v02_transition_metadata_is_fail_closed() -> None:
    witness = _load(WITNESS_V2_PATH)

    tampered = copy.deepcopy(witness)
    transition = tampered["checkpoint_signer_transition"]
    assert isinstance(transition, dict)
    transition["previous_witness_workflow_sha"] = "0" * 39
    assert validate_witness(tampered) is False

    same_signer = copy.deepcopy(witness)
    transition = same_signer["checkpoint_signer_transition"]
    assert isinstance(transition, dict)
    transition["previous_signer"] = copy.deepcopy(same_signer["checkpoint_signer"])
    assert validate_witness(same_signer) is False
