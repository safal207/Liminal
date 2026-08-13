from __future__ import annotations

import copy
import json
from pathlib import Path

from liminal.recovery_trust_consumer_checkpoint_attested import checkpoint_sha256
from liminal.recovery_trust_consumer_checkpoint_witness import witness_sha256
from liminal.recovery_trust_consumer_checkpoint_witness_v3 import (
    PortableCheckpointAuthority,
    VerifiedAuthorityMigrationEvidence,
    VerifiedPortableCheckpointEvidence,
    evaluate_portable_checkpoint_candidate,
    migrate_witness_v2_to_v3,
    validate_witness_v3,
    witness_v3_sha256,
)

ROOT = Path(__file__).resolve().parents[1]
CHECKPOINT_PATH = ROOT / "policies/recovery-trust-consumer-checkpoint-v0.2.json"
GEN1_REGISTRY = "5441072b0e550995a9ad0b27b4f3af7c7b5bf531f59e27c870ab1a8cf61789a1"
GEN1_MANIFEST = "b9cb0b37da2d74ece6c1cf780b06b17fbbb96f02e073ac64fb26be49cae24277"
PRODUCER_CONTRACT = "1" * 64
AUTHORIZATION_CONTRACT = "2" * 64


def _load_checkpoint() -> dict[str, object]:
    payload = json.loads(CHECKPOINT_PATH.read_text())
    assert isinstance(payload, dict)
    return payload


def _witness_v2(checkpoint_0: dict[str, object]) -> dict[str, object]:
    return {
        "schema_version": "liminal.recovery-trust-consumer-checkpoint-witness.v0.2",
        "repository": "safal207/Liminal",
        "observed_generation": 0,
        "checkpoint_sha256": checkpoint_sha256(checkpoint_0),
        "accepted_registry_sha256": checkpoint_0["accepted_registry_sha256"],
        "accepted_manifest_sha256": checkpoint_0["accepted_manifest_sha256"],
        "previous_witness_sha256": None,
        "checkpoint_signer": {
            "workflow_path": ".github/workflows/trusted-recovery-trust-consumer-checkpoint-attested.yml",
            "workflow_sha": "f31b56a5e21a668bcb98791b05542652760dcc27",
        },
        "checkpoint_signer_transition": {
            "previous_signer": {
                "workflow_path": ".github/workflows/trusted-recovery-trust-consumer-checkpoint-attested.yml",
                "workflow_sha": "d0688725bd76fdf7221e84ca7c5bfb51e363ff72",
            },
            "reason": "manifest_backed_checkpoint_producer_rotation",
            "previous_witness_workflow_sha": "3f0af42a680f42923cb18591ba127206b2292599",
        },
    }


def _authority() -> PortableCheckpointAuthority:
    return PortableCheckpointAuthority(
        logical_producer_id="liminal.trusted-recovery.checkpoint-producer",
        producer_contract_sha256=PRODUCER_CONTRACT,
        authorization_contract_sha256=AUTHORIZATION_CONTRACT,
        evidence_type="trusted-recovery-consumer-checkpoint",
    )


def _checkpoint_1(checkpoint_0: dict[str, object]) -> dict[str, object]:
    checkpoint_1 = copy.deepcopy(checkpoint_0)
    checkpoint_1["accepted_generation"] = 1
    checkpoint_1["accepted_registry_sha256"] = GEN1_REGISTRY
    checkpoint_1["accepted_manifest_sha256"] = GEN1_MANIFEST
    checkpoint_1["previous_checkpoint_sha256"] = checkpoint_sha256(checkpoint_0)
    checkpoint_1["accepted_evidence"] = {
        "kind": "rotation_authorization",
        "subject_sha256": "3" * 64,
        "signer_workflow_path": ".github/workflows/trusted-recovery-trust-root-rotation-drill.yml",
        "signer_workflow_sha": "e2cb6a014236bc561d03c405f4986146026041fa",
    }
    return checkpoint_1


def _migrate(checkpoint_0: dict[str, object]):
    witness_2 = _witness_v2(checkpoint_0)
    evidence = VerifiedAuthorityMigrationEvidence(
        verified=True,
        previous_witness_sha256=witness_sha256(witness_2),
        authority=_authority(),
    )
    return migrate_witness_v2_to_v3(
        witness_2,
        None,
        trust_domain="liminal.trusted-recovery",
        migration_evidence=evidence,
    )


def _evidence(checkpoint: dict[str, object]) -> VerifiedPortableCheckpointEvidence:
    return VerifiedPortableCheckpointEvidence(
        verified=True,
        subject_sha256=checkpoint_sha256(checkpoint),
        logical_producer_id=_authority().logical_producer_id,
        producer_contract_sha256=PRODUCER_CONTRACT,
        authorization_contract_sha256=AUTHORIZATION_CONTRACT,
        evidence_type=_authority().evidence_type,
    )


def test_verified_v2_witness_migrates_to_logical_authority() -> None:
    checkpoint_0 = _load_checkpoint()
    decision = _migrate(checkpoint_0)

    assert decision.authorized is True
    assert decision.reason == "witness_authority_migrated"
    assert decision.next_witness is not None
    witness_3 = decision.next_witness
    assert witness_3["checkpoint_authority"] == {
        "logical_producer_id": "liminal.trusted-recovery.checkpoint-producer",
        "producer_contract_sha256": PRODUCER_CONTRACT,
        "authorization_contract_sha256": AUTHORIZATION_CONTRACT,
        "evidence_type": "trusted-recovery-consumer-checkpoint",
    }
    assert "checkpoint_signer" not in witness_3
    assert validate_witness_v3(witness_3)


def test_migration_requires_verified_exact_predecessor_binding() -> None:
    checkpoint_0 = _load_checkpoint()
    witness_2 = _witness_v2(checkpoint_0)

    unverified = migrate_witness_v2_to_v3(
        witness_2,
        None,
        trust_domain="liminal.trusted-recovery",
        migration_evidence=VerifiedAuthorityMigrationEvidence(
            verified=False,
            previous_witness_sha256=witness_sha256(witness_2),
            authority=_authority(),
        ),
    )
    assert unverified.authorized is False
    assert unverified.reason == "authority_migration_unverified"

    wrong_predecessor = migrate_witness_v2_to_v3(
        witness_2,
        None,
        trust_domain="liminal.trusted-recovery",
        migration_evidence=VerifiedAuthorityMigrationEvidence(
            verified=True,
            previous_witness_sha256="0" * 64,
            authority=_authority(),
        ),
    )
    assert wrong_predecessor.authorized is False
    assert wrong_predecessor.reason == "authority_migration_predecessor_mismatch"


def test_v3_advance_uses_logical_authority_not_concrete_signer() -> None:
    checkpoint_0 = _load_checkpoint()
    checkpoint_1 = _checkpoint_1(checkpoint_0)
    migrated = _migrate(checkpoint_0)
    assert migrated.next_witness is not None

    decision = evaluate_portable_checkpoint_candidate(
        migrated.next_witness,
        checkpoint_1,
        previous_checkpoint=checkpoint_0,
        checkpoint_evidence=_evidence(checkpoint_1),
    )

    assert decision.authorized is True
    assert decision.reason == "checkpoint_witness_advanced"
    assert decision.next_witness is not None
    assert decision.next_witness["previous_witness_sha256"] == witness_v3_sha256(
        migrated.next_witness
    )
    assert validate_witness_v3(decision.next_witness, migrated.next_witness)


def test_v3_validator_supports_arbitrary_adjacent_chain_depth() -> None:
    checkpoint_0 = _load_checkpoint()
    checkpoint_1 = _checkpoint_1(checkpoint_0)
    migrated = _migrate(checkpoint_0)
    assert migrated.next_witness is not None
    advance = evaluate_portable_checkpoint_candidate(
        migrated.next_witness,
        checkpoint_1,
        previous_checkpoint=checkpoint_0,
        checkpoint_evidence=_evidence(checkpoint_1),
    )
    assert advance.next_witness is not None
    witness_1 = advance.next_witness

    witness_2 = copy.deepcopy(witness_1)
    witness_2["observed_generation"] = 2
    witness_2["checkpoint_sha256"] = "4" * 64
    witness_2["accepted_registry_sha256"] = "5" * 64
    witness_2["accepted_manifest_sha256"] = "6" * 64
    witness_2["previous_witness_sha256"] = witness_v3_sha256(witness_1)

    assert validate_witness_v3(witness_1, migrated.next_witness)
    assert validate_witness_v3(witness_2, witness_1)


def test_concrete_provider_can_change_without_changing_v3_authority() -> None:
    checkpoint_0 = _load_checkpoint()
    checkpoint_1 = _checkpoint_1(checkpoint_0)
    migrated = _migrate(checkpoint_0)
    assert migrated.next_witness is not None

    github_evidence = _evidence(checkpoint_1)
    external_evidence = copy.deepcopy(github_evidence)

    a = evaluate_portable_checkpoint_candidate(
        migrated.next_witness,
        checkpoint_1,
        previous_checkpoint=checkpoint_0,
        checkpoint_evidence=github_evidence,
    )
    b = evaluate_portable_checkpoint_candidate(
        migrated.next_witness,
        checkpoint_1,
        previous_checkpoint=checkpoint_0,
        checkpoint_evidence=external_evidence,
    )
    assert a.authorized is True and b.authorized is True
    assert a.next_witness == b.next_witness


def test_v3_fails_closed_on_unverified_authority_or_subject_drift() -> None:
    checkpoint_0 = _load_checkpoint()
    checkpoint_1 = _checkpoint_1(checkpoint_0)
    migrated = _migrate(checkpoint_0)
    assert migrated.next_witness is not None

    unverified = copy.deepcopy(_evidence(checkpoint_1))
    unverified = VerifiedPortableCheckpointEvidence(
        verified=False,
        subject_sha256=unverified.subject_sha256,
        logical_producer_id=unverified.logical_producer_id,
        producer_contract_sha256=unverified.producer_contract_sha256,
        authorization_contract_sha256=unverified.authorization_contract_sha256,
        evidence_type=unverified.evidence_type,
    )
    rejected_unverified = evaluate_portable_checkpoint_candidate(
        migrated.next_witness,
        checkpoint_1,
        previous_checkpoint=checkpoint_0,
        checkpoint_evidence=unverified,
    )
    assert rejected_unverified.authorized is False
    assert rejected_unverified.reason == "checkpoint_evidence_unverified"

    wrong_contract = VerifiedPortableCheckpointEvidence(
        verified=True,
        subject_sha256=checkpoint_sha256(checkpoint_1),
        logical_producer_id=_authority().logical_producer_id,
        producer_contract_sha256="9" * 64,
        authorization_contract_sha256=AUTHORIZATION_CONTRACT,
        evidence_type=_authority().evidence_type,
    )
    rejected_contract = evaluate_portable_checkpoint_candidate(
        migrated.next_witness,
        checkpoint_1,
        previous_checkpoint=checkpoint_0,
        checkpoint_evidence=wrong_contract,
    )
    assert rejected_contract.authorized is False
    assert rejected_contract.reason == "producer_contract_mismatch"

    wrong_subject = VerifiedPortableCheckpointEvidence(
        verified=True,
        subject_sha256="0" * 64,
        logical_producer_id=_authority().logical_producer_id,
        producer_contract_sha256=PRODUCER_CONTRACT,
        authorization_contract_sha256=AUTHORIZATION_CONTRACT,
        evidence_type=_authority().evidence_type,
    )
    rejected_subject = evaluate_portable_checkpoint_candidate(
        migrated.next_witness,
        checkpoint_1,
        previous_checkpoint=checkpoint_0,
        checkpoint_evidence=wrong_subject,
    )
    assert rejected_subject.authorized is False
    assert rejected_subject.reason == "checkpoint_subject_digest_mismatch"
