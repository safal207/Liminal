from __future__ import annotations

import copy
import json
from dataclasses import replace
from pathlib import Path

from liminal.checkpoint_witness_authority_v3 import (
    PortableCheckpointAuthorityEvidence,
    WitnessAuthorityMigrationEvidence,
    evaluate_checkpoint_candidate_v3,
    migrate_legacy_genesis_witness_to_v3,
    validate_witness_v3,
    witness_v3_sha256,
)
from liminal.recovery_trust_consumer_checkpoint_attested import checkpoint_sha256
from liminal.recovery_trust_consumer_checkpoint_witness import witness_sha256

ROOT = Path(__file__).resolve().parents[1]
CHECKPOINT_PATH = ROOT / "policies/recovery-trust-consumer-checkpoint-v0.2.json"
LEGACY_WITNESS_PATH = (
    ROOT / "policies/recovery-trust-consumer-checkpoint-witness-v0.2.json"
)

TRUST_DOMAIN = "liminal.trusted-recovery"
LOGICAL_PRODUCER_ID = "liminal.trusted-recovery.checkpoint-producer"
PRODUCER_CONTRACT_SHA256 = "1" * 64
AUTHORIZATION_CONTRACT_SHA256 = "2" * 64
MIGRATION_VERIFICATION_SHA256 = "3" * 64
EVIDENCE_TYPE = "trusted-recovery-consumer-checkpoint"
GEN1_REGISTRY = "5441072b0e550995a9ad0b27b4f3af7c7b5bf531f59e27c870ab1a8cf61789a1"
GEN1_MANIFEST = "b9cb0b37da2d74ece6c1cf780b06b17fbbb96f02e073ac64fb26be49cae24277"
ROTATION_SUBJECT = "17d97510206db3323c2a1642675659670563ff3401beef63de15a0ee1df31bd7"


def _load(path: Path) -> dict[str, object]:
    payload = json.loads(path.read_text())
    assert isinstance(payload, dict)
    return payload


def _checkpoint_1(checkpoint_0: dict[str, object]) -> dict[str, object]:
    checkpoint_1 = copy.deepcopy(checkpoint_0)
    checkpoint_1["accepted_generation"] = 1
    checkpoint_1["accepted_registry_sha256"] = GEN1_REGISTRY
    checkpoint_1["accepted_manifest_sha256"] = GEN1_MANIFEST
    checkpoint_1["previous_checkpoint_sha256"] = checkpoint_sha256(checkpoint_0)
    checkpoint_1["accepted_evidence"] = {
        "kind": "rotation_authorization",
        "subject_sha256": ROTATION_SUBJECT,
        "signer_workflow_path": (
            ".github/workflows/trusted-recovery-trust-root-rotation-drill.yml"
        ),
        "signer_workflow_sha": "e2cb6a014236bc561d03c405f4986146026041fa",
    }
    return checkpoint_1


def _migration_evidence(legacy_witness: dict[str, object]) -> WitnessAuthorityMigrationEvidence:
    signer = legacy_witness["checkpoint_signer"]
    assert isinstance(signer, dict)
    return WitnessAuthorityMigrationEvidence(
        verified=True,
        legacy_witness_sha256=witness_sha256(legacy_witness),
        legacy_signer_workflow_path=str(signer["workflow_path"]),
        legacy_signer_workflow_sha=str(signer["workflow_sha"]),
        logical_producer_id=LOGICAL_PRODUCER_ID,
        producer_contract_sha256=PRODUCER_CONTRACT_SHA256,
        authorization_contract_sha256=AUTHORIZATION_CONTRACT_SHA256,
        evidence_type=EVIDENCE_TYPE,
        migration_verification_sha256=MIGRATION_VERIFICATION_SHA256,
    )


def _witness_v3() -> dict[str, object]:
    legacy = _load(LEGACY_WITNESS_PATH)
    migration = migrate_legacy_genesis_witness_to_v3(
        legacy,
        trust_domain=TRUST_DOMAIN,
        migration_evidence=_migration_evidence(legacy),
    )
    assert migration.authorized is True
    assert migration.witness is not None
    return migration.witness


def _authority_evidence(
    checkpoint: dict[str, object],
) -> PortableCheckpointAuthorityEvidence:
    return PortableCheckpointAuthorityEvidence(
        verified=True,
        subject_sha256=checkpoint_sha256(checkpoint),
        logical_producer_id=LOGICAL_PRODUCER_ID,
        producer_contract_sha256=PRODUCER_CONTRACT_SHA256,
        authorization_contract_sha256=AUTHORIZATION_CONTRACT_SHA256,
        evidence_type=EVIDENCE_TYPE,
        generation=int(checkpoint["accepted_generation"]),
    )


def test_verified_legacy_genesis_migrates_to_provider_neutral_authority() -> None:
    legacy = _load(LEGACY_WITNESS_PATH)
    decision = migrate_legacy_genesis_witness_to_v3(
        legacy,
        trust_domain=TRUST_DOMAIN,
        migration_evidence=_migration_evidence(legacy),
    )

    assert decision.authorized is True
    assert decision.reason == "witness_authority_migrated"
    assert decision.witness is not None
    witness = decision.witness
    assert validate_witness_v3(witness)
    authority = witness["checkpoint_authority"]
    assert isinstance(authority, dict)
    assert authority["logical_producer_id"] == LOGICAL_PRODUCER_ID
    assert authority["producer_contract_sha256"] == PRODUCER_CONTRACT_SHA256
    assert authority["authorization_contract_sha256"] == AUTHORIZATION_CONTRACT_SHA256
    assert "workflow_path" not in authority
    assert "workflow_sha" not in authority


def test_migration_rejects_unverified_mapping() -> None:
    legacy = _load(LEGACY_WITNESS_PATH)
    evidence = replace(_migration_evidence(legacy), verified=False)

    decision = migrate_legacy_genesis_witness_to_v3(
        legacy,
        trust_domain=TRUST_DOMAIN,
        migration_evidence=evidence,
    )

    assert decision.authorized is False
    assert decision.reason == "migration_evidence_unverified"


def test_migration_rejects_wrong_legacy_witness_digest() -> None:
    legacy = _load(LEGACY_WITNESS_PATH)
    evidence = replace(_migration_evidence(legacy), legacy_witness_sha256="0" * 64)

    decision = migrate_legacy_genesis_witness_to_v3(
        legacy,
        trust_domain=TRUST_DOMAIN,
        migration_evidence=evidence,
    )

    assert decision.authorized is False
    assert decision.reason == "legacy_witness_digest_mismatch"


def test_migration_rejects_wrong_legacy_signer_mapping() -> None:
    legacy = _load(LEGACY_WITNESS_PATH)
    evidence = replace(
        _migration_evidence(legacy),
        legacy_signer_workflow_sha="0" * 40,
    )

    decision = migrate_legacy_genesis_witness_to_v3(
        legacy,
        trust_domain=TRUST_DOMAIN,
        migration_evidence=evidence,
    )

    assert decision.authorized is False
    assert decision.reason == "legacy_signer_mismatch"


def test_matching_logical_authority_advances_v3_witness() -> None:
    checkpoint_0 = _load(CHECKPOINT_PATH)
    checkpoint_1 = _checkpoint_1(checkpoint_0)
    witness_0 = _witness_v3()

    decision = evaluate_checkpoint_candidate_v3(
        witness_0,
        checkpoint_1,
        previous_checkpoint=checkpoint_0,
        authority_evidence=_authority_evidence(checkpoint_1),
    )

    assert decision.authorized is True
    assert decision.reason == "checkpoint_witness_advanced"
    assert decision.next_witness is not None
    witness_1 = decision.next_witness
    assert witness_1["observed_generation"] == 1
    assert witness_1["previous_witness_sha256"] == witness_v3_sha256(witness_0)
    assert witness_1["checkpoint_authority"] == witness_0["checkpoint_authority"]
    assert validate_witness_v3(witness_1, witness_0)


def test_same_generation_checkpoint_remains_idempotent() -> None:
    checkpoint_0 = _load(CHECKPOINT_PATH)
    witness_0 = _witness_v3()

    decision = evaluate_checkpoint_candidate_v3(
        witness_0,
        checkpoint_0,
        previous_checkpoint=None,
        authority_evidence=None,
    )

    assert decision.authorized is True
    assert decision.reason == "checkpoint_already_witnessed"
    assert decision.next_witness == witness_0


def test_unverified_authority_evidence_rejects_advance() -> None:
    checkpoint_0 = _load(CHECKPOINT_PATH)
    checkpoint_1 = _checkpoint_1(checkpoint_0)
    witness_0 = _witness_v3()
    evidence = replace(_authority_evidence(checkpoint_1), verified=False)

    decision = evaluate_checkpoint_candidate_v3(
        witness_0,
        checkpoint_1,
        previous_checkpoint=checkpoint_0,
        authority_evidence=evidence,
    )

    assert decision.authorized is False
    assert decision.reason == "checkpoint_authority_evidence_unverified"


def test_subject_and_generation_drift_fail_closed() -> None:
    checkpoint_0 = _load(CHECKPOINT_PATH)
    checkpoint_1 = _checkpoint_1(checkpoint_0)
    witness_0 = _witness_v3()
    good = _authority_evidence(checkpoint_1)

    wrong_subject = evaluate_checkpoint_candidate_v3(
        witness_0,
        checkpoint_1,
        previous_checkpoint=checkpoint_0,
        authority_evidence=replace(good, subject_sha256="0" * 64),
    )
    assert wrong_subject.reason == "checkpoint_authority_subject_mismatch"

    wrong_generation = evaluate_checkpoint_candidate_v3(
        witness_0,
        checkpoint_1,
        previous_checkpoint=checkpoint_0,
        authority_evidence=replace(good, generation=2),
    )
    assert wrong_generation.reason == "checkpoint_authority_generation_mismatch"


def test_logical_producer_and_contract_drift_fail_closed() -> None:
    checkpoint_0 = _load(CHECKPOINT_PATH)
    checkpoint_1 = _checkpoint_1(checkpoint_0)
    witness_0 = _witness_v3()
    good = _authority_evidence(checkpoint_1)

    wrong_producer = evaluate_checkpoint_candidate_v3(
        witness_0,
        checkpoint_1,
        previous_checkpoint=checkpoint_0,
        authority_evidence=replace(good, logical_producer_id="other.producer"),
    )
    assert wrong_producer.reason == "logical_producer_mismatch"

    wrong_producer_contract = evaluate_checkpoint_candidate_v3(
        witness_0,
        checkpoint_1,
        previous_checkpoint=checkpoint_0,
        authority_evidence=replace(good, producer_contract_sha256="4" * 64),
    )
    assert wrong_producer_contract.reason == "producer_contract_mismatch"

    wrong_authorization_contract = evaluate_checkpoint_candidate_v3(
        witness_0,
        checkpoint_1,
        previous_checkpoint=checkpoint_0,
        authority_evidence=replace(good, authorization_contract_sha256="5" * 64),
    )
    assert wrong_authorization_contract.reason == "authorization_contract_mismatch"


def test_evidence_type_drift_fails_closed() -> None:
    checkpoint_0 = _load(CHECKPOINT_PATH)
    checkpoint_1 = _checkpoint_1(checkpoint_0)
    witness_0 = _witness_v3()

    decision = evaluate_checkpoint_candidate_v3(
        witness_0,
        checkpoint_1,
        previous_checkpoint=checkpoint_0,
        authority_evidence=replace(
            _authority_evidence(checkpoint_1),
            evidence_type="other-evidence-type",
        ),
    )

    assert decision.authorized is False
    assert decision.reason == "evidence_type_mismatch"


def test_stale_checkpoint_is_rejected_after_v3_advance() -> None:
    checkpoint_0 = _load(CHECKPOINT_PATH)
    checkpoint_1 = _checkpoint_1(checkpoint_0)
    witness_0 = _witness_v3()
    advance = evaluate_checkpoint_candidate_v3(
        witness_0,
        checkpoint_1,
        previous_checkpoint=checkpoint_0,
        authority_evidence=_authority_evidence(checkpoint_1),
    )
    assert advance.next_witness is not None

    stale = evaluate_checkpoint_candidate_v3(
        advance.next_witness,
        checkpoint_0,
        previous_checkpoint=None,
        authority_evidence=None,
    )

    assert stale.authorized is False
    assert stale.reason == "stale_checkpoint"
