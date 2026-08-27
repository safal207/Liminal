from __future__ import annotations

import copy
import json
from dataclasses import replace
from pathlib import Path

from liminal.checkpoint_rotation_authority_v3 import (
    CheckpointRotationAuthorityMigrationEvidence,
    PortableRotationAuthorityEvidence,
    checkpoint_v3_sha256,
    evaluate_registry_candidate_v3,
    migrate_legacy_genesis_checkpoint_to_v3,
)
from liminal.checkpoint_witness_authority_v3 import (
    PortableCheckpointAuthorityEvidence,
    WitnessAuthorityMigrationEvidence,
    migrate_legacy_genesis_witness_to_v3,
    witness_v3_sha256,
)
from liminal.checkpoint_witness_authority_v4 import (
    WitnessCheckpointSchemaMigrationEvidence,
    checkpoint_schema_migration_claim_sha256,
    evaluate_checkpoint_candidate_v4,
    migrate_witness_v3_genesis_to_v4,
    validate_witness_v4,
    witness_v4_sha256,
)
from liminal.recovery_trust_consumer_checkpoint_attested import checkpoint_sha256
from liminal.recovery_trust_consumer_checkpoint_witness import witness_sha256
from liminal.recovery_trust_root_registry import (
    REGISTRY_SCHEMA_VERSION,
    canonical_json_bytes,
    sha256_hex,
)
from liminal.rotation_authority_portability import (
    RotationAuthorityObservation,
    rotation_authority_receipt_sha256,
)

LEGACY_CHECKPOINT_PATH = Path("policies/recovery-trust-consumer-checkpoint-v0.2.json")
LEGACY_WITNESS_PATH = Path("policies/recovery-trust-consumer-checkpoint-witness-v0.2.json")
REGISTRY_PATH = Path("policies/recovery-trust-root-registry-v0.1.json")
MANIFEST_PATH = Path("policies/recovery-trust-root-manifest-v0.1.json")
ROTATION_CONTRACT_PATH = Path("policies/portable-rotation-producer-contract-v0.1.json")
ROTATION_AUTH_PATH = Path("policies/portable-rotation-authorization-contract-v0.1.json")
CHECKPOINT_PRODUCER_V1_PATH = Path("policies/portable-checkpoint-producer-contract-v0.1.json")
CHECKPOINT_AUTH_V1_PATH = Path("policies/portable-checkpoint-authorization-contract-v0.1.json")
CHECKPOINT_PRODUCER_V2_PATH = Path("policies/portable-checkpoint-producer-contract-v0.2.json")
CHECKPOINT_AUTH_V2_PATH = Path("policies/portable-checkpoint-authorization-contract-v0.2.json")
TRUST_DOMAIN = "liminal.trusted-recovery"
GENESIS_MANIFEST_KEY = "policies/recovery-trust-root-manifest-v0.1.json"
GENERATION_1_MANIFEST_KEY = "drill/generation-1-manifest.json"
ROTATED_VERIFIER_SHA = "ca69a7d342a4e00dfc2d11b13016a8eb7733da6b"


def _load(path: Path) -> dict[str, object]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    assert isinstance(payload, dict)
    return payload


def _digest(path: Path) -> str:
    return sha256_hex(canonical_json_bytes(_load(path)))


def _checkpoint_v3_genesis() -> dict[str, object]:
    legacy = _load(LEGACY_CHECKPOINT_PATH)
    authorizer = legacy["advance_authorizer"]
    assert isinstance(authorizer, dict)
    evidence = CheckpointRotationAuthorityMigrationEvidence(
        verified=True,
        trust_domain=TRUST_DOMAIN,
        legacy_checkpoint_sha256=checkpoint_sha256(legacy),
        legacy_authorizer_workflow_path=str(authorizer["workflow_path"]),
        legacy_authorizer_workflow_sha=str(authorizer["workflow_sha"]),
        logical_rotation_id="liminal.trusted-recovery.registry-rotation",
        rotation_contract_sha256=_digest(ROTATION_CONTRACT_PATH),
        authorization_contract_sha256=_digest(ROTATION_AUTH_PATH),
    )
    decision = migrate_legacy_genesis_checkpoint_to_v3(
        legacy, trust_domain=TRUST_DOMAIN, migration_evidence=evidence
    )
    assert decision.authorized and decision.checkpoint is not None
    return decision.checkpoint


def _witness_v3_genesis() -> dict[str, object]:
    legacy = _load(LEGACY_WITNESS_PATH)
    signer = legacy["checkpoint_signer"]
    assert isinstance(signer, dict)
    evidence = WitnessAuthorityMigrationEvidence(
        verified=True,
        trust_domain=TRUST_DOMAIN,
        legacy_witness_sha256=witness_sha256(legacy),
        legacy_signer_workflow_path=str(signer["workflow_path"]),
        legacy_signer_workflow_sha=str(signer["workflow_sha"]),
        logical_producer_id="liminal.trusted-recovery.checkpoint-producer",
        producer_contract_sha256=_digest(CHECKPOINT_PRODUCER_V1_PATH),
        authorization_contract_sha256=_digest(CHECKPOINT_AUTH_V1_PATH),
        evidence_type="trusted-recovery-consumer-checkpoint",
    )
    decision = migrate_legacy_genesis_witness_to_v3(
        legacy, trust_domain=TRUST_DOMAIN, migration_evidence=evidence
    )
    assert decision.authorized and decision.witness is not None
    return decision.witness


def _migration_evidence(
    witness_v3: dict[str, object], checkpoint_v3: dict[str, object], *, verified: bool = True
) -> WitnessCheckpointSchemaMigrationEvidence:
    origin = checkpoint_v3["authority_origin"]
    authority = witness_v3["checkpoint_authority"]
    assert isinstance(origin, dict)
    assert isinstance(authority, dict)
    return WitnessCheckpointSchemaMigrationEvidence(
        verified=verified,
        trust_domain=TRUST_DOMAIN,
        legacy_witness_v3_sha256=witness_v3_sha256(witness_v3),
        legacy_checkpoint_sha256=str(witness_v3["checkpoint_sha256"]),
        checkpoint_v3_sha256=checkpoint_v3_sha256(checkpoint_v3),
        checkpoint_rotation_migration_claim_sha256=str(origin["migration_claim_sha256"]),
        logical_producer_id=str(authority["logical_producer_id"]),
        evidence_type=str(authority["evidence_type"]),
        legacy_producer_contract_sha256=_digest(CHECKPOINT_PRODUCER_V1_PATH),
        legacy_authorization_contract_sha256=_digest(CHECKPOINT_AUTH_V1_PATH),
        next_producer_contract_sha256=_digest(CHECKPOINT_PRODUCER_V2_PATH),
        next_authorization_contract_sha256=_digest(CHECKPOINT_AUTH_V2_PATH),
    )


def _witness_v4_genesis() -> tuple[dict[str, object], dict[str, object]]:
    witness_v3 = _witness_v3_genesis()
    checkpoint_v3 = _checkpoint_v3_genesis()
    evidence = _migration_evidence(witness_v3, checkpoint_v3)
    decision = migrate_witness_v3_genesis_to_v4(
        witness_v3, checkpoint_v3, migration_evidence=evidence
    )
    assert decision.authorized and decision.witness is not None
    return decision.witness, checkpoint_v3


def _candidate_registry() -> tuple[dict[str, object], dict[str, object]]:
    registry_0 = _load(REGISTRY_PATH)
    manifest_0 = _load(MANIFEST_PATH)
    manifest_1 = copy.deepcopy(manifest_0)
    manifest_1["generation"] = 1
    manifest_1["previous_manifest_sha256"] = registry_0["active_manifest_sha256"]
    roots = manifest_1["roots"]
    assert isinstance(roots, dict)
    verifier = roots["verifier"]
    assert isinstance(verifier, dict)
    verifier["workflow_sha"] = ROTATED_VERIFIER_SHA
    manifest_1_sha = sha256_hex(canonical_json_bytes(manifest_1))
    history = registry_0["history"]
    assert isinstance(history, list)
    registry_1 = {
        "schema_version": REGISTRY_SCHEMA_VERSION,
        "active_generation": 1,
        "active_manifest_sha256": manifest_1_sha,
        "history": [
            *history,
            {
                "generation": 1,
                "manifest_path": GENERATION_1_MANIFEST_KEY,
                "manifest_sha256": manifest_1_sha,
            },
        ],
    }
    manifests = {
        GENESIS_MANIFEST_KEY: manifest_0,
        GENERATION_1_MANIFEST_KEY: manifest_1,
    }
    return registry_1, manifests


def _checkpoint_v3_generation_1(
    checkpoint_v3_genesis: dict[str, object],
) -> dict[str, object]:
    registry_1, manifests = _candidate_registry()
    authority = checkpoint_v3_genesis["rotation_authority"]
    assert isinstance(authority, dict)
    observation = RotationAuthorityObservation(
        verified=True,
        rotation_producer_provider="external",
        rotation_producer_instance_id="external",
        control_plane_provider="external",
        control_plane_id="external",
        logical_rotation_id=str(authority["logical_rotation_id"]),
        rotation_contract_sha256=str(authority["rotation_contract_sha256"]),
        authorization_contract_sha256=str(authority["authorization_contract_sha256"]),
        previous_registry_sha256=str(checkpoint_v3_genesis["accepted_registry_sha256"]),
        current_registry_sha256=sha256_hex(canonical_json_bytes(registry_1)),
        previous_manifest_sha256=str(checkpoint_v3_genesis["accepted_manifest_sha256"]),
        current_manifest_sha256=str(registry_1["active_manifest_sha256"]),
        from_generation=0,
        to_generation=1,
        rotation_reason="registry_rotation_authorized",
    )
    evidence = PortableRotationAuthorityEvidence(
        verified=True,
        receipt_sha256=rotation_authority_receipt_sha256(observation),
        logical_rotation_id=observation.logical_rotation_id,
        rotation_contract_sha256=observation.rotation_contract_sha256,
        authorization_contract_sha256=observation.authorization_contract_sha256,
        previous_registry_sha256=observation.previous_registry_sha256,
        current_registry_sha256=observation.current_registry_sha256,
        previous_manifest_sha256=observation.previous_manifest_sha256,
        current_manifest_sha256=observation.current_manifest_sha256,
        from_generation=0,
        to_generation=1,
        rotation_reason="registry_rotation_authorized",
    )
    decision = evaluate_registry_candidate_v3(
        checkpoint_v3_genesis, registry_1, manifests, rotation_evidence=evidence
    )
    assert decision.authorized and decision.next_checkpoint is not None
    return decision.next_checkpoint


def _checkpoint_authority_evidence(
    witness_v4: dict[str, object], checkpoint_v3_gen1: dict[str, object]
) -> PortableCheckpointAuthorityEvidence:
    authority = witness_v4["checkpoint_authority"]
    assert isinstance(authority, dict)
    return PortableCheckpointAuthorityEvidence(
        verified=True,
        subject_sha256=checkpoint_v3_sha256(checkpoint_v3_gen1),
        logical_producer_id=str(authority["logical_producer_id"]),
        producer_contract_sha256=str(authority["producer_contract_sha256"]),
        authorization_contract_sha256=str(authority["authorization_contract_sha256"]),
        evidence_type=str(authority["evidence_type"]),
        generation=1,
    )


def test_migration_explicitly_updates_checkpoint_contract_authority() -> None:
    witness_v3 = _witness_v3_genesis()
    checkpoint_v3 = _checkpoint_v3_genesis()
    evidence = _migration_evidence(witness_v3, checkpoint_v3)
    decision = migrate_witness_v3_genesis_to_v4(
        witness_v3, checkpoint_v3, migration_evidence=evidence
    )
    assert decision.authorized
    assert decision.reason == "witness_checkpoint_schema_and_authority_migrated"
    assert decision.witness is not None
    assert validate_witness_v4(decision.witness)

    old_authority = witness_v3["checkpoint_authority"]
    new_authority = decision.witness["checkpoint_authority"]
    assert isinstance(old_authority, dict)
    assert isinstance(new_authority, dict)
    assert new_authority["logical_producer_id"] == old_authority["logical_producer_id"]
    assert new_authority["evidence_type"] == old_authority["evidence_type"]
    assert new_authority["producer_contract_sha256"] == _digest(CHECKPOINT_PRODUCER_V2_PATH)
    assert new_authority["authorization_contract_sha256"] == _digest(CHECKPOINT_AUTH_V2_PATH)
    assert new_authority["producer_contract_sha256"] != old_authority["producer_contract_sha256"]
    assert new_authority["authorization_contract_sha256"] != old_authority[
        "authorization_contract_sha256"
    ]

    migration = decision.witness["checkpoint_schema_migration"]
    assert isinstance(migration, dict)
    assert migration["from_checkpoint_authority"] == old_authority
    assert migration["to_checkpoint_authority"] == new_authority
    assert migration["claim_sha256"] == checkpoint_schema_migration_claim_sha256(evidence)


def test_unverified_schema_and_authority_migration_fails_closed() -> None:
    witness_v3 = _witness_v3_genesis()
    checkpoint_v3 = _checkpoint_v3_genesis()
    evidence = _migration_evidence(witness_v3, checkpoint_v3, verified=False)
    decision = migrate_witness_v3_genesis_to_v4(
        witness_v3, checkpoint_v3, migration_evidence=evidence
    )
    assert decision.reason == "migration_evidence_unverified"


def test_old_checkpoint_contract_cannot_be_reused_after_schema_migration() -> None:
    witness_v3 = _witness_v3_genesis()
    checkpoint_v3 = _checkpoint_v3_genesis()
    evidence = replace(
        _migration_evidence(witness_v3, checkpoint_v3),
        next_producer_contract_sha256=_digest(CHECKPOINT_PRODUCER_V1_PATH),
    )
    decision = migrate_witness_v3_genesis_to_v4(
        witness_v3, checkpoint_v3, migration_evidence=evidence
    )
    assert decision.reason == "producer_contract_not_migrated"


def test_wrong_legacy_authority_binding_fails_closed() -> None:
    witness_v3 = _witness_v3_genesis()
    checkpoint_v3 = _checkpoint_v3_genesis()
    evidence = replace(
        _migration_evidence(witness_v3, checkpoint_v3),
        legacy_authorization_contract_sha256="7" * 64,
    )
    decision = migrate_witness_v3_genesis_to_v4(
        witness_v3, checkpoint_v3, migration_evidence=evidence
    )
    assert decision.reason == "legacy_authorization_contract_mismatch"


def test_wrong_checkpoint_rotation_migration_claim_fails_closed() -> None:
    witness_v3 = _witness_v3_genesis()
    checkpoint_v3 = _checkpoint_v3_genesis()
    evidence = replace(
        _migration_evidence(witness_v3, checkpoint_v3),
        checkpoint_rotation_migration_claim_sha256="7" * 64,
    )
    decision = migrate_witness_v3_genesis_to_v4(
        witness_v3, checkpoint_v3, migration_evidence=evidence
    )
    assert decision.reason == "checkpoint_rotation_migration_claim_mismatch"


def test_checkpoint_v3_advances_witness_v4_under_new_contracts() -> None:
    witness_v4, checkpoint_v3_genesis = _witness_v4_genesis()
    checkpoint_v3_gen1 = _checkpoint_v3_generation_1(checkpoint_v3_genesis)
    decision = evaluate_checkpoint_candidate_v4(
        witness_v4,
        checkpoint_v3_gen1,
        previous_checkpoint=checkpoint_v3_genesis,
        authority_evidence=_checkpoint_authority_evidence(witness_v4, checkpoint_v3_gen1),
    )
    assert decision.authorized
    assert decision.reason == "checkpoint_witness_advanced"
    assert decision.next_witness is not None
    assert validate_witness_v4(decision.next_witness, witness_v4)
    assert decision.next_witness["previous_witness_sha256"] == witness_v4_sha256(witness_v4)


def test_legacy_checkpoint_authority_evidence_is_rejected_after_migration() -> None:
    witness_v4, checkpoint_v3_genesis = _witness_v4_genesis()
    checkpoint_v3_gen1 = _checkpoint_v3_generation_1(checkpoint_v3_genesis)
    evidence = replace(
        _checkpoint_authority_evidence(witness_v4, checkpoint_v3_gen1),
        producer_contract_sha256=_digest(CHECKPOINT_PRODUCER_V1_PATH),
        authorization_contract_sha256=_digest(CHECKPOINT_AUTH_V1_PATH),
    )
    decision = evaluate_checkpoint_candidate_v4(
        witness_v4,
        checkpoint_v3_gen1,
        previous_checkpoint=checkpoint_v3_genesis,
        authority_evidence=evidence,
    )
    assert decision.reason == "producer_contract_mismatch"


def test_checkpoint_subject_drift_fails_closed() -> None:
    witness_v4, checkpoint_v3_genesis = _witness_v4_genesis()
    checkpoint_v3_gen1 = _checkpoint_v3_generation_1(checkpoint_v3_genesis)
    evidence = replace(
        _checkpoint_authority_evidence(witness_v4, checkpoint_v3_gen1),
        subject_sha256="7" * 64,
    )
    decision = evaluate_checkpoint_candidate_v4(
        witness_v4,
        checkpoint_v3_gen1,
        previous_checkpoint=checkpoint_v3_genesis,
        authority_evidence=evidence,
    )
    assert decision.reason == "checkpoint_authority_subject_mismatch"
