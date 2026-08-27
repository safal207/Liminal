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
    migration_claim_sha256,
    validate_checkpoint_v3,
)
from liminal.recovery_trust_consumer_checkpoint_attested import checkpoint_sha256
from liminal.recovery_trust_root_registry import (
    REGISTRY_SCHEMA_VERSION,
    canonical_json_bytes,
    sha256_hex,
)
from liminal.rotation_authority_portability import (
    RotationAuthorityObservation,
    rotation_authority_receipt_sha256,
)

CHECKPOINT_PATH = Path("policies/recovery-trust-consumer-checkpoint-v0.2.json")
REGISTRY_PATH = Path("policies/recovery-trust-root-registry-v0.1.json")
MANIFEST_PATH = Path("policies/recovery-trust-root-manifest-v0.1.json")
ROTATION_CONTRACT_PATH = Path("policies/portable-rotation-producer-contract-v0.1.json")
AUTHORIZATION_CONTRACT_PATH = Path(
    "policies/portable-rotation-authorization-contract-v0.1.json"
)
GENESIS_MANIFEST_KEY = "policies/recovery-trust-root-manifest-v0.1.json"
GENERATION_1_MANIFEST_KEY = "drill/generation-1-manifest.json"
ROTATED_VERIFIER_SHA = "ca69a7d342a4e00dfc2d11b13016a8eb7733da6b"
TRUST_DOMAIN = "liminal.trusted-recovery"
LOGICAL_ROTATION_ID = "liminal.trusted-recovery.registry-rotation"


def _load(path: Path) -> dict[str, object]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    assert isinstance(payload, dict)
    return payload


def _digest(path: Path) -> str:
    return sha256_hex(canonical_json_bytes(_load(path)))


def _migration(
    legacy: dict[str, object], *, verified: bool = True
) -> CheckpointRotationAuthorityMigrationEvidence:
    authorizer = legacy["advance_authorizer"]
    assert isinstance(authorizer, dict)
    return CheckpointRotationAuthorityMigrationEvidence(
        verified=verified,
        trust_domain=TRUST_DOMAIN,
        legacy_checkpoint_sha256=checkpoint_sha256(legacy),
        legacy_authorizer_workflow_path=str(authorizer["workflow_path"]),
        legacy_authorizer_workflow_sha=str(authorizer["workflow_sha"]),
        logical_rotation_id=LOGICAL_ROTATION_ID,
        rotation_contract_sha256=_digest(ROTATION_CONTRACT_PATH),
        authorization_contract_sha256=_digest(AUTHORIZATION_CONTRACT_PATH),
    )


def _candidate() -> tuple[dict[str, object], dict[str, object], dict[str, object]]:
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
    return registry_0, registry_1, manifests


def _rotation_evidence(
    trusted: dict[str, object],
    registry_1: dict[str, object],
    *,
    verified: bool = True,
) -> PortableRotationAuthorityEvidence:
    current_registry_sha = sha256_hex(canonical_json_bytes(registry_1))
    authority = trusted["rotation_authority"]
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
        previous_registry_sha256=str(trusted["accepted_registry_sha256"]),
        current_registry_sha256=current_registry_sha,
        previous_manifest_sha256=str(trusted["accepted_manifest_sha256"]),
        current_manifest_sha256=str(registry_1["active_manifest_sha256"]),
        from_generation=0,
        to_generation=1,
        rotation_reason="registry_rotation_authorized",
    )
    return PortableRotationAuthorityEvidence(
        verified=verified,
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


def _migrated() -> dict[str, object]:
    legacy = _load(CHECKPOINT_PATH)
    decision = migrate_legacy_genesis_checkpoint_to_v3(
        legacy, trust_domain=TRUST_DOMAIN, migration_evidence=_migration(legacy)
    )
    assert decision.authorized
    assert decision.checkpoint is not None
    return decision.checkpoint


def test_migration_binds_exact_legacy_authorizer_and_portable_contracts() -> None:
    legacy = _load(CHECKPOINT_PATH)
    evidence = _migration(legacy)
    decision = migrate_legacy_genesis_checkpoint_to_v3(
        legacy, trust_domain=TRUST_DOMAIN, migration_evidence=evidence
    )
    assert decision.authorized
    assert decision.reason == "checkpoint_rotation_authority_migrated"
    assert decision.checkpoint is not None
    checkpoint = decision.checkpoint
    assert validate_checkpoint_v3(checkpoint)
    origin = checkpoint["authority_origin"]
    assert isinstance(origin, dict)
    assert origin["legacy_checkpoint_sha256"] == checkpoint_sha256(legacy)
    assert origin["legacy_authorizer_workflow_sha"] == (
        "e2cb6a014236bc561d03c405f4986146026041fa"
    )
    assert origin["migration_claim_sha256"] == migration_claim_sha256(evidence)
    authority = checkpoint["rotation_authority"]
    assert isinstance(authority, dict)
    assert "workflow_path" not in authority
    assert "workflow_sha" not in authority


def test_unverified_migration_fails_closed() -> None:
    legacy = _load(CHECKPOINT_PATH)
    decision = migrate_legacy_genesis_checkpoint_to_v3(
        legacy,
        trust_domain=TRUST_DOMAIN,
        migration_evidence=_migration(legacy, verified=False),
    )
    assert decision.reason == "migration_evidence_unverified"


def test_wrong_legacy_checkpoint_digest_fails_closed() -> None:
    legacy = _load(CHECKPOINT_PATH)
    evidence = replace(_migration(legacy), legacy_checkpoint_sha256="0" * 64)
    decision = migrate_legacy_genesis_checkpoint_to_v3(
        legacy, trust_domain=TRUST_DOMAIN, migration_evidence=evidence
    )
    assert decision.reason == "legacy_checkpoint_digest_mismatch"


def test_wrong_legacy_authorizer_fails_closed() -> None:
    legacy = _load(CHECKPOINT_PATH)
    evidence = replace(_migration(legacy), legacy_authorizer_workflow_sha="1" * 40)
    decision = migrate_legacy_genesis_checkpoint_to_v3(
        legacy, trust_domain=TRUST_DOMAIN, migration_evidence=evidence
    )
    assert decision.reason == "legacy_authorizer_mismatch"


def test_verified_portable_rotation_advances_checkpoint() -> None:
    trusted = _migrated()
    _, registry_1, manifests = _candidate()
    decision = evaluate_registry_candidate_v3(
        trusted,
        registry_1,
        manifests,
        rotation_evidence=_rotation_evidence(trusted, registry_1),
    )
    assert decision.authorized
    assert decision.reason == "consumer_checkpoint_advanced"
    assert decision.next_checkpoint is not None
    assert validate_checkpoint_v3(decision.next_checkpoint, trusted)
    assert decision.next_checkpoint["previous_checkpoint_sha256"] == checkpoint_v3_sha256(
        trusted
    )


def test_unverified_rotation_evidence_fails_closed() -> None:
    trusted = _migrated()
    _, registry_1, manifests = _candidate()
    evidence = _rotation_evidence(trusted, registry_1, verified=False)
    decision = evaluate_registry_candidate_v3(
        trusted, registry_1, manifests, rotation_evidence=evidence
    )
    assert decision.reason == "rotation_authority_evidence_unverified"


def test_rotation_contract_drift_fails_closed() -> None:
    trusted = _migrated()
    _, registry_1, manifests = _candidate()
    evidence = replace(
        _rotation_evidence(trusted, registry_1), rotation_contract_sha256="7" * 64
    )
    decision = evaluate_registry_candidate_v3(
        trusted, registry_1, manifests, rotation_evidence=evidence
    )
    assert decision.reason == "rotation_contract_mismatch"


def test_rotation_authorization_contract_drift_fails_closed() -> None:
    trusted = _migrated()
    _, registry_1, manifests = _candidate()
    evidence = replace(
        _rotation_evidence(trusted, registry_1),
        authorization_contract_sha256="7" * 64,
    )
    decision = evaluate_registry_candidate_v3(
        trusted, registry_1, manifests, rotation_evidence=evidence
    )
    assert decision.reason == "rotation_authorization_contract_mismatch"


def test_predecessor_registry_drift_fails_closed() -> None:
    trusted = _migrated()
    _, registry_1, manifests = _candidate()
    evidence = replace(
        _rotation_evidence(trusted, registry_1), previous_registry_sha256="7" * 64
    )
    decision = evaluate_registry_candidate_v3(
        trusted, registry_1, manifests, rotation_evidence=evidence
    )
    assert decision.reason == "rotation_previous_registry_mismatch"


def test_current_manifest_drift_fails_closed() -> None:
    trusted = _migrated()
    _, registry_1, manifests = _candidate()
    evidence = replace(
        _rotation_evidence(trusted, registry_1), current_manifest_sha256="7" * 64
    )
    decision = evaluate_registry_candidate_v3(
        trusted, registry_1, manifests, rotation_evidence=evidence
    )
    assert decision.reason == "rotation_current_manifest_mismatch"


def test_receipt_digest_tampering_fails_closed() -> None:
    trusted = _migrated()
    _, registry_1, manifests = _candidate()
    evidence = replace(_rotation_evidence(trusted, registry_1), receipt_sha256="7" * 64)
    decision = evaluate_registry_candidate_v3(
        trusted, registry_1, manifests, rotation_evidence=evidence
    )
    assert decision.reason == "rotation_receipt_digest_mismatch"


def test_post_transition_receipt_is_not_self_authorizing() -> None:
    trusted = _migrated()
    _, registry_1, manifests = _candidate()
    decision = evaluate_registry_candidate_v3(
        trusted, registry_1, manifests, rotation_evidence=None
    )
    assert decision.reason == "rotation_authority_evidence_invalid"
