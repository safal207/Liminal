#!/usr/bin/env python3
"""Recompute Upstream Rotation-Authority Portability v0.1 fail closed."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
from typing import Any

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
    evaluate_checkpoint_candidate_v4,
    migrate_witness_v3_genesis_to_v4,
    witness_v4_sha256,
)
from liminal.recovery_trust_consumer_checkpoint_attested import checkpoint_sha256
from liminal.recovery_trust_consumer_checkpoint_witness import witness_sha256
from liminal.recovery_trust_root_registry import canonical_json_bytes, sha256_hex
from liminal.rotation_authority_portability import (
    RotationAuthorityObservation,
    compare_rotation_authority,
    rotation_authority_receipt_sha256,
)

SCHEMA = "liminal-upstream-rotation-authority-portability-proof/v0.1"
PRIMARY_ROTATION_WORKFLOW_SHA = "e2cb6a014236bc561d03c405f4986146026041fa"
TRUST_DOMAIN = "liminal.trusted-recovery"


def _load(path: Path) -> dict[str, Any]:
    value = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(value, dict):
        raise ValueError(f"object_required:{path}")
    return value


def _canonical(value: object) -> bytes:
    return canonical_json_bytes(value)


def _digest(path: Path) -> str:
    return sha256_hex(_canonical(_load(path)))


def _sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _single(root: Path, name: str) -> Path:
    matches = sorted(root.rglob(name))
    if len(matches) != 1:
        raise ValueError(f"exactly_one_{name}_required:{len(matches)}")
    return matches[0]


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--external-envelope", required=True)
    parser.add_argument("--external-signature-verification", required=True)
    parser.add_argument("--standalone-source", required=True)
    parser.add_argument("--github-rotation-dir", required=True)
    parser.add_argument("--legacy-checkpoint", required=True)
    parser.add_argument("--legacy-witness", required=True)
    parser.add_argument("--baseline-registry", required=True)
    parser.add_argument("--baseline-manifest", required=True)
    parser.add_argument("--rotation-contract", required=True)
    parser.add_argument("--rotation-authorization-contract", required=True)
    parser.add_argument("--checkpoint-producer-v1", required=True)
    parser.add_argument("--checkpoint-authorization-v1", required=True)
    parser.add_argument("--checkpoint-producer-v2", required=True)
    parser.add_argument("--checkpoint-authorization-v2", required=True)
    parser.add_argument("--verifier-sha", required=True)
    parser.add_argument("--output", required=True)
    args = parser.parse_args()

    envelope_path = Path(args.external_envelope)
    envelope = _load(envelope_path)
    if envelope.get("schema") != "liminal-upstream-rotation-authority-external-proof/v0.1":
        raise ValueError("external_envelope_schema_invalid")

    signature_verification = _load(Path(args.external_signature_verification))
    if signature_verification.get("verified") is not True:
        raise ValueError("external_signatures_unverified")

    control = envelope.get("control_plane")
    producer = envelope.get("producer")
    if not isinstance(control, dict) or not isinstance(producer, dict):
        raise ValueError("external_envelope_sections_missing")
    if signature_verification.get("control_plane_root_id") != control.get("root_id"):
        raise ValueError("control_plane_root_mismatch")
    if signature_verification.get("producer_root_id") != producer.get("root_id"):
        raise ValueError("producer_root_mismatch")

    standalone = Path(args.standalone_source)
    standalone_sha = _sha256(standalone)
    if producer.get("standalone_implementation_sha256") != standalone_sha:
        raise ValueError("standalone_source_digest_mismatch")

    rotation_contract = _load(Path(args.rotation_contract))
    rotation_auth = _load(Path(args.rotation_authorization_contract))
    if _canonical(rotation_contract) != _canonical(control.get("producer_contract")):
        raise ValueError("rotation_contract_bytes_mismatch")
    if _canonical(rotation_auth) != _canonical(control.get("authorization_contract")):
        raise ValueError("rotation_authorization_contract_bytes_mismatch")

    rotation_contract_sha = sha256_hex(_canonical(rotation_contract))
    rotation_auth_sha = sha256_hex(_canonical(rotation_auth))
    intent = control.get("rotation_intent")
    if not isinstance(intent, dict):
        raise ValueError("rotation_intent_missing")
    intent_sha = sha256_hex(_canonical(intent))
    if intent.get("rotation_contract_sha256") != rotation_contract_sha:
        raise ValueError("rotation_contract_digest_mismatch")
    if intent.get("authorization_contract_sha256") != rotation_auth_sha:
        raise ValueError("rotation_authorization_contract_digest_mismatch")

    external_result = producer.get("result")
    if not isinstance(external_result, dict) or external_result.get("verified") is not True:
        raise ValueError("external_producer_result_invalid")
    if external_result.get("standalone_implementation_sha256") != standalone_sha:
        raise ValueError("external_result_source_digest_mismatch")
    if external_result.get("control_plane_authorization_intent_sha256") != intent_sha:
        raise ValueError("external_result_intent_mismatch")
    if external_result.get("rotation_contract_sha256") != rotation_contract_sha:
        raise ValueError("external_result_rotation_contract_mismatch")
    if external_result.get("authorization_contract_sha256") != rotation_auth_sha:
        raise ValueError("external_result_authorization_contract_mismatch")
    if external_result.get("reason") != "registry_rotation_authorized":
        raise ValueError("external_result_reason_invalid")

    primary_manifest_path = _single(
        Path(args.github_rotation_dir), "generation-1-manifest.json"
    )
    primary_registry_path = _single(
        Path(args.github_rotation_dir), "generation-1-registry.json"
    )
    primary_result_path = _single(
        Path(args.github_rotation_dir), "rotation-drill-result.json"
    )
    primary_manifest = _load(primary_manifest_path)
    primary_registry = _load(primary_registry_path)
    primary_result = _load(primary_result_path)
    if primary_result.get("verified") is not True:
        raise ValueError("primary_rotation_result_unverified")
    legitimate = primary_result.get("legitimate_rotation")
    if not isinstance(legitimate, dict):
        raise ValueError("primary_legitimate_rotation_missing")
    if legitimate.get("authorized") is not True:
        raise ValueError("primary_rotation_not_authorized")
    if legitimate.get("reason") != "registry_rotation_authorized":
        raise ValueError("primary_rotation_reason_invalid")

    external_manifest = external_result.get("generation_1_manifest")
    external_registry = external_result.get("generation_1_registry")
    if not isinstance(external_manifest, dict) or not isinstance(external_registry, dict):
        raise ValueError("external_generation_1_material_missing")
    if _canonical(primary_manifest) != _canonical(external_manifest):
        raise ValueError("rotation_manifest_bytes_mismatch")
    if _canonical(primary_registry) != _canonical(external_registry):
        raise ValueError("rotation_registry_bytes_mismatch")

    baseline_registry = _load(Path(args.baseline_registry))
    baseline_manifest = _load(Path(args.baseline_manifest))
    previous_registry_sha = sha256_hex(_canonical(baseline_registry))
    previous_manifest_sha = sha256_hex(_canonical(baseline_manifest))
    current_registry_sha = sha256_hex(_canonical(primary_registry))
    current_manifest_sha = sha256_hex(_canonical(primary_manifest))
    if intent.get("previous_registry_sha256") != previous_registry_sha:
        raise ValueError("intent_previous_registry_mismatch")
    if intent.get("previous_manifest_sha256") != previous_manifest_sha:
        raise ValueError("intent_previous_manifest_mismatch")
    if external_result.get("current_registry_sha256") != current_registry_sha:
        raise ValueError("external_current_registry_mismatch")
    if external_result.get("current_manifest_sha256") != current_manifest_sha:
        raise ValueError("external_current_manifest_mismatch")

    primary_observation = RotationAuthorityObservation(
        verified=True,
        rotation_producer_provider="github-actions-rotation-producer",
        rotation_producer_instance_id=PRIMARY_ROTATION_WORKFLOW_SHA,
        control_plane_provider="github-repository-rotation-policy",
        control_plane_id=f"safal207/Liminal@{args.verifier_sha}",
        logical_rotation_id=str(intent["logical_rotation_id"]),
        rotation_contract_sha256=rotation_contract_sha,
        authorization_contract_sha256=rotation_auth_sha,
        previous_registry_sha256=previous_registry_sha,
        current_registry_sha256=current_registry_sha,
        previous_manifest_sha256=previous_manifest_sha,
        current_manifest_sha256=current_manifest_sha,
        from_generation=int(intent["from_generation"]),
        to_generation=int(intent["to_generation"]),
        rotation_reason="registry_rotation_authorized",
    )
    secondary_observation = RotationAuthorityObservation(
        verified=True,
        rotation_producer_provider="openai-workspace-standalone-rotation-producer",
        rotation_producer_instance_id=standalone_sha,
        control_plane_provider="offline-ed25519-rotation-control-plane",
        control_plane_id=str(control["root_id"]),
        logical_rotation_id=str(external_result["logical_rotation_id"]),
        rotation_contract_sha256=str(external_result["rotation_contract_sha256"]),
        authorization_contract_sha256=str(
            external_result["authorization_contract_sha256"]
        ),
        previous_registry_sha256=str(external_result["previous_registry_sha256"]),
        current_registry_sha256=str(external_result["current_registry_sha256"]),
        previous_manifest_sha256=str(external_result["previous_manifest_sha256"]),
        current_manifest_sha256=str(external_result["current_manifest_sha256"]),
        from_generation=int(external_result["from_generation"]),
        to_generation=int(external_result["to_generation"]),
        rotation_reason=str(external_result["reason"]),
    )
    agreement = compare_rotation_authority(primary_observation, secondary_observation)
    if not agreement.verified or agreement.receipt is None:
        raise ValueError(f"rotation_authority_portability_rejected:{agreement.reason}")
    rotation_receipt_sha = rotation_authority_receipt_sha256(primary_observation)
    if rotation_receipt_sha != rotation_authority_receipt_sha256(secondary_observation):
        raise ValueError("rotation_receipt_digest_mismatch")

    legacy_checkpoint = _load(Path(args.legacy_checkpoint))
    legacy_authorizer = legacy_checkpoint.get("advance_authorizer")
    if not isinstance(legacy_authorizer, dict):
        raise ValueError("legacy_checkpoint_authorizer_missing")
    checkpoint_rotation_migration = CheckpointRotationAuthorityMigrationEvidence(
        verified=True,
        trust_domain=TRUST_DOMAIN,
        legacy_checkpoint_sha256=checkpoint_sha256(legacy_checkpoint),
        legacy_authorizer_workflow_path=str(legacy_authorizer["workflow_path"]),
        legacy_authorizer_workflow_sha=str(legacy_authorizer["workflow_sha"]),
        logical_rotation_id=primary_observation.logical_rotation_id,
        rotation_contract_sha256=rotation_contract_sha,
        authorization_contract_sha256=rotation_auth_sha,
    )
    checkpoint_migration = migrate_legacy_genesis_checkpoint_to_v3(
        legacy_checkpoint,
        trust_domain=TRUST_DOMAIN,
        migration_evidence=checkpoint_rotation_migration,
    )
    if not checkpoint_migration.authorized or checkpoint_migration.checkpoint is None:
        raise ValueError(
            f"checkpoint_v3_migration_rejected:{checkpoint_migration.reason}"
        )
    checkpoint_v3_genesis = checkpoint_migration.checkpoint

    manifests = {
        "policies/recovery-trust-root-manifest-v0.1.json": baseline_manifest,
        "drill/generation-1-manifest.json": primary_manifest,
    }
    primary_rotation_evidence = PortableRotationAuthorityEvidence(
        verified=True,
        receipt_sha256=rotation_receipt_sha,
        logical_rotation_id=primary_observation.logical_rotation_id,
        rotation_contract_sha256=primary_observation.rotation_contract_sha256,
        authorization_contract_sha256=primary_observation.authorization_contract_sha256,
        previous_registry_sha256=primary_observation.previous_registry_sha256,
        current_registry_sha256=primary_observation.current_registry_sha256,
        previous_manifest_sha256=primary_observation.previous_manifest_sha256,
        current_manifest_sha256=primary_observation.current_manifest_sha256,
        from_generation=primary_observation.from_generation,
        to_generation=primary_observation.to_generation,
        rotation_reason=primary_observation.rotation_reason,
    )
    secondary_rotation_evidence = PortableRotationAuthorityEvidence(
        verified=True,
        receipt_sha256=rotation_receipt_sha,
        logical_rotation_id=secondary_observation.logical_rotation_id,
        rotation_contract_sha256=secondary_observation.rotation_contract_sha256,
        authorization_contract_sha256=secondary_observation.authorization_contract_sha256,
        previous_registry_sha256=secondary_observation.previous_registry_sha256,
        current_registry_sha256=secondary_observation.current_registry_sha256,
        previous_manifest_sha256=secondary_observation.previous_manifest_sha256,
        current_manifest_sha256=secondary_observation.current_manifest_sha256,
        from_generation=secondary_observation.from_generation,
        to_generation=secondary_observation.to_generation,
        rotation_reason=secondary_observation.rotation_reason,
    )
    primary_checkpoint_decision = evaluate_registry_candidate_v3(
        checkpoint_v3_genesis,
        primary_registry,
        manifests,
        rotation_evidence=primary_rotation_evidence,
    )
    secondary_checkpoint_decision = evaluate_registry_candidate_v3(
        checkpoint_v3_genesis,
        external_registry,
        manifests,
        rotation_evidence=secondary_rotation_evidence,
    )
    if (
        not primary_checkpoint_decision.authorized
        or primary_checkpoint_decision.next_checkpoint is None
    ):
        raise ValueError(
            f"primary_checkpoint_v3_rejected:{primary_checkpoint_decision.reason}"
        )
    if (
        not secondary_checkpoint_decision.authorized
        or secondary_checkpoint_decision.next_checkpoint is None
    ):
        raise ValueError(
            f"secondary_checkpoint_v3_rejected:{secondary_checkpoint_decision.reason}"
        )
    if primary_checkpoint_decision.next_checkpoint != secondary_checkpoint_decision.next_checkpoint:
        raise ValueError("checkpoint_v3_semantics_mismatch")
    checkpoint_v3_gen1 = primary_checkpoint_decision.next_checkpoint
    checkpoint_v3_gen1_sha = checkpoint_v3_sha256(checkpoint_v3_gen1)

    legacy_witness = _load(Path(args.legacy_witness))
    legacy_signer = legacy_witness.get("checkpoint_signer")
    if not isinstance(legacy_signer, dict):
        raise ValueError("legacy_witness_signer_missing")
    witness_v3_migration = WitnessAuthorityMigrationEvidence(
        verified=True,
        trust_domain=TRUST_DOMAIN,
        legacy_witness_sha256=witness_sha256(legacy_witness),
        legacy_signer_workflow_path=str(legacy_signer["workflow_path"]),
        legacy_signer_workflow_sha=str(legacy_signer["workflow_sha"]),
        logical_producer_id="liminal.trusted-recovery.checkpoint-producer",
        producer_contract_sha256=_digest(Path(args.checkpoint_producer_v1)),
        authorization_contract_sha256=_digest(Path(args.checkpoint_authorization_v1)),
        evidence_type="trusted-recovery-consumer-checkpoint",
    )
    witness_v3_decision = migrate_legacy_genesis_witness_to_v3(
        legacy_witness,
        trust_domain=TRUST_DOMAIN,
        migration_evidence=witness_v3_migration,
    )
    if not witness_v3_decision.authorized or witness_v3_decision.witness is None:
        raise ValueError(f"witness_v3_migration_rejected:{witness_v3_decision.reason}")
    witness_v3_genesis = witness_v3_decision.witness

    checkpoint_origin = checkpoint_v3_genesis.get("authority_origin")
    if not isinstance(checkpoint_origin, dict):
        raise ValueError("checkpoint_v3_authority_origin_missing")
    witness_v4_migration = WitnessCheckpointSchemaMigrationEvidence(
        verified=True,
        trust_domain=TRUST_DOMAIN,
        legacy_witness_v3_sha256=witness_v3_sha256(witness_v3_genesis),
        legacy_checkpoint_sha256=str(witness_v3_genesis["checkpoint_sha256"]),
        checkpoint_v3_sha256=checkpoint_v3_sha256(checkpoint_v3_genesis),
        checkpoint_rotation_migration_claim_sha256=str(
            checkpoint_origin["migration_claim_sha256"]
        ),
        logical_producer_id="liminal.trusted-recovery.checkpoint-producer",
        evidence_type="trusted-recovery-consumer-checkpoint",
        legacy_producer_contract_sha256=_digest(Path(args.checkpoint_producer_v1)),
        legacy_authorization_contract_sha256=_digest(
            Path(args.checkpoint_authorization_v1)
        ),
        next_producer_contract_sha256=_digest(Path(args.checkpoint_producer_v2)),
        next_authorization_contract_sha256=_digest(
            Path(args.checkpoint_authorization_v2)
        ),
    )
    witness_v4_decision = migrate_witness_v3_genesis_to_v4(
        witness_v3_genesis,
        checkpoint_v3_genesis,
        migration_evidence=witness_v4_migration,
    )
    if not witness_v4_decision.authorized or witness_v4_decision.witness is None:
        raise ValueError(f"witness_v4_migration_rejected:{witness_v4_decision.reason}")
    witness_v4_genesis = witness_v4_decision.witness

    checkpoint_authority = witness_v4_genesis.get("checkpoint_authority")
    if not isinstance(checkpoint_authority, dict):
        raise ValueError("checkpoint_v4_authority_missing")
    checkpoint_authority_evidence = PortableCheckpointAuthorityEvidence(
        verified=True,
        subject_sha256=checkpoint_v3_gen1_sha,
        logical_producer_id=str(checkpoint_authority["logical_producer_id"]),
        producer_contract_sha256=str(checkpoint_authority["producer_contract_sha256"]),
        authorization_contract_sha256=str(
            checkpoint_authority["authorization_contract_sha256"]
        ),
        evidence_type=str(checkpoint_authority["evidence_type"]),
        generation=1,
    )
    primary_witness_decision = evaluate_checkpoint_candidate_v4(
        witness_v4_genesis,
        checkpoint_v3_gen1,
        previous_checkpoint=checkpoint_v3_genesis,
        authority_evidence=checkpoint_authority_evidence,
    )
    secondary_witness_decision = evaluate_checkpoint_candidate_v4(
        witness_v4_genesis,
        checkpoint_v3_gen1,
        previous_checkpoint=checkpoint_v3_genesis,
        authority_evidence=checkpoint_authority_evidence,
    )
    if not primary_witness_decision.authorized or primary_witness_decision.next_witness is None:
        raise ValueError(f"primary_witness_v4_rejected:{primary_witness_decision.reason}")
    if (
        not secondary_witness_decision.authorized
        or secondary_witness_decision.next_witness is None
    ):
        raise ValueError(
            f"secondary_witness_v4_rejected:{secondary_witness_decision.reason}"
        )
    if primary_witness_decision.next_witness != secondary_witness_decision.next_witness:
        raise ValueError("witness_v4_semantics_mismatch")
    next_witness_sha = witness_v4_sha256(primary_witness_decision.next_witness)

    result = {
        "schema": SCHEMA,
        "verified": True,
        "reason": "independent_upstream_rotation_authority_semantics_agree",
        "external_envelope_sha256": _sha256(envelope_path),
        "control_plane_root_id": control["root_id"],
        "producer_root_id": producer["root_id"],
        "standalone_implementation_sha256": standalone_sha,
        "rotation_contract_sha256": rotation_contract_sha,
        "rotation_authorization_contract_sha256": rotation_auth_sha,
        "rotation_authorization_intent_sha256": intent_sha,
        "previous_registry_sha256": previous_registry_sha,
        "current_registry_sha256": current_registry_sha,
        "previous_manifest_sha256": previous_manifest_sha,
        "current_manifest_sha256": current_manifest_sha,
        "portable_rotation_authority_receipt_sha256": rotation_receipt_sha,
        "checkpoint_v3_genesis_sha256": checkpoint_v3_sha256(checkpoint_v3_genesis),
        "checkpoint_v3_generation_1_sha256": checkpoint_v3_gen1_sha,
        "witness_v4_genesis_sha256": witness_v4_sha256(witness_v4_genesis),
        "witness_reason": primary_witness_decision.reason,
        "next_witness_sha256": next_witness_sha,
        "primary_rotation_workflow_sha": PRIMARY_ROTATION_WORKFLOW_SHA,
        "verifier_workflow_sha": args.verifier_sha,
        "primary_rotation_producer_provider": primary_observation.rotation_producer_provider,
        "secondary_rotation_producer_provider": secondary_observation.rotation_producer_provider,
        "primary_control_plane_provider": primary_observation.control_plane_provider,
        "secondary_control_plane_provider": secondary_observation.control_plane_provider,
        "claim_boundary": envelope["claim_boundary"],
    }
    output = Path(args.output)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_bytes(_canonical(result))
    print(json.dumps(result, sort_keys=True, separators=(",", ":")))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
