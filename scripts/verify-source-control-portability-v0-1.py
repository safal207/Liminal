#!/usr/bin/env python3
"""Recompute Source-Producer + Control-Plane Portability v0.1 fail closed."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
from typing import Any

from liminal.checkpoint_witness_authority_v3 import (
    PortableCheckpointAuthorityEvidence,
    WitnessAuthorityMigrationEvidence,
    evaluate_checkpoint_candidate_v3,
    migrate_legacy_genesis_witness_to_v3,
    migration_claim_sha256,
    witness_v3_sha256,
)
from liminal.recovery_trust_consumer_checkpoint_attested import checkpoint_sha256
from liminal.recovery_trust_consumer_checkpoint_witness import witness_sha256
from liminal.recovery_trust_root_registry import canonical_json_bytes, sha256_hex
from liminal.source_control_portability import (
    SourceControlAgreementReason,
    SourceControlObservation,
    compare_independent_source_control_paths,
)

SCHEMA = "liminal-source-control-portability-proof/v0.1"
PRODUCER_WORKFLOW_SHA = "f31b56a5e21a668bcb98791b05542652760dcc27"


def _load_object(path: Path) -> dict[str, Any]:
    value = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(value, dict):
        raise ValueError(f"object_required:{path}")
    return value


def _sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def _canonical(value: object) -> bytes:
    return canonical_json_bytes(value)


def _single_checkpoint(root: Path) -> Path:
    matches = sorted(root.rglob("checkpoint-generation-1.json"))
    if len(matches) != 1:
        raise ValueError(f"exactly_one_github_checkpoint_required:{len(matches)}")
    return matches[0]


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--external-envelope", required=True)
    parser.add_argument("--external-signature-verification", required=True)
    parser.add_argument("--github-artifact-dir", required=True)
    parser.add_argument("--legacy-witness", required=True)
    parser.add_argument("--baseline-checkpoint", required=True)
    parser.add_argument("--producer-contract", required=True)
    parser.add_argument("--authorization-contract", required=True)
    parser.add_argument("--verifier-sha", required=True)
    parser.add_argument("--output", required=True)
    args = parser.parse_args()

    envelope_path = Path(args.external_envelope)
    envelope = _load_object(envelope_path)
    if envelope.get("schema") != "liminal-external-source-control-proof-envelope/v0.1":
        raise ValueError("external_envelope_schema_invalid")
    payload = envelope.get("payload")
    if not isinstance(payload, dict):
        raise ValueError("external_payload_missing")
    payload_raw = _canonical(payload)
    payload_sha = _sha256(payload_raw)
    if envelope.get("payload_sha256") != payload_sha:
        raise ValueError("external_payload_digest_mismatch")

    signature_verification = _load_object(Path(args.external_signature_verification))
    if signature_verification.get("verified") is not True:
        raise ValueError("external_signatures_unverified")
    for key in ("control_plane_root_id", "producer_root_id"):
        if signature_verification.get(key) != payload.get(key):
            raise ValueError(f"external_signature_{key}_mismatch")

    producer_contract = _load_object(Path(args.producer_contract))
    authorization_contract = _load_object(Path(args.authorization_contract))
    if _canonical(producer_contract) != _canonical(payload.get("producer_contract")):
        raise ValueError("producer_contract_bytes_mismatch")
    if _canonical(authorization_contract) != _canonical(payload.get("authorization_contract")):
        raise ValueError("authorization_contract_bytes_mismatch")

    producer_contract_sha = sha256_hex(_canonical(producer_contract))
    authorization_contract_sha = sha256_hex(_canonical(authorization_contract))
    if payload.get("control_plane_bundle", {}).get("producer_contract_sha256") != producer_contract_sha:
        raise ValueError("producer_contract_digest_mismatch")
    if (
        payload.get("control_plane_bundle", {}).get("authorization_contract_sha256")
        != authorization_contract_sha
    ):
        raise ValueError("authorization_contract_digest_mismatch")

    control_plane_bundle = payload.get("control_plane_bundle")
    if not isinstance(control_plane_bundle, dict):
        raise ValueError("control_plane_bundle_missing")
    control_plane_bundle_sha = sha256_hex(_canonical(control_plane_bundle))

    migration_claim = payload.get("migration_claim")
    if not isinstance(migration_claim, dict):
        raise ValueError("migration_claim_missing")
    legacy_witness = _load_object(Path(args.legacy_witness))
    legacy_witness_digest = witness_sha256(legacy_witness)
    if migration_claim.get("legacy_witness_sha256") != legacy_witness_digest:
        raise ValueError("migration_legacy_witness_mismatch")

    legacy_signer = legacy_witness.get("checkpoint_signer")
    if not isinstance(legacy_signer, dict):
        raise ValueError("legacy_signer_missing")
    migration_evidence = WitnessAuthorityMigrationEvidence(
        verified=True,
        trust_domain=str(migration_claim["trust_domain"]),
        legacy_witness_sha256=str(migration_claim["legacy_witness_sha256"]),
        legacy_signer_workflow_path=str(migration_claim["legacy_signer_workflow_path"]),
        legacy_signer_workflow_sha=str(migration_claim["legacy_signer_workflow_sha"]),
        logical_producer_id=str(migration_claim["logical_producer_id"]),
        producer_contract_sha256=str(migration_claim["producer_contract_sha256"]),
        authorization_contract_sha256=str(migration_claim["authorization_contract_sha256"]),
        evidence_type=str(migration_claim["evidence_type"]),
    )
    if migration_evidence.legacy_signer_workflow_path != legacy_signer.get("workflow_path"):
        raise ValueError("migration_legacy_signer_path_mismatch")
    if migration_evidence.legacy_signer_workflow_sha != legacy_signer.get("workflow_sha"):
        raise ValueError("migration_legacy_signer_sha_mismatch")
    migration_sha = migration_claim_sha256(migration_evidence)
    if control_plane_bundle.get("migration_claim_sha256") != migration_sha:
        raise ValueError("migration_claim_digest_mismatch")

    migration = migrate_legacy_genesis_witness_to_v3(
        legacy_witness,
        trust_domain=migration_evidence.trust_domain,
        migration_evidence=migration_evidence,
    )
    if not migration.authorized or migration.witness is None:
        raise ValueError(f"v3_migration_rejected:{migration.reason}")
    witness_0 = migration.witness
    if witness_0 != payload.get("witness_generation_0"):
        raise ValueError("external_v3_root_mismatch")

    baseline_checkpoint = _load_object(Path(args.baseline_checkpoint))
    github_checkpoint_path = _single_checkpoint(Path(args.github_artifact_dir))
    github_checkpoint_raw = github_checkpoint_path.read_bytes()
    github_checkpoint = _load_object(github_checkpoint_path)
    external_checkpoint = payload.get("checkpoint")
    if not isinstance(external_checkpoint, dict):
        raise ValueError("external_checkpoint_missing")
    external_checkpoint_raw = _canonical(external_checkpoint)
    if github_checkpoint_raw != external_checkpoint_raw:
        raise ValueError("producer_checkpoint_bytes_mismatch")
    if github_checkpoint != external_checkpoint:
        raise ValueError("producer_checkpoint_semantics_mismatch")
    subject_sha = checkpoint_sha256(github_checkpoint)
    if subject_sha != _sha256(github_checkpoint_raw):
        raise ValueError("checkpoint_not_canonical")

    producer_result = payload.get("producer_result")
    if not isinstance(producer_result, dict) or producer_result.get("verified") is not True:
        raise ValueError("external_producer_result_invalid")
    if producer_result.get("checkpoint_subject_sha256") != subject_sha:
        raise ValueError("external_producer_subject_mismatch")
    if producer_result.get("producer_contract_sha256") != producer_contract_sha:
        raise ValueError("external_producer_contract_mismatch")
    if producer_result.get("authorization_contract_sha256") != authorization_contract_sha:
        raise ValueError("external_authorization_contract_mismatch")
    if producer_result.get("control_plane_bundle_sha256") != control_plane_bundle_sha:
        raise ValueError("external_control_plane_bundle_mismatch")
    producer_result_sha = sha256_hex(_canonical(producer_result))

    external_authority = payload.get("authority_evidence")
    if not isinstance(external_authority, dict) or external_authority.get("verified") is not True:
        raise ValueError("external_authority_evidence_invalid")
    if external_authority.get("producer_result_sha256") != producer_result_sha:
        raise ValueError("external_authority_producer_result_mismatch")
    if external_authority.get("control_plane_bundle_sha256") != control_plane_bundle_sha:
        raise ValueError("external_authority_control_plane_mismatch")
    if external_authority.get("migration_claim_sha256") != migration_sha:
        raise ValueError("external_authority_migration_claim_mismatch")

    primary_authority = PortableCheckpointAuthorityEvidence(
        verified=True,
        subject_sha256=subject_sha,
        logical_producer_id=migration_evidence.logical_producer_id,
        producer_contract_sha256=producer_contract_sha,
        authorization_contract_sha256=authorization_contract_sha,
        evidence_type=migration_evidence.evidence_type,
        generation=1,
    )
    secondary_authority = PortableCheckpointAuthorityEvidence(
        verified=True,
        subject_sha256=str(external_authority["subject_sha256"]),
        logical_producer_id=str(external_authority["logical_producer_id"]),
        producer_contract_sha256=str(external_authority["producer_contract_sha256"]),
        authorization_contract_sha256=str(
            external_authority["authorization_contract_sha256"]
        ),
        evidence_type=str(external_authority["evidence_type"]),
        generation=int(external_authority["generation"]),
    )

    primary_decision = evaluate_checkpoint_candidate_v3(
        witness_0,
        github_checkpoint,
        previous_checkpoint=baseline_checkpoint,
        authority_evidence=primary_authority,
    )
    secondary_decision = evaluate_checkpoint_candidate_v3(
        witness_0,
        external_checkpoint,
        previous_checkpoint=baseline_checkpoint,
        authority_evidence=secondary_authority,
    )
    if not primary_decision.authorized or primary_decision.next_witness is None:
        raise ValueError(f"primary_v3_transition_rejected:{primary_decision.reason}")
    if not secondary_decision.authorized or secondary_decision.next_witness is None:
        raise ValueError(f"secondary_v3_transition_rejected:{secondary_decision.reason}")
    if primary_decision.next_witness != secondary_decision.next_witness:
        raise ValueError("next_witness_semantics_mismatch")
    next_witness_sha = witness_v3_sha256(primary_decision.next_witness)
    if primary_decision.next_witness != payload.get("witness_generation_1"):
        raise ValueError("external_next_witness_mismatch")

    external_observation_payload = payload.get("source_control_observation")
    if not isinstance(external_observation_payload, dict):
        raise ValueError("external_source_control_observation_missing")
    secondary_observation = SourceControlObservation(
        producer_provider=str(external_observation_payload["producer_provider"]),
        producer_instance_id=str(external_observation_payload["producer_instance_id"]),
        control_plane_provider=str(external_observation_payload["control_plane_provider"]),
        control_plane_id=str(external_observation_payload["control_plane_id"]),
        subject_sha256=str(external_observation_payload["subject_sha256"]),
        logical_producer_id=str(external_observation_payload["logical_producer_id"]),
        producer_contract_sha256=str(
            external_observation_payload["producer_contract_sha256"]
        ),
        authorization_contract_sha256=str(
            external_observation_payload["authorization_contract_sha256"]
        ),
        evidence_type=str(external_observation_payload["evidence_type"]),
        generation=int(external_observation_payload["generation"]),
        witness_reason=str(external_observation_payload["witness_reason"]),
        next_witness_sha256=str(external_observation_payload["next_witness_sha256"]),
        verified=bool(external_observation_payload["verified"]),
    )
    if secondary_observation.next_witness_sha256 != next_witness_sha:
        raise ValueError("external_observation_next_witness_mismatch")

    primary_observation = SourceControlObservation(
        producer_provider="github-actions",
        producer_instance_id=PRODUCER_WORKFLOW_SHA,
        control_plane_provider="github-repository-policy",
        control_plane_id=f"safal207/Liminal@{args.verifier_sha}",
        subject_sha256=subject_sha,
        logical_producer_id=migration_evidence.logical_producer_id,
        producer_contract_sha256=producer_contract_sha,
        authorization_contract_sha256=authorization_contract_sha,
        evidence_type=migration_evidence.evidence_type,
        generation=1,
        witness_reason=primary_decision.reason,
        next_witness_sha256=next_witness_sha,
        verified=True,
    )
    agreement = compare_independent_source_control_paths(
        primary_observation,
        secondary_observation,
    )
    if not agreement.agreed:
        raise ValueError(f"source_control_portability_rejected:{agreement.reason.value}")
    if agreement.reason != SourceControlAgreementReason.VERIFIED_SOURCE_CONTROL_CLAIMS_AGREE:
        raise ValueError("source_control_agreement_reason_invalid")
    if agreement.portable_receipt_sha256 is None:
        raise ValueError("portable_source_control_receipt_missing")

    result = {
        "schema": SCHEMA,
        "verified": True,
        "reason": agreement.reason.value,
        "external_envelope_sha256": _sha256(envelope_path.read_bytes()),
        "external_payload_sha256": payload_sha,
        "control_plane_root_id": payload["control_plane_root_id"],
        "producer_root_id": payload["producer_root_id"],
        "producer_contract_sha256": producer_contract_sha,
        "authorization_contract_sha256": authorization_contract_sha,
        "migration_claim_sha256": migration_sha,
        "legacy_witness_sha256": legacy_witness_digest,
        "v3_witness_generation_0_sha256": witness_v3_sha256(witness_0),
        "checkpoint_subject_sha256": subject_sha,
        "primary_producer_workflow_sha": PRODUCER_WORKFLOW_SHA,
        "primary_verifier_workflow_sha": args.verifier_sha,
        "primary_producer_provider": primary_observation.producer_provider,
        "secondary_producer_provider": secondary_observation.producer_provider,
        "primary_control_plane_provider": primary_observation.control_plane_provider,
        "secondary_control_plane_provider": secondary_observation.control_plane_provider,
        "witness_reason": primary_decision.reason,
        "next_witness_sha256": next_witness_sha,
        "portable_source_control_receipt_sha256": agreement.portable_receipt_sha256,
        "upstream_rotation_provider_independence_claimed": False,
    }
    output = Path(args.output)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_bytes(_canonical(result))
    print(json.dumps(result, sort_keys=True, separators=(",", ":")))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
