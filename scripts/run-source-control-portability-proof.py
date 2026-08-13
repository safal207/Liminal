#!/usr/bin/env python3
"""Compare GitHub and independent source/control paths through witness authority v0.3."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
from typing import Any

from liminal.recovery_trust_consumer_checkpoint_attested import checkpoint_sha256
from liminal.recovery_trust_consumer_checkpoint_witness import witness_sha256
from liminal.recovery_trust_consumer_checkpoint_witness_v3 import (
    PortableCheckpointAuthority,
    VerifiedAuthorityMigrationEvidence,
    VerifiedPortableCheckpointEvidence,
    evaluate_portable_checkpoint_candidate,
    migrate_witness_v2_to_v3,
    witness_v3_sha256,
)
from liminal.recovery_trust_root_registry import canonical_json_bytes
from liminal.source_control_external_proof import verify_external_source_control_bundle
from liminal.source_control_portability import (
    SourceControlObservation,
    compare_independent_source_control_paths,
)

GITHUB_PRODUCER_SHA = "f31b56a5e21a668bcb98791b05542652760dcc27"
PREVIOUS_PRODUCER_SHA = "d0688725bd76fdf7221e84ca7c5bfb51e363ff72"
PREVIOUS_WITNESS_WORKFLOW_SHA = "3f0af42a680f42923cb18591ba127206b2292599"
CHECKPOINT_SIGNER_PATH = (
    ".github/workflows/trusted-recovery-trust-consumer-checkpoint-attested.yml"
)
TRUST_DOMAIN = "liminal.trusted-recovery"


def _load(path: Path) -> dict[str, Any]:
    payload = json.loads(path.read_text())
    if not isinstance(payload, dict):
        raise ValueError(f"object_required:{path}")
    return payload


def _require_verification_json(path: Path) -> str:
    raw = path.read_bytes()
    payload = json.loads(raw)
    if not isinstance(payload, (dict, list)) or not payload:
        raise ValueError("github_attestation_verification_invalid")
    return hashlib.sha256(raw).hexdigest()


def _witness_v2(checkpoint_0: dict[str, Any]) -> dict[str, Any]:
    return {
        "schema_version": "liminal.recovery-trust-consumer-checkpoint-witness.v0.2",
        "repository": "safal207/Liminal",
        "observed_generation": 0,
        "checkpoint_sha256": checkpoint_sha256(checkpoint_0),
        "accepted_registry_sha256": checkpoint_0["accepted_registry_sha256"],
        "accepted_manifest_sha256": checkpoint_0["accepted_manifest_sha256"],
        "previous_witness_sha256": None,
        "checkpoint_signer": {
            "workflow_path": CHECKPOINT_SIGNER_PATH,
            "workflow_sha": GITHUB_PRODUCER_SHA,
        },
        "checkpoint_signer_transition": {
            "previous_signer": {
                "workflow_path": CHECKPOINT_SIGNER_PATH,
                "workflow_sha": PREVIOUS_PRODUCER_SHA,
            },
            "reason": "manifest_backed_checkpoint_producer_rotation",
            "previous_witness_workflow_sha": PREVIOUS_WITNESS_WORKFLOW_SHA,
        },
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--checkpoint-generation-0", required=True)
    parser.add_argument("--github-checkpoint", required=True)
    parser.add_argument("--github-verification-json", required=True)
    parser.add_argument("--external-bundle", required=True)
    parser.add_argument("--output", required=True)
    args = parser.parse_args()

    checkpoint_0 = _load(Path(args.checkpoint_generation_0))
    github_checkpoint = _load(Path(args.github_checkpoint))
    github_verification_sha = _require_verification_json(Path(args.github_verification_json))
    external_bundle = _load(Path(args.external_bundle))
    external_proof = verify_external_source_control_bundle(external_bundle)
    if not external_proof.verified:
        raise ValueError("external_source_control_proof_unverified")

    external_checkpoint = external_bundle["checkpoint_generation_1"]
    if not isinstance(external_checkpoint, dict):
        raise ValueError("external_checkpoint_invalid")
    github_subject = checkpoint_sha256(github_checkpoint)
    external_subject = checkpoint_sha256(external_checkpoint)
    if github_subject != external_subject:
        raise ValueError("producer_subject_mismatch")
    if github_subject != external_proof.subject_sha256:
        raise ValueError("external_proof_subject_mismatch")
    if canonical_json_bytes(github_checkpoint) != canonical_json_bytes(external_checkpoint):
        raise ValueError("producer_checkpoint_bytes_mismatch")

    witness_2 = _witness_v2(checkpoint_0)
    predecessor_witness_sha = witness_sha256(witness_2)
    if predecessor_witness_sha != external_proof.predecessor_witness_sha256:
        raise ValueError("migration_predecessor_witness_mismatch")

    authority = PortableCheckpointAuthority(
        logical_producer_id=external_proof.logical_producer_id,
        producer_contract_sha256=external_proof.producer_contract_sha256,
        authorization_contract_sha256=external_proof.authorization_contract_sha256,
        evidence_type=external_proof.evidence_type,
    )
    migration = migrate_witness_v2_to_v3(
        witness_2,
        None,
        trust_domain=TRUST_DOMAIN,
        migration_evidence=VerifiedAuthorityMigrationEvidence(
            verified=True,
            previous_witness_sha256=predecessor_witness_sha,
            authority=authority,
        ),
    )
    if not migration.authorized or migration.next_witness is None:
        raise ValueError(f"witness_authority_migration_failed:{migration.reason}")
    witness_3 = migration.next_witness

    primary_evidence = VerifiedPortableCheckpointEvidence(
        verified=True,
        subject_sha256=github_subject,
        logical_producer_id=authority.logical_producer_id,
        producer_contract_sha256=authority.producer_contract_sha256,
        authorization_contract_sha256=authority.authorization_contract_sha256,
        evidence_type=authority.evidence_type,
    )
    secondary_evidence = VerifiedPortableCheckpointEvidence(
        verified=True,
        subject_sha256=external_subject,
        logical_producer_id=authority.logical_producer_id,
        producer_contract_sha256=authority.producer_contract_sha256,
        authorization_contract_sha256=authority.authorization_contract_sha256,
        evidence_type=authority.evidence_type,
    )
    primary = evaluate_portable_checkpoint_candidate(
        witness_3,
        github_checkpoint,
        previous_checkpoint=checkpoint_0,
        checkpoint_evidence=primary_evidence,
    )
    secondary = evaluate_portable_checkpoint_candidate(
        witness_3,
        external_checkpoint,
        previous_checkpoint=checkpoint_0,
        checkpoint_evidence=secondary_evidence,
    )
    if not primary.authorized or primary.next_witness is None:
        raise ValueError(f"github_v3_transition_failed:{primary.reason}")
    if not secondary.authorized or secondary.next_witness is None:
        raise ValueError(f"external_v3_transition_failed:{secondary.reason}")

    primary_next_sha = witness_v3_sha256(primary.next_witness)
    secondary_next_sha = witness_v3_sha256(secondary.next_witness)
    if primary_next_sha != secondary_next_sha:
        raise ValueError("v3_next_witness_mismatch")

    primary_observation = SourceControlObservation(
        producer_provider="github-actions-checkpoint-producer",
        producer_instance_id=GITHUB_PRODUCER_SHA,
        control_plane_provider="github-repository-policy",
        control_plane_id="safal207/Liminal:recovery-policy",
        subject_sha256=github_subject,
        logical_producer_id=authority.logical_producer_id,
        producer_contract_sha256=authority.producer_contract_sha256,
        authorization_contract_sha256=authority.authorization_contract_sha256,
        evidence_type=authority.evidence_type,
        generation=github_checkpoint["accepted_generation"],
        witness_reason=primary.reason,
        next_witness_sha256=primary_next_sha,
        verified=True,
    )
    producer_claim = external_bundle["producer_claim"]
    if not isinstance(producer_claim, dict):
        raise ValueError("external_producer_claim_invalid")
    secondary_observation = SourceControlObservation(
        producer_provider=str(producer_claim["producer_provider"]),
        producer_instance_id=str(producer_claim["producer_instance_id"]),
        control_plane_provider="offline-ed25519-control-plane",
        control_plane_id=external_proof.control_plane_root_id,
        subject_sha256=external_subject,
        logical_producer_id=authority.logical_producer_id,
        producer_contract_sha256=authority.producer_contract_sha256,
        authorization_contract_sha256=authority.authorization_contract_sha256,
        evidence_type=authority.evidence_type,
        generation=external_checkpoint["accepted_generation"],
        witness_reason=secondary.reason,
        next_witness_sha256=secondary_next_sha,
        verified=True,
    )
    agreement = compare_independent_source_control_paths(
        primary_observation, secondary_observation
    )
    if not agreement.agreed or agreement.portable_receipt_sha256 is None:
        raise ValueError(f"source_control_portability_failed:{agreement.reason}")

    result = {
        "schema": "liminal-source-control-portability-proof/v0.1",
        "verified": True,
        "reason": agreement.reason,
        "github_attestation_verification_sha256": github_verification_sha,
        "external_producer_root_id": external_proof.producer_root_id,
        "external_control_plane_root_id": external_proof.control_plane_root_id,
        "checkpoint_subject_sha256": github_subject,
        "logical_producer_id": authority.logical_producer_id,
        "producer_contract_sha256": authority.producer_contract_sha256,
        "authorization_contract_sha256": authority.authorization_contract_sha256,
        "predecessor_witness_v2_sha256": predecessor_witness_sha,
        "migrated_witness_v3_sha256": witness_v3_sha256(witness_3),
        "next_witness_v3_sha256": primary_next_sha,
        "portable_source_control_receipt_sha256": agreement.portable_receipt_sha256,
        "primary": {
            "producer_provider": primary_observation.producer_provider,
            "control_plane_provider": primary_observation.control_plane_provider,
            "witness_reason": primary.reason,
        },
        "secondary": {
            "producer_provider": secondary_observation.producer_provider,
            "control_plane_provider": secondary_observation.control_plane_provider,
            "witness_reason": secondary.reason,
        },
    }
    output = Path(args.output)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_bytes(canonical_json_bytes(result))
    print(json.dumps(result, sort_keys=True, separators=(",", ":")))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
