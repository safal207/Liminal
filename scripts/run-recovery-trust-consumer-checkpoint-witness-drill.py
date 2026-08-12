#!/usr/bin/env python3
"""Exercise recovery of consumer anti-rollback state from an external witness."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
from typing import Any

from liminal.evidence_bundle import (
    canonical_evidence_bundle_bytes,
    evidence_bundle_sha256,
    parse_evidence_bundle_bytes,
)
from liminal.recovery_trust_consumer_checkpoint_attested import checkpoint_sha256
from liminal.recovery_trust_consumer_checkpoint_witness import (
    VerifiedCheckpointEvidence,
    evaluate_checkpoint_candidate,
    validate_witness,
    witness_sha256,
)
from liminal.recovery_trust_root_registry import canonical_json_bytes


DRILL_SCHEMA_VERSION = "liminal.recovery-trust-consumer-checkpoint-witness-drill.v0.1"


def _load(path: Path) -> dict[str, Any]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise ValueError(f"object_required:{path}")
    return payload


def _require_verification_json(path: Path) -> str:
    raw = path.read_bytes()
    payload = json.loads(raw)
    if not isinstance(payload, (dict, list)) or not payload:
        raise ValueError("checkpoint_attestation_verification_invalid")
    return hashlib.sha256(raw).hexdigest()


def _write(path: Path, payload: object) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(canonical_json_bytes(payload))


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--witness",
        default="policies/recovery-trust-consumer-checkpoint-witness-v0.1.json",
    )
    parser.add_argument(
        "--baseline-checkpoint",
        default="policies/recovery-trust-consumer-checkpoint-v0.2.json",
    )
    parser.add_argument("--candidate-checkpoint", required=True)
    parser.add_argument("--checkpoint-verification-json", required=True)
    parser.add_argument("--evidence-bundle-json", required=True)
    parser.add_argument("--output-dir", required=True)
    args = parser.parse_args()

    witness_0 = _load(Path(args.witness))
    checkpoint_0 = _load(Path(args.baseline_checkpoint))
    candidate_path = Path(args.candidate_checkpoint)
    checkpoint_1 = _load(candidate_path)
    verification_path = Path(args.checkpoint_verification_json)
    bundle_path = Path(args.evidence_bundle_json)
    output_dir = Path(args.output_dir)

    if not validate_witness(witness_0):
        raise ValueError("genesis_witness_invalid")
    if witness_0["checkpoint_sha256"] != checkpoint_sha256(checkpoint_0):
        raise ValueError("genesis_witness_checkpoint_mismatch")

    verification_sha256 = _require_verification_json(verification_path)
    raw_candidate_sha256 = hashlib.sha256(candidate_path.read_bytes()).hexdigest()
    canonical_candidate_sha256 = checkpoint_sha256(checkpoint_1)
    if raw_candidate_sha256 != canonical_candidate_sha256:
        raise ValueError("candidate_checkpoint_not_canonical")

    raw_bundle = bundle_path.read_bytes()
    bundle = parse_evidence_bundle_bytes(raw_bundle)
    canonical_bundle = canonical_evidence_bundle_bytes(bundle)
    if raw_bundle != canonical_bundle:
        raise ValueError("evidence_bundle_not_canonical")
    bundle_sha256 = evidence_bundle_sha256(bundle)
    if bundle.logical_id != "trust-consumer-checkpoint:generation-1":
        raise ValueError("evidence_bundle_logical_id_mismatch")
    if bundle.generation != 1:
        raise ValueError("evidence_bundle_generation_mismatch")
    if bundle.evidence.sha256 != canonical_candidate_sha256:
        raise ValueError("evidence_bundle_checkpoint_digest_mismatch")
    if bundle.evidence.verification_json_sha256 != verification_sha256:
        raise ValueError("evidence_bundle_checkpoint_verification_digest_mismatch")

    signer = witness_0["checkpoint_signer"]
    if bundle.evidence.signer_workflow != signer["workflow_path"]:
        raise ValueError("evidence_bundle_checkpoint_signer_workflow_mismatch")
    if bundle.evidence.signer_digest != signer["workflow_sha"]:
        raise ValueError("evidence_bundle_checkpoint_signer_digest_mismatch")

    evidence = VerifiedCheckpointEvidence(
        verified=True,
        signer_workflow_path=signer["workflow_path"],
        signer_workflow_sha=signer["workflow_sha"],
        subject_sha256=canonical_candidate_sha256,
    )
    advance = evaluate_checkpoint_candidate(
        witness_0,
        checkpoint_1,
        previous_checkpoint=checkpoint_0,
        checkpoint_evidence=evidence,
    )
    if not advance.authorized or advance.next_witness is None:
        raise ValueError(f"witness_advance_rejected:{advance.reason}")
    witness_1 = advance.next_witness
    if not validate_witness(witness_1, witness_0):
        raise ValueError("generation_1_witness_invalid")

    # Recovery boundary: after this point the consumer is modeled as having lost its local
    # generation-1 checkpoint. The recovered witness alone must still reject the old checkpoint.
    stale = evaluate_checkpoint_candidate(
        witness_1,
        checkpoint_0,
        previous_checkpoint=None,
        checkpoint_evidence=None,
    )
    if stale.authorized or stale.reason != "stale_checkpoint":
        raise ValueError(f"stale_checkpoint_not_rejected:{stale.reason}")

    current = evaluate_checkpoint_candidate(
        witness_1,
        checkpoint_1,
        previous_checkpoint=None,
        checkpoint_evidence=None,
    )
    if not current.authorized or current.reason != "checkpoint_already_witnessed":
        raise ValueError(f"current_checkpoint_not_recovered:{current.reason}")

    result = {
        "schema_version": DRILL_SCHEMA_VERSION,
        "verified": True,
        "reason": "checkpoint_witness_recovery_and_stale_rejection_verified",
        "external_provider_calls": 0,
        "permanent_checkpoint_mutated": False,
        "permanent_witness_mutated": False,
        "evidence_bundle": {
            "schema": bundle.schema,
            "logical_id": bundle.logical_id,
            "generation": bundle.generation,
            "bundle_sha256": bundle_sha256,
            "manifest_sha256": bundle.manifest.sha256,
            "evidence_sha256": bundle.evidence.sha256,
            "manifest_verification_json_sha256": (
                bundle.manifest.verification_json_sha256
            ),
            "evidence_verification_json_sha256": (
                bundle.evidence.verification_json_sha256
            ),
            "manifest_signer_workflow": bundle.manifest.signer_workflow,
            "manifest_signer_digest": bundle.manifest.signer_digest,
            "evidence_signer_workflow": bundle.evidence.signer_workflow,
            "evidence_signer_digest": bundle.evidence.signer_digest,
        },
        "checkpoint_attestation": {
            "cryptographically_verified": True,
            "subject_sha256": canonical_candidate_sha256,
            "verification_json_sha256": verification_sha256,
            "signer_workflow_path": signer["workflow_path"],
            "signer_workflow_sha": signer["workflow_sha"],
        },
        "baseline": {
            "generation": witness_0["observed_generation"],
            "witness_sha256": witness_sha256(witness_0),
            "checkpoint_sha256": witness_0["checkpoint_sha256"],
        },
        "advance": {
            "authorized": advance.authorized,
            "reason": advance.reason,
            "observed_generation": witness_1["observed_generation"],
            "witness_sha256": witness_sha256(witness_1),
            "previous_witness_sha256": witness_1["previous_witness_sha256"],
            "checkpoint_sha256": witness_1["checkpoint_sha256"],
        },
        "recovered_consumer": {
            "local_generation_1_checkpoint_available": False,
            "verified_witness_available": True,
            "current_checkpoint_authorized": current.authorized,
            "current_checkpoint_reason": current.reason,
        },
        "stale_checkpoint_replay": {
            "presented_generation": checkpoint_0["accepted_generation"],
            "presented_checkpoint_sha256": checkpoint_sha256(checkpoint_0),
            "structurally_historical": True,
            "authorized": stale.authorized,
            "reason": stale.reason,
        },
    }

    _write(output_dir / "witness-generation-0.json", witness_0)
    _write(output_dir / "witness-generation-1.json", witness_1)
    _write(output_dir / "checkpoint-witness-drill-result.json", result)
    print(json.dumps(result, sort_keys=True, separators=(",", ":")))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
