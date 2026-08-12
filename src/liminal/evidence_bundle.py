"""Verified evidence bundle for chaining manifest, evidence, and recovery proof.

The bundle is created only after manifest and evidence verification have succeeded.
It deliberately excludes physical artifact paths from trust identity.  Downstream
recovery evidence can bind to the canonical bundle SHA-256 and thereby expose an
inspectable chain from verified manifest to verified evidence without making
artifact packaging authoritative.
"""

from __future__ import annotations

import hashlib
import json
import re
from dataclasses import dataclass

from liminal.evidence_manifest import EvidenceManifest, validate_manifest

_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")
_SCHEMA = "liminal-evidence-bundle/v0.1"


@dataclass(frozen=True)
class VerifiedBundleSubject:
    """One externally verified subject bound into the bundle."""

    sha256: str
    verification_json_sha256: str
    signer_workflow: str
    signer_digest: str


@dataclass(frozen=True)
class EvidenceBundle:
    """Canonical path-independent binding for verified manifest + evidence."""

    logical_id: str
    generation: int
    producer: str
    evidence_type: str
    manifest: VerifiedBundleSubject
    evidence: VerifiedBundleSubject
    schema: str = _SCHEMA


def _validate_sha256(value: str, *, field: str) -> None:
    if not _SHA256_RE.fullmatch(value):
        raise ValueError(f"{field}_must_be_lowercase_sha256")


def _validate_subject(subject: VerifiedBundleSubject, *, prefix: str) -> None:
    _validate_sha256(subject.sha256, field=f"{prefix}_sha256")
    _validate_sha256(
        subject.verification_json_sha256,
        field=f"{prefix}_verification_json_sha256",
    )
    if not subject.signer_workflow:
        raise ValueError(f"{prefix}_signer_workflow_must_be_non_empty")
    _validate_sha256(subject.signer_digest, field=f"{prefix}_signer_digest")


def validate_evidence_bundle(bundle: EvidenceBundle) -> None:
    if bundle.schema != _SCHEMA:
        raise ValueError("unsupported_evidence_bundle_schema")
    if not bundle.logical_id:
        raise ValueError("evidence_bundle_logical_id_must_be_non_empty")
    if bundle.generation < 0:
        raise ValueError("evidence_bundle_generation_must_be_non_negative")
    if not bundle.producer:
        raise ValueError("evidence_bundle_producer_must_be_non_empty")
    if not bundle.evidence_type:
        raise ValueError("evidence_bundle_evidence_type_must_be_non_empty")
    _validate_subject(bundle.manifest, prefix="manifest")
    _validate_subject(bundle.evidence, prefix="evidence")


def canonical_evidence_bundle_bytes(bundle: EvidenceBundle) -> bytes:
    """Serialize a validated evidence bundle deterministically."""

    validate_evidence_bundle(bundle)
    payload = {
        "schema": bundle.schema,
        "logical_id": bundle.logical_id,
        "generation": bundle.generation,
        "producer": bundle.producer,
        "evidence_type": bundle.evidence_type,
        "manifest": {
            "sha256": bundle.manifest.sha256,
            "verification_json_sha256": bundle.manifest.verification_json_sha256,
            "signer_workflow": bundle.manifest.signer_workflow,
            "signer_digest": bundle.manifest.signer_digest,
        },
        "evidence": {
            "sha256": bundle.evidence.sha256,
            "verification_json_sha256": bundle.evidence.verification_json_sha256,
            "signer_workflow": bundle.evidence.signer_workflow,
            "signer_digest": bundle.evidence.signer_digest,
        },
    }
    return (json.dumps(payload, sort_keys=True, separators=(",", ":")) + "\n").encode()


def evidence_bundle_sha256(bundle: EvidenceBundle) -> str:
    return hashlib.sha256(canonical_evidence_bundle_bytes(bundle)).hexdigest()


def _require_mapping(value: object, *, field: str) -> dict[str, object]:
    if not isinstance(value, dict):
        raise ValueError(f"{field}_must_be_object")
    return value


def _require_string(value: object, *, field: str) -> str:
    if not isinstance(value, str):
        raise ValueError(f"{field}_must_be_string")
    return value


def _parse_subject(value: object, *, prefix: str) -> VerifiedBundleSubject:
    item = _require_mapping(value, field=prefix)
    return VerifiedBundleSubject(
        sha256=_require_string(item.get("sha256"), field=f"{prefix}_sha256"),
        verification_json_sha256=_require_string(
            item.get("verification_json_sha256"),
            field=f"{prefix}_verification_json_sha256",
        ),
        signer_workflow=_require_string(
            item.get("signer_workflow"), field=f"{prefix}_signer_workflow"
        ),
        signer_digest=_require_string(
            item.get("signer_digest"), field=f"{prefix}_signer_digest"
        ),
    )


def parse_evidence_bundle_bytes(data: bytes) -> EvidenceBundle:
    """Strictly parse one canonical-compatible v0.1 evidence bundle."""

    try:
        payload = json.loads(data.decode("utf-8"))
    except (UnicodeDecodeError, json.JSONDecodeError) as exc:
        raise ValueError("invalid_evidence_bundle_json") from exc
    document = _require_mapping(payload, field="evidence_bundle")
    generation = document.get("generation")
    if not isinstance(generation, int) or isinstance(generation, bool):
        raise ValueError("evidence_bundle_generation_must_be_integer")
    bundle = EvidenceBundle(
        schema=_require_string(document.get("schema"), field="evidence_bundle_schema"),
        logical_id=_require_string(
            document.get("logical_id"), field="evidence_bundle_logical_id"
        ),
        generation=generation,
        producer=_require_string(
            document.get("producer"), field="evidence_bundle_producer"
        ),
        evidence_type=_require_string(
            document.get("evidence_type"), field="evidence_bundle_evidence_type"
        ),
        manifest=_parse_subject(document.get("manifest"), prefix="manifest"),
        evidence=_parse_subject(document.get("evidence"), prefix="evidence"),
    )
    validate_evidence_bundle(bundle)
    return bundle


def build_verified_evidence_bundle(
    manifest_document: EvidenceManifest,
    *,
    logical_id: str,
    generation: int,
    manifest_sha256: str,
    manifest_verification_json_sha256: str,
    manifest_signer_workflow: str,
    manifest_signer_digest: str,
    evidence_sha256: str,
    evidence_verification_json_sha256: str,
    evidence_signer_workflow: str,
    evidence_signer_digest: str,
    manifest_verification_succeeded: bool,
    evidence_verification_succeeded: bool,
) -> EvidenceBundle:
    """Build a bundle only after both external verification steps succeeded."""

    if not manifest_verification_succeeded:
        raise ValueError("manifest_verification_required_for_evidence_bundle")
    if not evidence_verification_succeeded:
        raise ValueError("evidence_verification_required_for_evidence_bundle")
    validate_manifest(manifest_document)

    matches = tuple(
        entry
        for entry in manifest_document.entries
        if entry.logical_id == logical_id and entry.generation == generation
    )
    if len(matches) != 1:
        raise ValueError("evidence_bundle_requires_unique_manifest_entry")
    entry = matches[0]
    if entry.sha256 != evidence_sha256:
        raise ValueError("evidence_bundle_manifest_digest_mismatch")
    if entry.verification.expected_signer != evidence_signer_workflow:
        raise ValueError("evidence_bundle_evidence_signer_workflow_mismatch")
    if entry.verification.expected_signer_digest != evidence_signer_digest:
        raise ValueError("evidence_bundle_evidence_signer_digest_mismatch")

    bundle = EvidenceBundle(
        logical_id=logical_id,
        generation=generation,
        producer=entry.producer,
        evidence_type=entry.evidence_type,
        manifest=VerifiedBundleSubject(
            sha256=manifest_sha256,
            verification_json_sha256=manifest_verification_json_sha256,
            signer_workflow=manifest_signer_workflow,
            signer_digest=manifest_signer_digest,
        ),
        evidence=VerifiedBundleSubject(
            sha256=evidence_sha256,
            verification_json_sha256=evidence_verification_json_sha256,
            signer_workflow=evidence_signer_workflow,
            signer_digest=evidence_signer_digest,
        ),
    )
    validate_evidence_bundle(bundle)
    return bundle
