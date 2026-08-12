"""Receipt-backed Evidence Bundle v0.2.

v0.1 binds raw verifier JSON digests. v0.2 instead binds canonical normalized
verification receipts while keeping raw verifier outputs as separate audit artifacts.
This permits equivalent verifier representations to produce the same portable bundle
without weakening signer, subject, source-ref, or runner-policy bindings.
"""

from __future__ import annotations

import hashlib
import json
import re
from dataclasses import dataclass

from liminal.evidence_manifest import (
    EvidenceManifest,
    canonical_manifest_bytes,
    validate_manifest,
)
from liminal.verification_receipt import (
    NormalizedVerificationReceipt,
    build_normalized_verification_receipt,
    validate_verification_receipt,
    verification_receipt_sha256,
)

_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")
_GIT_SHA_RE = re.compile(r"^[0-9a-f]{40}$")
_SCHEMA = "liminal-evidence-bundle/v0.2"


@dataclass(frozen=True)
class ReceiptBoundSubject:
    """One verified subject and its normalized verification semantics."""

    sha256: str
    verification_receipt_sha256: str
    verification_scheme: str
    repository: str
    signer_workflow: str
    signer_digest: str
    source_ref: str
    deny_self_hosted_runners: bool


@dataclass(frozen=True)
class ReceiptBackedEvidenceBundle:
    """Canonical path- and raw-verifier-output-independent evidence receipt."""

    logical_id: str
    generation: int
    producer: str
    evidence_type: str
    manifest: ReceiptBoundSubject
    evidence: ReceiptBoundSubject
    schema: str = _SCHEMA


def _validate_sha256(value: str, *, field: str) -> None:
    if not _SHA256_RE.fullmatch(value):
        raise ValueError(f"{field}_must_be_lowercase_sha256")


def _validate_git_sha(value: str, *, field: str) -> None:
    if not _GIT_SHA_RE.fullmatch(value):
        raise ValueError(f"{field}_must_be_lowercase_git_sha")


def _receipt_from_subject(subject: ReceiptBoundSubject) -> NormalizedVerificationReceipt:
    return build_normalized_verification_receipt(
        verification_scheme=subject.verification_scheme,
        subject_sha256=subject.sha256,
        repository=subject.repository,
        signer_workflow=subject.signer_workflow,
        signer_digest=subject.signer_digest,
        source_ref=subject.source_ref,
        deny_self_hosted_runners=subject.deny_self_hosted_runners,
        verification_succeeded=True,
    )


def _validate_subject(subject: ReceiptBoundSubject, *, prefix: str) -> None:
    _validate_sha256(subject.sha256, field=f"{prefix}_sha256")
    _validate_sha256(
        subject.verification_receipt_sha256,
        field=f"{prefix}_verification_receipt_sha256",
    )
    if not subject.verification_scheme:
        raise ValueError(f"{prefix}_verification_scheme_must_be_non_empty")
    if not subject.repository:
        raise ValueError(f"{prefix}_repository_must_be_non_empty")
    if not subject.signer_workflow:
        raise ValueError(f"{prefix}_signer_workflow_must_be_non_empty")
    _validate_git_sha(subject.signer_digest, field=f"{prefix}_signer_digest")
    receipt = _receipt_from_subject(subject)
    if verification_receipt_sha256(receipt) != subject.verification_receipt_sha256:
        raise ValueError(f"{prefix}_verification_receipt_digest_mismatch")


def validate_receipt_backed_evidence_bundle(
    bundle: ReceiptBackedEvidenceBundle,
) -> None:
    if bundle.schema != _SCHEMA:
        raise ValueError("unsupported_receipt_backed_evidence_bundle_schema")
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


def _subject_payload(subject: ReceiptBoundSubject) -> dict[str, object]:
    return {
        "sha256": subject.sha256,
        "verification": {
            "receipt_sha256": subject.verification_receipt_sha256,
            "scheme": subject.verification_scheme,
            "repository": subject.repository,
            "signer_workflow": subject.signer_workflow,
            "signer_digest": subject.signer_digest,
            "source_ref": subject.source_ref,
            "policy": {
                "deny_self_hosted_runners": subject.deny_self_hosted_runners,
            },
        },
    }


def canonical_receipt_backed_evidence_bundle_bytes(
    bundle: ReceiptBackedEvidenceBundle,
) -> bytes:
    """Serialize one validated v0.2 bundle deterministically."""

    validate_receipt_backed_evidence_bundle(bundle)
    payload = {
        "schema": bundle.schema,
        "logical_id": bundle.logical_id,
        "generation": bundle.generation,
        "producer": bundle.producer,
        "evidence_type": bundle.evidence_type,
        "manifest": _subject_payload(bundle.manifest),
        "evidence": _subject_payload(bundle.evidence),
    }
    return (json.dumps(payload, sort_keys=True, separators=(",", ":")) + "\n").encode()


def receipt_backed_evidence_bundle_sha256(bundle: ReceiptBackedEvidenceBundle) -> str:
    return hashlib.sha256(canonical_receipt_backed_evidence_bundle_bytes(bundle)).hexdigest()


def _subject_from_receipt(receipt: NormalizedVerificationReceipt) -> ReceiptBoundSubject:
    validate_verification_receipt(receipt)
    if not receipt.verified:
        raise ValueError("verified_receipt_required_for_evidence_bundle")
    return ReceiptBoundSubject(
        sha256=receipt.subject_sha256,
        verification_receipt_sha256=verification_receipt_sha256(receipt),
        verification_scheme=receipt.verification_scheme,
        repository=receipt.repository,
        signer_workflow=receipt.signer_workflow,
        signer_digest=receipt.signer_digest,
        source_ref=receipt.source_ref,
        deny_self_hosted_runners=receipt.deny_self_hosted_runners,
    )


def build_receipt_backed_evidence_bundle(
    manifest_document: EvidenceManifest,
    *,
    logical_id: str,
    generation: int,
    manifest_receipt: NormalizedVerificationReceipt,
    evidence_receipt: NormalizedVerificationReceipt,
) -> ReceiptBackedEvidenceBundle:
    """Build v0.2 only from externally verified normalized receipts."""

    validate_manifest(manifest_document)
    validate_verification_receipt(manifest_receipt)
    validate_verification_receipt(evidence_receipt)
    if not manifest_receipt.verified:
        raise ValueError("manifest_verified_receipt_required_for_evidence_bundle")
    if not evidence_receipt.verified:
        raise ValueError("evidence_verified_receipt_required_for_evidence_bundle")

    expected_manifest_sha = hashlib.sha256(
        canonical_manifest_bytes(manifest_document)
    ).hexdigest()
    if manifest_receipt.subject_sha256 != expected_manifest_sha:
        raise ValueError("evidence_bundle_manifest_receipt_subject_mismatch")

    matches = tuple(
        entry
        for entry in manifest_document.entries
        if entry.logical_id == logical_id and entry.generation == generation
    )
    if len(matches) != 1:
        raise ValueError("evidence_bundle_requires_unique_manifest_entry")
    entry = matches[0]

    if evidence_receipt.subject_sha256 != entry.sha256:
        raise ValueError("evidence_bundle_manifest_digest_mismatch")
    if entry.verification.verifier != evidence_receipt.verification_scheme:
        raise ValueError("evidence_bundle_verification_scheme_mismatch")
    if entry.verification.expected_signer != evidence_receipt.signer_workflow:
        raise ValueError("evidence_bundle_evidence_signer_workflow_mismatch")
    if entry.verification.expected_signer_digest != evidence_receipt.signer_digest:
        raise ValueError("evidence_bundle_evidence_signer_digest_mismatch")

    bundle = ReceiptBackedEvidenceBundle(
        logical_id=logical_id,
        generation=generation,
        producer=entry.producer,
        evidence_type=entry.evidence_type,
        manifest=_subject_from_receipt(manifest_receipt),
        evidence=_subject_from_receipt(evidence_receipt),
    )
    validate_receipt_backed_evidence_bundle(bundle)
    return bundle


def _require_mapping(value: object, *, field: str) -> dict[str, object]:
    if not isinstance(value, dict):
        raise ValueError(f"{field}_must_be_object")
    return value


def _require_string(value: object, *, field: str) -> str:
    if not isinstance(value, str):
        raise ValueError(f"{field}_must_be_string")
    return value


def _require_bool(value: object, *, field: str) -> bool:
    if not isinstance(value, bool):
        raise ValueError(f"{field}_must_be_boolean")
    return value


def _parse_subject(value: object, *, prefix: str) -> ReceiptBoundSubject:
    item = _require_mapping(value, field=prefix)
    verification = _require_mapping(
        item.get("verification"), field=f"{prefix}_verification"
    )
    policy = _require_mapping(
        verification.get("policy"), field=f"{prefix}_verification_policy"
    )
    return ReceiptBoundSubject(
        sha256=_require_string(item.get("sha256"), field=f"{prefix}_sha256"),
        verification_receipt_sha256=_require_string(
            verification.get("receipt_sha256"),
            field=f"{prefix}_verification_receipt_sha256",
        ),
        verification_scheme=_require_string(
            verification.get("scheme"), field=f"{prefix}_verification_scheme"
        ),
        repository=_require_string(
            verification.get("repository"), field=f"{prefix}_repository"
        ),
        signer_workflow=_require_string(
            verification.get("signer_workflow"), field=f"{prefix}_signer_workflow"
        ),
        signer_digest=_require_string(
            verification.get("signer_digest"), field=f"{prefix}_signer_digest"
        ),
        source_ref=_require_string(
            verification.get("source_ref"), field=f"{prefix}_source_ref"
        ),
        deny_self_hosted_runners=_require_bool(
            policy.get("deny_self_hosted_runners"),
            field=f"{prefix}_deny_self_hosted_runners",
        ),
    )


def parse_receipt_backed_evidence_bundle_bytes(data: bytes) -> ReceiptBackedEvidenceBundle:
    """Strictly parse one canonical-compatible v0.2 Evidence Bundle."""

    try:
        payload = json.loads(data.decode("utf-8"))
    except (UnicodeDecodeError, json.JSONDecodeError) as exc:
        raise ValueError("invalid_receipt_backed_evidence_bundle_json") from exc
    document = _require_mapping(payload, field="evidence_bundle")
    generation = document.get("generation")
    if not isinstance(generation, int) or isinstance(generation, bool):
        raise ValueError("evidence_bundle_generation_must_be_integer")
    bundle = ReceiptBackedEvidenceBundle(
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
    validate_receipt_backed_evidence_bundle(bundle)
    return bundle
