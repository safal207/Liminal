"""Canonical verification semantics for portable evidence receipts.

A normalized verification receipt records the security-relevant contract that was
successfully (or unsuccessfully) checked. It deliberately excludes raw verifier
output, CLI versions, timestamps, physical paths, and other implementation-local
bytes from the canonical identity.

Raw verifier output should be retained separately as audit evidence. The normalized
receipt is not a verifier and never turns unverified bytes into trusted evidence.
"""

from __future__ import annotations

import hashlib
import json
import re
from dataclasses import dataclass

_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")
_GIT_SHA_RE = re.compile(r"^[0-9a-f]{40}$")
_REPOSITORY_RE = re.compile(r"^[^/\s]+/[^/\s]+$")
_SCHEMA = "liminal-verification-receipt/v0.1"


@dataclass(frozen=True)
class NormalizedVerificationReceipt:
    """Path- and verifier-output-independent verification semantics."""

    verification_scheme: str
    subject_sha256: str
    repository: str
    signer_workflow: str
    signer_digest: str
    source_ref: str
    deny_self_hosted_runners: bool
    verified: bool
    schema: str = _SCHEMA


def _require_non_empty(value: str, *, field: str) -> None:
    if not value:
        raise ValueError(f"{field}_must_be_non_empty")


def _validate_sha256(value: str, *, field: str) -> None:
    if not _SHA256_RE.fullmatch(value):
        raise ValueError(f"{field}_must_be_lowercase_sha256")


def _validate_git_sha(value: str, *, field: str) -> None:
    if not _GIT_SHA_RE.fullmatch(value):
        raise ValueError(f"{field}_must_be_lowercase_git_sha")


def validate_verification_receipt(receipt: NormalizedVerificationReceipt) -> None:
    if receipt.schema != _SCHEMA:
        raise ValueError("unsupported_verification_receipt_schema")
    _require_non_empty(receipt.verification_scheme, field="verification_scheme")
    _validate_sha256(receipt.subject_sha256, field="subject_sha256")
    if not _REPOSITORY_RE.fullmatch(receipt.repository):
        raise ValueError("repository_must_be_owner_slash_name")
    _require_non_empty(receipt.signer_workflow, field="signer_workflow")
    _validate_git_sha(receipt.signer_digest, field="signer_digest")
    if not receipt.source_ref.startswith("refs/"):
        raise ValueError("source_ref_must_be_fully_qualified_ref")
    if not isinstance(receipt.deny_self_hosted_runners, bool):
        raise ValueError("deny_self_hosted_runners_must_be_boolean")
    if not isinstance(receipt.verified, bool):
        raise ValueError("verified_must_be_boolean")


def canonical_verification_receipt_bytes(
    receipt: NormalizedVerificationReceipt,
) -> bytes:
    """Serialize security-relevant verification semantics deterministically."""

    validate_verification_receipt(receipt)
    payload = {
        "schema": receipt.schema,
        "verification_scheme": receipt.verification_scheme,
        "subject_sha256": receipt.subject_sha256,
        "repository": receipt.repository,
        "signer_workflow": receipt.signer_workflow,
        "signer_digest": receipt.signer_digest,
        "source_ref": receipt.source_ref,
        "policy": {
            "deny_self_hosted_runners": receipt.deny_self_hosted_runners,
        },
        "verified": receipt.verified,
    }
    return (json.dumps(payload, sort_keys=True, separators=(",", ":")) + "\n").encode()


def verification_receipt_sha256(receipt: NormalizedVerificationReceipt) -> str:
    return hashlib.sha256(canonical_verification_receipt_bytes(receipt)).hexdigest()


def build_normalized_verification_receipt(
    *,
    verification_scheme: str,
    subject_sha256: str,
    repository: str,
    signer_workflow: str,
    signer_digest: str,
    source_ref: str,
    deny_self_hosted_runners: bool,
    verification_succeeded: bool,
) -> NormalizedVerificationReceipt:
    """Build a receipt from the exact verification contract and its outcome.

    The caller must obtain ``verification_succeeded`` from an external verifier.
    This function performs no signature, provenance, transparency-log, or policy
    verification itself.
    """

    receipt = NormalizedVerificationReceipt(
        verification_scheme=verification_scheme,
        subject_sha256=subject_sha256,
        repository=repository,
        signer_workflow=signer_workflow,
        signer_digest=signer_digest,
        source_ref=source_ref,
        deny_self_hosted_runners=deny_self_hosted_runners,
        verified=verification_succeeded,
    )
    validate_verification_receipt(receipt)
    return receipt


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


def parse_verification_receipt_bytes(data: bytes) -> NormalizedVerificationReceipt:
    """Strictly parse one normalized verification receipt."""

    try:
        payload = json.loads(data.decode("utf-8"))
    except (UnicodeDecodeError, json.JSONDecodeError) as exc:
        raise ValueError("invalid_verification_receipt_json") from exc

    document = _require_mapping(payload, field="verification_receipt")
    policy = _require_mapping(document.get("policy"), field="verification_policy")
    receipt = NormalizedVerificationReceipt(
        schema=_require_string(document.get("schema"), field="verification_receipt_schema"),
        verification_scheme=_require_string(
            document.get("verification_scheme"), field="verification_scheme"
        ),
        subject_sha256=_require_string(
            document.get("subject_sha256"), field="subject_sha256"
        ),
        repository=_require_string(document.get("repository"), field="repository"),
        signer_workflow=_require_string(
            document.get("signer_workflow"), field="signer_workflow"
        ),
        signer_digest=_require_string(
            document.get("signer_digest"), field="signer_digest"
        ),
        source_ref=_require_string(document.get("source_ref"), field="source_ref"),
        deny_self_hosted_runners=_require_bool(
            policy.get("deny_self_hosted_runners"),
            field="deny_self_hosted_runners",
        ),
        verified=_require_bool(document.get("verified"), field="verified"),
    )
    validate_verification_receipt(receipt)
    return receipt
