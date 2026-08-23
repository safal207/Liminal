"""Cryptographic evidence verification for independent causal-evolution Path B."""

from __future__ import annotations

import base64
import hashlib
import json
import subprocess
import tempfile
from pathlib import Path
from typing import Any

from liminal.recovery_trust_root_registry import canonical_json_bytes

ENVELOPE_SCHEMA = "liminal-portable-causal-evolution-path-b-envelope/v0.1"
CLAIM_SCHEMA = "liminal-portable-causal-evolution-path-b-claim/v0.1"


def load_json(path: Path) -> dict[str, Any]:
    value = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(value, dict):
        raise ValueError(f"object_required:{path}")
    return value


def sha256_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def verify_evolution_envelope(
    public_key: Path,
    envelope: dict[str, Any],
) -> tuple[str, dict[str, Any]]:
    if envelope.get("schema") != ENVELOPE_SCHEMA:
        raise ValueError("path_b_envelope_schema_invalid")
    claim = envelope.get("claim")
    signature = envelope.get("signature")
    if not isinstance(claim, dict) or not isinstance(signature, dict):
        raise ValueError("path_b_envelope_sections_missing")
    if claim.get("schema") != CLAIM_SCHEMA:
        raise ValueError("path_b_claim_schema_invalid")
    if signature.get("algorithm") != "ed25519":
        raise ValueError("path_b_signature_algorithm_invalid")

    payload = canonical_json_bytes(claim)
    if signature.get("payload_sha256") != sha256_bytes(payload):
        raise ValueError("path_b_payload_digest_mismatch")

    der = subprocess.check_output(
        ["openssl", "pkey", "-pubin", "-in", str(public_key), "-outform", "DER"]
    )
    prefix = bytes.fromhex("302a300506032b6570032100")
    if len(der) != len(prefix) + 32 or not der.startswith(prefix):
        raise ValueError("path_b_public_key_encoding_invalid")
    fingerprint = sha256_bytes(der[-32:])
    if signature.get("public_key_sha256") != fingerprint:
        raise ValueError("path_b_public_key_fingerprint_mismatch")

    try:
        raw_signature = base64.b64decode(str(signature["value_base64"]), validate=True)
    except (KeyError, ValueError) as exc:
        raise ValueError("path_b_signature_encoding_invalid") from exc

    with tempfile.TemporaryDirectory() as temp_dir:
        temp = Path(temp_dir)
        payload_path = temp / "payload.bin"
        signature_path = temp / "signature.bin"
        payload_path.write_bytes(payload)
        signature_path.write_bytes(raw_signature)
        completed = subprocess.run(
            [
                "openssl",
                "pkeyutl",
                "-verify",
                "-pubin",
                "-inkey",
                str(public_key),
                "-rawin",
                "-in",
                str(payload_path),
                "-sigfile",
                str(signature_path),
            ],
            check=False,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
    if completed.returncode != 0:
        raise ValueError("path_b_signature_invalid")

    authority = f"ed25519-sha256:{fingerprint}"
    if claim.get("evolution_signer_authority_id") != authority:
        raise ValueError("path_b_evolution_signer_mismatch")
    return authority, claim


def verify_policy_source_material(
    repository_root: Path,
    claim: dict[str, Any],
) -> None:
    source_sha = str(claim.get("bootstrap_source_sha", ""))
    if len(source_sha) != 40:
        raise ValueError("path_b_bootstrap_source_invalid")
    expected = {
        "policies/portable-causal-evolution-state-step-1-v0.1.json": str(
            claim.get("policy_step_1_sha256", "")
        ),
        "policies/portable-causal-evolution-state-step-2-v0.1.json": str(
            claim.get("policy_step_2_sha256", "")
        ),
        "policies/portable-causal-evolution-transition-contract-v0.1.json": str(
            claim.get("transition_contract_sha256", "")
        ),
        "policies/portable-causal-evolution-transition-authorization-contract-v0.1.json": str(
            claim.get("transition_authorization_contract_sha256", "")
        ),
    }
    for path, expected_digest in expected.items():
        actual = subprocess.check_output(
            ["git", "show", f"{source_sha}:{path}"],
            cwd=repository_root,
        )
        if sha256_bytes(actual) != expected_digest:
            raise ValueError(f"path_b_policy_source_mismatch:{path}")
