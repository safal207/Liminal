"""Provider-specific evidence verification for causal fork reconciliation."""

from __future__ import annotations

import base64
import hashlib
import json
import subprocess
import tempfile
from pathlib import Path
from typing import Any

from liminal.recovery_trust_root_registry import canonical_json_bytes

ENVELOPE_SCHEMA = "liminal-causal-fork-branch-b-signed-envelope/v0.1"


def load_json(path: Path) -> dict[str, Any]:
    value = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(value, dict):
        raise ValueError(f"object_required:{path}")
    return value


def sha256_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def verify_ed25519_envelope(
    public_key: Path,
    envelope: dict[str, Any],
) -> tuple[str, dict[str, Any]]:
    if envelope.get("schema") != ENVELOPE_SCHEMA:
        raise ValueError("fork_branch_b_envelope_schema_invalid")
    claim = envelope.get("claim")
    signature = envelope.get("signature")
    if not isinstance(claim, dict) or not isinstance(signature, dict):
        raise ValueError("fork_branch_b_envelope_sections_missing")
    if signature.get("algorithm") != "ed25519":
        raise ValueError("fork_branch_b_signature_algorithm_invalid")

    payload = canonical_json_bytes(claim)
    if signature.get("payload_sha256") != sha256_bytes(payload):
        raise ValueError("fork_branch_b_payload_digest_mismatch")

    der = subprocess.check_output(
        ["openssl", "pkey", "-pubin", "-in", str(public_key), "-outform", "DER"]
    )
    prefix = bytes.fromhex("302a300506032b6570032100")
    if len(der) != len(prefix) + 32 or not der.startswith(prefix):
        raise ValueError("fork_branch_b_public_key_encoding_invalid")
    fingerprint = sha256_bytes(der[-32:])
    if signature.get("public_key_sha256") != fingerprint:
        raise ValueError("fork_branch_b_public_key_fingerprint_mismatch")

    try:
        raw_signature = base64.b64decode(str(signature["value_base64"]), validate=True)
    except (KeyError, ValueError) as exc:
        raise ValueError("fork_branch_b_signature_encoding_invalid") from exc

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
        raise ValueError("fork_branch_b_signature_invalid")

    authority = f"ed25519-sha256:{fingerprint}"
    if claim.get("authority_id") != authority:
        raise ValueError("fork_branch_b_authority_mismatch")
    return authority, claim


def verify_source_material(
    repository_root: Path,
    source_material: object,
) -> None:
    if not isinstance(source_material, list) or not source_material:
        raise ValueError("fork_source_material_missing")
    seen: set[tuple[str, str]] = set()
    for item in source_material:
        if not isinstance(item, dict) or set(item) != {"path", "source_sha", "sha256"}:
            raise ValueError("fork_source_material_invalid")
        path = item.get("path")
        source_sha = item.get("source_sha")
        expected = item.get("sha256")
        if not all(isinstance(value, str) and value for value in (path, source_sha, expected)):
            raise ValueError("fork_source_material_invalid")
        key = (source_sha, path)
        if key in seen:
            raise ValueError("fork_source_material_duplicate")
        seen.add(key)
        actual = subprocess.check_output(
            ["git", "show", f"{source_sha}:{path}"],
            cwd=repository_root,
        )
        if sha256_bytes(actual) != expected:
            raise ValueError(f"fork_source_material_mismatch:{path}")
