"""Provider-specific verification for the independent Historical Root B."""

from __future__ import annotations

import base64
import hashlib
import json
import subprocess
import tempfile
from pathlib import Path
from typing import Any

from liminal.recovery_trust_root_registry import canonical_json_bytes

ENVELOPE_SCHEMA = "liminal-historical-root-b-signed-history-envelope/v0.1"


def load_json(path: Path) -> dict[str, Any]:
    value = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(value, dict):
        raise ValueError(f"object_required:{path}")
    return value


def sha256_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def verify_ed25519_envelope(public_key: Path, envelope: dict[str, Any]) -> str:
    if envelope.get("schema") != ENVELOPE_SCHEMA:
        raise ValueError("root_b_envelope_schema_invalid")
    claim = envelope.get("claim")
    signature = envelope.get("signature")
    if not isinstance(claim, dict) or not isinstance(signature, dict):
        raise ValueError("root_b_envelope_sections_missing")
    if signature.get("algorithm") != "ed25519":
        raise ValueError("root_b_signature_algorithm_invalid")

    payload = canonical_json_bytes(claim)
    if signature.get("payload_sha256") != sha256_bytes(payload):
        raise ValueError("root_b_payload_digest_mismatch")

    der = subprocess.check_output(
        ["openssl", "pkey", "-pubin", "-in", str(public_key), "-outform", "DER"]
    )
    prefix = bytes.fromhex("302a300506032b6570032100")
    if len(der) != len(prefix) + 32 or not der.startswith(prefix):
        raise ValueError("root_b_public_key_encoding_invalid")
    fingerprint = sha256_bytes(der[-32:])
    if signature.get("public_key_sha256") != fingerprint:
        raise ValueError("root_b_public_key_fingerprint_mismatch")

    try:
        raw_signature = base64.b64decode(str(signature["value_base64"]), validate=True)
    except (KeyError, ValueError) as exc:
        raise ValueError("root_b_signature_encoding_invalid") from exc

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
        raise ValueError("root_b_signature_invalid")

    authority = f"ed25519-sha256:{fingerprint}"
    if claim.get("genesis_authority_id") != authority:
        raise ValueError("root_b_genesis_authority_mismatch")
    return authority


def verify_genesis_source_material(
    repository_root: Path,
    manifest: dict[str, Any],
) -> None:
    for entry in manifest["roots"].values():
        source_sha = str(entry["workflow_sha"])
        path = str(entry["workflow_path"])
        actual_blob = subprocess.check_output(
            ["git", "rev-parse", f"{source_sha}:{path}"],
            cwd=repository_root,
        ).decode("ascii").strip()
        if actual_blob != entry["git_blob_sha"]:
            raise ValueError(f"root_b_workflow_blob_mismatch:{path}")

    for item in manifest["policy_material"].values():
        source_sha = str(item["source_sha"])
        path = str(item["path"])
        actual = subprocess.check_output(
            ["git", "show", f"{source_sha}:{path}"],
            cwd=repository_root,
        )
        if sha256_bytes(actual) != item["sha256"]:
            raise ValueError(f"root_b_policy_material_mismatch:{path}")
