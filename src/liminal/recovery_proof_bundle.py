"""Deterministic bundle for independently attested recovery decision evidence.

The inner receipt/ledger proves decision integrity and deterministic replay. This
bundle gives an outer provenance system (for example GitHub Artifact
Attestations) one stable subject digest to bind to a workflow identity.

Only compact evidence files are bundled. Raw prompts, provider responses,
reasoning text, credentials, and private signing keys are intentionally absent.
"""

from __future__ import annotations

import hashlib
import json
import zipfile
from dataclasses import dataclass
from pathlib import Path


PROOF_BUNDLE_SCHEMA_VERSION = "liminal.recovery-proof-bundle.v0.1"
PROOF_MANIFEST_NAME = "proof-manifest.json"
PROOF_BUNDLE_NAME = "recovery-proof-bundle.zip"
PROOF_MEMBERS = (
    "decision-receipt.json",
    "public-key.json",
    "recovery-evidence.jsonl",
    "summary.json",
)
_FIXED_ZIP_TIME = (1980, 1, 1, 0, 0, 0)


def _sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def _canonical_json(value: object) -> bytes:
    return (json.dumps(value, sort_keys=True, separators=(",", ":")) + "\n").encode("utf-8")


@dataclass(frozen=True)
class RecoveryProofBundle:
    path: Path
    sha256: str
    manifest_sha256: str
    member_count: int


def _zip_info(name: str) -> zipfile.ZipInfo:
    info = zipfile.ZipInfo(filename=name, date_time=_FIXED_ZIP_TIME)
    info.compress_type = zipfile.ZIP_STORED
    info.create_system = 3
    info.external_attr = 0o644 << 16
    info.extra = b""
    info.comment = b""
    return info


def build_recovery_proof_bundle(
    directory: str | Path,
    *,
    output_name: str = PROOF_BUNDLE_NAME,
) -> RecoveryProofBundle:
    """Build a deterministic ZIP plus canonical per-file hash manifest."""

    root = Path(directory)
    files: list[dict[str, object]] = []
    for name in PROOF_MEMBERS:
        path = root / name
        if not path.is_file():
            raise FileNotFoundError(f"recovery_proof_member_missing:{name}")
        payload = path.read_bytes()
        files.append(
            {
                "path": name,
                "sha256": _sha256(payload),
                "size_bytes": len(payload),
            }
        )

    manifest = {
        "schema_version": PROOF_BUNDLE_SCHEMA_VERSION,
        "files": files,
    }
    manifest_bytes = _canonical_json(manifest)
    manifest_path = root / PROOF_MANIFEST_NAME
    manifest_path.write_bytes(manifest_bytes)

    bundle_path = root / output_name
    with zipfile.ZipFile(bundle_path, mode="w", compression=zipfile.ZIP_STORED) as archive:
        for name in sorted((*PROOF_MEMBERS, PROOF_MANIFEST_NAME)):
            archive.writestr(_zip_info(name), (root / name).read_bytes())

    bundle_bytes = bundle_path.read_bytes()
    return RecoveryProofBundle(
        path=bundle_path,
        sha256=_sha256(bundle_bytes),
        manifest_sha256=_sha256(manifest_bytes),
        member_count=len(PROOF_MEMBERS) + 1,
    )


def verify_recovery_proof_bundle(path: str | Path) -> bool:
    """Fail closed unless bundle membership and every manifest hash match."""

    try:
        with zipfile.ZipFile(Path(path), mode="r") as archive:
            names = archive.namelist()
            expected_names = set((*PROOF_MEMBERS, PROOF_MANIFEST_NAME))
            if len(names) != len(set(names)) or set(names) != expected_names:
                return False
            manifest_raw = archive.read(PROOF_MANIFEST_NAME)
            manifest = json.loads(manifest_raw)
            if manifest.get("schema_version") != PROOF_BUNDLE_SCHEMA_VERSION:
                return False
            files = manifest.get("files")
            if not isinstance(files, list) or len(files) != len(PROOF_MEMBERS):
                return False

            by_path: dict[str, dict[str, object]] = {}
            for item in files:
                if not isinstance(item, dict):
                    return False
                name = item.get("path")
                if not isinstance(name, str) or name in by_path:
                    return False
                by_path[name] = item
            if set(by_path) != set(PROOF_MEMBERS):
                return False

            for name in PROOF_MEMBERS:
                payload = archive.read(name)
                item = by_path[name]
                if item.get("sha256") != _sha256(payload):
                    return False
                if item.get("size_bytes") != len(payload):
                    return False
            return True
    except (OSError, KeyError, json.JSONDecodeError, zipfile.BadZipFile):
        return False
