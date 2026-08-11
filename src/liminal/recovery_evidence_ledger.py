"""Durable tamper-evident ledger for observed recovery outcomes.

The ledger persists only the compact evidence needed by the Recovery Router:
recovery class, selected mode, deterministic verification result, and explicit
provider finish reason. Raw model text, prompts, credentials, and hidden state
are never written here.

Records are newline-delimited JSON and form a SHA-256 chain. Loading is
fail-closed: malformed records, unsupported schemas, or broken hash links are
rejected instead of being silently ignored.
"""

from __future__ import annotations

import hashlib
import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any

from liminal.recovery_evidence import RecoveryAttemptEvidence
from liminal.recovery_policy import RecoveryMode


SCHEMA_VERSION = "liminal.recovery-evidence-ledger.v0.1"
GENESIS_SHA256 = "0" * 64


def _canonical_json(payload: dict[str, Any]) -> str:
    return json.dumps(payload, sort_keys=True, separators=(",", ":"), ensure_ascii=True)


def _record_hash(payload_without_hash: dict[str, Any]) -> str:
    return hashlib.sha256(_canonical_json(payload_without_hash).encode("utf-8")).hexdigest()


@dataclass(frozen=True)
class RecoveryEvidenceLedgerRecord:
    attempt: RecoveryAttemptEvidence
    previous_sha256: str
    record_sha256: str


class RecoveryEvidenceLedger:
    """Append and verify compact recovery evidence across process restarts."""

    def __init__(self, path: str | Path) -> None:
        self.path = Path(path)

    def _read_records(self) -> tuple[RecoveryEvidenceLedgerRecord, ...]:
        if not self.path.exists():
            return ()

        previous = GENESIS_SHA256
        records: list[RecoveryEvidenceLedgerRecord] = []
        for line_number, raw_line in enumerate(
            self.path.read_text(encoding="utf-8").splitlines(), start=1
        ):
            if not raw_line.strip():
                raise ValueError(f"recovery_evidence_ledger_blank_line:{line_number}")
            try:
                payload = json.loads(raw_line)
            except json.JSONDecodeError as exc:
                raise ValueError(
                    f"recovery_evidence_ledger_invalid_json:{line_number}"
                ) from exc
            if not isinstance(payload, dict):
                raise ValueError(f"recovery_evidence_ledger_invalid_record:{line_number}")

            required = {
                "schema_version",
                "previous_sha256",
                "recovery_class",
                "mode",
                "verification_passed",
                "finish_reason",
                "record_sha256",
            }
            if set(payload) != required:
                raise ValueError(f"recovery_evidence_ledger_invalid_keys:{line_number}")
            if payload["schema_version"] != SCHEMA_VERSION:
                raise ValueError(
                    f"recovery_evidence_ledger_unsupported_schema:{line_number}"
                )
            if payload["previous_sha256"] != previous:
                raise ValueError(f"recovery_evidence_ledger_chain_broken:{line_number}")

            provided_hash = payload["record_sha256"]
            if not isinstance(provided_hash, str) or len(provided_hash) != 64:
                raise ValueError(f"recovery_evidence_ledger_invalid_hash:{line_number}")
            hash_payload = dict(payload)
            del hash_payload["record_sha256"]
            expected_hash = _record_hash(hash_payload)
            if provided_hash != expected_hash:
                raise ValueError(f"recovery_evidence_ledger_hash_mismatch:{line_number}")

            recovery_class = payload["recovery_class"]
            if not isinstance(recovery_class, str) or not recovery_class:
                raise ValueError(
                    f"recovery_evidence_ledger_invalid_recovery_class:{line_number}"
                )
            try:
                mode = RecoveryMode(payload["mode"])
            except (TypeError, ValueError) as exc:
                raise ValueError(
                    f"recovery_evidence_ledger_invalid_mode:{line_number}"
                ) from exc
            verification_passed = payload["verification_passed"]
            if not isinstance(verification_passed, bool):
                raise ValueError(
                    f"recovery_evidence_ledger_invalid_verification:{line_number}"
                )
            finish_reason = payload["finish_reason"]
            if finish_reason is not None and not isinstance(finish_reason, str):
                raise ValueError(
                    f"recovery_evidence_ledger_invalid_finish_reason:{line_number}"
                )

            attempt = RecoveryAttemptEvidence(
                recovery_class=recovery_class,
                mode=mode,
                verification_passed=verification_passed,
                finish_reason=finish_reason,
            )
            records.append(
                RecoveryEvidenceLedgerRecord(
                    attempt=attempt,
                    previous_sha256=previous,
                    record_sha256=provided_hash,
                )
            )
            previous = provided_hash

        return tuple(records)

    def records(self) -> tuple[RecoveryEvidenceLedgerRecord, ...]:
        """Return the fully verified ledger."""

        return self._read_records()

    def attempts(self) -> tuple[RecoveryAttemptEvidence, ...]:
        return tuple(record.attempt for record in self._read_records())

    def append(self, attempt: RecoveryAttemptEvidence) -> RecoveryEvidenceLedgerRecord:
        """Verify the existing chain, then append one compact evidence record."""

        if not attempt.recovery_class:
            raise ValueError("recovery_class_required")

        records = self._read_records()
        previous = records[-1].record_sha256 if records else GENESIS_SHA256
        payload: dict[str, Any] = {
            "schema_version": SCHEMA_VERSION,
            "previous_sha256": previous,
            "recovery_class": attempt.recovery_class,
            "mode": attempt.mode.value,
            "verification_passed": attempt.verification_passed,
            "finish_reason": attempt.finish_reason,
        }
        record_sha256 = _record_hash(payload)
        payload["record_sha256"] = record_sha256

        self.path.parent.mkdir(parents=True, exist_ok=True)
        with self.path.open("a", encoding="utf-8", newline="\n") as handle:
            handle.write(_canonical_json(payload) + "\n")
            handle.flush()

        return RecoveryEvidenceLedgerRecord(
            attempt=attempt,
            previous_sha256=previous,
            record_sha256=record_sha256,
        )
