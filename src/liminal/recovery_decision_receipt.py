"""Signed, independently reproducible receipts for Recovery Router decisions.

A receipt binds one deterministic recovery decision to:
- the compact RecoverySignals used by the router;
- the explicit RecoveryPolicy thresholds;
- a signed recovery-evidence ledger-head attestation.

An offline verifier can validate both Ed25519 signatures and recompute the
router decision from the declared inputs. Raw prompts, model responses,
credentials, and private keys are never included.
"""

from __future__ import annotations

import base64
import json
from dataclasses import dataclass
from typing import Any

from cryptography.exceptions import InvalidSignature
from cryptography.hazmat.primitives.asymmetric.ed25519 import (
    Ed25519PrivateKey,
    Ed25519PublicKey,
)

from liminal.recovery_evidence_attestation import (
    RecoveryLedgerAttestation,
    parse_attestation,
    verify_attestation,
    verify_attestation_matches_ledger,
)
from liminal.recovery_evidence_ledger import RecoveryEvidenceLedger
from liminal.recovery_policy import (
    RecoveryDecision,
    RecoveryMode,
    RecoveryPolicy,
    RecoverySignals,
    choose_recovery_mode,
)


RECEIPT_SCHEMA_VERSION = "liminal.recovery-decision-receipt.v0.1"
ALGORITHM = "ed25519"


def _canonical_json(payload: dict[str, Any]) -> bytes:
    return json.dumps(
        payload,
        sort_keys=True,
        separators=(",", ":"),
        ensure_ascii=True,
    ).encode("utf-8")


def _signals_dict(signals: RecoverySignals) -> dict[str, Any]:
    return {
        "replay_steps": signals.replay_steps,
        "candidate_count": signals.candidate_count,
        "best_anchor_score": signals.best_anchor_score,
        "uncertainty": signals.uncertainty,
        "verified_candidate_available": signals.verified_candidate_available,
        "require_verified": signals.require_verified,
        "field_scan_cost": signals.field_scan_cost,
        "field_verification_success_rate": signals.field_verification_success_rate,
        "field_completion_pressure": signals.field_completion_pressure,
        "field_observation_count": signals.field_observation_count,
    }


def _policy_dict(policy: RecoveryPolicy) -> dict[str, Any]:
    return {
        "max_sequential_steps": policy.max_sequential_steps,
        "min_field_anchor_score": policy.min_field_anchor_score,
        "max_field_uncertainty": policy.max_field_uncertainty,
        "min_field_savings_ratio": policy.min_field_savings_ratio,
        "max_field_candidates": policy.max_field_candidates,
        "min_field_observations": policy.min_field_observations,
        "min_field_verification_success_rate": policy.min_field_verification_success_rate,
        "max_field_completion_pressure": policy.max_field_completion_pressure,
    }


def _decision_dict(decision: RecoveryDecision) -> dict[str, Any]:
    return {
        "mode": decision.mode.value,
        "reason": decision.reason,
        "replay_steps": decision.replay_steps,
        "estimated_field_cost": decision.estimated_field_cost,
        "estimated_savings_ratio": decision.estimated_savings_ratio,
    }


def _parse_signals(payload: dict[str, Any]) -> RecoverySignals:
    required = set(_signals_dict(RecoverySignals(0, 0, 0.0)))
    if set(payload) != required:
        raise ValueError("recovery_decision_receipt_invalid_signal_keys")
    return RecoverySignals(**payload)


def _parse_policy(payload: dict[str, Any]) -> RecoveryPolicy:
    required = set(_policy_dict(RecoveryPolicy()))
    if set(payload) != required:
        raise ValueError("recovery_decision_receipt_invalid_policy_keys")
    return RecoveryPolicy(**payload)


def _parse_decision(payload: dict[str, Any]) -> RecoveryDecision:
    required = {
        "mode",
        "reason",
        "replay_steps",
        "estimated_field_cost",
        "estimated_savings_ratio",
    }
    if set(payload) != required:
        raise ValueError("recovery_decision_receipt_invalid_decision_keys")
    try:
        mode = RecoveryMode(payload["mode"])
    except (TypeError, ValueError) as exc:
        raise ValueError("recovery_decision_receipt_invalid_mode") from exc
    reason = payload["reason"]
    if not isinstance(reason, str) or not reason:
        raise ValueError("recovery_decision_receipt_invalid_reason")
    return RecoveryDecision(
        mode=mode,
        reason=reason,
        replay_steps=payload["replay_steps"],
        estimated_field_cost=payload["estimated_field_cost"],
        estimated_savings_ratio=payload["estimated_savings_ratio"],
    )


@dataclass(frozen=True)
class RecoveryDecisionReceipt:
    statement: dict[str, Any]
    algorithm: str
    key_id: str
    signature_base64: str

    def as_dict(self) -> dict[str, Any]:
        return {
            "statement": self.statement,
            "algorithm": self.algorithm,
            "key_id": self.key_id,
            "signature_base64": self.signature_base64,
        }


def build_decision_statement(
    *,
    recovery_class: str,
    signals: RecoverySignals,
    policy: RecoveryPolicy,
    decision: RecoveryDecision,
    ledger_attestation: RecoveryLedgerAttestation,
) -> dict[str, Any]:
    """Build a strict reproducible statement after checking the decision."""

    if not recovery_class:
        raise ValueError("recovery_class_required")
    recomputed = choose_recovery_mode(signals, policy)
    if recomputed != decision:
        raise ValueError("recovery_decision_receipt_decision_mismatch")
    return {
        "schema_version": RECEIPT_SCHEMA_VERSION,
        "recovery_class": recovery_class,
        "recovery_signals": _signals_dict(signals),
        "recovery_policy": _policy_dict(policy),
        "recovery_decision": _decision_dict(decision),
        "ledger_attestation": ledger_attestation.as_dict(),
    }


def sign_decision_receipt(
    *,
    recovery_class: str,
    signals: RecoverySignals,
    policy: RecoveryPolicy,
    decision: RecoveryDecision,
    ledger_attestation: RecoveryLedgerAttestation,
    private_key: Ed25519PrivateKey,
    key_id: str,
) -> RecoveryDecisionReceipt:
    """Sign one reproducible recovery decision statement."""

    if not key_id:
        raise ValueError("key_id_required")
    statement = build_decision_statement(
        recovery_class=recovery_class,
        signals=signals,
        policy=policy,
        decision=decision,
        ledger_attestation=ledger_attestation,
    )
    signature = private_key.sign(_canonical_json(statement))
    return RecoveryDecisionReceipt(
        statement=statement,
        algorithm=ALGORITHM,
        key_id=key_id,
        signature_base64=base64.b64encode(signature).decode("ascii"),
    )


def parse_decision_receipt(payload: dict[str, Any]) -> RecoveryDecisionReceipt:
    required = {"statement", "algorithm", "key_id", "signature_base64"}
    if set(payload) != required:
        raise ValueError("recovery_decision_receipt_invalid_keys")
    if payload["algorithm"] != ALGORITHM:
        raise ValueError("recovery_decision_receipt_unsupported_algorithm")
    key_id = payload["key_id"]
    signature = payload["signature_base64"]
    statement = payload["statement"]
    if not isinstance(key_id, str) or not key_id:
        raise ValueError("recovery_decision_receipt_invalid_key_id")
    if not isinstance(signature, str) or not signature:
        raise ValueError("recovery_decision_receipt_invalid_signature")
    if not isinstance(statement, dict):
        raise ValueError("recovery_decision_receipt_invalid_statement")
    statement_keys = {
        "schema_version",
        "recovery_class",
        "recovery_signals",
        "recovery_policy",
        "recovery_decision",
        "ledger_attestation",
    }
    if set(statement) != statement_keys:
        raise ValueError("recovery_decision_receipt_invalid_statement_keys")
    if statement["schema_version"] != RECEIPT_SCHEMA_VERSION:
        raise ValueError("recovery_decision_receipt_unsupported_schema")
    if not isinstance(statement["recovery_class"], str) or not statement["recovery_class"]:
        raise ValueError("recovery_decision_receipt_invalid_recovery_class")
    for name in ("recovery_signals", "recovery_policy", "recovery_decision", "ledger_attestation"):
        if not isinstance(statement[name], dict):
            raise ValueError(f"recovery_decision_receipt_invalid_{name}")

    # Strictly parse nested payloads now so malformed receipts fail before use.
    _parse_signals(statement["recovery_signals"])
    _parse_policy(statement["recovery_policy"])
    _parse_decision(statement["recovery_decision"])
    parse_attestation(statement["ledger_attestation"])

    return RecoveryDecisionReceipt(
        statement=statement,
        algorithm=ALGORITHM,
        key_id=key_id,
        signature_base64=signature,
    )


def verify_decision_receipt(
    receipt: RecoveryDecisionReceipt,
    *,
    public_key: Ed25519PublicKey,
    expected_key_id: str | None = None,
    ledger: RecoveryEvidenceLedger | None = None,
) -> bool:
    """Verify signature, ledger provenance, and deterministic router replay."""

    if receipt.algorithm != ALGORITHM:
        return False
    if expected_key_id is not None and receipt.key_id != expected_key_id:
        return False
    try:
        signature = base64.b64decode(receipt.signature_base64, validate=True)
    except (ValueError, TypeError):
        return False
    try:
        public_key.verify(signature, _canonical_json(receipt.statement))
    except InvalidSignature:
        return False

    try:
        signals = _parse_signals(receipt.statement["recovery_signals"])
        policy = _parse_policy(receipt.statement["recovery_policy"])
        decision = _parse_decision(receipt.statement["recovery_decision"])
        ledger_attestation = parse_attestation(receipt.statement["ledger_attestation"])
    except (KeyError, TypeError, ValueError):
        return False

    # v0.1 deliberately requires the same externally trusted signer for the
    # ledger attestation and the decision receipt.
    if not verify_attestation(
        ledger_attestation,
        public_key=public_key,
        expected_key_id=receipt.key_id,
    ):
        return False
    if ledger is not None and not verify_attestation_matches_ledger(
        ledger_attestation,
        ledger,
        public_key=public_key,
        expected_key_id=receipt.key_id,
    ):
        return False

    try:
        recomputed = choose_recovery_mode(signals, policy)
    except (TypeError, ValueError):
        return False
    return recomputed == decision
