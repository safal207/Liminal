"""Deterministic fixture and aggregation helpers for a live recovery A/B probe.

The benchmark compares two ways to present the *same* interrupted-workflow
recovery task to a real provider:

* ``sequential`` receives the full 12-checkpoint history;
* ``focus_field`` receives a bounded 3-candidate recovery field drawn from that
  exact same history and ranked by generic verification/lifecycle evidence.

The model must infer the continuation anchor from evidence in the supplied
context. The JSON schema constrains only output shape; it does not reveal the
expected goal or parent-step values. A per-pair probe nonce can be included to
reduce provider-side cache reuse; the same nonce is supplied to both arms.
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from statistics import median
from typing import Iterable


EXPECTED_CHECKPOINT_ID = "checkpoint-09"
EXPECTED_GOAL_ID = "invoice-reconciliation-v3"
EXPECTED_PARENT_STEP_ID = "ledger-apply-07"
EXPECTED_STATUS = "verified"


@dataclass(frozen=True)
class RecoveryCheckpoint:
    checkpoint_id: str
    goal_id: str
    parent_step_id: str
    verification: str
    lifecycle: str
    note: str

    def render(self) -> str:
        return (
            f"{self.checkpoint_id} | goal_id={self.goal_id} | "
            f"parent_step_id={self.parent_step_id} | verification={self.verification} | "
            f"lifecycle={self.lifecycle} | note={self.note}"
        )


@dataclass(frozen=True)
class RecoveryVerification:
    valid_json: bool
    exact_key_set: bool
    goal_match: bool
    parent_match: bool
    status_match: bool
    evidence_match: bool

    @property
    def passed(self) -> bool:
        return all(
            (
                self.valid_json,
                self.exact_key_set,
                self.goal_match,
                self.parent_match,
                self.status_match,
                self.evidence_match,
            )
        )


CHECKPOINTS: tuple[RecoveryCheckpoint, ...] = (
    RecoveryCheckpoint(
        "checkpoint-01",
        "invoice-intake-v1",
        "parse-batch-01",
        "verified",
        "superseded",
        "Initial ingestion anchor; later workflow versions replace this state.",
    ),
    RecoveryCheckpoint(
        "checkpoint-02",
        "invoice-intake-v1",
        "normalize-vendors-02",
        "verified",
        "superseded",
        "Vendor normalization completed under the retired v1 goal.",
    ),
    RecoveryCheckpoint(
        "checkpoint-03",
        "invoice-reconciliation-v2",
        "match-ledger-03",
        "verified",
        "superseded",
        "A valid historical anchor, but the v2 goal was explicitly replaced.",
    ),
    RecoveryCheckpoint(
        "checkpoint-04",
        "invoice-reconciliation-v2",
        "resolve-duplicates-04",
        "verified",
        "superseded",
        "Duplicate resolution succeeded before the v3 migration decision.",
    ),
    RecoveryCheckpoint(
        "checkpoint-05",
        "invoice-reconciliation-v3",
        "prepare-ledger-05",
        "unverified",
        "candidate",
        "Migration draft opened; no verification receipt was emitted.",
    ),
    RecoveryCheckpoint(
        "checkpoint-06",
        "invoice-reconciliation-v3",
        "prepare-ledger-06",
        "verified",
        "superseded",
        "Verified v3 preparation point, later superseded by a newer verified anchor.",
    ),
    RecoveryCheckpoint(
        "checkpoint-07",
        "invoice-reconciliation-v3",
        "ledger-dry-run-06",
        "unverified",
        "candidate",
        "Dry-run observations exist but causal completion was not verified.",
    ),
    RecoveryCheckpoint(
        "checkpoint-08",
        "invoice-reconciliation-v3",
        "ledger-apply-07",
        "unverified",
        "candidate",
        "Ledger apply began; this record alone is not a continuation anchor.",
    ),
    RecoveryCheckpoint(
        EXPECTED_CHECKPOINT_ID,
        EXPECTED_GOAL_ID,
        EXPECTED_PARENT_STEP_ID,
        "verified",
        "active",
        "Latest verified non-superseded continuation anchor before interruption.",
    ),
    RecoveryCheckpoint(
        "checkpoint-10",
        "invoice-reconciliation-v3",
        "post-ledger-audit-08",
        "unverified",
        "speculative",
        "A speculative next step was drafted but never executed or verified.",
    ),
    RecoveryCheckpoint(
        "checkpoint-11",
        "invoice-reconciliation-v4",
        "experimental-audit-09",
        "unverified",
        "speculative",
        "Goal v4 is only a hypothesis and must not replace the verified v3 intent.",
    ),
    RecoveryCheckpoint(
        "checkpoint-12",
        "unknown-after-interruption",
        "none",
        "unverified",
        "interrupted",
        "Execution stopped here; recover from prior verified evidence rather than guessing.",
    ),
)

# This candidate set is a configured benchmark input. Ranking below is generic and
# does not reference EXPECTED_* values.
FOCUS_FIELD_IDS = ("checkpoint-03", EXPECTED_CHECKPOINT_ID, "checkpoint-11")


def _render_context(checkpoints: Iterable[RecoveryCheckpoint]) -> str:
    return "\n".join(item.render() for item in checkpoints)


def sequential_context() -> str:
    return _render_context(CHECKPOINTS)


def _field_rank_key(checkpoint: RecoveryCheckpoint) -> tuple[int, int, str]:
    """Rank by generic evidence quality, never by expected answer constants."""

    verification_rank = 1 if checkpoint.verification == "verified" else 0
    lifecycle_rank = {
        "active": 4,
        "candidate": 3,
        "superseded": 2,
        "speculative": 1,
        "interrupted": 0,
    }.get(checkpoint.lifecycle, 0)
    return verification_rank, lifecycle_rank, checkpoint.checkpoint_id


def focus_field_candidates() -> tuple[RecoveryCheckpoint, ...]:
    wanted = set(FOCUS_FIELD_IDS)
    candidates = [item for item in CHECKPOINTS if item.checkpoint_id in wanted]
    return tuple(sorted(candidates, key=_field_rank_key, reverse=True))


def focus_field_context() -> str:
    return "\n".join(
        f"field_rank={rank} | {item.render()}"
        for rank, item in enumerate(focus_field_candidates(), start=1)
    )


def recovery_prompt(mode: str, *, probe_nonce: str | None = None) -> str:
    """Build the same recovery task over two different context geometries.

    ``probe_nonce`` is intentionally irrelevant to recovery semantics. The live
    paired runner gives both arms the same unique nonce for each trial so an
    upstream cache cannot turn repeated trials into local cache-hit timing.
    """

    if mode == "sequential":
        context = sequential_context()
        label = "FULL SEQUENTIAL CHECKPOINT HISTORY"
    elif mode == "focus_field":
        context = focus_field_context()
        label = "BOUNDED RANKED FOCUS-FIELD CANDIDATES"
    else:
        raise ValueError("unsupported_recovery_ab_mode")

    nonce_line = (
        f"Probe nonce (ignore for recovery semantics): {probe_nonce}\n"
        if probe_nonce
        else ""
    )
    return (
        "An agent was interrupted after checkpoint-12. Recover the latest continuation "
        "anchor that is VERIFIED, is not superseded/speculative/interrupted, and preserves "
        "the active goal plus causal parent step. Do not choose an unverified later guess. "
        "For a ranked focus field, rank is retrieval evidence but does not override the "
        "verification/lifecycle facts. Return exactly one JSON object with keys goal_id, "
        "parent_step_id, status, evidence. Set status to verified and set evidence to the "
        "checkpoint_id you selected.\n"
        f"{nonce_line}\n"
        f"{label}:\n{context}"
    )


def recovery_response_format() -> dict:
    """Constrain shape without leaking the expected recovery anchor."""

    return {
        "type": "json_schema",
        "json_schema": {
            "name": "liminal_live_recovery_ab",
            "strict": True,
            "schema": {
                "type": "object",
                "additionalProperties": False,
                "required": ["goal_id", "parent_step_id", "status", "evidence"],
                "properties": {
                    "goal_id": {"type": "string"},
                    "parent_step_id": {"type": "string"},
                    "status": {"type": "string", "enum": [EXPECTED_STATUS]},
                    "evidence": {"type": "string"},
                },
            },
        },
    }


def parse_recovery_payload(content: str) -> dict[str, str] | None:
    """Return only the structured synthetic output, never provider reasoning text."""

    try:
        payload = json.loads(content)
    except (json.JSONDecodeError, TypeError):
        return None
    if not isinstance(payload, dict):
        return None
    allowed = {"goal_id", "parent_step_id", "status", "evidence"}
    if not set(payload).issubset(allowed):
        return None
    return {str(key): str(value) for key, value in payload.items()}


def verify_recovery_response(content: str) -> RecoveryVerification:
    expected_keys = {"goal_id", "parent_step_id", "status", "evidence"}
    payload = parse_recovery_payload(content)
    if payload is None:
        return RecoveryVerification(False, False, False, False, False, False)

    return RecoveryVerification(
        valid_json=True,
        exact_key_set=set(payload) == expected_keys,
        goal_match=payload.get("goal_id") == EXPECTED_GOAL_ID,
        parent_match=payload.get("parent_step_id") == EXPECTED_PARENT_STEP_ID,
        status_match=payload.get("status") == EXPECTED_STATUS,
        evidence_match=payload.get("evidence") == EXPECTED_CHECKPOINT_ID,
    )


def summarize_records(records: list[dict]) -> dict:
    """Aggregate real provider measurements without claiming statistical significance."""

    if not records:
        raise ValueError("records_required")

    by_mode: dict[str, list[dict]] = {"sequential": [], "focus_field": []}
    for record in records:
        mode = record["mode"]
        if mode not in by_mode:
            raise ValueError("unexpected_mode")
        by_mode[mode].append(record)

    if not by_mode["sequential"] or not by_mode["focus_field"]:
        raise ValueError("both_modes_required")

    def aggregate(items: list[dict]) -> dict:
        return {
            "trials": len(items),
            "verified_trials": sum(bool(item["verification_passed"]) for item in items),
            "prompt_tokens_total": sum(int(item["prompt_tokens"]) for item in items),
            "completion_tokens_total": sum(int(item["completion_tokens"]) for item in items),
            "total_tokens": sum(int(item["total_tokens"]) for item in items),
            "median_latency_seconds": round(
                median(float(item["latency_seconds"]) for item in items), 6
            ),
        }

    sequential = aggregate(by_mode["sequential"])
    focus_field = aggregate(by_mode["focus_field"])

    def savings_pct(baseline: float, candidate: float) -> float | None:
        if baseline <= 0:
            return None
        return round((baseline - candidate) / baseline * 100.0, 3)

    qualified = (
        sequential["verified_trials"] == sequential["trials"]
        and focus_field["verified_trials"] == focus_field["trials"]
    )

    return {
        "sequential": sequential,
        "focus_field": focus_field,
        "comparison": {
            "qualified_for_success_cost_comparison": qualified,
            "prompt_token_savings_pct": savings_pct(
                sequential["prompt_tokens_total"], focus_field["prompt_tokens_total"]
            ),
            "total_token_savings_pct": savings_pct(
                sequential["total_tokens"], focus_field["total_tokens"]
            ),
            "median_latency_savings_pct": savings_pct(
                sequential["median_latency_seconds"],
                focus_field["median_latency_seconds"],
            ),
            "context_char_reduction_pct": savings_pct(
                len(sequential_context()), len(focus_field_context())
            ),
        },
    }
