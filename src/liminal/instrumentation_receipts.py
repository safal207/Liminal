"""Experimental evidence receipts for runtime telemetry collection.

Receipts capture observable facts needed by flow/recovery control without
silently inferring missing state. They are append-friendly, deterministic data
records that can later be persisted by a trace store or exported to metrics.
"""

from __future__ import annotations

from dataclasses import dataclass
from datetime import datetime, timezone
from enum import Enum


class ReceiptKind(str, Enum):
    TOKEN_USAGE = "token_usage"
    LOGICAL_RETRY = "logical_retry"
    GOAL_CONTINUITY = "goal_continuity"
    CAUSAL_CONTINUITY = "causal_continuity"
    VERIFIED_PROGRESS = "verified_progress"


@dataclass(frozen=True)
class InstrumentationReceipt:
    receipt_id: str
    trace_id: str
    step_id: str
    kind: ReceiptKind
    observed_at: datetime
    source: str

    def __post_init__(self) -> None:
        if not self.receipt_id:
            raise ValueError("receipt_id_required")
        if not self.trace_id:
            raise ValueError("trace_id_required")
        if not self.step_id:
            raise ValueError("step_id_required")
        if not self.source:
            raise ValueError("source_required")
        if self.observed_at.tzinfo is None:
            raise ValueError("observed_at_must_be_timezone_aware")


@dataclass(frozen=True)
class TokenUsageReceipt(InstrumentationReceipt):
    input_tokens: int
    output_tokens: int
    context_window_tokens: int | None = None

    def __post_init__(self) -> None:
        super().__post_init__()
        if self.kind is not ReceiptKind.TOKEN_USAGE:
            raise ValueError("kind_must_be_token_usage")
        if self.input_tokens < 0 or self.output_tokens < 0:
            raise ValueError("token_counts_must_be_non_negative")
        if self.context_window_tokens is not None and self.context_window_tokens <= 0:
            raise ValueError("context_window_tokens_must_be_positive")


@dataclass(frozen=True)
class LogicalRetryReceipt(InstrumentationReceipt):
    logical_action_id: str
    retry_index: int
    reason_code: str

    def __post_init__(self) -> None:
        super().__post_init__()
        if self.kind is not ReceiptKind.LOGICAL_RETRY:
            raise ValueError("kind_must_be_logical_retry")
        if not self.logical_action_id:
            raise ValueError("logical_action_id_required")
        if self.retry_index < 1:
            raise ValueError("retry_index_must_be_positive")
        if not self.reason_code:
            raise ValueError("reason_code_required")


@dataclass(frozen=True)
class ContinuityReceipt(InstrumentationReceipt):
    continuity_score: float
    evidence_refs: tuple[str, ...] = ()

    def __post_init__(self) -> None:
        super().__post_init__()
        if self.kind not in (ReceiptKind.GOAL_CONTINUITY, ReceiptKind.CAUSAL_CONTINUITY):
            raise ValueError("kind_must_be_goal_or_causal_continuity")
        if not 0.0 <= self.continuity_score <= 1.0:
            raise ValueError("continuity_score_must_be_between_0_and_1")


@dataclass(frozen=True)
class VerifiedProgressReceipt(InstrumentationReceipt):
    completed_units: int
    expected_units: int
    verification_refs: tuple[str, ...] = ()

    def __post_init__(self) -> None:
        super().__post_init__()
        if self.kind is not ReceiptKind.VERIFIED_PROGRESS:
            raise ValueError("kind_must_be_verified_progress")
        if self.completed_units < 0:
            raise ValueError("completed_units_must_be_non_negative")
        if self.expected_units <= 0:
            raise ValueError("expected_units_must_be_positive")
        if self.completed_units > self.expected_units:
            raise ValueError("completed_units_cannot_exceed_expected_units")

    @property
    def progress_ratio(self) -> float:
        return self.completed_units / self.expected_units


def utc_now() -> datetime:
    """Return a timezone-aware timestamp for receipt creation."""

    return datetime.now(timezone.utc)
