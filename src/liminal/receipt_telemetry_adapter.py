"""Aggregate instrumentation receipts into observable runtime telemetry.

The adapter deliberately requires explicit defaults for signals that receipts do
not yet cover. This prevents absent measurements from being mistaken for healthy
values while still allowing callers to compose Prometheus and receipt sources.
"""

from __future__ import annotations

from dataclasses import dataclass

from liminal.instrumentation_receipts import (
    ContinuityReceipt,
    InstrumentationReceipt,
    LogicalRetryReceipt,
    ReceiptKind,
    TokenUsageReceipt,
    VerifiedProgressReceipt,
)


@dataclass(frozen=True)
class ReceiptTelemetry:
    token_utilization: float | None
    retry_rate: float | None
    goal_drift: float | None
    causal_drift: float | None
    verified_progress_rate: float | None
    receipt_count: int



def _mean(values: list[float]) -> float | None:
    if not values:
        return None
    return sum(values) / len(values)



def aggregate_receipts(receipts: tuple[InstrumentationReceipt, ...]) -> ReceiptTelemetry:
    """Aggregate one trace window into normalized receipt-backed telemetry."""

    token_utils: list[float] = []
    retry_steps: set[str] = set()
    step_ids: set[str] = set()
    goal_scores: list[float] = []
    causal_scores: list[float] = []
    progress_scores: list[float] = []

    for receipt in receipts:
        step_ids.add(receipt.step_id)

        if isinstance(receipt, TokenUsageReceipt):
            if receipt.context_window_tokens is not None:
                used = receipt.input_tokens + receipt.output_tokens
                token_utils.append(min(1.0, used / receipt.context_window_tokens))
            continue

        if isinstance(receipt, LogicalRetryReceipt):
            retry_steps.add(receipt.step_id)
            continue

        if isinstance(receipt, ContinuityReceipt):
            if receipt.kind is ReceiptKind.GOAL_CONTINUITY:
                goal_scores.append(receipt.continuity_score)
            elif receipt.kind is ReceiptKind.CAUSAL_CONTINUITY:
                causal_scores.append(receipt.continuity_score)
            continue

        if isinstance(receipt, VerifiedProgressReceipt):
            progress_scores.append(receipt.progress_ratio)

    retry_rate = None
    if step_ids:
        retry_rate = len(retry_steps) / len(step_ids)

    goal_continuity = _mean(goal_scores)
    causal_continuity = _mean(causal_scores)

    return ReceiptTelemetry(
        token_utilization=_mean(token_utils),
        retry_rate=retry_rate,
        goal_drift=None if goal_continuity is None else 1.0 - goal_continuity,
        causal_drift=None if causal_continuity is None else 1.0 - causal_continuity,
        verified_progress_rate=_mean(progress_scores),
        receipt_count=len(receipts),
    )
