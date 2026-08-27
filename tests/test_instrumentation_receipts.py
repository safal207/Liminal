from datetime import datetime, timezone

import pytest

from liminal.instrumentation_receipts import (
    ContinuityReceipt,
    InstrumentationReceipt,
    LogicalRetryReceipt,
    ReceiptKind,
    TokenUsageReceipt,
    VerifiedProgressReceipt,
)


NOW = datetime(2026, 8, 10, 12, 0, tzinfo=timezone.utc)


def base(kind: ReceiptKind) -> dict:
    return {
        "receipt_id": f"r-{kind.value}",
        "trace_id": "trace-1",
        "step_id": "step-7",
        "kind": kind,
        "observed_at": NOW,
        "source": "agent-runtime",
    }


def test_token_usage_receipt_accepts_real_counts() -> None:
    receipt = TokenUsageReceipt(
        **base(ReceiptKind.TOKEN_USAGE),
        input_tokens=1200,
        output_tokens=300,
        context_window_tokens=32000,
    )
    assert receipt.input_tokens + receipt.output_tokens == 1500


def test_logical_retry_requires_positive_retry_index() -> None:
    with pytest.raises(ValueError, match="retry_index_must_be_positive"):
        LogicalRetryReceipt(
            **base(ReceiptKind.LOGICAL_RETRY),
            logical_action_id="act-1",
            retry_index=0,
            reason_code="timeout",
        )


def test_goal_and_causal_continuity_are_explicit_scores() -> None:
    goal = ContinuityReceipt(
        **base(ReceiptKind.GOAL_CONTINUITY),
        continuity_score=0.8,
        evidence_refs=("goal-anchor",),
    )
    causal = ContinuityReceipt(
        **base(ReceiptKind.CAUSAL_CONTINUITY),
        continuity_score=0.7,
        evidence_refs=("cause-42",),
    )
    assert goal.continuity_score == 0.8
    assert causal.continuity_score == 0.7


def test_verified_progress_ratio_is_derived_from_verified_units() -> None:
    receipt = VerifiedProgressReceipt(
        **base(ReceiptKind.VERIFIED_PROGRESS),
        completed_units=3,
        expected_units=4,
        verification_refs=("test-report",),
    )
    assert receipt.progress_ratio == 0.75


def test_naive_timestamps_are_rejected() -> None:
    with pytest.raises(ValueError, match="observed_at_must_be_timezone_aware"):
        InstrumentationReceipt(
            receipt_id="r1",
            trace_id="t1",
            step_id="s1",
            kind=ReceiptKind.TOKEN_USAGE,
            observed_at=datetime(2026, 8, 10),
            source="runtime",
        )


def test_wrong_kind_is_rejected_by_specialized_receipt() -> None:
    with pytest.raises(ValueError, match="kind_must_be_token_usage"):
        TokenUsageReceipt(
            **base(ReceiptKind.LOGICAL_RETRY),
            input_tokens=1,
            output_tokens=1,
        )
