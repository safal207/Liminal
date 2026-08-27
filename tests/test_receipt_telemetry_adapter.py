from datetime import datetime, timezone

from liminal.instrumentation_receipts import (
    ContinuityReceipt,
    LogicalRetryReceipt,
    ReceiptKind,
    TokenUsageReceipt,
    VerifiedProgressReceipt,
)
from liminal.receipt_telemetry_adapter import aggregate_receipts


NOW = datetime(2026, 8, 10, 12, 0, tzinfo=timezone.utc)


def common(receipt_id: str, step_id: str, kind: ReceiptKind) -> dict:
    return {
        "receipt_id": receipt_id,
        "trace_id": "trace-1",
        "step_id": step_id,
        "kind": kind,
        "observed_at": NOW,
        "source": "runtime",
    }


def test_aggregate_receipts_builds_normalized_trace_window() -> None:
    receipts = (
        TokenUsageReceipt(
            **common("tok-1", "s1", ReceiptKind.TOKEN_USAGE),
            input_tokens=4000,
            output_tokens=1000,
            context_window_tokens=10000,
        ),
        LogicalRetryReceipt(
            **common("ret-1", "s2", ReceiptKind.LOGICAL_RETRY),
            logical_action_id="act-2",
            retry_index=1,
            reason_code="timeout",
        ),
        ContinuityReceipt(
            **common("goal-1", "s2", ReceiptKind.GOAL_CONTINUITY),
            continuity_score=0.8,
            evidence_refs=("goal-anchor",),
        ),
        ContinuityReceipt(
            **common("cause-1", "s2", ReceiptKind.CAUSAL_CONTINUITY),
            continuity_score=0.7,
            evidence_refs=("cause-1",),
        ),
        VerifiedProgressReceipt(
            **common("prog-1", "s3", ReceiptKind.VERIFIED_PROGRESS),
            completed_units=3,
            expected_units=4,
            verification_refs=("test-report",),
        ),
    )

    telemetry = aggregate_receipts(receipts)

    assert telemetry.token_utilization == 0.5
    assert telemetry.retry_rate == 1 / 3
    assert telemetry.goal_drift == 0.2
    assert round(telemetry.causal_drift or 0.0, 6) == 0.3
    assert telemetry.verified_progress_rate == 0.75
    assert telemetry.receipt_count == 5


def test_missing_receipt_classes_stay_unknown() -> None:
    telemetry = aggregate_receipts(())

    assert telemetry.token_utilization is None
    assert telemetry.retry_rate is None
    assert telemetry.goal_drift is None
    assert telemetry.causal_drift is None
    assert telemetry.verified_progress_rate is None
    assert telemetry.receipt_count == 0
