from collections import defaultdict

import pytest

from backend.ml.instrumented_openai_service import InstrumentedOpenAIService
from backend.ml.openai_wrapper import LLMResponse
from backend.ml.receipt_instrumented_client import InstrumentedLLMResult
from liminal.end_to_end_trace import (
    TraceRuntimeObservations,
    evaluate_end_to_end_trace,
)
from liminal.flow_regulator import FlowState
from liminal.instrumentation_receipts import (
    ContinuityReceipt,
    ReceiptKind,
    TokenUsageReceipt,
    VerifiedProgressReceipt,
    utc_now,
)
from liminal.recovery_policy import RecoveryMode


def _service() -> InstrumentedOpenAIService:
    service = InstrumentedOpenAIService.__new__(InstrumentedOpenAIService)
    service.client = object()
    service.model = "test-model"
    service.max_tokens = 512
    service.temperature = 0.0
    service.system_context = "system"
    service.response_cache = {}
    service.cache_ttl = 3600
    service.trace_id = "trace-e2e"
    service.context_window_tokens = 4096
    service._receipt_sequence = 0
    service._receipts = []
    service._logical_attempts = defaultdict(int)
    return service


def _continuity_and_progress_receipts():
    now = utc_now()
    return (
        ContinuityReceipt(
            receipt_id="trace-e2e:goal",
            trace_id="trace-e2e",
            step_id="verify-1",
            kind=ReceiptKind.GOAL_CONTINUITY,
            observed_at=now,
            source="test.verifier",
            continuity_score=0.52,
            evidence_refs=("goal:continuation-token",),
        ),
        ContinuityReceipt(
            receipt_id="trace-e2e:causal",
            trace_id="trace-e2e",
            step_id="verify-1",
            kind=ReceiptKind.CAUSAL_CONTINUITY,
            observed_at=now,
            source="test.verifier",
            continuity_score=0.80,
            evidence_refs=("causal:decision-receipt",),
        ),
        VerifiedProgressReceipt(
            receipt_id="trace-e2e:progress",
            trace_id="trace-e2e",
            step_id="verify-1",
            kind=ReceiptKind.VERIFIED_PROGRESS,
            observed_at=now,
            source="test.verifier",
            completed_units=2,
            expected_units=4,
            verification_refs=("check:analysis-json",),
        ),
    )


def _observations() -> TraceRuntimeObservations:
    return TraceRuntimeObservations(
        tool_failure_rate=0.05,
        latency_pressure=0.20,
        context_pressure=0.20,
        feedback_success_rate=0.95,
        interruption_rate=0.10,
        recent_recovery_rate=0.20,
        task_difficulty=0.70,
        available_capability=0.68,
        replay_steps_estimate=12,
        field_candidate_count=3,
        best_anchor_score=0.82,
        field_uncertainty=0.10,
        verified_candidate_available=True,
        require_verified=True,
        measured_field_cost=3,
    )


@pytest.mark.asyncio
async def test_application_call_to_receipts_to_flow_to_focus_field(monkeypatch):
    service = _service()

    async def fake_call_with_receipts(request, **kwargs):
        receipt = TokenUsageReceipt(
            receipt_id="trace-e2e:llm-1:token_usage",
            trace_id="trace-e2e",
            step_id="llm-1",
            kind=ReceiptKind.TOKEN_USAGE,
            observed_at=utc_now(),
            source="backend.ml.openai_wrapper.OpenAIWrapper.call",
            input_tokens=800,
            output_tokens=200,
            context_window_tokens=4096,
        )
        return InstrumentedLLMResult(
            response=LLMResponse(
                content='{"analysis":"ok","recommendations":[],"severity":"low","action_items":[],"summary":"ok"}',
                model=request.model,
                usage={"prompt_tokens": 800, "completion_tokens": 200},
            ),
            receipts=(receipt,),
        )

    monkeypatch.setattr(
        "backend.ml.instrumented_openai_service.call_with_receipts",
        fake_call_with_receipts,
    )

    result = await service._get_analysis("inspect this trace", "anomaly")
    assert result.analysis == "ok"

    service.add_receipts(_continuity_and_progress_receipts())
    decision = evaluate_end_to_end_trace(service.receipts, _observations())

    assert decision.trace_id == "trace-e2e"
    assert decision.verification_evidence_present is True
    assert decision.receipt_telemetry.token_utilization == pytest.approx(1000 / 4096)
    assert decision.receipt_telemetry.goal_drift == pytest.approx(0.48)
    assert decision.receipt_telemetry.causal_drift == pytest.approx(0.20)
    assert decision.receipt_telemetry.verified_progress_rate == pytest.approx(0.50)
    assert decision.flow.state is FlowState.RECOVERY
    assert decision.flow.reason == "goal_clarity_below_flow_corridor"
    assert decision.recovery is not None
    assert decision.recovery.mode is RecoveryMode.FOCUS_FIELD
    assert decision.recovery.estimated_savings_ratio == pytest.approx(0.75)


def test_incomplete_trace_fails_closed_without_verification_evidence():
    token = TokenUsageReceipt(
        receipt_id="trace-e2e:llm-1:token_usage",
        trace_id="trace-e2e",
        step_id="llm-1",
        kind=ReceiptKind.TOKEN_USAGE,
        observed_at=utc_now(),
        source="test",
        input_tokens=10,
        output_tokens=5,
        context_window_tokens=100,
    )

    with pytest.raises(ValueError, match="goal_continuity_evidence_required"):
        evaluate_end_to_end_trace((token,), _observations())


def test_mixed_trace_ids_are_rejected():
    now = utc_now()
    receipts = (
        TokenUsageReceipt(
            receipt_id="a:token",
            trace_id="trace-a",
            step_id="s1",
            kind=ReceiptKind.TOKEN_USAGE,
            observed_at=now,
            source="test",
            input_tokens=1,
            output_tokens=1,
            context_window_tokens=10,
        ),
        ContinuityReceipt(
            receipt_id="b:goal",
            trace_id="trace-b",
            step_id="s2",
            kind=ReceiptKind.GOAL_CONTINUITY,
            observed_at=now,
            source="test",
            continuity_score=1.0,
            evidence_refs=("goal:evidence",),
        ),
    )

    with pytest.raises(ValueError, match="mixed_trace_ids_not_allowed"):
        evaluate_end_to_end_trace(receipts, _observations())
