import pytest

from backend.ml.openai_wrapper import LLMRequest, LLMResponse
from backend.ml.receipt_instrumented_client import call_with_receipts
from liminal.instrumentation_receipts import (
    ContinuityReceipt,
    LogicalRetryReceipt,
    ReceiptKind,
    TokenUsageReceipt,
    VerifiedProgressReceipt,
)


@pytest.mark.asyncio
async def test_real_wrapper_boundary_emits_provider_usage_and_explicit_evidence() -> None:
    async def fake_call(request: LLMRequest) -> LLMResponse:
        assert request.model == "test-model"
        return LLMResponse(
            content="ok",
            model=request.model,
            usage={"prompt_tokens": 120, "completion_tokens": 30},
        )

    result = await call_with_receipts(
        LLMRequest(model="test-model", messages=[{"role": "user", "content": "hi"}]),
        trace_id="trace-1",
        step_id="step-1",
        logical_action_id="action-1",
        context_window_tokens=1000,
        retry_index=1,
        retry_reason_code="provider_timeout",
        goal_continuity_score=0.9,
        goal_evidence_refs=("goal:1",),
        causal_continuity_score=0.8,
        causal_evidence_refs=("cause:1",),
        completed_units=3,
        expected_units=4,
        verification_refs=("test:green",),
        caller=fake_call,
    )

    assert result.response.content == "ok"
    assert len(result.receipts) == 5

    token = next(r for r in result.receipts if isinstance(r, TokenUsageReceipt))
    assert token.input_tokens == 120
    assert token.output_tokens == 30
    assert token.context_window_tokens == 1000

    retry = next(r for r in result.receipts if isinstance(r, LogicalRetryReceipt))
    assert retry.retry_index == 1
    assert retry.reason_code == "provider_timeout"

    continuity = [r for r in result.receipts if isinstance(r, ContinuityReceipt)]
    assert {r.kind for r in continuity} == {
        ReceiptKind.GOAL_CONTINUITY,
        ReceiptKind.CAUSAL_CONTINUITY,
    }

    progress = next(r for r in result.receipts if isinstance(r, VerifiedProgressReceipt))
    assert progress.progress_ratio == 0.75


@pytest.mark.asyncio
async def test_missing_optional_evidence_is_not_invented() -> None:
    async def fake_call(request: LLMRequest) -> LLMResponse:
        return LLMResponse(content="ok", model=request.model, usage=None)

    result = await call_with_receipts(
        LLMRequest(model="test-model", messages=[]),
        trace_id="trace-2",
        step_id="step-2",
        logical_action_id="action-2",
        caller=fake_call,
    )

    assert len(result.receipts) == 1
    token = result.receipts[0]
    assert isinstance(token, TokenUsageReceipt)
    assert token.input_tokens == 0
    assert token.output_tokens == 0


@pytest.mark.asyncio
async def test_retry_requires_reason_code() -> None:
    async def fake_call(request: LLMRequest) -> LLMResponse:
        return LLMResponse(content="ok", model=request.model)

    with pytest.raises(ValueError, match="retry_reason_code_required_for_retry"):
        await call_with_receipts(
            LLMRequest(model="test-model", messages=[]),
            trace_id="trace-3",
            step_id="step-3",
            logical_action_id="action-3",
            retry_index=1,
            caller=fake_call,
        )
