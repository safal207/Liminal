from collections import defaultdict

import pytest

from backend.ml.instrumented_openai_service import InstrumentedOpenAIService
from backend.ml.openai_wrapper import LLMResponse
from backend.ml.receipt_instrumented_client import InstrumentedLLMResult
from liminal.instrumentation_receipts import ReceiptKind, TokenUsageReceipt, utc_now


def _service() -> InstrumentedOpenAIService:
    service = InstrumentedOpenAIService.__new__(InstrumentedOpenAIService)
    service.client = object()
    service.model = "test-model"
    service.max_tokens = 256
    service.temperature = 0.0
    service.system_context = "system"
    service.response_cache = {}
    service.cache_ttl = 3600
    service.trace_id = "trace-live"
    service.context_window_tokens = 4096
    service._receipt_sequence = 0
    service._receipts = []
    service._logical_attempts = defaultdict(int)
    return service


@pytest.mark.asyncio
async def test_inherited_get_analysis_emits_token_receipt(monkeypatch):
    service = _service()

    async def fake_call_with_receipts(request, **kwargs):
        receipt = TokenUsageReceipt(
            receipt_id="trace-live:llm-1:token_usage",
            trace_id="trace-live",
            step_id="llm-1",
            kind=ReceiptKind.TOKEN_USAGE,
            observed_at=utc_now(),
            source="test",
            input_tokens=120,
            output_tokens=30,
            context_window_tokens=4096,
        )
        return InstrumentedLLMResult(
            response=LLMResponse(
                content='{"analysis":"ok","recommendations":[],"severity":"low","action_items":[],"summary":"ok"}',
                model=request.model,
                usage={"prompt_tokens": 120, "completion_tokens": 30},
            ),
            receipts=(receipt,),
        )

    monkeypatch.setattr(
        "backend.ml.instrumented_openai_service.call_with_receipts",
        fake_call_with_receipts,
    )

    result = await service._get_analysis("inspect this", "anomaly")

    assert result.analysis == "ok"
    assert len(service.receipts) == 1
    assert service.receipts[0].kind is ReceiptKind.TOKEN_USAGE
    assert service.receipts[0].input_tokens == 120
    assert service.receipts[0].output_tokens == 30


@pytest.mark.asyncio
async def test_repeated_logical_action_is_marked_as_retry(monkeypatch):
    service = _service()
    calls = []

    async def fake_call_with_receipts(request, **kwargs):
        calls.append(kwargs)
        return InstrumentedLLMResult(
            response=LLMResponse(content="ok", model=request.model, usage={}),
            receipts=(),
        )

    monkeypatch.setattr(
        "backend.ml.instrumented_openai_service.call_with_receipts",
        fake_call_with_receipts,
    )

    await service._call_openai("same prompt")
    await service._call_openai("same prompt")

    assert calls[0]["retry_index"] == 0
    assert calls[0]["retry_reason_code"] is None
    assert calls[1]["retry_index"] == 1
    assert calls[1]["retry_reason_code"] == "repeat_logical_action"


def test_drain_receipts_is_append_only_then_clear():
    service = _service()
    receipt = TokenUsageReceipt(
        receipt_id="r1",
        trace_id="trace-live",
        step_id="s1",
        kind=ReceiptKind.TOKEN_USAGE,
        observed_at=utc_now(),
        source="test",
        input_tokens=1,
        output_tokens=2,
    )
    service.add_receipts((receipt,))

    drained = service.drain_receipts()

    assert drained == (receipt,)
    assert service.receipts == ()
