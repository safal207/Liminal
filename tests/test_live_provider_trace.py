import hashlib
from collections import defaultdict

import pytest

from backend.ml.instrumented_openai_service import InstrumentedOpenAIService
from backend.ml.openai_wrapper import LLMResponse
from backend.ml.receipt_instrumented_client import InstrumentedLLMResult
from liminal.flow_regulator import FlowState
from liminal.instrumentation_receipts import ReceiptKind, TokenUsageReceipt, utc_now
from liminal.live_provider_trace import (
    LIVE_PROBE_GOAL_ID,
    LIVE_PROBE_PARENT_STEP_ID,
    evaluate_live_provider_trace,
    live_probe_prompt,
    verification_receipts,
    verify_live_probe_response,
)
from liminal.recovery_policy import RecoveryMode


def _provider_receipt(trace_id: str = "live-test") -> TokenUsageReceipt:
    return TokenUsageReceipt(
        receipt_id=f"{trace_id}:llm-1:token_usage",
        trace_id=trace_id,
        step_id="llm-1",
        kind=ReceiptKind.TOKEN_USAGE,
        observed_at=utc_now(),
        source="test-live-provider",
        input_tokens=100,
        output_tokens=20,
        context_window_tokens=4096,
    )


def _verified_receipts(trace_id: str = "live-test"):
    content = (
        '{"goal_id":"%s","parent_step_id":"%s",'
        '"status":"verified","evidence":"provider-response"}'
        % (LIVE_PROBE_GOAL_ID, LIVE_PROBE_PARENT_STEP_ID)
    )
    verification = verify_live_probe_response(content)
    receipts = (_provider_receipt(trace_id),) + verification_receipts(
        trace_id=trace_id,
        step_id="llm-1",
        verification=verification,
    )
    return verification, receipts


def test_live_probe_contract_is_deterministically_verified():
    prompt = live_probe_prompt()
    assert LIVE_PROBE_GOAL_ID in prompt
    assert LIVE_PROBE_PARENT_STEP_ID in prompt

    verification, _ = _verified_receipts()
    assert verification.passed is True


def test_live_probe_rejects_extra_keys():
    content = (
        '{"goal_id":"%s","parent_step_id":"%s",'
        '"status":"verified","evidence":"provider-response","extra":true}'
        % (LIVE_PROBE_GOAL_ID, LIVE_PROBE_PARENT_STEP_ID)
    )
    verification = verify_live_probe_response(content)
    assert verification.exact_key_set is False
    assert verification.passed is False


def test_nominal_live_provider_trace_stays_in_flow():
    verification, receipts = _verified_receipts()
    decision = evaluate_live_provider_trace(
        receipts,
        latency_seconds=0.5,
        latency_budget_seconds=10.0,
        verification=verification,
        probe_mode="nominal",
    )

    assert decision.flow.state is FlowState.FLOW
    assert decision.recovery is None
    assert decision.verification_evidence_present is True


def test_induced_recovery_uses_real_control_path_and_focus_field():
    verification, receipts = _verified_receipts()
    decision = evaluate_live_provider_trace(
        receipts,
        latency_seconds=0.5,
        latency_budget_seconds=10.0,
        verification=verification,
        probe_mode="induced-recovery",
    )

    assert decision.flow.state is FlowState.RECOVERY
    assert decision.recovery is not None
    assert decision.recovery.mode is RecoveryMode.FOCUS_FIELD
    assert decision.recovery.estimated_savings_ratio == 0.75


@pytest.mark.asyncio
async def test_logical_action_id_uses_stable_sha256(monkeypatch):
    service = InstrumentedOpenAIService.__new__(InstrumentedOpenAIService)
    service.client = object()
    service.model = "test-model"
    service.max_tokens = 64
    service.temperature = 0.0
    service.system_context = "system"
    service.trace_id = "stable-trace"
    service.context_window_tokens = 4096
    service._receipt_sequence = 0
    service._receipts = []
    service._logical_attempts = defaultdict(int)
    calls = []

    async def fake_call_with_receipts(request, **kwargs):
        calls.append(kwargs)
        return InstrumentedLLMResult(
            response=LLMResponse(content="{}", model=request.model, usage={}),
            receipts=(),
        )

    monkeypatch.setattr(
        "backend.ml.instrumented_openai_service.call_with_receipts",
        fake_call_with_receipts,
    )

    prompt = "stable prompt"
    await service._call_openai(prompt)

    expected = hashlib.sha256(prompt.encode("utf-8")).hexdigest()
    assert calls[0]["logical_action_id"] == f"analysis:{expected}"
