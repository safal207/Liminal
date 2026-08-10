"""Receipt-instrumented execution path for the existing LLM wrapper.

This module deliberately wraps ``OpenAIWrapper.call`` instead of modifying the
legacy wrapper. It records only facts available at the execution boundary:
provider token usage plus explicitly supplied retry/continuity/verification
facts. Missing evidence is not inferred.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Awaitable, Callable

from liminal.instrumentation_receipts import (
    ContinuityReceipt,
    LogicalRetryReceipt,
    ReceiptKind,
    TokenUsageReceipt,
    VerifiedProgressReceipt,
    utc_now,
)

from .openai_wrapper import LLMRequest, LLMResponse, llm_client


@dataclass(frozen=True)
class InstrumentedLLMResult:
    response: LLMResponse
    receipts: tuple[object, ...]


def _usage_value(usage: dict[str, int] | None, *names: str) -> int:
    usage = usage or {}
    for name in names:
        value = usage.get(name)
        if value is not None:
            return max(0, int(value))
    return 0


async def call_with_receipts(
    request: LLMRequest,
    *,
    trace_id: str,
    step_id: str,
    logical_action_id: str,
    context_window_tokens: int | None = None,
    retry_index: int = 0,
    retry_reason_code: str | None = None,
    goal_continuity_score: float | None = None,
    goal_evidence_refs: tuple[str, ...] = (),
    causal_continuity_score: float | None = None,
    causal_evidence_refs: tuple[str, ...] = (),
    completed_units: int | None = None,
    expected_units: int | None = None,
    verification_refs: tuple[str, ...] = (),
    caller: Callable[[LLMRequest], Awaitable[LLMResponse]] | None = None,
) -> InstrumentedLLMResult:
    """Execute one existing LLM-wrapper call and emit evidence receipts.

    ``caller`` is injectable for deterministic tests. Production callers omit it
    and use the repository's existing global ``llm_client.call`` path.
    """

    if not trace_id:
        raise ValueError("trace_id_required")
    if not step_id:
        raise ValueError("step_id_required")
    if not logical_action_id:
        raise ValueError("logical_action_id_required")
    if retry_index < 0:
        raise ValueError("retry_index_must_be_non_negative")
    if retry_index > 0 and not retry_reason_code:
        raise ValueError("retry_reason_code_required_for_retry")
    if (completed_units is None) != (expected_units is None):
        raise ValueError("completed_and_expected_units_must_be_supplied_together")

    execute = caller or llm_client.call
    response = await execute(request)
    observed_at = utc_now()
    source = "backend.ml.openai_wrapper.OpenAIWrapper.call"
    receipts: list[object] = []

    input_tokens = _usage_value(response.usage, "prompt_tokens", "input_tokens")
    output_tokens = _usage_value(response.usage, "completion_tokens", "output_tokens")
    receipts.append(
        TokenUsageReceipt(
            receipt_id=f"{trace_id}:{step_id}:token_usage",
            trace_id=trace_id,
            step_id=step_id,
            kind=ReceiptKind.TOKEN_USAGE,
            observed_at=observed_at,
            source=source,
            input_tokens=input_tokens,
            output_tokens=output_tokens,
            context_window_tokens=context_window_tokens,
        )
    )

    if retry_index > 0:
        receipts.append(
            LogicalRetryReceipt(
                receipt_id=f"{trace_id}:{step_id}:retry:{retry_index}",
                trace_id=trace_id,
                step_id=step_id,
                kind=ReceiptKind.LOGICAL_RETRY,
                observed_at=observed_at,
                source=source,
                logical_action_id=logical_action_id,
                retry_index=retry_index,
                reason_code=retry_reason_code or "unknown",
            )
        )

    if goal_continuity_score is not None:
        receipts.append(
            ContinuityReceipt(
                receipt_id=f"{trace_id}:{step_id}:goal_continuity",
                trace_id=trace_id,
                step_id=step_id,
                kind=ReceiptKind.GOAL_CONTINUITY,
                observed_at=observed_at,
                source=source,
                continuity_score=goal_continuity_score,
                evidence_refs=goal_evidence_refs,
            )
        )

    if causal_continuity_score is not None:
        receipts.append(
            ContinuityReceipt(
                receipt_id=f"{trace_id}:{step_id}:causal_continuity",
                trace_id=trace_id,
                step_id=step_id,
                kind=ReceiptKind.CAUSAL_CONTINUITY,
                observed_at=observed_at,
                source=source,
                continuity_score=causal_continuity_score,
                evidence_refs=causal_evidence_refs,
            )
        )

    if completed_units is not None and expected_units is not None:
        receipts.append(
            VerifiedProgressReceipt(
                receipt_id=f"{trace_id}:{step_id}:verified_progress",
                trace_id=trace_id,
                step_id=step_id,
                kind=ReceiptKind.VERIFIED_PROGRESS,
                observed_at=observed_at,
                source=source,
                completed_units=completed_units,
                expected_units=expected_units,
                verification_refs=verification_refs,
            )
        )

    return InstrumentedLLMResult(response=response, receipts=tuple(receipts))
