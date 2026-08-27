"""Opt-in receipt instrumentation for a real OpenAIService application path.

This subclass preserves the legacy OpenAIService business logic while replacing
only the LLM execution boundary. Calls inherited from ``OpenAIService`` (for
example anomaly analysis, incident response, and ML explanation) flow through
``_call_openai`` and therefore emit runtime evidence receipts automatically.
"""

from __future__ import annotations

import hashlib
from collections import defaultdict
from typing import Iterable

from liminal.instrumentation_receipts import InstrumentationReceipt

from .openai_service import OpenAIService
from .openai_wrapper import LLMRequest
from .receipt_instrumented_client import call_with_receipts


class InstrumentedOpenAIService(OpenAIService):
    """OpenAIService variant that emits receipts at the real LLM boundary."""

    def __init__(
        self,
        api_key: str | None = None,
        *,
        trace_id: str = "openai-service",
        context_window_tokens: int | None = None,
    ) -> None:
        super().__init__(api_key=api_key)
        if not trace_id:
            raise ValueError("trace_id_required")
        self.trace_id = trace_id
        self.context_window_tokens = context_window_tokens
        self._receipt_sequence = 0
        self._receipts: list[InstrumentationReceipt] = []
        self._logical_attempts: defaultdict[str, int] = defaultdict(int)
        self._last_finish_reason: str | None = None
        self.response_format: dict | None = None

    @property
    def receipts(self) -> tuple[InstrumentationReceipt, ...]:
        return tuple(self._receipts)

    @property
    def last_finish_reason(self) -> str | None:
        """Return provider finish metadata without retaining response content."""

        return getattr(self, "_last_finish_reason", None)

    def drain_receipts(self) -> tuple[InstrumentationReceipt, ...]:
        receipts = tuple(self._receipts)
        self._receipts.clear()
        return receipts

    async def _call_openai(self, prompt: str) -> str:
        """Execute the inherited application flow through the receipt boundary."""

        if not self.client:
            raise Exception("OpenAI клиент не инициализирован")

        self._receipt_sequence += 1
        step_id = f"llm-{self._receipt_sequence}"
        prompt_digest = hashlib.sha256(prompt.encode("utf-8")).hexdigest()
        logical_action_id = f"analysis:{prompt_digest}"
        self._logical_attempts[logical_action_id] += 1
        attempt = self._logical_attempts[logical_action_id]

        response_format = getattr(self, "response_format", None) or {
            "type": "json_object"
        }
        request = LLMRequest(
            model=self.model,
            messages=[
                {"role": "system", "content": self.system_context},
                {"role": "user", "content": prompt},
            ],
            max_tokens=self.max_tokens,
            temperature=self.temperature,
            response_format=response_format,
        )

        result = await call_with_receipts(
            request,
            trace_id=self.trace_id,
            step_id=step_id,
            logical_action_id=logical_action_id,
            context_window_tokens=self.context_window_tokens,
            retry_index=max(0, attempt - 1),
            retry_reason_code="repeat_logical_action" if attempt > 1 else None,
        )
        self._last_finish_reason = result.response.finish_reason
        self._receipts.extend(result.receipts)
        return result.response.content

    def add_receipts(self, receipts: Iterable[InstrumentationReceipt]) -> None:
        """Attach independently verified continuity/progress receipts to the trace."""

        self._receipts.extend(receipts)
