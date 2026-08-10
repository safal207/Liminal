#!/usr/bin/env python3
"""Run one fail-closed live OpenAI provider trace and write a JSON artifact."""

from __future__ import annotations

import argparse
import asyncio
import json
import os
import sys
import time
from collections import defaultdict
from datetime import datetime, timezone
from pathlib import Path

from backend.ml.instrumented_openai_service import InstrumentedOpenAIService
from backend.ml.openai_wrapper import llm_client
from liminal.instrumentation_receipts import ReceiptKind, TokenUsageReceipt
from liminal.live_provider_trace import (
    evaluate_live_provider_trace,
    live_probe_prompt,
    make_live_provider_artifact,
    to_jsonable,
    verification_receipts,
    verify_live_probe_response,
)


def _build_service(*, trace_id: str, model: str, context_window_tokens: int) -> InstrumentedOpenAIService:
    """Construct only the already-tested receipt boundary, avoiding legacy async __init__."""

    service = InstrumentedOpenAIService.__new__(InstrumentedOpenAIService)
    service.client = object()  # _call_openai delegates to the initialized global wrapper.
    service.model = model
    service.max_tokens = 160
    service.temperature = 0.0
    service.system_context = (
        "You are a deterministic runtime trace probe. Follow the JSON contract exactly."
    )
    service.response_cache = {}
    service.cache_ttl = 0
    service.trace_id = trace_id
    service.context_window_tokens = context_window_tokens
    service._receipt_sequence = 0
    service._receipts = []
    service._logical_attempts = defaultdict(int)
    return service


async def _run(args: argparse.Namespace) -> int:
    api_key = os.getenv("OPENAI_API_KEY", "").strip()
    if not api_key:
        raise RuntimeError("OPENAI_API_KEY is required; committed .env files are not used")

    llm_client.mock_only = False
    llm_client.fallback_to_local = False
    llm_client.debug_level = 0
    llm_client.cache_ttl = 0
    llm_client.response_cache.clear()
    llm_client.api_key = api_key

    initialized = await llm_client.initialize()
    if not initialized or llm_client.openai_client is None:
        raise RuntimeError("real OpenAI client initialization failed")

    trace_id = args.trace_id or (
        "live-openai-" + datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    )
    service = _build_service(
        trace_id=trace_id,
        model=args.model,
        context_window_tokens=args.context_window_tokens,
    )

    started = time.perf_counter()
    response_content = await service._call_openai(live_probe_prompt())
    latency_seconds = time.perf_counter() - started

    provider_receipts = service.drain_receipts()
    token_receipts = [
        receipt for receipt in provider_receipts if isinstance(receipt, TokenUsageReceipt)
    ]
    if len(token_receipts) != 1:
        raise RuntimeError("exactly one provider token-usage receipt is required")

    token_receipt = token_receipts[0]
    if token_receipt.kind is not ReceiptKind.TOKEN_USAGE:
        raise RuntimeError("provider token receipt kind mismatch")

    verification = verify_live_probe_response(response_content)
    evidence_receipts = verification_receipts(
        trace_id=trace_id,
        step_id=token_receipt.step_id,
        verification=verification,
    )
    receipts = provider_receipts + evidence_receipts

    decision = evaluate_live_provider_trace(
        receipts,
        latency_seconds=latency_seconds,
        latency_budget_seconds=args.latency_budget_seconds,
        verification=verification,
        probe_mode=args.probe_mode,
    )

    usage = {
        "prompt_tokens": token_receipt.input_tokens,
        "completion_tokens": token_receipt.output_tokens,
        "total_tokens": token_receipt.input_tokens + token_receipt.output_tokens,
    }
    artifact = make_live_provider_artifact(
        trace_id=trace_id,
        model=args.model,
        probe_mode=args.probe_mode,
        latency_seconds=latency_seconds,
        response_content=response_content,
        provider_usage=usage,
        verification=verification,
        receipts=receipts,
        decision=decision,
    )

    output = Path(args.output)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(
        json.dumps(to_jsonable(artifact), indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )

    summary = {
        "trace_id": trace_id,
        "model": args.model,
        "probe_mode": args.probe_mode,
        "verification_passed": verification.passed,
        "latency_seconds": round(latency_seconds, 6),
        "total_tokens": usage["total_tokens"],
        "flow_state": decision.flow.state.value,
        "recovery_mode": decision.recovery.mode.value if decision.recovery else None,
        "artifact": str(output),
    }
    print(json.dumps(summary, sort_keys=True))

    if not verification.passed:
        print("live provider response failed deterministic verification", file=sys.stderr)
        return 2
    return 0


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--model", required=True)
    parser.add_argument("--context-window-tokens", required=True, type=int)
    parser.add_argument("--latency-budget-seconds", type=float, default=10.0)
    parser.add_argument(
        "--probe-mode",
        choices=("nominal", "induced-recovery"),
        default="nominal",
    )
    parser.add_argument("--trace-id")
    parser.add_argument(
        "--output",
        default="artifacts/live-provider-trace.json",
    )
    args = parser.parse_args()

    if args.context_window_tokens <= 0:
        parser.error("--context-window-tokens must be positive")
    if args.latency_budget_seconds <= 0:
        parser.error("--latency-budget-seconds must be positive")

    return asyncio.run(_run(args))


if __name__ == "__main__":
    raise SystemExit(main())
