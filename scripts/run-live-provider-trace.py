#!/usr/bin/env python3
"""Run one fail-closed live provider trace and write a JSON artifact."""

from __future__ import annotations

import argparse
import asyncio
import hashlib
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


def _build_service(
    *,
    trace_id: str,
    model: str,
    context_window_tokens: int,
    max_output_tokens: int,
) -> InstrumentedOpenAIService:
    """Construct only the already-tested receipt boundary, avoiding legacy async __init__."""

    service = InstrumentedOpenAIService.__new__(InstrumentedOpenAIService)
    service.client = object()  # _call_openai delegates to the configured global wrapper.
    service.model = model
    service.max_tokens = max_output_tokens
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
    service._last_finish_reason = None
    return service


def _provider_settings(provider: str) -> tuple[str, str | None]:
    """Resolve provider credentials without ever reading committed dotenv files."""

    if provider == "openai":
        api_key = os.getenv("OPENAI_API_KEY", "").strip()
        if not api_key:
            raise RuntimeError("OPENAI_API_KEY is required")
        return api_key, None

    if provider == "gonka":
        api_key = (
            os.getenv("GONKA_BROKER_API_KEY", "").strip()
            or os.getenv("GONKA_API_KEY", "").strip()
        )
        base_url = (
            os.getenv("GONKA_BROKER_URL", "").strip()
            or os.getenv("GONKA_BASE_URL", "").strip()
        )
        if not api_key:
            raise RuntimeError("GONKA_BROKER_API_KEY (or GONKA_API_KEY) is required")
        if not base_url:
            raise RuntimeError("GONKA_BROKER_URL (or GONKA_BASE_URL) is required")
        return api_key, base_url.rstrip("/")

    raise RuntimeError(f"unsupported live provider: {provider}")


def _normalize_provider_content(
    provider: str, model: str, content: str
) -> tuple[str, str | None]:
    """Remove only documented provider wrappers before contract verification.

    Gonka's MiniMax-M2.7 reasoning parser emits a leading ``<think>...</think>``
    block in ``message.content``. The raw response remains hash-addressed in the
    artifact; this function only exposes a deterministic normalized view for the
    exact JSON verifier. Unknown wrappers are never guessed or stripped.
    """

    if provider != "gonka" or "minimax-m2.7" not in model.lower():
        return content, None

    candidate = content.lstrip()
    if not candidate.startswith("<think>"):
        return content, None

    closing_tag = "</think>"
    close_index = candidate.find(closing_tag)
    if close_index < 0:
        return content, "gonka:minimax_think_wrapper_unclosed"

    normalized = candidate[close_index + len(closing_tag) :].lstrip()
    return normalized, "gonka:minimax_think_wrapper_stripped"


async def _configure_live_provider(provider: str) -> None:
    """Attach a real OpenAI-compatible client to the existing receipt boundary."""

    api_key, base_url = _provider_settings(provider)

    llm_client.mock_only = False
    llm_client.fallback_to_local = False
    llm_client.debug_level = 0
    llm_client.cache_ttl = 0
    llm_client.response_cache.clear()
    llm_client.api_key = api_key

    if provider == "openai":
        initialized = await llm_client.initialize()
        if not initialized or llm_client.openai_client is None:
            raise RuntimeError("real OpenAI client initialization failed")
        return

    # Gonka community brokers expose an OpenAI-compatible API. Construct the
    # official SDK client with the broker base URL, then reuse llm_client.call()
    # and the exact same receipt/control path as the OpenAI provider.
    from openai import AsyncOpenAI

    llm_client.openai_client = AsyncOpenAI(api_key=api_key, base_url=base_url)


async def _run(args: argparse.Namespace) -> int:
    await _configure_live_provider(args.provider)

    trace_id = args.trace_id or (
        f"live-{args.provider}-"
        + datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    )
    service = _build_service(
        trace_id=trace_id,
        model=args.model,
        context_window_tokens=args.context_window_tokens,
        max_output_tokens=args.max_output_tokens,
    )

    started = time.perf_counter()
    raw_response_content = await service._call_openai(live_probe_prompt())
    latency_seconds = time.perf_counter() - started

    normalized_content, normalization_strategy = _normalize_provider_content(
        args.provider, args.model, raw_response_content
    )
    raw_strict_verification = verify_live_probe_response(raw_response_content)
    verification = verify_live_probe_response(normalized_content)

    provider_receipts = service.drain_receipts()
    token_receipts = [
        receipt for receipt in provider_receipts if isinstance(receipt, TokenUsageReceipt)
    ]
    if len(token_receipts) != 1:
        raise RuntimeError("exactly one provider token-usage receipt is required")

    token_receipt = token_receipts[0]
    if token_receipt.kind is not ReceiptKind.TOKEN_USAGE:
        raise RuntimeError("provider token receipt kind mismatch")

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
        response_content=raw_response_content,
        provider_usage=usage,
        verification=verification,
        receipts=receipts,
        decision=decision,
    )
    artifact_json = to_jsonable(artifact)
    # Keep raw and normalized evidence distinct. No provider response content is
    # persisted; only hashes and deterministic verification metadata are stored.
    artifact_json["provider"] = args.provider
    artifact_json["provider_finish_reason"] = service.last_finish_reason
    artifact_json["requested_max_output_tokens"] = args.max_output_tokens
    artifact_json["raw_strict_verification"] = to_jsonable(raw_strict_verification)
    artifact_json["normalization"] = {
        "applied": normalization_strategy == "gonka:minimax_think_wrapper_stripped",
        "strategy": normalization_strategy,
        "normalized_response_sha256": hashlib.sha256(
            normalized_content.encode("utf-8")
        ).hexdigest(),
    }
    artifact_json["observation_sources"]["provider_normalization"] = (
        "documented:gonka_minimax_m2_append_think"
        if normalization_strategy
        else "none"
    )

    output = Path(args.output)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(
        json.dumps(artifact_json, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )

    summary = {
        "trace_id": trace_id,
        "provider": args.provider,
        "model": args.model,
        "probe_mode": args.probe_mode,
        "raw_strict_verification_passed": raw_strict_verification.passed,
        "normalization_strategy": normalization_strategy,
        "verification_passed": verification.passed,
        "latency_seconds": round(latency_seconds, 6),
        "total_tokens": usage["total_tokens"],
        "finish_reason": service.last_finish_reason,
        "requested_max_output_tokens": args.max_output_tokens,
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
    parser.add_argument(
        "--provider",
        choices=("openai", "gonka"),
        default=os.getenv("LIVE_PROVIDER", "openai"),
    )
    parser.add_argument("--model", required=True)
    parser.add_argument("--context-window-tokens", required=True, type=int)
    parser.add_argument("--max-output-tokens", type=int, default=1024)
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
    if args.max_output_tokens <= 0:
        parser.error("--max-output-tokens must be positive")
    if args.latency_budget_seconds <= 0:
        parser.error("--latency-budget-seconds must be positive")

    return asyncio.run(_run(args))


if __name__ == "__main__":
    raise SystemExit(main())
