#!/usr/bin/env python3
"""Run a small real-provider sequential-vs-focus-field recovery A/B probe."""

from __future__ import annotations

import argparse
import asyncio
import hashlib
import json
import os
import sys
import time
from datetime import datetime, timezone
from pathlib import Path

from backend.ml.openai_wrapper import LLMRequest, llm_client
from backend.ml.receipt_instrumented_client import call_with_receipts
from liminal.instrumentation_receipts import TokenUsageReceipt
from liminal.live_recovery_ab import (
    EXPECTED_CHECKPOINT_ID,
    EXPECTED_GOAL_ID,
    EXPECTED_PARENT_STEP_ID,
    focus_field_context,
    parse_recovery_payload,
    recovery_prompt,
    recovery_response_format,
    sequential_context,
    summarize_records,
    verify_recovery_response,
)
from liminal.recovery_evidence import RecoveryEvidenceWindow
from liminal.recovery_policy import RecoveryMode


SYSTEM_MESSAGE = (
    "You are a deterministic recovery verifier. Select only an evidence-backed "
    "continuation anchor from the supplied context and follow the JSON schema exactly."
)
RECOVERY_CLASS = "live-ab:deep-ledger-recovery"


def _gonka_settings() -> tuple[str, str]:
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


def _normalize_minimax_content(model: str, content: str) -> tuple[str, str | None]:
    """Strip only the known MiniMax reasoning wrapper used by the Gonka path."""

    if "minimax-m2.7" not in model.lower():
        return content, None

    candidate = content.lstrip()
    if not candidate.startswith("<think>"):
        return content, None

    closing_tag = "</think>"
    close_index = candidate.find(closing_tag)
    if close_index < 0:
        return content, "gonka:minimax_think_wrapper_unclosed"

    return (
        candidate[close_index + len(closing_tag) :].lstrip(),
        "gonka:minimax_think_wrapper_stripped",
    )


async def _configure_gonka() -> None:
    api_key, base_url = _gonka_settings()
    from openai import AsyncOpenAI

    llm_client.mock_only = False
    llm_client.fallback_to_local = False
    llm_client.debug_level = 0
    llm_client.cache_ttl = 0
    llm_client.response_cache.clear()
    llm_client.api_key = api_key
    llm_client.openai_client = AsyncOpenAI(api_key=api_key, base_url=base_url)


async def _run_one(
    *,
    model: str,
    context_window_tokens: int,
    max_output_tokens: int,
    benchmark_id: str,
    trial: int,
    position: int,
    mode: str,
    probe_nonce: str,
) -> dict:
    prompt = recovery_prompt(mode, probe_nonce=probe_nonce)
    request = LLMRequest(
        model=model,
        messages=[
            {"role": "system", "content": SYSTEM_MESSAGE},
            {"role": "user", "content": prompt},
        ],
        max_tokens=max_output_tokens,
        temperature=0.0,
        response_format=recovery_response_format(),
    )

    # Cache is deliberately cleared before every arm. A unique shared pair nonce
    # also changes the upstream request so a broker-side response cache cannot
    # collapse later trials into near-zero-latency repeats.
    llm_client.response_cache.clear()
    trace_id = f"{benchmark_id}:trial-{trial}:{mode}"
    started = time.perf_counter()
    result = await call_with_receipts(
        request,
        trace_id=trace_id,
        step_id=f"provider-{mode}",
        logical_action_id=f"live-recovery-ab:{trial}:{mode}",
        context_window_tokens=context_window_tokens,
    )
    latency_seconds = time.perf_counter() - started

    token_receipts = [
        receipt for receipt in result.receipts if isinstance(receipt, TokenUsageReceipt)
    ]
    if len(token_receipts) != 1:
        raise RuntimeError("exactly one token usage receipt required per A/B arm")
    token_receipt = token_receipts[0]

    raw_content = result.response.content
    normalized_content, normalization_strategy = _normalize_minimax_content(
        model, raw_content
    )
    verification = verify_recovery_response(normalized_content)
    structured_output = parse_recovery_payload(normalized_content)

    return {
        "trial": trial,
        "position_in_pair": position,
        "mode": mode,
        "probe_nonce_sha256": hashlib.sha256(probe_nonce.encode("utf-8")).hexdigest(),
        "verification_passed": verification.passed,
        "verification": {
            "valid_json": verification.valid_json,
            "exact_key_set": verification.exact_key_set,
            "goal_match": verification.goal_match,
            "parent_match": verification.parent_match,
            "status_match": verification.status_match,
            "evidence_match": verification.evidence_match,
        },
        # This is only the four-field synthetic structured output. Provider
        # reasoning / <think> text is never persisted.
        "structured_output": structured_output,
        "prompt_chars": len(prompt),
        "prompt_sha256": hashlib.sha256(prompt.encode("utf-8")).hexdigest(),
        "raw_response_sha256": hashlib.sha256(raw_content.encode("utf-8")).hexdigest(),
        "normalized_response_sha256": hashlib.sha256(
            normalized_content.encode("utf-8")
        ).hexdigest(),
        "normalization_strategy": normalization_strategy,
        "finish_reason": result.response.finish_reason,
        "prompt_tokens": token_receipt.input_tokens,
        "completion_tokens": token_receipt.output_tokens,
        "total_tokens": token_receipt.input_tokens + token_receipt.output_tokens,
        "latency_seconds": round(latency_seconds, 6),
    }


async def _run(args: argparse.Namespace) -> int:
    await _configure_gonka()
    benchmark_id = (
        "live-recovery-ab-" + datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    )

    # Alternate arm order to reduce first-call/warm-path bias in this small probe.
    pair_orders = (
        ("sequential", "focus_field"),
        ("focus_field", "sequential"),
        ("sequential", "focus_field"),
    )

    records: list[dict] = []
    evidence_window = RecoveryEvidenceWindow(max_attempts_per_class=20)
    for trial, order in enumerate(pair_orders, start=1):
        # Same nonce inside a pair = equal nuisance input; unique across pairs =
        # a different upstream request for cache resistance.
        probe_nonce = hashlib.sha256(
            f"{benchmark_id}:pair:{trial}".encode("utf-8")
        ).hexdigest()[:16]
        for position, mode in enumerate(order, start=1):
            record = await _run_one(
                model=args.model,
                context_window_tokens=args.context_window_tokens,
                max_output_tokens=args.max_output_tokens,
                benchmark_id=benchmark_id,
                trial=trial,
                position=position,
                mode=mode,
                probe_nonce=probe_nonce,
            )
            records.append(record)
            evidence_window.record_outcome(
                recovery_class=RECOVERY_CLASS,
                mode=RecoveryMode(mode),
                verification_passed=bool(record["verification_passed"]),
                finish_reason=record["finish_reason"],
            )
            await asyncio.sleep(args.inter_call_delay_seconds)

    aggregate = summarize_records(records)
    field_evidence = evidence_window.summarize_field(recovery_class=RECOVERY_CLASS)
    artifact = {
        "schema_version": "liminal.live-recovery-ab.v0.4",
        "benchmark_id": benchmark_id,
        "provider": "gonka",
        "model": args.model,
        "design": {
            "paired_trials": 3,
            "pair_orders": [list(order) for order in pair_orders],
            "pair_nonce": "unique_per_pair_and_shared_between_arms",
            "same_recovery_rule": True,
            "sequential_checkpoint_count": 12,
            "focus_field_candidate_count": 3,
            "field_candidates_source": "configured:deterministic_subset_of_same_fixture_history",
            "field_candidate_ranking": "derived:verification_plus_lifecycle_evidence",
            "response_schema_leaks_expected_anchor": False,
            "raw_model_reasoning_persisted": False,
            "structured_synthetic_output_persisted": True,
            "max_output_tokens": args.max_output_tokens,
            "evidence_window_changes_ab_arm_schedule": False,
        },
        "expected_anchor": {
            "checkpoint_id": EXPECTED_CHECKPOINT_ID,
            "goal_id": EXPECTED_GOAL_ID,
            "parent_step_id": EXPECTED_PARENT_STEP_ID,
        },
        "context": {
            "sequential_chars_without_nonce": len(sequential_context()),
            "focus_field_chars_without_nonce": len(focus_field_context()),
            "sequential_sha256": hashlib.sha256(
                sequential_context().encode("utf-8")
            ).hexdigest(),
            "focus_field_sha256": hashlib.sha256(
                focus_field_context().encode("utf-8")
            ).hexdigest(),
        },
        "records": records,
        "aggregate": aggregate,
        "recovery_evidence_window": {
            "recovery_class": field_evidence.recovery_class,
            "retained_attempts": len(
                evidence_window.attempts(recovery_class=RECOVERY_CLASS)
            ),
            "field_observation_count": field_evidence.observation_count,
            "field_verification_success_rate": field_evidence.verification_success_rate,
            "field_completion_pressure": field_evidence.completion_pressure,
            "source": "observed_live_provider_outcomes",
        },
        "interpretation_limits": [
            "Provider token usage and wall-clock latency are measured live.",
            "The recovery fixture and three focus-field candidates are configured benchmark inputs.",
            "Focus-field ranking is deterministic benchmark preprocessing, not a production retrieval measurement.",
            "Unique shared pair nonces reduce reuse of identical broker-side cached responses.",
            "Three paired trials are exploratory evidence, not a statistically powered performance study.",
            "Latency can vary with provider/network conditions; token differences are the cleaner primary measure.",
            "Cost comparison is qualified only when every trial in both modes verifies successfully.",
            "The evidence window passively records live A/B outcomes and does not adapt the fixed benchmark arm schedule.",
        ],
    }

    output = Path(args.output)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(json.dumps(artifact, indent=2, sort_keys=True) + "\n", encoding="utf-8")

    print(
        json.dumps(
            {
                "benchmark_id": benchmark_id,
                "artifact": str(output),
                "all_verified": all(record["verification_passed"] for record in records),
                "sequential": aggregate["sequential"],
                "focus_field": aggregate["focus_field"],
                "comparison": aggregate["comparison"],
                "field_evidence": {
                    "observation_count": field_evidence.observation_count,
                    "verification_success_rate": field_evidence.verification_success_rate,
                    "completion_pressure": field_evidence.completion_pressure,
                },
            },
            sort_keys=True,
        )
    )

    if not all(record["verification_passed"] for record in records):
        print("one or more live A/B recovery arms failed verification", file=sys.stderr)
        return 2
    return 0


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--model", default="MiniMaxAI/MiniMax-M2.7")
    parser.add_argument("--context-window-tokens", type=int, default=180000)
    parser.add_argument("--max-output-tokens", type=int, default=1536)
    parser.add_argument("--inter-call-delay-seconds", type=float, default=0.25)
    parser.add_argument("--output", default="artifacts/live-recovery-ab.json")
    args = parser.parse_args()

    if args.context_window_tokens <= 0:
        parser.error("--context-window-tokens must be positive")
    if args.max_output_tokens <= 0:
        parser.error("--max-output-tokens must be positive")
    if args.inter_call_delay_seconds < 0:
        parser.error("--inter-call-delay-seconds must be non-negative")

    return asyncio.run(_run(args))


if __name__ == "__main__":
    raise SystemExit(main())
