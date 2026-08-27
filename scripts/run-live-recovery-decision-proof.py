#!/usr/bin/env python3
"""Run live Gonka recovery attempts and emit a signed reproducible decision proof."""

from __future__ import annotations

import argparse
import asyncio
import base64
import hashlib
import json
import os
import time
from pathlib import Path

from cryptography.hazmat.primitives import serialization
from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PrivateKey

from backend.ml.openai_wrapper import LLMRequest, llm_client
from backend.ml.receipt_instrumented_client import call_with_receipts
from liminal.builder_environment_receipt import (
    BUILDER_ENVIRONMENT_SCHEMA_VERSION,
    verify_builder_environment_receipt,
)
from liminal.instrumentation_receipts import TokenUsageReceipt
from liminal.live_recovery_ab import (
    parse_recovery_payload,
    recovery_prompt,
    recovery_response_format,
    verify_recovery_response,
)
from liminal.recovery_decision_receipt import verify_decision_receipt
from liminal.recovery_evidence_ledger import RecoveryEvidenceLedger
from liminal.recovery_policy import RecoveryMode
from liminal.recovery_proof_bundle import (
    PROOF_BUNDLE_SCHEMA_VERSION,
    build_recovery_proof_bundle,
    verify_recovery_proof_bundle,
)
from liminal.recovery_runtime import EvidenceAwareRecoveryRuntime
from liminal.telemetry_bridge import RuntimeTelemetry


SYSTEM_MESSAGE = (
    "You are a deterministic recovery verifier. Select only an evidence-backed "
    "continuation anchor from the supplied context and follow the JSON schema exactly."
)
RECOVERY_CLASS = "deep-ledger-recovery"


def _gonka_settings() -> tuple[str, str]:
    api_key = os.getenv("GONKA_BROKER_API_KEY", "").strip() or os.getenv("GONKA_API_KEY", "").strip()
    base_url = os.getenv("GONKA_BROKER_URL", "").strip() or os.getenv("GONKA_BASE_URL", "").strip()
    if not api_key:
        raise RuntimeError("GONKA_BROKER_API_KEY (or GONKA_API_KEY) is required")
    if not base_url:
        raise RuntimeError("GONKA_BROKER_URL (or GONKA_BASE_URL) is required")
    return api_key, base_url.rstrip("/")


def _normalize_minimax(model: str, content: str) -> tuple[str, str | None]:
    if "minimax-m2.7" not in model.lower():
        return content, None
    candidate = content.lstrip()
    if not candidate.startswith("<think>"):
        return content, None
    close = candidate.find("</think>")
    if close < 0:
        return content, "gonka:minimax_think_wrapper_unclosed"
    return candidate[close + len("</think>") :].lstrip(), "gonka:minimax_think_wrapper_stripped"


async def _configure_gonka() -> None:
    from openai import AsyncOpenAI

    api_key, base_url = _gonka_settings()
    llm_client.mock_only = False
    llm_client.fallback_to_local = False
    llm_client.debug_level = 0
    llm_client.cache_ttl = 0
    llm_client.response_cache.clear()
    llm_client.api_key = api_key
    llm_client.openai_client = AsyncOpenAI(api_key=api_key, base_url=base_url)


def _telemetry() -> RuntimeTelemetry:
    return RuntimeTelemetry(
        token_utilization=0.35,
        retry_rate=0.0,
        tool_failure_rate=0.0,
        latency_pressure=0.20,
        context_pressure=0.55,
        goal_drift=0.05,
        causal_drift=0.05,
        verified_progress_rate=0.70,
        feedback_success_rate=0.95,
        interruption_rate=0.60,
        recent_recovery_rate=0.80,
        task_difficulty=0.75,
        available_capability=0.75,
        replay_steps_estimate=12,
        field_candidate_count=3,
        best_anchor_score=0.82,
        field_uncertainty=0.12,
        verified_candidate_available=True,
        require_verified=True,
        measured_field_cost=3,
    )


async def _run_attempt(*, model: str, max_output_tokens: int, trace_id: str, nonce: str) -> dict:
    prompt = recovery_prompt("focus_field", probe_nonce=nonce)
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
    llm_client.response_cache.clear()
    started = time.perf_counter()
    result = await call_with_receipts(
        request,
        trace_id=trace_id,
        step_id="provider-focus-field",
        logical_action_id=trace_id,
        context_window_tokens=180000,
    )
    latency = time.perf_counter() - started
    token_receipts = [r for r in result.receipts if isinstance(r, TokenUsageReceipt)]
    if len(token_receipts) != 1:
        raise RuntimeError("exactly one token usage receipt required")
    usage = token_receipts[0]
    raw = result.response.content
    normalized, strategy = _normalize_minimax(model, raw)
    verification = verify_recovery_response(normalized)
    return {
        "verification_passed": verification.passed,
        "finish_reason": result.response.finish_reason,
        "structured_output": parse_recovery_payload(normalized),
        "prompt_tokens": usage.input_tokens,
        "completion_tokens": usage.output_tokens,
        "total_tokens": usage.input_tokens + usage.output_tokens,
        "latency_seconds": round(latency, 6),
        "normalization_strategy": strategy,
        "raw_response_sha256": hashlib.sha256(raw.encode("utf-8")).hexdigest(),
        "normalized_response_sha256": hashlib.sha256(normalized.encode("utf-8")).hexdigest(),
        "prompt_sha256": hashlib.sha256(prompt.encode("utf-8")).hexdigest(),
    }


async def _run(args: argparse.Namespace) -> int:
    out = Path(args.output_dir)
    out.mkdir(parents=True, exist_ok=True)
    builder_repository = os.getenv("LIMINAL_BUILDER_REPOSITORY", "").strip()
    builder_workflow_sha = os.getenv("LIMINAL_BUILDER_WORKFLOW_SHA", "").strip()
    if not builder_repository or not builder_workflow_sha:
        raise RuntimeError("trusted builder identity environment is required")
    builder_environment_path = out / "builder-environment.json"
    if not verify_builder_environment_receipt(
        builder_environment_path,
        repository_root=Path.cwd(),
        expected_builder_repository=builder_repository,
        expected_builder_workflow_sha=builder_workflow_sha,
    ):
        raise RuntimeError("builder environment receipt verification failed")

    await _configure_gonka()
    ledger = RecoveryEvidenceLedger(out / "recovery-evidence.jsonl")
    runtime = EvidenceAwareRecoveryRuntime(evidence_ledger=ledger)

    attempts: list[dict] = []
    for index in range(1, 4):
        nonce = hashlib.sha256(f"{args.run_id}:{index}".encode()).hexdigest()[:16]
        record = await _run_attempt(
            model=args.model,
            max_output_tokens=args.max_output_tokens,
            trace_id=f"live-decision-proof:{args.run_id}:{index}",
            nonce=nonce,
        )
        attempts.append({"attempt": index, **record})
        runtime.record_attempt(
            recovery_class=RECOVERY_CLASS,
            mode=RecoveryMode.FOCUS_FIELD,
            verification_passed=bool(record["verification_passed"]),
            finish_reason=record["finish_reason"],
        )
        await asyncio.sleep(0.25)

    private_key = Ed25519PrivateKey.generate()
    key_id = f"ci-ephemeral:{args.run_id}"
    decision_with_receipt = runtime.decide_with_receipt(
        _telemetry(),
        recovery_class=RECOVERY_CLASS,
        private_key=private_key,
        key_id=key_id,
    )
    decision = decision_with_receipt.decision
    receipt = decision_with_receipt.receipt
    public_key = private_key.public_key()
    verified = verify_decision_receipt(
        receipt,
        public_key=public_key,
        expected_key_id=key_id,
        ledger=ledger,
    )
    if not verified:
        raise RuntimeError("offline decision receipt verification failed")

    public_raw = public_key.public_bytes(
        encoding=serialization.Encoding.Raw,
        format=serialization.PublicFormat.Raw,
    )
    (out / "decision-receipt.json").write_text(
        json.dumps(receipt.as_dict(), indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )
    (out / "public-key.json").write_text(
        json.dumps(
            {
                "algorithm": "ed25519",
                "key_id": key_id,
                "public_key_raw_base64": base64.b64encode(public_raw).decode("ascii"),
                "trust_scope": "ephemeral_ci_integrity_only_not_long_term_identity",
            },
            indent=2,
            sort_keys=True,
        )
        + "\n",
        encoding="utf-8",
    )
    evidence = runtime.field_evidence(recovery_class=RECOVERY_CLASS)
    summary = {
        "schema_version": "liminal.live-recovery-decision-proof.v0.2",
        "proof_bundle_schema_version": PROOF_BUNDLE_SCHEMA_VERSION,
        "builder_environment_schema_version": BUILDER_ENVIRONMENT_SCHEMA_VERSION,
        "builder_environment_verified": True,
        "provider": "gonka",
        "model": args.model,
        "recovery_class": RECOVERY_CLASS,
        "live_attempts": attempts,
        "field_evidence": {
            "observation_count": evidence.observation_count,
            "verification_success_rate": evidence.verification_success_rate,
            "completion_pressure": evidence.completion_pressure,
        },
        "router_decision": {
            "mode": decision.mode.value,
            "reason": decision.reason,
            "replay_steps": decision.replay_steps,
            "estimated_field_cost": decision.estimated_field_cost,
            "estimated_savings_ratio": decision.estimated_savings_ratio,
        },
        "receipt_offline_verified": verified,
        "signer_trust_scope": "ephemeral_ci_integrity_only_not_long_term_identity",
        "raw_model_reasoning_persisted": False,
    }
    (out / "summary.json").write_text(
        json.dumps(summary, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )

    bundle = build_recovery_proof_bundle(out)
    if not verify_recovery_proof_bundle(bundle.path):
        raise RuntimeError("recovery proof bundle self-verification failed")

    result = {
        **summary,
        "proof_bundle": {
            "name": bundle.path.name,
            "sha256": bundle.sha256,
            "manifest_sha256": bundle.manifest_sha256,
            "member_count": bundle.member_count,
            "self_verified": True,
        },
    }
    print(json.dumps(result, sort_keys=True))
    return 0


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--model", default="MiniMaxAI/MiniMax-M2.7")
    parser.add_argument("--max-output-tokens", type=int, default=1536)
    parser.add_argument("--run-id", required=True)
    parser.add_argument("--output-dir", default="artifacts/live-recovery-decision-proof")
    args = parser.parse_args()
    if args.max_output_tokens <= 0:
        parser.error("--max-output-tokens must be positive")
    return asyncio.run(_run(args))


if __name__ == "__main__":
    raise SystemExit(main())
