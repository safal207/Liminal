"""Evidence helpers for live-provider runtime traces.

Provider measurements are kept separate from configured probe inputs so a live
OpenAI call is never mistaken for a fully production-derived recovery trace.
"""

from __future__ import annotations

import hashlib
import json
from dataclasses import asdict, dataclass, is_dataclass
from datetime import datetime
from enum import Enum
from typing import Any

from liminal.end_to_end_trace import (
    EndToEndTraceDecision,
    TraceRuntimeObservations,
    evaluate_end_to_end_trace,
)
from liminal.instrumentation_receipts import (
    ContinuityReceipt,
    InstrumentationReceipt,
    ReceiptKind,
    VerifiedProgressReceipt,
    utc_now,
)
from liminal.receipt_telemetry_adapter import aggregate_receipts


LIVE_PROBE_GOAL_ID = "liminal-live-provider-trace-v0.1"
LIVE_PROBE_PARENT_STEP_ID = "provider-call-1"
LIVE_PROBE_EXPECTED_STATUS = "verified"


@dataclass(frozen=True)
class ProbeVerification:
    valid_json: bool
    goal_match: bool
    parent_match: bool
    status_match: bool
    exact_key_set: bool

    @property
    def passed(self) -> bool:
        return (
            self.valid_json
            and self.goal_match
            and self.parent_match
            and self.status_match
            and self.exact_key_set
        )


@dataclass(frozen=True)
class LiveProviderTraceArtifact:
    schema_version: str
    trace_id: str
    provider: str
    model: str
    probe_mode: str
    latency_seconds: float
    response_sha256: str
    provider_usage: dict[str, int]
    verification: ProbeVerification
    receipts: tuple[InstrumentationReceipt, ...]
    decision: EndToEndTraceDecision
    observation_sources: dict[str, str]


def live_probe_prompt() -> str:
    """Return the fixed, non-sensitive provider probe prompt."""

    return (
        "Return exactly one JSON object and no surrounding prose. "
        "Use exactly these four keys and values: "
        f'{{"goal_id":"{LIVE_PROBE_GOAL_ID}",'
        f'"parent_step_id":"{LIVE_PROBE_PARENT_STEP_ID}",'
        f'"status":"{LIVE_PROBE_EXPECTED_STATUS}",'
        '"evidence":"provider-response"}}'
    )


def verify_live_probe_response(content: str) -> ProbeVerification:
    """Deterministically verify provider output against the probe contract."""

    expected_keys = {"goal_id", "parent_step_id", "status", "evidence"}
    try:
        payload = json.loads(content)
    except (json.JSONDecodeError, TypeError):
        return ProbeVerification(False, False, False, False, False)

    if not isinstance(payload, dict):
        return ProbeVerification(True, False, False, False, False)

    return ProbeVerification(
        valid_json=True,
        goal_match=payload.get("goal_id") == LIVE_PROBE_GOAL_ID,
        parent_match=payload.get("parent_step_id") == LIVE_PROBE_PARENT_STEP_ID,
        status_match=payload.get("status") == LIVE_PROBE_EXPECTED_STATUS,
        exact_key_set=set(payload) == expected_keys,
    )


def verification_receipts(
    *,
    trace_id: str,
    step_id: str,
    verification: ProbeVerification,
) -> tuple[InstrumentationReceipt, ...]:
    """Turn deterministic contract checks into evidence-backed receipts."""

    observed_at = utc_now()
    source = "liminal.live_provider_trace.verify_live_probe_response"
    goal_ref = "live-probe:goal_id_match"
    causal_ref = "live-probe:parent_step_id_match"
    verification_ref = "live-probe:exact_contract_verifier"

    return (
        ContinuityReceipt(
            receipt_id=f"{trace_id}:{step_id}:goal_continuity",
            trace_id=trace_id,
            step_id=step_id,
            kind=ReceiptKind.GOAL_CONTINUITY,
            observed_at=observed_at,
            source=source,
            continuity_score=1.0 if verification.goal_match else 0.0,
            evidence_refs=(goal_ref,),
        ),
        ContinuityReceipt(
            receipt_id=f"{trace_id}:{step_id}:causal_continuity",
            trace_id=trace_id,
            step_id=step_id,
            kind=ReceiptKind.CAUSAL_CONTINUITY,
            observed_at=observed_at,
            source=source,
            continuity_score=1.0 if verification.parent_match else 0.0,
            evidence_refs=(causal_ref,),
        ),
        VerifiedProgressReceipt(
            receipt_id=f"{trace_id}:{step_id}:verified_progress",
            trace_id=trace_id,
            step_id=step_id,
            kind=ReceiptKind.VERIFIED_PROGRESS,
            observed_at=observed_at,
            source=source,
            completed_units=1 if verification.passed else 0,
            expected_units=1,
            verification_refs=(verification_ref,),
        ),
    )


def live_probe_observations(
    receipts: tuple[InstrumentationReceipt, ...],
    *,
    latency_seconds: float,
    latency_budget_seconds: float,
    verification: ProbeVerification,
    probe_mode: str,
) -> TraceRuntimeObservations:
    """Compose measured provider facts with explicitly configured probe inputs."""

    if latency_seconds < 0:
        raise ValueError("latency_seconds_must_be_non_negative")
    if latency_budget_seconds <= 0:
        raise ValueError("latency_budget_seconds_must_be_positive")
    if probe_mode not in {"nominal", "induced-recovery"}:
        raise ValueError("unsupported_probe_mode")

    telemetry = aggregate_receipts(receipts)
    if telemetry.token_utilization is None:
        raise ValueError("token_utilization_required_for_live_probe")

    induced = probe_mode == "induced-recovery"
    return TraceRuntimeObservations(
        tool_failure_rate=0.0,
        latency_pressure=min(1.0, latency_seconds / latency_budget_seconds),
        context_pressure=telemetry.token_utilization,
        feedback_success_rate=1.0 if verification.passed else 0.0,
        interruption_rate=0.0,
        # Configured probe input. It is intentionally high only in induced mode
        # to exercise the real Flow -> Recovery routing code with live provider evidence.
        recent_recovery_rate=0.90 if induced else 0.0,
        task_difficulty=0.50,
        available_capability=0.55,
        replay_steps_estimate=12 if induced else 0,
        field_candidate_count=3 if induced else 0,
        best_anchor_score=0.82 if induced else 0.0,
        field_uncertainty=0.12 if induced else 0.0,
        verified_candidate_available=True,
        require_verified=True,
        measured_field_cost=3 if induced else None,
    )


def evaluate_live_provider_trace(
    receipts: tuple[InstrumentationReceipt, ...],
    *,
    latency_seconds: float,
    latency_budget_seconds: float,
    verification: ProbeVerification,
    probe_mode: str,
) -> EndToEndTraceDecision:
    observations = live_probe_observations(
        receipts,
        latency_seconds=latency_seconds,
        latency_budget_seconds=latency_budget_seconds,
        verification=verification,
        probe_mode=probe_mode,
    )
    return evaluate_end_to_end_trace(receipts, observations)


def make_live_provider_artifact(
    *,
    trace_id: str,
    model: str,
    probe_mode: str,
    latency_seconds: float,
    response_content: str,
    provider_usage: dict[str, int],
    verification: ProbeVerification,
    receipts: tuple[InstrumentationReceipt, ...],
    decision: EndToEndTraceDecision,
) -> LiveProviderTraceArtifact:
    return LiveProviderTraceArtifact(
        schema_version="liminal.live-provider-trace.v0.1",
        trace_id=trace_id,
        provider="openai",
        model=model,
        probe_mode=probe_mode,
        latency_seconds=round(latency_seconds, 6),
        response_sha256=hashlib.sha256(response_content.encode("utf-8")).hexdigest(),
        provider_usage=dict(provider_usage),
        verification=verification,
        receipts=receipts,
        decision=decision,
        observation_sources={
            "token_usage": "measured:provider_response_usage",
            "latency": "measured:wall_clock_around_provider_call",
            "goal_continuity": "measured:deterministic_goal_id_verifier",
            "causal_continuity": "measured:deterministic_parent_step_verifier",
            "verified_progress": "measured:deterministic_exact_contract_verifier",
            "tool_failure_rate": "measured:provider_call_completed",
            "feedback_success_rate": "measured:deterministic_verifier",
            "context_pressure": "derived:provider_token_utilization",
            "interruption_rate": "harness:single_atomic_probe",
            "task_difficulty": "configured:probe_input",
            "available_capability": "configured:probe_input",
            "recent_recovery_rate": "configured:probe_mode",
            "recovery_geometry": "configured:probe_mode",
        },
    )


def to_jsonable(value: Any) -> Any:
    """Convert trace dataclasses/enums/timestamps to stable JSON-compatible data."""

    if is_dataclass(value):
        return to_jsonable(asdict(value))
    if isinstance(value, Enum):
        return value.value
    if isinstance(value, datetime):
        return value.isoformat()
    if isinstance(value, dict):
        return {str(key): to_jsonable(item) for key, item in value.items()}
    if isinstance(value, (tuple, list)):
        return [to_jsonable(item) for item in value]
    return value
