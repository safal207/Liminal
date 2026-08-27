"""End-to-end evaluation of one receipt-backed agent runtime control window.

The evaluator composes the existing experimental primitives without inventing
missing evidence:

instrumented application call -> receipts -> receipt telemetry -> runtime
telemetry -> Flow Regulator -> Recovery Router -> verification gate.

A "complete" trace is intentionally fail-closed. Receipt-backed measurements
required by the control path must be present, continuity scores must carry
inspectable evidence references, and verified progress must carry verification
references before a final decision is returned.
"""

from __future__ import annotations

from dataclasses import dataclass

from liminal.flow_regulator import FlowDecision, FlowState, regulate_flow
from liminal.instrumentation_receipts import (
    ContinuityReceipt,
    InstrumentationReceipt,
    ReceiptKind,
    VerifiedProgressReceipt,
)
from liminal.receipt_telemetry_adapter import ReceiptTelemetry, aggregate_receipts
from liminal.recovery_policy import RecoveryDecision, choose_recovery_mode
from liminal.telemetry_bridge import RuntimeTelemetry, to_flow_signals, to_recovery_signals


@dataclass(frozen=True)
class TraceRuntimeObservations:
    """Non-receipt runtime observations needed to complete one control window."""

    tool_failure_rate: float
    latency_pressure: float
    context_pressure: float
    feedback_success_rate: float
    interruption_rate: float
    recent_recovery_rate: float
    task_difficulty: float
    available_capability: float
    replay_steps_estimate: int
    field_candidate_count: int
    best_anchor_score: float
    field_uncertainty: float
    verified_candidate_available: bool = True
    require_verified: bool = True
    measured_field_cost: int | None = None


@dataclass(frozen=True)
class EndToEndTraceDecision:
    trace_id: str
    receipt_telemetry: ReceiptTelemetry
    runtime_telemetry: RuntimeTelemetry
    flow: FlowDecision
    recovery: RecoveryDecision | None
    verification_evidence_present: bool


def _require_complete_receipt_telemetry(telemetry: ReceiptTelemetry) -> None:
    required = {
        "token_utilization": telemetry.token_utilization,
        "retry_rate": telemetry.retry_rate,
        "goal_drift": telemetry.goal_drift,
        "causal_drift": telemetry.causal_drift,
        "verified_progress_rate": telemetry.verified_progress_rate,
    }
    missing = tuple(name for name, value in required.items() if value is None)
    if missing:
        raise ValueError("incomplete_receipt_telemetry:" + ",".join(missing))


def _validate_trace_evidence(receipts: tuple[InstrumentationReceipt, ...]) -> str:
    if not receipts:
        raise ValueError("trace_receipts_required")

    trace_ids = {receipt.trace_id for receipt in receipts}
    if len(trace_ids) != 1:
        raise ValueError("mixed_trace_ids_not_allowed")
    trace_id = next(iter(trace_ids))

    goal_receipts = [
        receipt
        for receipt in receipts
        if isinstance(receipt, ContinuityReceipt)
        and receipt.kind is ReceiptKind.GOAL_CONTINUITY
    ]
    causal_receipts = [
        receipt
        for receipt in receipts
        if isinstance(receipt, ContinuityReceipt)
        and receipt.kind is ReceiptKind.CAUSAL_CONTINUITY
    ]
    if not goal_receipts or not all(receipt.evidence_refs for receipt in goal_receipts):
        raise ValueError("goal_continuity_evidence_required")
    if not causal_receipts or not all(receipt.evidence_refs for receipt in causal_receipts):
        raise ValueError("causal_continuity_evidence_required")

    verified_progress = [
        receipt for receipt in receipts if isinstance(receipt, VerifiedProgressReceipt)
    ]
    if not verified_progress or not any(
        receipt.verification_refs for receipt in verified_progress
    ):
        raise ValueError("verification_receipt_required")

    return trace_id


def evaluate_end_to_end_trace(
    receipts: tuple[InstrumentationReceipt, ...],
    observations: TraceRuntimeObservations,
) -> EndToEndTraceDecision:
    """Evaluate a complete evidence-backed runtime control window.

    Recovery routing is invoked only when the Flow Regulator explicitly enters
    RECOVERY. Other states keep their own regulator adjustment and do not gain
    recovery/action authority implicitly.
    """

    trace_id = _validate_trace_evidence(receipts)
    receipt_telemetry = aggregate_receipts(receipts)
    _require_complete_receipt_telemetry(receipt_telemetry)

    runtime = RuntimeTelemetry(
        token_utilization=float(receipt_telemetry.token_utilization),
        retry_rate=float(receipt_telemetry.retry_rate),
        tool_failure_rate=observations.tool_failure_rate,
        latency_pressure=observations.latency_pressure,
        context_pressure=observations.context_pressure,
        goal_drift=float(receipt_telemetry.goal_drift),
        causal_drift=float(receipt_telemetry.causal_drift),
        verified_progress_rate=float(receipt_telemetry.verified_progress_rate),
        feedback_success_rate=observations.feedback_success_rate,
        interruption_rate=observations.interruption_rate,
        recent_recovery_rate=observations.recent_recovery_rate,
        task_difficulty=observations.task_difficulty,
        available_capability=observations.available_capability,
        replay_steps_estimate=observations.replay_steps_estimate,
        field_candidate_count=observations.field_candidate_count,
        best_anchor_score=observations.best_anchor_score,
        field_uncertainty=observations.field_uncertainty,
        verified_candidate_available=observations.verified_candidate_available,
        require_verified=observations.require_verified,
        measured_field_cost=observations.measured_field_cost,
    )

    flow = regulate_flow(to_flow_signals(runtime))
    recovery = None
    if flow.state is FlowState.RECOVERY:
        recovery = choose_recovery_mode(to_recovery_signals(runtime))

    return EndToEndTraceDecision(
        trace_id=trace_id,
        receipt_telemetry=receipt_telemetry,
        runtime_telemetry=runtime,
        flow=flow,
        recovery=recovery,
        verification_evidence_present=True,
    )
