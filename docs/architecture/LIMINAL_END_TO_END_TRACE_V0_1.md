# Liminal End-to-End Runtime Trace v0.1

Status: experimental, non-normative.

## Purpose

Connect the existing runtime-control pieces into one inspectable control window:

```text
Instrumented application call
        |
        v
Instrumentation receipts
        |
        v
Receipt telemetry aggregation
        |
        v
RuntimeTelemetry
        |
        v
Flow Regulator
        |
        | RECOVERY only
        v
Recovery Router
        |
        v
Sequential | Focus-Field | Defer
```

The evaluator is implemented in `src/liminal/end_to_end_trace.py`.

## Evidence boundary

A complete trace fails closed unless all receipt-backed control measurements are
present:

- token utilization;
- retry rate;
- goal continuity/drift;
- causal continuity/drift;
- verified progress.

Goal and causal continuity receipts must carry evidence references. Verified
progress must include at least one verification reference. Mixed trace IDs are
rejected. Missing measurements are not replaced with healthy zero values.

Non-receipt observations such as latency pressure, tool failure rate,
interruption rate, task difficulty, and recovery-field measurements remain
explicit inputs. They must come from an observable runtime source (for example
Prometheus, trace counters, or a caller-owned measurement window).

## Routing semantics

The Flow Regulator remains the first control decision. The Recovery Router is
invoked only when Flow enters `RECOVERY`; an underloaded or overloaded state does
not implicitly gain recovery or action authority.

The recovery decision remains advisory. `FOCUS_FIELD` means the measured field
is credible enough to attempt a re-anchor under the current experimental
thresholds. It does not prove that the selected anchor is correct and does not
bypass downstream verification.

## Reproducible CI trace

`tests/test_end_to_end_trace.py` exercises the inherited `OpenAIService`
application flow through `InstrumentedOpenAIService`, then attaches explicit
continuity and verification receipts and evaluates the full control path.

The LLM provider response is mocked in CI. This means the test proves code-path
composition and deterministic routing, not production OpenAI latency, token
cost, model quality, or real-world recovery effectiveness.

The reference trace is intentionally degraded on goal continuity so that:

1. Flow enters `RECOVERY` with reason `goal_clarity_below_flow_corridor`;
2. a deep, small, verified, low-uncertainty candidate field reaches the Recovery
   Router;
3. the router chooses `FOCUS_FIELD` with an estimated savings ratio of 0.75.

## Next evidence step

Run the same path with a provider-backed execution and persisted raw telemetry,
then retain the receipts and runtime observations as a replayable trace artifact.
Only after that should token, latency, retry, or recovery-cost claims be treated
as measured rather than synthetic/proxy evidence.
