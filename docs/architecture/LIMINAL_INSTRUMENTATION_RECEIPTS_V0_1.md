# Liminal Instrumentation Receipts v0.1

**Status:** experimental / non-normative

## Purpose

Instrumentation receipts turn runtime observations into explicit evidence records before those observations influence Flow Regulation or Recovery Routing.

The principle is:

```text
observe -> receipt -> aggregate -> telemetry -> policy
```

A missing receipt remains missing data. The adapter must not silently replace absence with a healthy value.

## Receipt classes

- `TokenUsageReceipt` — input/output token counts and optional context-window size.
- `LogicalRetryReceipt` — one retry of the same logical action, with retry index and reason code.
- `ContinuityReceipt` — explicit goal or causal continuity score with optional evidence references.
- `VerifiedProgressReceipt` — completed verified units over expected units, with verification references.

## Evidence boundary

Direct observations:

- token counts;
- logical retry events;
- completed/expected verified work units.

Measured or externally scored observations:

- goal continuity score;
- causal continuity score.

These continuity scores are not treated as ground truth. They should be backed by inspectable evidence references and calibrated against task-specific validators.

## Aggregation

`aggregate_receipts()` produces a compact receipt-backed telemetry window:

- token utilization;
- retry rate;
- goal drift = `1 - mean(goal continuity)`;
- causal drift = `1 - mean(causal continuity)`;
- verified progress rate.

Unknown measurements remain `None`.

## Relationship to existing telemetry

Receipt-backed telemetry complements Prometheus ingestion:

```text
Prometheus snapshot
  -> failures / duration / queue / freshness

Instrumentation receipts
  -> tokens / retries / continuity / verified progress

            both
             |
             v
      RuntimeTelemetry
             |
      +------+------+
      |             |
      v             v
Flow Regulator   Recovery Router
```

## Safety properties

1. Missing evidence is not interpreted as success.
2. Every receipt carries trace, step, timestamp, and source identity.
3. Specialized receipts reject mismatched kinds and invalid ranges.
4. Goal/causal continuity remains an explicit scored observation, not a hidden inference.
5. Recovery or flow decisions still require their own policy and verification gates.

## Next step

Instrument one real agent execution path so receipts are emitted during model calls, logical retries, verification events, and continuity checks. Persist the resulting ordered receipt set as the first full runtime trace and compare fixed-loop, recovery-routed, and flow-regulated execution on the same evidence.
