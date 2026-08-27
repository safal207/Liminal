# Liminal Real Telemetry Ingestion v0.1

**Status:** experimental / non-normative

## Purpose

This note defines the first bridge from telemetry that LIMINAL already exposes at runtime into the Flow / Recovery control stack.

The key rule is:

> Do not convert an unavailable signal into a confident synthetic value.

The existing ML observability surface exposes four useful classes of runtime evidence:

- pipeline run counts and statuses;
- stage duration histograms;
- queue depth;
- last-success timestamp.

`src/liminal/prometheus_telemetry_adapter.py` converts a snapshot of those observations into normalized pressures:

```text
Prometheus snapshot
   |
   +-> failure rate
   +-> latency pressure
   +-> queue pressure
   +-> freshness pressure
```

## Evidence boundary

### Directly observed in v0.1

- pipeline runs;
- pipeline failures;
- stage duration;
- queue depth;
- time since last successful stage.

### Derived deterministically

- tool/pipeline failure rate;
- latency pressure relative to an explicit budget;
- queue pressure relative to an explicit budget;
- freshness pressure relative to an explicit staleness budget.

### Not yet observed

The current repository telemetry does not provide enough evidence to directly derive:

- token utilization;
- retry rate for logical agent actions;
- context-window pressure;
- goal drift;
- causal drift;
- verified progress rate;
- feedback success rate;
- interruption rate;
- recent recovery rate;
- CML field candidate count / anchor score / uncertainty.

These must remain unavailable until instrumentation is added. They must not be silently set to convenient defaults and then described as real telemetry.

## Next instrumentation

Add counters/histograms/receipts for:

1. model input/output tokens per control window;
2. logical retries keyed by stable action identity;
3. context compaction / context-window occupancy;
4. verified progress receipts;
5. goal-anchor continuity checks;
6. causal-continuity checks;
7. interruption/checkpoint events;
8. recovery route and result;
9. CML field size, selected anchor score, uncertainty and verification result.

Once those exist, a complete `RuntimeTelemetry` can be constructed from real observations and fed into:

```text
RuntimeTelemetry
   +-> Flow Regulator
   +-> Adaptive Monitor
   +-> Recovery Router
             |
             v
      Sequential | Field | Defer
```

## Claim discipline

Until the missing instrumentation exists, the correct claim is:

> LIMINAL can ingest a real subset of runtime telemetry into the control model; full real-agent Flow/Recovery evaluation is not yet available.

That boundary is part of the evidence model, not a limitation to hide.
