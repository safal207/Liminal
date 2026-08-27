# Liminal Runtime Telemetry Bridge v0.1

**Status:** experimental / non-normative  
**Project:** LIMINAL

## Purpose

This bridge converts observable agent-runtime telemetry into the normalized inputs consumed by:

- the Agent Flow Regulator;
- the Recovery Routing Policy.

It does not claim to measure human emotion, consciousness, happiness, or subjective flow. It is an engineering adapter from runtime evidence to deterministic control signals.

## Observable telemetry

The v0.1 input envelope includes:

- token utilization;
- retry rate;
- tool failure rate;
- latency pressure;
- context pressure / compaction pressure;
- goal drift;
- causal drift;
- verified progress rate;
- feedback success rate;
- interruption rate;
- recent recovery rate;
- task difficulty estimate;
- available capability estimate;
- replay-depth estimate;
- field candidate count;
- best anchor score;
- field uncertainty;
- verification availability;
- measured field cost when available.

The first group should come from traces, counters, receipts, evaluators, or explicit task metadata. Goal drift, causal drift, task difficulty, and available capability are still estimator outputs and must remain inspectable and replaceable.

## Mapping

```text
runtime telemetry
      |
      +--------------------+
      |                    |
      v                    v
FlowSignals          RecoverySignals
      |                    |
      v                    v
Flow Regulator       Recovery Router
      |                    |
      +---------+----------+
                |
                v
          agent control loop
```

### Flow-derived signals

- goal clarity = inverse goal drift;
- feedback quality = successful feedback discounted by tool failures;
- interruption pressure = interruption rate + retry pressure;
- recovery load = recent recovery + context pressure + causal drift;
- compute pressure = token utilization + latency + context pressure.

### Recovery signals

Recovery routing consumes mostly direct telemetry:

- estimated replay depth;
- field candidate count;
- best anchor score;
- field uncertainty;
- verification availability;
- optional measured field scan cost.

## Evidence boundary

A control decision must preserve the difference between:

```text
observed metric
estimated metric
derived metric
policy threshold
```

For example, token utilization can be directly observed, while causal drift may be produced by a verifier. The runtime must not present both as equally direct evidence.

## Current limitations

v0.1 does not yet define:

- a standard trace schema;
- online aggregation windows;
- provider-specific token/latency adapters;
- causal-drift verifier implementation;
- automatic capability calibration;
- production threshold calibration.

## Next experiment

Feed recorded agent traces into this bridge and compare:

1. fixed loop;
2. recovery routing;
3. flow regulation with full monitoring;
4. flow regulation with adaptive monitoring.

Measure actual tokens, latency, tool/model calls, retries, wrong transitions, recovery frequency, drift, and downstream task success.
