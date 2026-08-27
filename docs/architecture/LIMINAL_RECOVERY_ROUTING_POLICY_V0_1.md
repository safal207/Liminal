# Liminal Recovery Routing Policy v0.1

**Status:** experimental / non-normative  
**Project:** LIMINAL

## Purpose

Liminal already models durable continuation: a suspended workflow can preserve intent, evidence, decisions, actions, and a continuation boundary. This policy adds the missing runtime question:

> **How should the agent recover enough context to continue?**

The policy chooses one of three modes:

```text
SEQUENTIAL  |  FOCUS_FIELD  |  DEFER
```

It does not implement memory retrieval itself. It sits above retrieval systems such as CML and consumes compact recovery signals.

## Control loop

```text
Continuation / interruption
          |
          v
   collect recovery signals
          |
          v
   +---------------------+
   | Recovery Router     |
   +---------------------+
      |        |       |
      v        v       v
 Sequential  Field    Defer
 replay      recovery  / observe
      |        |       |
      +--------+-------+
               |
               v
          verify resume
```

A broader conceptual cycle is:

```text
VALUE / INTENT
      |
      v
FOCUS -> interruption / saturation
      |
      v
OBSERVE
      |
      v
ROUTE
  |           |
  v           v
SEQUENTIAL   FIELD
               |
               v
           RE-ANCHOR
               |
               v
            VERIFY
               |
               v
             FOCUS
```

## Why routing matters

Sequential replay is attractive when the lost context is shallow: it is simple, legible, and avoids unnecessary search.

Field-mediated recovery becomes attractive when replay is deep and a small bounded set of credible anchors can restore the working state for materially less recovery work.

When evidence or confidence is insufficient, the system should not force a choice. `DEFER` preserves the non-action state and allows additional context, human input, or evidence to arrive.

## v0.1 signals

`RecoverySignals` contains:

- `replay_steps` — estimated sequential recovery work;
- `candidate_count` — number of bounded field anchors;
- `best_anchor_score` — confidence in the strongest candidate;
- `uncertainty` — residual uncertainty around field selection;
- `verified_candidate_available` — whether a verification-qualified anchor exists;
- `require_verified` — whether unverified recovery is forbidden;
- `field_scan_cost` — optional measured field cost; otherwise candidate count is used as a deterministic proxy.

## Default policy

```text
max_sequential_steps      = 4
min_field_anchor_score    = 0.35
max_field_uncertainty     = 0.45
min_field_savings_ratio   = 0.25
max_field_candidates      = 32
```

The thresholds are experimental and must be calibrated on real workloads.

## Decision order

The ordering is intentional and safety-oriented:

1. **Verification first.** If verification is required and no verified anchor exists, `DEFER`.
2. **Keep shallow recovery simple.** If replay is at most four steps, use `SEQUENTIAL`.
3. **Require a bounded field.** Empty fields fall back to sequential; fields above the candidate bound defer.
4. **Require a credible anchor.** Low anchor confidence falls back to sequential.
5. **Do not re-anchor through high uncertainty.** High uncertainty returns `DEFER`.
6. **Require economic benefit.** Focus–Field is selected only when expected recovery work clears the configured break-even threshold.

## Relationship to CML Focus–Field Recovery

CML owns the experimental field-recovery mechanism: candidate anchors, value/intent overlap, causal and goal continuity, evidence quality, deterministic scoring, and re-anchoring.

Liminal owns runtime orchestration. The bridge is intentionally narrow:

```text
CML field result / telemetry
        |
        v
RecoverySignals
        |
        v
Liminal Recovery Router
        |
  +-----+------+------+
  |            |      |
Sequential   Field   Defer
```

This keeps CML useful as an independent memory/recovery primitive while allowing LiminalOSAI-style control to choose the computational mode.

## Break-even principle

The router does not assume Focus–Field is universally better.

For shallow loss:

```text
sequential cost = 2
field cost      = 2
=> sequential
```

For deep loss with a small credible field:

```text
sequential cost = 17
field cost      = 2
savings         ≈ 88%
=> focus_field
```

This matches the first synthetic CML A/B benchmark while keeping the runtime policy independent from those fixtures.

## Safety invariants

- no forced field recovery under high uncertainty;
- verification requirements take priority over efficiency;
- field search stays explicitly bounded;
- shallow replay remains available as the simpler path;
- decisions expose stable reason codes;
- the router never grants action authority; normal LIMINAL authorization and verification still apply after recovery;
- thresholds are configuration, not claims about human cognition or metaphysical mechanisms.

## Next experiment

Feed real agent traces into the router and measure:

- actual input/output tokens spent on recovery;
- wall-clock recovery latency;
- number of tool/model calls;
- wrong-anchor rate;
- goal and causal drift;
- post-recovery task success;
- break-even depth where Focus–Field becomes cheaper than sequential replay.

The desired result is not "Field always wins." The desired result is a calibrated runtime that selects the cheapest trustworthy recovery geometry for the current interruption.
