# Liminal Recovery Routing Policy v0.2

**Status:** experimental / non-normative  
**Project:** LIMINAL

## Why v0.2 exists

The first live Gonka recovery A/B showed an important boundary condition:

> A smaller recovery context can reduce prompt tokens while still producing worse or unstable verified recovery.

Across the exploratory runs, ranked Focus–Field sometimes recovered the exact anchor more reliably, but later cache-resistant paired trials did not establish a universal win. Completion length, malformed identifiers, stochastic reasoning length, and verification failures all mattered.

Therefore the router must not treat context compression or estimated field cost as sufficient evidence of a trustworthy recovery mode.

## New optional signals

`RecoverySignals` now accepts:

- `field_verification_success_rate` — observed fraction of comparable Focus–Field attempts that passed deterministic verification;
- `field_completion_pressure` — observed fraction/normalized pressure of comparable Focus–Field attempts exhausting or approaching the completion budget;
- `field_observation_count` — number of observations supporting those reliability metrics.

These signals are optional. Missing evidence remains missing; it is not converted to a healthy zero/default.

## Evidence gate

Default policy additions:

```text
min_field_observations                 = 3
min_field_verification_success_rate    = 0.67
max_field_completion_pressure          = 0.50
```

Observed reliability influences routing only when:

```text
field_observation_count >= min_field_observations
```

This prevents one or two stochastic provider responses from rewriting the routing policy.

## Routing behavior

After the v0.1 confidence and uncertainty checks, but before the economic break-even check:

```text
if enough observations:
    if field verification success is too low:
        -> SEQUENTIAL

    if field completion pressure is too high:
        -> SEQUENTIAL
```

Only then can estimated field savings qualify a deep recovery for `FOCUS_FIELD`.

The fallback is sequential rather than an automatic defer because a poor observed field path does not by itself prove that deterministic replay is unsafe. Existing verification requirements still apply after recovery.

## Live A/B implication

The live experiments now support a narrower claim than the original synthetic benchmark:

- Focus–Field can materially reduce the amount of recovery context presented to the model.
- Recovery geometry changes provider behavior, including reasoning length and verification reliability.
- Lower prompt-token cost alone is not sufficient evidence of lower total cost, lower latency, or higher correctness.
- Deterministic post-recovery verification is required.
- Runtime routing should be calibrated from repeated comparable traces rather than one benchmark outcome.

This is intentionally not a claim that Focus–Field is universally superior.

## Updated control principle

```text
credible anchor
    + bounded uncertainty
    + sufficient observed reliability
    + acceptable completion pressure
    + economic benefit
    -> FOCUS_FIELD
```

Otherwise the router selects the safer available fallback (`SEQUENTIAL` or the existing `DEFER` paths).

## Next calibration step

Collect a larger set of cache-resistant paired traces across multiple recovery fixtures and, ideally, multiple provider/model families. Persist aggregate evidence by recovery class rather than globally so that reliability for one task geometry does not automatically transfer to another.
