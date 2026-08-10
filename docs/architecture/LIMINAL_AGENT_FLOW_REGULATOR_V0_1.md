# Liminal Agent Flow Regulator v0.1

**Status:** experimental / non-normative  
**Project:** LIMINAL

## Purpose

The Flow Regulator treats flow as an engineering operating corridor for sustained agent work. It does **not** claim that an AI agent has human happiness, fatigue, or consciousness.

The design translates useful properties of human flow research into runtime signals:

- challenge–capability balance;
- clear goal / value anchor;
- fast, useful feedback;
- visible progress;
- low interruption pressure;
- bounded recovery load;
- bounded compute pressure.

The runtime objective is:

> Keep work inside a productive, trustworthy corridor for as long as possible, and switch modes before recovery becomes expensive.

## Control cycle

```text
VALUE / INTENT
      |
      v
  FLOW REGULATOR
      |
      +-------------------------------+
      |                               |
      v                               v
 challenge ≈ capability          corridor broken
 goal clear                      /      |       \
 feedback useful          overload   recovery   underload
 progress visible             |          |          |
      |                       split    observe     batch /
      v                       task     + route     increase challenge
    FLOW                       |          |          |
      |                        +----------+----------+
      |                                   |
      v                                   v
 continue                           restored work
      |                                   |
      +---------------<-------------------+
```

## Relationship to Focus–Field Recovery

Flow regulation is **preventive**. Focus–Field Recovery is **restorative**.

```text
Flow Regulator
   |
   | detects stall / goal loss / interruption / overload
   v
Observe
   |
   v
Recovery Router
   |                  |
   v                  v
Sequential        Focus–Field
                      |
                      v
                  Re-anchor
                      |
                      v
                    Verify
                      |
                      v
                Flow Regulator
```

This makes flow a higher-level control objective rather than a memory primitive.

## Signals

`FlowSignals` uses normalized values from `0.0` to `1.0`:

- `challenge` — estimated task demand;
- `capability` — currently available capability, including tools and retrieved context;
- `goal_clarity` — how explicit and stable the next objective is;
- `feedback_quality` — speed and usefulness of verifiable feedback;
- `progress_rate` — recent useful progress;
- `interruption_pressure` — context switching / external interruption load;
- `recovery_load` — how much effort is currently spent reconstructing context;
- `compute_pressure` — token/context/tool/latency pressure.

## Flow score

v0.1 uses an inspectable deterministic score:

```text
positive =
  0.30 * challenge-capability balance
+ 0.18 * goal clarity
+ 0.16 * feedback quality
+ 0.16 * progress rate

friction =
  0.08 * interruption pressure
+ 0.07 * recovery load
+ 0.05 * compute pressure

flow_score = clamp(positive - friction, 0, 1)
```

The score is not a claim about human psychology. It is a runtime heuristic to be calibrated empirically.

## States

```text
UNDERLOADED | FLOW | OVERLOADED | RECOVERY
```

### FLOW
Continue without unnecessary mode switching.

### OVERLOADED
Challenge exceeds available capability or compute pressure is too high. Decompose the task, retrieve supporting context, or reduce context pressure.

### UNDERLOADED
Capability substantially exceeds challenge. Increase safe task granularity or batch routine work.

### RECOVERY
The task may be balanced in difficulty but the working conditions are broken: goal clarity is low, feedback is poor, progress stalled, interruption pressure is high, or recovery load is already excessive.

## Key invariant: no forced continuation

A system outside the flow corridor should not increase effort blindly.

Instead:

```text
stalled -> observe
lost goal -> restore value / intent
high interruption -> checkpoint
high recovery load -> route recovery
high challenge -> decompose
low challenge -> batch / increase granularity
```

## Why this matters for agent economics

The desired effect is not "make the agent happy." The desired effect is to reduce wasteful computation caused by:

- repeated context reconstruction;
- excessive mode switching;
- continuing after goal clarity is lost;
- overlong contexts;
- stalled loops;
- unnecessary fine-grained work when the task is too easy;
- attempting tasks whose challenge exceeds currently available capability.

## Research inspiration and boundary

Human flow research frequently emphasizes challenge–skill balance, clear goals, feedback, concentration, and intrinsic engagement. Flow has also been associated with positive affect and well-being, and some work reports negative associations with burnout.

LIMINAL uses those findings only as design inspiration. The runtime variables above are operational software metrics, not measurements of an agent's subjective experience.

## Next experiments

1. Derive `challenge` from task graph depth, novelty, branching, tool diversity, and error history.
2. Derive `capability` from available tools, relevant context coverage, historical success, and model/tool limits.
3. Derive `feedback_quality` from verification latency and evidence strength.
4. Derive `progress_rate` from verified state transitions per unit cost.
5. Measure whether staying in the corridor reduces tokens, latency, retries, wrong transitions, and recovery frequency.
6. Compare three runtimes on real traces:
   - fixed loop;
   - recovery routing only;
   - flow-regulated recovery routing.

The hypothesis is falsifiable: if flow regulation does not reduce cost or improve trustworthy completion on real workloads, its thresholds or the model itself must be revised.
