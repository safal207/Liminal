# Liminal Glossary

This glossary explains the core language used across the Liminal ecosystem.

The goal is to make the project understandable without private founder context.

---

## Continuity

**Continuity** means preserving a coherent thread across time, state, memory, decision, and action.

In human-facing Liminal, continuity means a person can move through uncertainty without losing contact with their inner direction.

In AI-agent infrastructure, continuity means an agent's behavior can be traced, replayed, and audited across steps.

---

## Transition

A **transition** is a movement from one state to another.

Examples:

- anxiety → clarity
- raw signal → intention
- draft → committed decision
- recommendation → external action
- unverified trace → auditable execution

Liminal focuses on transitions because many failures happen at boundaries, not inside a single isolated step.

---

## Liminal State

A **liminal state** is an in-between state where a person, agent, or system is no longer in the old mode but has not fully entered the new one.

For humans, this may feel like uncertainty, emotional overload, or inner fragmentation.

For AI agents, this may appear as context drift, unclear permission scope, or unstable execution state.

---

## State

A **state** is a snapshot of a person, agent, module, or system at a specific moment.

In early Liminal human-facing docs, state may refer to an emotional or psychological pattern such as:

```text
love=0.8, fear=0.2
```

In AI-agent infrastructure, state may refer to context, memory, permissions, trace position, or execution readiness.

---

## Connection

A **connection** is a relationship between states, memories, agents, modules, or decisions.

In early human-facing docs, a connection can have a weight or score and may need attention when the score is low.

In infrastructure docs, a connection may be represented as a causal parent, anchor, permission link, dependency, or trace edge.

---

## Trace

A **trace** is a structured record of what happened during an agent workflow.

A trace may include:

- transitions
- tool calls
- inputs
- outputs
- environment responses
- branch metadata
- timestamps
- anchors

In Liminal, a trace is not just logging. It is the basis for replay, verification, and auditability.

---

## Transition ID

A **transition ID** is a stable identifier for one step in a trace.

Example:

```json
{"transition_id":"t5","action":"execute_refund"}
```

Transition IDs make it possible to point to the exact step where drift, missing permission, or unsafe execution occurred.

---

## Anchor

An **anchor** is evidence that connects a claim or action to an earlier trace state.

Example:

```json
{"transition_id":"t4","claim":"we will compensate you","anchors":["t3"]}
```

An action without anchors may look correct but fail auditability.

---

## Causal Memory

**Causal memory** records not only what happened, but why an action was allowed, what it depended on, and who or what carried responsibility.

Traditional logs often answer:

> What happened?

Causal memory asks:

> Why was this action valid?

---

## CML — Causal Memory Layer

**CML** is the layer responsible for recording and validating causal relationships.

It tracks:

- parent reasons
- permissions
- responsibility
- scope
- causal validity
- invalid action chains

A system can be functionally correct while causally invalid. CML exists to make that difference visible.

---

## Permission Chain

A **permission chain** is the durable sequence of approvals, policies, scopes, or commitments that allows an action to execute.

Example:

```text
policy check → recommendation → human approval → committed permission → external action
```

If an agent skips from recommendation directly to external action, the permission chain is broken.

---

## Scope

**Scope** defines what an actor is allowed to do.

Example:

- allowed: read ticket
- allowed: check policy
- allowed: draft recommendation
- not allowed: execute refund without approval

Scope failures are common in AI-agent systems because models can smoothly move from suggestion to action unless boundaries are explicit.

---

## Causal Validity

**Causal validity** means an action has the required parent reasons, permissions, and preconditions.

An output can be high quality and still causally invalid.

Example:

A customer refund email may be polite and accurate, but if the refund was executed without approval, the action is causally invalid.

---

## Auditability

**Auditability** means a human or system can inspect what happened, why it happened, and whether it should have been allowed.

Auditability requires more than logs.

It needs:

- trace continuity
- stable IDs
- anchors
- permission records
- causal memory
- safe execution decisions

---

## Replay

**Replay** means reproducing or inspecting a workflow from recorded trace data.

Replay helps answer:

- Did the same inputs lead to the same decision?
- Where did the agent drift?
- Which step introduced risk?
- Can the decision be verified after the fact?

---

## Drift

**Drift** is a deviation from the expected thread, state, scope, or meaning.

Agent drift can happen when an agent:

- loses context
- changes task scope
- invents unsupported claims
- moves from recommendation to execution
- uses a tool without valid permission

Liminal treats drift as something to detect, explain, and audit.

---

## Agent Drift

**Agent drift** is drift specifically in AI-agent behavior.

Example:

A support agent is asked to prepare a recommendation but proceeds to execute a refund.

The final message may look helpful, but the agent has drifted from recommendation into external action.

---

## Safe Execution

**Safe execution** means side effects only happen after required checks and commitments.

Examples of side effects:

- sending an email
- charging a card
- executing a refund
- modifying a database
- calling an external API

Safe execution requires explicit boundaries between thinking, drafting, committing, and acting.

---

## CaPU — Causal Processing Unit

**CaPU** is a permission-first runtime concept for safe action lifecycles.

Its core lifecycle is:

```text
Gate → Incubate → Commit → Execute
```

Core invariant:

> Execute happens only after Commit.

CaPU decisions may include:

- ACCEPT
- HOLD
- REJECT
- EXPIRE

---

## HOLD

**HOLD** means the system does not execute yet because required preconditions are missing.

Example:

```json
{
  "decision": "HOLD",
  "reason_code": "PRECONDITIONS_UNMET"
}
```

HOLD is useful because the system does not need to reject the whole task. It can pause unsafe execution until approval or missing context arrives.

---

## BLOCK

**BLOCK** means an action should not proceed because a required invariant failed.

Example reasons:

- missing anchors
- invalid permission chain
- inadmissible action
- unsafe external side effect

---

## LTP — Liminal Thread Protocol

**LTP** is a protocol for AI-agent continuity, deterministic replay, and auditable handoffs.

It focuses on:

- trace structure
- thread continuity
- transition verification
- replayability
- anchors
- conformance fixtures

LTP asks whether an agent workflow can be followed and verified as a coherent thread.

---

## LiminalDB

**LiminalDB** is a reactive state/storage direction in the Liminal ecosystem.

It explores biologically inspired storage concepts such as:

- cells
- impulses
- reflexes
- adaptive state
- timeline / journal records

Its role in the wider stack is to support stateful, reactive, auditable agent and system behavior.

---

## DAO_lim

**DAO_lim** is an intent-aware routing and reverse-proxy direction for AI backends.

It helps decide where requests should go based on factors like:

- intent
- latency
- cost
- fallback
- provider health
- routing reliability

In the Liminal Stack, DAO_lim supports efficient and observable agent infrastructure.

---

## LiminalQAengineer

**LiminalQAengineer** is a QA intelligence direction for test observability and decision support.

It focuses on:

- flake risk
- adaptive timeouts
- test stability
- merge decisions
- risk scoring
- QA observability

Its role is to connect Liminal's audit philosophy to software quality and CI/CD workflows.

---

## Inner Council

**Inner Council** is a human-facing Liminal concept.

It represents structured inner dialogue: different voices, values, perspectives, or parts of the person can be heard and integrated.

The goal is not to fragment the person, but to help them regain clarity and mature direction.

---

## Maturity Engine

The **Maturity Engine** is a human-facing concept for tracking movement from reaction toward more stable, reflective, and responsible action.

It is experimental.

It should be treated as a design/research direction, not as a finished psychological model.

---

## Reactive State

**Reactive state** is state that changes in response to signals, impulses, feedback, or system conditions.

In Liminal, reactive state can apply to:

- human emotional patterns
- agent memory
- system telemetry
- database state
- audit workflows

---

## Agent Efficiency Audit

**Liminal Agent Efficiency Audit** is the first practical commercial entry point for the Liminal ecosystem.

It checks where AI-agent workflows lose:

- context
- money
- safety
- permission chains
- replayability
- audit evidence
- test stability
- routing efficiency

See [`docs/commercial/AGENT_EFFICIENCY_AUDIT.md`](commercial/AGENT_EFFICIENCY_AUDIT.md).

---

## Minimal Demo

The current minimal demo is located at:

```text
examples/agent_audit_demo/
```

It shows:

1. an agent attempts a refund
2. the output looks acceptable
3. the trace has no anchor for execution
4. CML marks the action causally invalid
5. CaPU returns HOLD
6. the audit report explains why

Run it with:

```bash
python examples/agent_audit_demo/run_demo.py
```

---

## Status Vocabulary

### Working prototype

A component that can be run or inspected, but may not be production-ready.

### Experimental

A concept or module that is still being explored and should not be treated as stable.

### Spec-first

A project whose core invariants, lifecycle, and expected behavior are being defined before or alongside implementation.

### Production-ready

A component that has stable deployment, testing, monitoring, documentation, and security expectations.

Most of the root Liminal ecosystem should currently be treated as early-access, prototype, or experimental unless a specific component says otherwise.
