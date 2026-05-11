# Liminal Agent Efficiency Audit

**Liminal Agent Efficiency Audit** is the first practical commercial entry point for the Liminal ecosystem.

It helps teams find where AI-agent workflows lose context, money, safety, and auditability.

See also: [Agent Audit Demo Story](../demo/AGENT_AUDIT_DEMO_STORY.md).

---

## Problem

AI-agent systems can appear functional while hiding deep operational problems:

- context is lost between steps
- outputs cannot be deterministically replayed
- actions happen without durable permission chains
- logs show events but not responsibility
- routing decisions waste money
- safety failures are discovered too late
- flaky tests block delivery
- compliance evidence is incomplete

Traditional logs answer: **what happened?**

Liminal asks deeper questions:

- Why was this action allowed?
- Which trace proves it?
- Which permission chain supports it?
- Can we replay the decision?
- Where did drift begin?
- What should have been blocked, held, or audited?

---

## What we audit

1. **Trace continuity**
   - Missing steps
   - Broken transitions
   - Unreplayable actions
   - Drift between expected and actual behavior

2. **Causal validity**
   - Missing parent reasons
   - Missing permission chains
   - Unsafe SECRET → NET / tool-use chains
   - Actions that look correct but are causally invalid

3. **Execution safety**
   - Execute-before-commit patterns
   - Side effects without durable state
   - Missing hold/reject/block decisions
   - Weak retry and timeout behavior

4. **Routing efficiency**
   - Overpriced model usage
   - Poor fallback behavior
   - No latency/cost-aware routing
   - Provider failure handling gaps

5. **QA intelligence**
   - Flaky tests
   - Unstable pipelines
   - Weak merge gates
   - Missing risk scoring

---

## Liminal Stack components

| Component | Role in the audit |
|---|---|
| LTP | Trace replay and continuity inspection |
| CML | Causal memory and permission validation |
| CaPU | Safe action lifecycle: Gate → Incubate → Commit → Execute |
| LiminalDB | Reactive state and audit storage |
| DAO_lim | Intent-aware routing and fallback analysis |
| LiminalQAengineer | Test risk, flake risk, and QA observability |

---

## Audit output

The audit should produce:

- risk map
- trace continuity report
- causal validity report
- unsafe action examples
- routing waste analysis
- QA risk summary
- recommended fixes
- demo-ready before/after story

---

## Target users

- AI startups
- agentic workflow teams
- fintech teams
- QA automation teams
- LLM infrastructure teams
- compliance-heavy companies
- teams using multi-step AI agents in production or near-production environments

---

## Offer

> We audit your AI-agent workflow and show where it loses context, money, safety, and auditability.

Russian version:

> Мы проверяем ваш AI-agent pipeline и показываем, где он теряет контекст, деньги, безопасность и доказуемость.

---

## Demo story

A simple demo should show:

1. An agent performs a multi-step task.
2. The output looks acceptable.
3. LTP shows trace drift or missing continuity.
4. CML shows a missing reason or permission chain.
5. CaPU shows the action should have been held or blocked.
6. Liminal audit report explains what happened and how to fix it.

Full version: [Agent Audit Demo Story](../demo/AGENT_AUDIT_DEMO_STORY.md).

---

## What this is not

This is not a generic AI consulting package.

This is not prompt engineering.

This is not just observability.

This is a focused audit of agent continuity, causal validity, safe execution, routing efficiency, and QA reliability.
