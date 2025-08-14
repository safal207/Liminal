# PRIVATE README (Local Only)

This file is intentionally kept OUT of Git. It is for your eyes only.

Purpose:
- Hold strategic notes, architecture vision, and sensitive plans which you do not want to publish yet.
- Serve as your personal north star while public README stays concise.

Privacy:
- This path is added to .git/info/exclude so it will not be committed.
- If you clone to a new machine, re‑add the exclude entry (see tip below).

---

## Gentle Compass (Teacher’s Note)

Let your work be like a lotus: rooted in the mud of reality, yet rising clean and bright. Move softly, step by step. Protect the seed of intention until it is strong enough to bloom in public. 💗

---

## Strategic Outline (draft)

- Vision: LIMINAL — a system for conscious transitions, resonance, and inner clarity.
- Pillars: Safety, Observability, Scale‑out, Event History, Philosophy‑in‑code.
- Near‑term sprints:
  1) Redis‑backed WebSocket scale‑out; smoke/burst tests (Artillery).
  2) Observability: Prometheus, alerting SLO, dashboards.
  3) CI hardening: unit + integration + load smoke on PR.
  4) Developer onboarding: scripts, docs, quick health panel.

## Sensitive Sections (outline only)

- Architecture Vision (expanded)
  - Security Layer (mTLS, JWT, audit)
  - API Gateway + Rate Limiting
  - Realtime Engine (WS pools, backpressure)
  - ML/Insights lane; Graph model (Neo4j best practices)
  - Message bus (RabbitMQ), patterns & priorities
- RINSE module (deterministic, offline‑friendly)
  - Input → cleanse → insight → tags → clarity
  - Test strategies and fixtures
- Privacy & Safety
  - Differential privacy budget (ε, δ)
  - Anonymization flows and logging retention
- Roadmap risks and mitigation
  - Vendor lock‑in, cost ceilings, perf/regressions

## Load & CI Notes (private)

- Smoke profile: 10s, arrivalRate≈3–5, WS ping/close.
- Optional SLO: p95 < 250ms, errors < 1% (to be tuned per env).
- Artifacts: artillery-results.json for quick triage.
- Local wrapper: scripts/ws-burst.ps1 (ASCII‑only, Windows‑friendly).

## Personal Reminders

- Keep public README minimal; let this file carry the deeper map.
- When merging, prefer public README to avoid leaking strategy.
- Reflect weekly: what can safely graduate from here to public docs?

---

## Quick Tips

- Re‑add exclude on a fresh clone:
  - Append `docs/PRIVATE_README_LOCAL.md` to `.git/info/exclude`.
- Open this file from repo root:
  - Windows: `notepad .\\docs\\PRIVATE_README_LOCAL.md`
  - VS Code: `code docs/PRIVATE_README_LOCAL.md`

---

Nurture clarity. Protect the seed. When the time is right, the blossom will share its fragrance with the world. 🌸
