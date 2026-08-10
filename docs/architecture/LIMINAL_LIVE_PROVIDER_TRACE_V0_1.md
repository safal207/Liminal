# Liminal Live Provider Trace v0.1

## Purpose

This probe upgrades the receipt pipeline from a mocked provider response to a real OpenAI API execution while preserving a strict evidence boundary.

The live run is intentionally small and non-sensitive. It asks the provider to return a fixed JSON contract containing a goal identifier, parent-step identifier, status, and evidence marker. The response body is not persisted in the trace artifact; only its SHA-256 digest is stored.

## Execution path

```text
GitHub Secret: OPENAI_API_KEY
        |
        v
OpenAIWrapper (mock disabled, fallback disabled)
        |
        v
InstrumentedOpenAIService._call_openai
        |
        v
provider TokenUsageReceipt
        |
        +--> deterministic goal-id verifier
        +--> deterministic parent-step verifier
        +--> exact JSON-contract verifier
        |
        v
Goal / Causal / VerifiedProgress receipts
        |
        v
ReceiptTelemetry
        |
        v
RuntimeTelemetry
        |
        v
Flow Regulator
        |
        +--> nominal: FLOW is expected when the verified provider call is healthy
        |
        +--> induced-recovery: configured recovery pressure exercises
              Flow -> Recovery Router -> Focus-Field using the same live evidence
```

## Fail-closed guarantees

The runner fails instead of silently falling back when:

- `OPENAI_API_KEY` is absent;
- the real OpenAI client cannot initialize;
- the provider call fails;
- a token-usage receipt is absent or duplicated;
- required receipt-backed telemetry is incomplete;
- the provider response fails the deterministic probe contract.

`OPENAI_FALLBACK_TO_LOCAL=false` and `OPENAI_MOCK_ONLY=false` are enforced by the workflow. Repository `.env` files are not used as a secret source.

## Evidence classification

### Measured in the live run

- provider prompt/completion token counts;
- wall-clock provider-call latency;
- successful provider-call completion;
- goal identifier continuity;
- parent-step causal continuity;
- exact response-contract verification;
- derived token/context utilization.

### Configured probe inputs

The following are not yet production measurements and are labelled as configured inputs in the artifact:

- task difficulty;
- available capability;
- recent recovery rate;
- replay-step estimate;
- Focus-Field candidate count;
- anchor score;
- field uncertainty/cost.

This distinction is essential: a live provider call does not by itself prove production recovery savings.

## Probe modes

### `nominal`

Uses the live provider evidence with no induced recovery pressure. A verified, low-pressure call should remain in the Flow corridor and should not invoke the Recovery Router.

### `induced-recovery`

Uses the same live provider evidence but explicitly configures high recent-recovery pressure and a bounded recovery geometry. This exercises the real Flow -> Recovery -> Focus-Field control code. Any estimated savings are policy calculations for the configured probe, not measured token or wall-clock savings.

## GitHub Actions

Workflow: `.github/workflows/live-provider-trace.yml`

It is manual (`workflow_dispatch`) and uses the protected `live-provider-trace` environment. Supply:

- GitHub Secret `OPENAI_API_KEY`;
- the exact model ID to test;
- that model's context-window size;
- probe mode;
- latency budget used for normalization.

The workflow uploads `artifacts/live-provider-trace.json` and never writes the API key or raw response body into the artifact.

## Next evidence step

Replace configured recovery geometry with measurements from a real multi-step agent execution: checkpoints, interruption events, actual replay depth, candidate retrieval counts, verification outcomes, retries, and recovery latency. At that point Focus-Field savings can be compared against a true sequential baseline instead of a configured probe.
