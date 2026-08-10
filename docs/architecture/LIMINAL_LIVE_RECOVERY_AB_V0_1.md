# Live Recovery A/B Evidence v0.1

Status: experimental evidence, not a production performance claim.

## Question

Does a bounded, evidence-ranked Focus-Field recovery context reduce the cost of restoring a verified continuation anchor compared with presenting the full sequential checkpoint history?

The experiment intentionally tests a stronger claim than the synthetic recovery benchmark: both arms use a real Gonka provider call and must recover the same anchor without the JSON schema revealing the expected goal, parent step, or checkpoint.

## Common task

Provider/model:

- Gonka OpenAI-compatible broker path
- `MiniMaxAI/MiniMax-M2.7`

Expected recovery anchor:

- checkpoint: `checkpoint-09`
- goal: `invoice-reconciliation-v3`
- parent step: `ledger-apply-07`
- status: `verified`

Sequential input contains 12 checkpoint records.

Focus-Field input contains three candidates drawn from that exact history. In the ranked variant, candidates are ordered using generic evidence only:

1. verification state;
2. lifecycle state (`active > candidate > superseded > speculative > interrupted`);
3. deterministic checkpoint ID tie-break.

The ranking function does not use the expected goal, parent-step, or checkpoint constants.

## Safety / anti-cheating controls

- The response JSON schema constrains shape and generic identifier syntax only.
- Expected anchor values are not present as schema enums.
- Both arms receive the same recovery rule.
- Each live arm uses the receipt-instrumented real provider boundary.
- The local wrapper cache is cleared before every call.
- Later paired trials include a unique nonce per pair, shared by both arms, to reduce reuse of identical upstream broker-cache responses.
- Provider reasoning / `<think>` content is never persisted; only hashes and the four-field synthetic structured output are retained.
- A cost comparison is marked qualified only if every trial in both arms verifies.

## What the exploratory runs showed

### Unranked field, 512 output-token budget

Sequential:

- verified: 3/3
- prompt tokens: 770 per trial

Focus-Field:

- verified: 0/3
- prompt tokens: 305 per trial
- all three calls reached the 512 completion-token limit before producing verifiable JSON

The field reduced prompt tokens by about 60%, but the recovery failed. Therefore no successful-cost claim is valid.

### Unranked field, 1024 output-token budget

Sequential verified 1/3. Focus-Field verified 0/3.

The Focus-Field calls selected the correct checkpoint/evidence and parent step, but repeatedly failed exact goal continuity. This exposed an important failure mode: truncating history into a small unranked candidate set is not equivalent to field-mediated recovery.

### Ranked field, 640 output-token budget

After generic evidence ranking:

Sequential:

- verified: 0/3
- prompt tokens total: 2373
- all three calls reached the 640 completion-token limit

Ranked Focus-Field:

- verified: 3/3
- prompt tokens total: 1041
- total tokens: 2604 vs 4293 for Sequential

This run strongly suggested that candidate ranking matters. However repeated identical response hashes and near-zero latency on later calls indicated possible upstream broker caching, so its latency/repeatability results are not treated as independent trials.

Artifact:

- Actions run: `31408099404`
- artifact: `9070438687`
- digest: `sha256:0ef7c0f6eddbfa457ef9b41dff5ee38272250b02d97b557bcd25daa37c932d62`

## Cache-resistant paired run

The next version added a unique nonce per pair, shared by Sequential and Focus-Field, and increased the common output budget to 1536 tokens.

Before adding generic identifier syntax constraints, both methods verified 2/3 trials. Focus-Field used materially fewer prompt tokens, but the all-trials qualification gate remained false.

The final protocol also constrained identifier syntax generically (`^[a-z0-9-]+$`) without revealing expected values.

Final cache-resistant run:

| Metric | Sequential | Ranked Focus-Field |
|---|---:|---:|
| Verified trials | 2/3 | 1/3 |
| Prompt tokens total | 2407 | 1131 |
| Completion tokens total | 4193 | 3513 |
| Total tokens | 6600 | 4644 |
| Median latency | 18.641 s | 22.320 s |

Raw differences for this run:

- Focus-Field prompt-token reduction: **53.012%**
- Focus-Field total-token reduction: **29.636%**
- Focus-Field median latency difference: **19.732% slower**
- Focus-Field context-character reduction: **72.844%**

These cost/latency deltas are descriptive measurements, **not a qualified successful-recovery savings claim**, because not every trial verified.

Final artifact:

- Actions run: `31408790526`
- artifact: `9070826253`
- digest: `sha256:c1eaa4eba0861be964ef9ba8fdebe52d2ea91e0d7b9322ebef3811e0a65f2668`

## Interpretation

The live evidence does **not** support the claim that a smaller recovery context is automatically cheaper, faster, or more reliable.

It does support several narrower engineering conclusions:

1. Recovery geometry materially changes provider behavior.
2. A bounded field can cut input-token load substantially (roughly 53–60% in these fixtures).
3. Candidate ranking is important; an unranked compact field can lose goal continuity even when it finds the correct checkpoint.
4. Lower input-token cost can be offset by longer model reasoning/output.
5. Provider/model stochasticity and structured-output behavior remain first-class recovery risks.
6. Verification must gate continuation; token savings from an incorrect recovery are not savings.
7. Flow/Recovery policy should learn not only field size/cost, but observed field success probability and model-specific completion pressure.

## Updated hypothesis

> When a relevant recovery anchor is deep in history, a bounded evidence-ranked candidate field can reduce the amount of context that must be presented to the model. Whether this produces a net efficiency gain depends on the field's verification success rate and on model-specific completion/reasoning behavior.

This replaces the stronger and unsupported formulation that Focus-Field should generally be cheaper whenever its input context is smaller.

## Next engineering implication

The Recovery Router should eventually incorporate empirical signals such as:

- `field_success_rate`
- `sequential_success_rate`
- `field_completion_pressure`
- `sequential_completion_pressure`
- provider/model-specific historical recovery cost

and choose Focus-Field only when expected verified utility, not raw context size, is better.
