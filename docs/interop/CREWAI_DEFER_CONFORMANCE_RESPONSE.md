@rpelevin @Rul1an @haroldmalikfrimpong-ops Thank you — I tightened the framework-neutral DEFER/continuation package around the exact profile/local-ID boundary you called out.

Updated fixture:
https://github.com/safal207/Liminal/blob/main/fixtures/defer-continuation-conformance-v0.1.json

Updated note:
https://github.com/safal207/Liminal/blob/main/docs/interop/DEFER_CONTINUATION_CONFORMANCE_V0_1.md

Independent dependency-free generator:
https://github.com/safal207/Liminal/blob/main/tools/generate_defer_continuation_vector.py

The generator intentionally does not read the fixture. It independently emits canonical UTF-8 preimage bytes and SHA-256 digests from the published profile inputs.

The fixture now makes the portable-profile boundary explicit:

- same framework-local IDs + different profile ID -> `NOT_COMPARABLE_PROFILE`;
- different framework-local IDs + identical profile IDs, canonical bytes, and digests -> `COMPARABLE`;
- local action/pending IDs are record locators only and are excluded from the normative preimages;
- every case names `expected_comparability`;
- every reject/expiry/replay case still asserts zero additional side effects.

I also added the exact canonical action-identity and binding bytes to the baseline fixture so independent implementations can compare bytes before comparing digests.

The published generator reproduces the fixture digests for caller, tool, scope, args, policy, and target-state mutations. The action/binding split remains localized:

- caller/tool/scope change only `action_ref`;
- args/policy/target state change only `binding_digest`.

The invariant is now:

> A continuation can resume only against the same frozen authorization context under the same declared comparison profile; framework-local identifiers do not alter normative comparison semantics, and an unchanged retry must not create a second approval or side effect.

Boundary: architecture and test feedback only; no claim about running CrewAI or this issue's proposed implementation.
