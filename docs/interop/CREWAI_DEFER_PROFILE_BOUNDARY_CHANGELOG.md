# CrewAI DEFER Profile Boundary Change Log

## 2026-06-23

- Added canonical UTF-8 preimage bytes to the fixture.
- Added an independent dependency-free generator that does not read the fixture.
- Added explicit framework-local identifier fields outside normative preimages.
- Added `different local IDs + same profile bytes/digests -> COMPARABLE`.
- Tightened `same local IDs + different profile ID -> NOT_COMPARABLE_PROFILE`.
- Added explicit comparability semantics to every case.
- Preserved zero-additional-side-effect assertions for every rejection.
