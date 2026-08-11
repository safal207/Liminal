# Liminal Builder Environment Receipt v0.1

Status: experimental.

## Goal

Bind machine-readable builder context into the same canonical recovery proof bundle that receives the GitHub OIDC / Sigstore artifact attestation.

The receipt answers a narrower question than the outer attestation:

> What immutable builder revision and observed execution environment produced this proof bundle, and what critical files/actions did that builder use?

It does not replace GitHub Artifact Attestations or the existing Liminal signer-identity policy.

## Receipt

The trusted builder writes:

`builder-environment.json`

before any live provider call. The live proof runner verifies the receipt against the current immutable checkout before it configures Gonka.

The canonical receipt records:

- builder repository;
- builder workflow path;
- immutable builder workflow Git SHA;
- SHA-256 of the builder workflow file;
- dependency lock path and SHA-256;
- live proof script path and SHA-256;
- Python implementation and exact runtime version;
- pip version supplied by the selected toolchain;
- runner OS and architecture;
- GitHub runner image OS identifier and image version;
- exact Git commit pins for builder actions.

The receipt intentionally excludes secrets, broker URLs, API keys, prompts, provider response text, and model reasoning.

## Proof bundle binding

Recovery proof bundle schema v0.2 requires `builder-environment.json` as a canonical member.

The normal chain becomes:

```text
immutable builder commit
        |
        +-- workflow bytes hash
        +-- dependency lock hash
        +-- proof script hash
        +-- runtime/image observation
        +-- action commit pins
        |
        v
builder-environment.json
        |
        v
proof-manifest.json
        |
        v
recovery-proof-bundle.zip
        |
        v
GitHub OIDC / Sigstore attestation
```

Changing any receipt bytes changes the proof manifest and therefore the attested bundle digest.

## Fail-closed rules

The live proof runner refuses to call the provider unless:

1. trusted builder repository and workflow SHA are provided by the reusable workflow environment;
2. the receipt is canonical JSON;
3. the receipt workflow SHA matches the expected immutable builder SHA;
4. workflow, dependency lock, and proof script hashes match the checked-out bytes;
5. the receipt schema and immutable action pins are valid.

The builder also validates that its checked-out Git HEAD equals `job.workflow_sha` before writing the receipt.

## Relationship to signer identity

The GitHub/Fulcio signer digest remains the external identity root. The builder workflow SHA in this receipt must match that same immutable builder revision during the live trust-rotation proof.

The Git commit digest binds the complete source tree, including receipt-generation code. The explicit SHA-256 values in the receipt make the critical builder inputs directly inspectable without requiring a repository tree walk.

## Remaining trust boundary

This receipt improves evidence and reproducibility but does not claim a hermetic build or a SLSA level.

External dependencies still include:

- GitHub Actions control plane and hosted-runner provisioning;
- the contents behind the recorded runner image version;
- the Python toolcache/bootstrap installer;
- package-index availability, although accepted wheel bytes are hash-locked;
- Sigstore/Fulcio/Rekor infrastructure;
- protected environment secrets;
- Gonka/provider behavior.

A later layer can include a machine-readable runner-image/source descriptor and bootstrap-tool hashes, or move the proof builder to a content-addressed execution image where practical.
