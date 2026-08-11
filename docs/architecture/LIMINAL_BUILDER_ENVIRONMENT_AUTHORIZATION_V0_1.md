# Liminal Builder Environment Authorization v0.1

Status: experimental / non-normative.

## Purpose

A valid GitHub/Sigstore artifact attestation proves that a particular workflow identity signed a particular proof bundle. It does not, by itself, authorize every runtime or dependency environment that workflow might use.

This layer authorizes the machine-readable `builder-environment.json` embedded inside the already-attested recovery proof bundle.

## Verification order

```text
recovery-proof-bundle.zip
        |
        v
GitHub/Sigstore cryptographic verification
        |
        v
GitHub workflow identity authorization
        |
        v
canonical proof-bundle verification
        |
        v
embedded builder-environment.json
        |
        v
Builder Environment Policy
        |
        +-- exact builder SHA
        +-- exact builder workflow file hash
        +-- exact dependency lock hash
        +-- exact proof-script hash
        +-- exact CPython and pip versions
        +-- exact GitHub Action SHAs
        +-- allowed runner family
        |
        v
AUTHORIZED / REJECT
```

The environment receipt is read from the attested ZIP itself. A detached sidecar file with the same name is not evidence for this policy.

## Trusted Builder v0.3 policy

The current policy is stored at:

`policies/trusted-recovery-proof-builder-v0.3.json`

It pins:

- repository `safal207/Liminal`;
- immutable builder commit `02beb48b9c8a61d67c585573aac6c5781c000e89`;
- the SHA-256 of the builder workflow file;
- the SHA-256 of the hash-locked dependency closure;
- the SHA-256 of the live proof script;
- CPython `3.11.15`;
- pip `26.1.2`;
- exact commit SHAs for the GitHub Actions used by the builder;
- runner family `Linux / X64 / ubuntu24`.

The exact GitHub runner image version remains observable evidence but is not currently an authorization pin. GitHub-hosted runner image patch releases can therefore change without silently changing the trusted OS family. A future policy may pin an exact image version if stronger reproducibility requirements justify the operational cost.

## Failure behavior

Authorization is fail-closed. Examples include:

- dependency lock digest mismatch;
- proof-script digest mismatch;
- Python or pip version mismatch;
- unexpected action SHA or additional action;
- unexpected runner OS, architecture, or image family;
- builder workflow or commit mismatch;
- malformed/non-canonical environment receipt;
- invalid/tampered proof bundle.

A failure does not imply that the artifact is malicious. It means that the artifact was not produced under the explicitly authorized environment policy.

## Trust boundary

This layer does not make the GitHub-hosted runner hermetic and does not independently reproduce the operating system image. It records and authorizes critical build inputs while retaining external trust in GitHub Actions, the GitHub-hosted runner infrastructure, the Python toolcache, and Sigstore/Fulcio/Rekor.

It also does not grant runtime or tool authority to an AI agent. It only evaluates provenance for recovery decision evidence.
