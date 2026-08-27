# Liminal Trusted Recovery Proof Builder v0.2

Status: experimental.

## Goal

Strengthen trusted builder v0.1 by removing mutable Python dependency resolution from the live recovery proof path.

The builder still uses an immutable reusable-workflow commit as its signer identity. v0.2 additionally binds the Python dependency closure to exact versions and wheel SHA-256 values for the supported builder platform.

## Supported builder platform

v0.2 fixes:

- GitHub-hosted runner label: `ubuntu-24.04`;
- architecture used to generate the lock: x86_64;
- CPython: `3.11.15`;
- dependency source form: binary wheels only;
- dependency installation: pip hash-checking mode.

The lock is:

`requirements/trusted-recovery-proof.lock`

It contains the 20-package closure observed in the previously validated live proof environment, including direct and transitive dependencies. Every entry is exact-version pinned and carries the SHA-256 of the wheel selected on Ubuntu 24.04 / CPython 3.11 x86_64.

The closure was generated in GitHub Actions workflow run `31462868476`. The generator downloaded the exact versions with `--only-binary=:all:` and computed SHA-256 over every downloaded wheel.

## Installation rule

The trusted builder must install with:

```text
python -m pip install \
  --disable-pip-version-check \
  --require-hashes \
  --only-binary=:all: \
  -r requirements/trusted-recovery-proof.lock
```

It then runs `python -m pip check` before any live provider call.

Consequences:

1. an unpinned transitive dependency cannot be silently introduced by ordinary resolver drift;
2. a package file with bytes different from the reviewed wheel hash is rejected;
3. source-distribution fallback is disabled;
4. the supported OS/Python surface is explicit rather than `ubuntu-latest` / floating Python 3.11.

## Immutable builder identity

The reusable workflow remains:

`.github/workflows/trusted-recovery-proof-builder.yml`

A trusted caller must reference the builder by exact Git commit SHA. Inside the reusable job, the builder checks out `job.workflow_sha` and verifies that `git rev-parse HEAD` equals that SHA before installing dependencies or running the proof.

The signer digest therefore binds both the workflow and the lock file stored in that commit.

## Trust rotation

v0.1 remains independently identifiable by its historical signer digest:

`0a02df376a91ef870573e811370ff62ce2461111`

v0.2 must not replace that trust anchor automatically. The v0.2 builder commit is trusted only after:

1. ordinary exact-head CI succeeds;
2. the permanent wrapper is explicitly rotated to the reviewed v0.2 signer digest;
3. a live proof succeeds through the pinned v0.2 builder;
4. GitHub attestation verification and Liminal's claim authorization both accept the same signer digest.

## Remaining boundary

This is stronger than v0.1 but is not claimed to be fully hermetic or a particular SLSA level.

The exact package payloads are hash-locked, but the bootstrap installer (`pip` supplied with the selected Python toolchain), GitHub-hosted runner image contents, package index availability, GitHub Actions service, Gonka provider behavior, and protected environment secrets remain external trust dependencies.

A later version can additionally pin/bootstrap the installer, move from a hosted image label toward an image/content digest where practical, and persist a machine-readable builder environment descriptor inside the proof bundle.
