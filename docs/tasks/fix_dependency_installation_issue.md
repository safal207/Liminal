# Codex Task: Repair Development Dependency Installation

## Context
Liminal is a Python 3.11 project whose CI/CD workflows run on Ubuntu images in GitHub Actions. The pipeline installs both runtime (`requirements.txt`) and development (`requirements-dev.txt`) dependencies. Recent runs fail when pip tries to install the local package `tools/flake8_shim` during the development dependency step executed with `--no-build-isolation`.

## Observed Failure
```
error: invalid command 'bdist_wheel'
× Encountered error while generating package metadata
```
The error occurs while executing:
```bash
python -m pip install --upgrade pip
pip install -r requirements.txt
pip install --no-build-isolation -r requirements-dev.txt
```
Within `requirements-dev.txt` the line `./tools/flake8_shim` pulls in the local shim package, but the package metadata lacks an explicit dependency on `wheel`, so the build backend cannot emit wheels inside the isolated environment.

## Objectives
1. **Unblock CI installations** by ensuring the GitHub Actions workflows pre-install the `wheel` package alongside `pip` and `setuptools` before installing project requirements.
2. **Define the shim package metadata** so local installs know how to build `flake8_shim` without relying on implicit defaults.
3. **Validate directory structure** to guarantee that the shim package is importable when referenced from `requirements-dev.txt`.
4. **Optionally improve development ergonomics** by installing the shim in editable mode if it simplifies developer workflows.

## Required Changes

### 1. Update GitHub Actions workflows
Locate every workflow in `.github/workflows/` that installs dependencies (for example `python-ci.yml` and `artillery-smoke.yml`). In each relevant job step, ensure the install command upgrades `wheel` and `setuptools` together with `pip` before consuming `requirements*.txt` files.

```yaml
- name: Install dependencies
  run: |
    python -m pip install --upgrade pip wheel setuptools
    pip install -r requirements.txt
    pip install --no-build-isolation -r requirements-dev.txt
```

### 2. Add build metadata for `tools/flake8_shim`
Create `tools/flake8_shim/pyproject.toml` with the following minimum configuration so pip knows which build backend to use and which build dependencies to preload:
```toml
[build-system]
requires = ["setuptools>=45", "wheel"]
build-backend = "setuptools.build_meta"

[project]
name = "flake8_shim"
version = "0.1.0"
description = "Offline-friendly Flake8 shim for the Liminal project"
requires-python = ">=3.11"
dependencies = []

[tool.setuptools]
packages = ["flake8_shim"]
```
If the directory is missing an `__init__.py`, add an empty file at `tools/flake8_shim/src/flake8_shim/__init__.py` so the package resolves properly.

### 3. Confirm `requirements-dev.txt`
Verify that `requirements-dev.txt` references the shim. If developers benefit from editable installs, switch the entry from `./tools/flake8_shim` to `-e ./tools/flake8_shim`.

### 4. Document verification commands
Update or create documentation (for example in `docs/` or the workflow comments) to show the local verification command sequence:
```bash
python -m pip install --upgrade pip wheel setuptools
pip install -r requirements.txt
pip install --no-build-isolation -r requirements-dev.txt
```

## Acceptance Criteria
- GitHub Actions no longer fail with `invalid command 'bdist_wheel'` when installing development dependencies.
- `tools/flake8_shim` installs successfully in both CI and local environments.
- Any new documentation clearly explains how to reproduce the fix locally.
- Existing functionality and dependency versions remain unchanged aside from the additions above.
