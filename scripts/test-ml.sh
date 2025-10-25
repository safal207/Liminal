#!/usr/bin/env bash
set -euo pipefail

# Move to repo root (directory of this script is /scripts)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="${SCRIPT_DIR%/scripts}"
cd "$REPO_ROOT"

# Usage
if [[ "${1-}" == "--help" || "${1-}" == "-h" ]]; then
  echo "Usage: ./scripts/test-ml.sh [--all]"
  echo "  --all   Run full non-integration test suite"
  exit 0
fi

if [[ "${1-}" == "--all" ]]; then
  echo "[ML TEST] Running full non-integration suite..."
  python3 -m pytest -v -m "not integration"
else
  echo "[ML TEST] Running multilingual expected_lang tests..."
  python3 -m pytest -q backend/tests/test_multilingual_expected_lang.py
fi
