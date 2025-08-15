#!/usr/bin/env bash
# Quick health/readiness check for LIMINAL API
# Usage:
#   ./scripts/check-health.sh [--url http://127.0.0.1:8000] [--retries 30] [--delay 2] [--fail-on-not-ready]

set -euo pipefail

BASE_URL="http://127.0.0.1:8000"
RETRIES=30
DELAY=2
FAIL_ON_NOT_READY=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --url|-u)
      BASE_URL="${2:-}"; shift 2 ;;
    --retries|-r)
      RETRIES="${2:-}"; shift 2 ;;
    --delay|-d)
      DELAY="${2:-}"; shift 2 ;;
    --fail-on-not-ready|-f)
      FAIL_ON_NOT_READY=1; shift ;;
    --help|-h)
      echo "Usage: $0 [--url URL] [--retries N] [--delay SEC] [--fail-on-not-ready]"; exit 0 ;;
    *)
      echo "Unknown argument: $1" >&2; exit 1 ;;
  esac
done

HEALTH_URL="${BASE_URL%/}/health"
READY_URL="${BASE_URL%/}/ready"

attempt=0
health_ok=0
ready_ok=0
HEALTH_JSON=""
READY_JSON=""

echo "Checking API at ${BASE_URL} (health/ready)" >&2

fetch_json() {
  # $1 = url
  local out
  if out=$(curl -fsS --max-time 5 "$1"); then
    printf '%s' "$out"
    return 0
  else
    return 1
  fi
}

while [[ $attempt -lt $RETRIES ]]; do
  attempt=$((attempt+1))
  if HEALTH_JSON=$(fetch_json "$HEALTH_URL"); then health_ok=1; else health_ok=0; fi
  if READY_JSON=$(fetch_json "$READY_URL"); then ready_ok=1; else ready_ok=0; fi

  if [[ $health_ok -eq 1 && $ready_ok -eq 1 ]]; then
    break
  fi
  sleep "$DELAY"
done

echo
echo "— /health —"
if [[ $health_ok -eq 1 ]]; then
  echo "$HEALTH_JSON" | sed 's/^/  /'
else
  echo "failed" >&2
fi

echo
echo "— /ready —"
if [[ $ready_ok -eq 1 ]]; then
  echo "$READY_JSON" | sed 's/^/  /'
else
  echo "failed" >&2
fi

ready_flag=0
if [[ $ready_ok -eq 1 ]]; then
  if echo "$READY_JSON" | grep -Eq '"ready"\s*:\s*true'; then
    ready_flag=1
  fi
fi

echo
echo "Summary:"
if [[ $health_ok -eq 1 ]]; then echo "health: ok"; else echo "health: fail"; fi
if [[ $ready_flag -eq 1 ]]; then echo "ready:  true"; else echo "ready:  false"; fi

if [[ $FAIL_ON_NOT_READY -eq 1 && $ready_flag -ne 1 ]]; then
  echo "API is not ready (ready:false)" >&2
  exit 2
fi

exit 0
