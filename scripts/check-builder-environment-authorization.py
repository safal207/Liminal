#!/usr/bin/env python3
"""Authorize the environment receipt embedded in an attested recovery proof bundle."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from liminal.builder_environment_policy import (
    BUILDER_ENVIRONMENT_AUTHORIZATION_SCHEMA_VERSION,
    authorize_recovery_proof_builder_environment,
    load_builder_environment_policy,
)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--bundle", required=True)
    parser.add_argument("--policy", required=True)
    parser.add_argument("--output", required=True)
    args = parser.parse_args()

    policy = load_builder_environment_policy(args.policy)
    result = authorize_recovery_proof_builder_environment(args.bundle, policy=policy)
    output = {
        "schema_version": BUILDER_ENVIRONMENT_AUTHORIZATION_SCHEMA_VERSION,
        "authorized": result.authorized,
        "reason": result.reason,
        "policy": policy.as_dict(),
        "receipt": None if result.receipt is None else result.receipt.as_dict(),
    }
    target = Path(args.output)
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(json.dumps(output, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    print(json.dumps(output, sort_keys=True))
    return 0 if result.authorized else 1


if __name__ == "__main__":
    raise SystemExit(main())
