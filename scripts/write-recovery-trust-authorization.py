#!/usr/bin/env python3
"""Write a canonical recovery trust authorization receipt."""

from __future__ import annotations

import argparse
from pathlib import Path

from liminal.recovery_trust_authorization import (
    build_recovery_trust_authorization_receipt,
    write_recovery_trust_authorization_receipt,
)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--proof-bundle", required=True)
    parser.add_argument("--identity-authorization", required=True)
    parser.add_argument("--environment-authorization", required=True)
    parser.add_argument("--verifier-repository", required=True)
    parser.add_argument("--verifier-workflow-path", required=True)
    parser.add_argument("--verifier-workflow-sha", required=True)
    parser.add_argument("--output", required=True)
    args = parser.parse_args()

    receipt = build_recovery_trust_authorization_receipt(
        proof_bundle_path=args.proof_bundle,
        identity_authorization_path=args.identity_authorization,
        environment_authorization_path=args.environment_authorization,
        verifier_repository=args.verifier_repository,
        verifier_workflow_path=args.verifier_workflow_path,
        verifier_workflow_sha=args.verifier_workflow_sha,
    )
    write_recovery_trust_authorization_receipt(receipt, Path(args.output))
    print(Path(args.output))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
