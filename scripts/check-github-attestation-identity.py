#!/usr/bin/env python3
"""Enforce Liminal's GitHub attestation identity policy after gh verification."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from liminal.github_attestation_identity import (
    GitHubAttestationIdentityPolicy,
    authorize_verified_github_attestation,
)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--verification-json", required=True)
    parser.add_argument("--repository", required=True)
    parser.add_argument("--repository-id", required=True)
    parser.add_argument("--signer-workflow", required=True)
    parser.add_argument("--environment", required=True)
    parser.add_argument("--source-ref", required=True)
    parser.add_argument("--output", required=True)
    args = parser.parse_args()

    payload = json.loads(Path(args.verification_json).read_text(encoding="utf-8"))
    policy = GitHubAttestationIdentityPolicy(
        repository=args.repository,
        repository_id=args.repository_id,
        signer_workflow_path=args.signer_workflow,
        source_ref=args.source_ref,
        deployment_environment=args.environment,
    )
    result = authorize_verified_github_attestation(payload, policy=policy)
    output = {
        "schema_version": "liminal.github-attestation-identity-authorization.v0.1",
        "authorized": result.authorized,
        "reason": result.reason,
        "claims": None if result.claims is None else {
            "repository_uri": result.claims.repository_uri,
            "repository_id": result.claims.repository_id,
            "source_ref": result.claims.source_ref,
            "signer_uri": result.claims.signer_uri,
            "signer_digest": result.claims.signer_digest,
            "deployment_environment": result.claims.deployment_environment,
            "runner_environment": result.claims.runner_environment,
            "oidc_issuer": result.claims.oidc_issuer,
        },
    }
    Path(args.output).write_text(json.dumps(output, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    print(json.dumps(output, sort_keys=True))
    return 0 if result.authorized else 1


if __name__ == "__main__":
    raise SystemExit(main())
