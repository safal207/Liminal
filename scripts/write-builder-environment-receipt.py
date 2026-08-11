#!/usr/bin/env python3
"""Write canonical trusted-builder environment evidence without secrets."""

from __future__ import annotations

import argparse
from pathlib import Path

from liminal.builder_environment_receipt import (
    BuilderActionPin,
    build_builder_environment_receipt,
    verify_builder_environment_receipt,
    write_builder_environment_receipt,
)


def _action_pin(value: str) -> BuilderActionPin:
    action, separator, sha = value.partition("=")
    if not separator:
        raise argparse.ArgumentTypeError("--action must use NAME=SHA")
    try:
        return BuilderActionPin(action=action, sha=sha)
    except ValueError as exc:
        raise argparse.ArgumentTypeError(str(exc)) from exc


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--repository-root", default=".")
    parser.add_argument("--output", required=True)
    parser.add_argument("--builder-repository", required=True)
    parser.add_argument("--builder-workflow-path", required=True)
    parser.add_argument("--builder-workflow-sha", required=True)
    parser.add_argument("--dependency-lock-path", required=True)
    parser.add_argument("--proof-script-path", required=True)
    parser.add_argument("--runner-os", required=True)
    parser.add_argument("--runner-arch", required=True)
    parser.add_argument("--runner-image-os", required=True)
    parser.add_argument("--runner-image-version", required=True)
    parser.add_argument("--action", action="append", type=_action_pin, default=[])
    args = parser.parse_args()

    receipt = build_builder_environment_receipt(
        repository_root=args.repository_root,
        builder_repository=args.builder_repository,
        builder_workflow_path=args.builder_workflow_path,
        builder_workflow_sha=args.builder_workflow_sha,
        dependency_lock_path=args.dependency_lock_path,
        proof_script_path=args.proof_script_path,
        runner_os=args.runner_os,
        runner_arch=args.runner_arch,
        runner_image_os=args.runner_image_os,
        runner_image_version=args.runner_image_version,
        action_pins=tuple(args.action),
    )
    target = Path(args.output)
    write_builder_environment_receipt(receipt, target)
    if not verify_builder_environment_receipt(
        target,
        repository_root=args.repository_root,
        expected_builder_repository=args.builder_repository,
        expected_builder_workflow_sha=args.builder_workflow_sha,
    ):
        raise RuntimeError("builder environment receipt self-verification failed")
    print(target)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
