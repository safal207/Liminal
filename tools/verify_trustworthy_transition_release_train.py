#!/usr/bin/env python3
"""Verify the trustworthy-transition release train and pinned GitHub state."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path
from typing import Any

PROFILE = "org.liminal.trustworthy-transition.release-train.v0.1"
RECEIPT_SCHEMA = "org.liminal.trustworthy-transition.release-train-receipt.v0.1"
SHA_RE = re.compile(r"^[0-9a-f]{40}$")
DIGEST_RE = re.compile(r"^sha256:[0-9a-f]{64}$")
ALLOWED_TRAIN_STATES = {"OPEN_STACKED", "MERGED"}
REQUIRED_BLOCKERS = {
    "CODEX_REVIEW_PENDING",
    "STACK_NOT_MERGED",
    "FINAL_MAIN_MANIFEST_NOT_ISSUED",
}
REQUIRED_INVALIDATION_RULES = {
    "TARGET_HEAD_CHANGED",
    "PINNED_PR_HEAD_CHANGED",
    "PINNED_PR_BASE_CHANGED",
    "ANCESTRY_CHANGED",
    "WORKFLOW_RUN_CHANGED_OR_FAILED",
    "RECEIPT_ARTIFACT_CHANGED_OR_EXPIRED",
    "REQUIRED_COMMIT_STATUS_NOT_SUCCESSFUL",
    "SUCCESSOR_HEAD_CHANGED_AFTER_RETARGET",
    "NON_MERGE_COMMIT_STRATEGY_USED",
}


class TrainVerificationError(ValueError):
    """Raised when the release train or its live pins are inconsistent."""


def load_json(path: Path) -> dict[str, Any]:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except OSError as error:
        raise TrainVerificationError(f"cannot read {path}: {error}") from error
    except json.JSONDecodeError as error:
        raise TrainVerificationError(f"invalid JSON in {path}: {error.msg}") from error
    if not isinstance(value, dict):
        raise TrainVerificationError(f"{path} root must be an object")
    return value


def canonical_json(value: Any) -> str:
    return json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":"))


def digest_json(value: Any) -> str:
    return "sha256:" + hashlib.sha256(canonical_json(value).encode("utf-8")).hexdigest()


def require_object(value: Any, label: str) -> dict[str, Any]:
    if not isinstance(value, dict):
        raise TrainVerificationError(f"{label} must be an object")
    return value


def require_text(value: Any, label: str) -> str:
    if not isinstance(value, str) or not value.strip():
        raise TrainVerificationError(f"{label} must be a non-empty string")
    return value.strip()


def require_bool(value: Any, label: str) -> bool:
    if not isinstance(value, bool):
        raise TrainVerificationError(f"{label} must be a boolean")
    return value


def require_positive_int(value: Any, label: str) -> int:
    if not isinstance(value, int) or isinstance(value, bool) or value <= 0:
        raise TrainVerificationError(f"{label} must be a positive integer")
    return value


def require_sha(value: Any, label: str) -> str:
    sha = require_text(value, label)
    if not SHA_RE.fullmatch(sha):
        raise TrainVerificationError(f"{label} must be a lowercase 40-character SHA")
    return sha


def require_digest(value: Any, label: str) -> str:
    digest = require_text(value, label)
    if not DIGEST_RE.fullmatch(digest):
        raise TrainVerificationError(f"{label} must be a sha256 reference")
    return digest


def require_string_list(value: Any, label: str) -> list[str]:
    if not isinstance(value, list) or any(
        not isinstance(item, str) or not item for item in value
    ):
        raise TrainVerificationError(f"{label} must be an array of strings")
    if len(value) != len(set(value)):
        raise TrainVerificationError(f"{label} must not contain duplicates")
    return value


def verify_manifest(manifest: dict[str, Any]) -> list[dict[str, Any]]:
    required_root = {
        "schema_version",
        "profile",
        "candidate_id",
        "candidate_state",
        "release_ready",
        "repository",
        "target",
        "merge_policy",
        "review_policy",
        "stages",
        "next_action",
        "release_blockers",
        "invalidation_rules",
        "claim_boundary",
    }
    if set(manifest) != required_root:
        raise TrainVerificationError(
            f"manifest keys invalid: missing={sorted(required_root - set(manifest))}, "
            f"unknown={sorted(set(manifest) - required_root)}"
        )
    if manifest["schema_version"] != 1 or manifest["profile"] != PROFILE:
        raise TrainVerificationError("manifest schema/profile mismatch")
    require_text(manifest["candidate_id"], "candidate_id")
    if manifest["candidate_state"] != "PRE_MERGE_FROZEN":
        raise TrainVerificationError("candidate_state must be PRE_MERGE_FROZEN")
    if require_bool(manifest["release_ready"], "release_ready"):
        raise TrainVerificationError("pre-merge candidate must not claim release readiness")
    repository = require_text(manifest["repository"], "repository")
    if repository != "safal207/Liminal":
        raise TrainVerificationError("repository pin mismatch")

    target = require_object(manifest["target"], "target")
    if set(target) != {"branch", "expected_head", "drift_policy"}:
        raise TrainVerificationError("target keys invalid")
    if target["branch"] != "main" or target["drift_policy"] != "REFRESH_REQUIRED":
        raise TrainVerificationError("target branch/drift policy mismatch")
    target_head = require_sha(target["expected_head"], "target.expected_head")

    policy = require_object(manifest["merge_policy"], "merge_policy")
    expected_policy = {
        "required_method": "merge_commit",
        "forbidden_methods": ["squash", "rebase"],
        "preserve_successor_head": True,
        "retarget_immediate_successor_only": True,
        "rerun_after_retarget": True,
        "final_main_manifest_required": True,
    }
    if policy != expected_policy:
        raise TrainVerificationError("merge policy must preserve stacked ancestry")

    review = require_object(manifest["review_policy"], "review_policy")
    if review != {
        "coderabbit_required": True,
        "codex_required": True,
        "codex_status": "PENDING",
    }:
        raise TrainVerificationError("review policy must keep mandatory Codex pending")

    blockers = set(require_string_list(manifest["release_blockers"], "release_blockers"))
    if blockers != REQUIRED_BLOCKERS:
        raise TrainVerificationError("release blockers mismatch")
    invalidation = set(
        require_string_list(manifest["invalidation_rules"], "invalidation_rules")
    )
    if invalidation != REQUIRED_INVALIDATION_RULES:
        raise TrainVerificationError("invalidation rule coverage mismatch")
    require_text(manifest["next_action"], "next_action")

    boundary = require_object(manifest["claim_boundary"], "claim_boundary")
    if boundary != {
        "pre_merge_candidate_only": True,
        "semantic_conformance_replaced": False,
        "code_review_replaced": False,
        "branch_protection_replaced": False,
        "human_merge_decision_replaced": False,
    }:
        raise TrainVerificationError("claim boundary mismatch")

    stages = manifest["stages"]
    if not isinstance(stages, list) or len(stages) != 3:
        raise TrainVerificationError("stages must contain exactly three entries")

    normalized: list[dict[str, Any]] = []
    seen_prs: set[int] = set()
    seen_heads: set[str] = set()
    merged_prefix = True
    for index, raw in enumerate(stages):
        label = f"stages[{index}]"
        stage = require_object(raw, label)
        required_stage = {
            "order",
            "layer",
            "pull_request",
            "train_state",
            "head_branch",
            "head_sha",
            "expected_base_branch",
            "expected_base_sha",
            "ancestry_parent_sha",
            "predecessor_pull_request",
            "delta_commits",
            "workflow_runs",
            "required_commit_statuses",
            "receipt",
        }
        optional_stage = {"merge_commit_sha"}
        missing = required_stage - set(stage)
        unknown = set(stage) - required_stage - optional_stage
        if missing or unknown:
            raise TrainVerificationError(
                f"{label} keys invalid: missing={sorted(missing)}, unknown={sorted(unknown)}"
            )
        order = require_positive_int(stage["order"], f"{label}.order")
        if order != index + 1:
            raise TrainVerificationError("stage order must be contiguous")
        require_text(stage["layer"], f"{label}.layer")
        pr_number = require_positive_int(stage["pull_request"], f"{label}.pull_request")
        if pr_number in seen_prs:
            raise TrainVerificationError("pull request pins must be unique")
        seen_prs.add(pr_number)
        train_state = require_text(stage["train_state"], f"{label}.train_state")
        if train_state not in ALLOWED_TRAIN_STATES:
            raise TrainVerificationError(f"{label}.train_state is invalid")
        if train_state == "MERGED":
            if not merged_prefix:
                raise TrainVerificationError("merged stages must form a prefix")
            require_sha(stage.get("merge_commit_sha"), f"{label}.merge_commit_sha")
        else:
            merged_prefix = False
            if "merge_commit_sha" in stage:
                raise TrainVerificationError("open stage must not pin a merge commit")

        head_branch = require_text(stage["head_branch"], f"{label}.head_branch")
        head_sha = require_sha(stage["head_sha"], f"{label}.head_sha")
        if head_sha in seen_heads:
            raise TrainVerificationError("stage head SHAs must be unique")
        seen_heads.add(head_sha)
        base_branch = require_text(
            stage["expected_base_branch"], f"{label}.expected_base_branch"
        )
        base_sha = require_sha(stage["expected_base_sha"], f"{label}.expected_base_sha")
        ancestry_parent = require_sha(
            stage["ancestry_parent_sha"], f"{label}.ancestry_parent_sha"
        )
        delta_commits = require_positive_int(
            stage["delta_commits"], f"{label}.delta_commits"
        )

        predecessor = stage["predecessor_pull_request"]
        if index == 0:
            if predecessor is not None:
                raise TrainVerificationError("first stage must not have a predecessor")
            if base_branch != target["branch"] or base_sha != target_head:
                raise TrainVerificationError("first stage must be based on frozen target head")
            if ancestry_parent != target_head:
                raise TrainVerificationError("first ancestry parent must equal target head")
        else:
            previous = normalized[index - 1]
            if predecessor != previous["pull_request"]:
                raise TrainVerificationError("successor predecessor PR mismatch")
            if ancestry_parent != previous["head_sha"]:
                raise TrainVerificationError("successor ancestry must point to predecessor head")
            if previous["train_state"] == "OPEN_STACKED":
                if base_branch != previous["head_branch"] or base_sha != previous["head_sha"]:
                    raise TrainVerificationError(
                        "open stacked successor must target predecessor branch and head"
                    )
            else:
                if base_branch != target["branch"] or base_sha != target_head:
                    raise TrainVerificationError(
                        "successor of merged stage must be retargeted to current target head"
                    )

        runs = stage["workflow_runs"]
        if not isinstance(runs, list) or not runs:
            raise TrainVerificationError(f"{label}.workflow_runs must be non-empty")
        run_ids: set[int] = set()
        run_names: set[str] = set()
        normalized_runs: list[dict[str, Any]] = []
        for run_index, run_raw in enumerate(runs):
            run = require_object(run_raw, f"{label}.workflow_runs[{run_index}]")
            if set(run) != {"name", "run_id"}:
                raise TrainVerificationError("workflow run keys invalid")
            name = require_text(run["name"], f"{label}.workflow_runs[{run_index}].name")
            run_id = require_positive_int(
                run["run_id"], f"{label}.workflow_runs[{run_index}].run_id"
            )
            if name in run_names or run_id in run_ids:
                raise TrainVerificationError("workflow run names and IDs must be unique per stage")
            run_names.add(name)
            run_ids.add(run_id)
            normalized_runs.append({"name": name, "run_id": run_id})

        statuses = require_string_list(
            stage["required_commit_statuses"], f"{label}.required_commit_statuses"
        )
        if statuses != ["CodeRabbit"]:
            raise TrainVerificationError("every stage must require CodeRabbit success")

        receipt = require_object(stage["receipt"], f"{label}.receipt")
        if set(receipt) != {
            "workflow_run_id",
            "artifact_id",
            "artifact_name",
            "artifact_digest",
        }:
            raise TrainVerificationError("receipt keys invalid")
        receipt_run = require_positive_int(
            receipt["workflow_run_id"], f"{label}.receipt.workflow_run_id"
        )
        if receipt_run not in run_ids:
            raise TrainVerificationError("receipt workflow run must be pinned in workflow_runs")
        require_positive_int(receipt["artifact_id"], f"{label}.receipt.artifact_id")
        require_text(receipt["artifact_name"], f"{label}.receipt.artifact_name")
        require_digest(receipt["artifact_digest"], f"{label}.receipt.artifact_digest")

        normalized.append(
            {
                **stage,
                "head_branch": head_branch,
                "head_sha": head_sha,
                "expected_base_branch": base_branch,
                "expected_base_sha": base_sha,
                "ancestry_parent_sha": ancestry_parent,
                "delta_commits": delta_commits,
                "workflow_runs": normalized_runs,
            }
        )

    if [stage["pull_request"] for stage in normalized] != [110, 113, 116]:
        raise TrainVerificationError("canonical train must pin PRs 110, 113, and 116")
    if any(stage["train_state"] != "OPEN_STACKED" for stage in normalized):
        raise TrainVerificationError("PRE_MERGE_FROZEN candidate must have all stages open")
    return normalized


class GitHubClient:
    def __init__(self, token: str, api_url: str) -> None:
        self.token = token
        self.api_url = api_url.rstrip("/")

    def get(self, path: str) -> Any:
        url = f"{self.api_url}/{path.lstrip('/')}"
        request = urllib.request.Request(
            url,
            headers={
                "Accept": "application/vnd.github+json",
                "Authorization": f"Bearer {self.token}",
                "X-GitHub-Api-Version": "2022-11-28",
                "User-Agent": "liminal-release-train-verifier",
            },
        )
        try:
            with urllib.request.urlopen(request, timeout=30) as response:
                return json.loads(response.read().decode("utf-8"))
        except urllib.error.HTTPError as error:
            body = error.read().decode("utf-8", errors="replace")[:1000]
            raise TrainVerificationError(
                f"GitHub API {error.code} for {path}: {body}"
            ) from error
        except (urllib.error.URLError, TimeoutError, json.JSONDecodeError) as error:
            raise TrainVerificationError(f"GitHub API request failed for {path}: {error}") from error


def verify_live(
    manifest: dict[str, Any],
    stages: list[dict[str, Any]],
    client: GitHubClient,
) -> dict[str, Any]:
    repository = manifest["repository"]
    encoded_repo = "/".join(urllib.parse.quote(part, safe="") for part in repository.split("/"))
    target = manifest["target"]
    branch = client.get(
        f"repos/{encoded_repo}/branches/{urllib.parse.quote(target['branch'], safe='')}"
    )
    actual_target_head = branch.get("commit", {}).get("sha")
    if actual_target_head != target["expected_head"]:
        raise TrainVerificationError(
            f"target head drifted: {actual_target_head} != {target['expected_head']}"
        )

    live_stages: list[dict[str, Any]] = []
    for stage in stages:
        pr_number = stage["pull_request"]
        pr = client.get(f"repos/{encoded_repo}/pulls/{pr_number}")
        if stage["train_state"] == "OPEN_STACKED":
            if pr.get("state") != "open" or pr.get("merged") is True:
                raise TrainVerificationError(f"PR #{pr_number} is no longer open and unmerged")
        else:
            if pr.get("state") != "closed" or pr.get("merged") is not True:
                raise TrainVerificationError(f"PR #{pr_number} is not merged as pinned")
            merge_sha = stage["merge_commit_sha"]
            if pr.get("merge_commit_sha") != merge_sha:
                raise TrainVerificationError(f"PR #{pr_number} merge commit changed")
            merge_commit = client.get(f"repos/{encoded_repo}/commits/{merge_sha}")
            parents = [parent.get("sha") for parent in merge_commit.get("parents", [])]
            if len(parents) < 2 or stage["head_sha"] not in parents:
                raise TrainVerificationError(
                    f"PR #{pr_number} was not preserved as a merge-commit parent"
                )

        if pr.get("head", {}).get("ref") != stage["head_branch"]:
            raise TrainVerificationError(f"PR #{pr_number} head branch drifted")
        if pr.get("head", {}).get("sha") != stage["head_sha"]:
            raise TrainVerificationError(f"PR #{pr_number} head SHA drifted")
        if pr.get("base", {}).get("ref") != stage["expected_base_branch"]:
            raise TrainVerificationError(f"PR #{pr_number} base branch drifted")
        if pr.get("base", {}).get("sha") != stage["expected_base_sha"]:
            raise TrainVerificationError(f"PR #{pr_number} base SHA drifted")

        compare = client.get(
            f"repos/{encoded_repo}/compare/{stage['ancestry_parent_sha']}...{stage['head_sha']}"
        )
        if compare.get("status") != "ahead" or compare.get("behind_by") != 0:
            raise TrainVerificationError(f"PR #{pr_number} ancestry is not a strict descendant")
        if compare.get("ahead_by") != stage["delta_commits"]:
            raise TrainVerificationError(f"PR #{pr_number} delta commit count changed")
        if compare.get("merge_base_commit", {}).get("sha") != stage["ancestry_parent_sha"]:
            raise TrainVerificationError(f"PR #{pr_number} merge base changed")

        live_runs: list[dict[str, Any]] = []
        for run_pin in stage["workflow_runs"]:
            run = client.get(f"repos/{encoded_repo}/actions/runs/{run_pin['run_id']}")
            if run.get("name") != run_pin["name"]:
                raise TrainVerificationError(
                    f"workflow run {run_pin['run_id']} name changed"
                )
            if run.get("head_sha") != stage["head_sha"]:
                raise TrainVerificationError(
                    f"workflow run {run_pin['run_id']} is not bound to pinned head"
                )
            if run.get("event") != "pull_request":
                raise TrainVerificationError(
                    f"workflow run {run_pin['run_id']} is not a pull-request run"
                )
            if run.get("status") != "completed" or run.get("conclusion") != "success":
                raise TrainVerificationError(
                    f"workflow run {run_pin['run_id']} is not completed successfully"
                )
            live_runs.append(
                {
                    "name": run_pin["name"],
                    "run_id": run_pin["run_id"],
                    "conclusion": "success",
                }
            )

        receipt_pin = stage["receipt"]
        artifact = client.get(
            f"repos/{encoded_repo}/actions/artifacts/{receipt_pin['artifact_id']}"
        )
        if artifact.get("name") != receipt_pin["artifact_name"]:
            raise TrainVerificationError(f"PR #{pr_number} artifact name changed")
        if artifact.get("digest") != receipt_pin["artifact_digest"]:
            raise TrainVerificationError(f"PR #{pr_number} artifact digest changed")
        if artifact.get("expired") is not False:
            raise TrainVerificationError(f"PR #{pr_number} artifact is expired")
        artifact_run = artifact.get("workflow_run") or {}
        if artifact_run.get("id") != receipt_pin["workflow_run_id"]:
            raise TrainVerificationError(f"PR #{pr_number} artifact run changed")
        if artifact_run.get("head_sha") != stage["head_sha"]:
            raise TrainVerificationError(f"PR #{pr_number} artifact head binding changed")

        combined = client.get(f"repos/{encoded_repo}/commits/{stage['head_sha']}/status")
        statuses = combined.get("statuses") or []
        normalized_statuses: list[dict[str, str]] = []
        for required_context in stage["required_commit_statuses"]:
            matches = [item for item in statuses if item.get("context") == required_context]
            if not matches:
                raise TrainVerificationError(
                    f"PR #{pr_number} missing required status {required_context}"
                )
            latest = max(matches, key=lambda item: item.get("updated_at") or "")
            if latest.get("state") != "success":
                raise TrainVerificationError(
                    f"PR #{pr_number} status {required_context} is not successful"
                )
            normalized_statuses.append(
                {"context": required_context, "state": "success"}
            )

        live_stages.append(
            {
                "pull_request": pr_number,
                "state": pr.get("state"),
                "draft": bool(pr.get("draft")),
                "head_sha": stage["head_sha"],
                "base_branch": stage["expected_base_branch"],
                "base_sha": stage["expected_base_sha"],
                "ancestry_parent_sha": stage["ancestry_parent_sha"],
                "delta_commits": stage["delta_commits"],
                "workflow_runs": live_runs,
                "statuses": normalized_statuses,
                "artifact": {
                    "artifact_id": receipt_pin["artifact_id"],
                    "name": receipt_pin["artifact_name"],
                    "digest": receipt_pin["artifact_digest"],
                },
            }
        )

    live_state = {
        "target": {"branch": target["branch"], "head_sha": actual_target_head},
        "stages": live_stages,
    }
    return {
        "live_state": live_state,
        "live_state_digest": digest_json(live_state),
    }


def build_receipt(
    manifest: dict[str, Any],
    stages: list[dict[str, Any]],
    live_result: dict[str, Any] | None,
) -> dict[str, Any]:
    receipt: dict[str, Any] = {
        "schema": RECEIPT_SCHEMA,
        "profile": PROFILE,
        "verdict": "PASS",
        "mode": "LIVE" if live_result is not None else "OFFLINE",
        "candidate_id": manifest["candidate_id"],
        "candidate_state": manifest["candidate_state"],
        "manifest_digest": digest_json(manifest),
        "target_head": manifest["target"]["expected_head"],
        "stage_count": len(stages),
        "stage_heads": {
            str(stage["pull_request"]): stage["head_sha"] for stage in stages
        },
        "receipt_artifacts": {
            str(stage["pull_request"]): stage["receipt"]["artifact_digest"]
            for stage in stages
        },
        "merge_method": manifest["merge_policy"]["required_method"],
        "release_ready": manifest["release_ready"],
        "release_blockers": manifest["release_blockers"],
        "next_action": manifest["next_action"],
        "claim_boundary": manifest["claim_boundary"],
    }
    if live_result is not None:
        receipt.update(live_result)
    return receipt


def main() -> None:
    root = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--manifest",
        type=Path,
        default=root / "releases/trustworthy-transition-release-candidate-v0.3.json",
    )
    parser.add_argument("--live", action="store_true")
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()

    manifest = load_json(args.manifest)
    stages = verify_manifest(manifest)
    live_result: dict[str, Any] | None = None
    if args.live:
        token = os.environ.get("GITHUB_TOKEN")
        if not token:
            raise TrainVerificationError("GITHUB_TOKEN is required for --live")
        configured_repository = os.environ.get("GITHUB_REPOSITORY")
        if configured_repository and configured_repository != manifest["repository"]:
            raise TrainVerificationError("GITHUB_REPOSITORY does not match manifest")
        client = GitHubClient(
            token=token,
            api_url=os.environ.get("GITHUB_API_URL", "https://api.github.com"),
        )
        live_result = verify_live(manifest, stages, client)

    receipt = build_receipt(manifest, stages, live_result)
    rendered = json.dumps(receipt, ensure_ascii=False, indent=2, sort_keys=True) + "\n"
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(rendered, encoding="utf-8")
    print(rendered, end="")


if __name__ == "__main__":
    try:
        main()
    except TrainVerificationError as error:
        raise SystemExit(f"release train verification failed: {error}") from error
