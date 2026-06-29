#!/usr/bin/env python3
"""Verify the trustworthy-transition release train and its pinned GitHub state."""

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
PHASE_BY_MERGED_COUNT = {
    0: "PRE_MERGE_FROZEN",
    1: "STAGE_1_MERGED",
    2: "STAGE_2_MERGED",
    3: "STACK_MERGED_PENDING_FINAL",
}
ALLOWED_REVIEW_STATES = {"PENDING", "PASSED"}
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
    """Raised when the manifest or live release-train state is inconsistent."""


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


def expected_blockers(
    coderabbit_status: str, codex_status: str, merged_count: int
) -> set[str]:
    blockers = {"FINAL_MAIN_MANIFEST_NOT_ISSUED"}
    if coderabbit_status != "PASSED":
        blockers.add("CODERABBIT_REVIEW_PENDING")
    if codex_status != "PASSED":
        blockers.add("CODEX_REVIEW_PENDING")
    if merged_count != 3:
        blockers.add("STACK_NOT_MERGED")
    return blockers


def expected_next_action(
    coderabbit_status: str, codex_status: str, merged_count: int
) -> str:
    if coderabbit_status != "PASSED":
        return "COMPLETE_CODERABBIT_REVIEW"
    if codex_status != "PASSED":
        return "COMPLETE_CODEX_REVIEW"
    return {
        0: "MERGE_PR_110_WITH_MERGE_COMMIT",
        1: "RETARGET_PR_113_TO_MAIN_RERUN_AND_REPIN",
        2: "RETARGET_PR_116_TO_MAIN_RERUN_AND_REPIN",
        3: "ISSUE_FINAL_MAIN_MANIFEST",
    }[merged_count]


def verify_stage_runs(stage: dict[str, Any], label: str) -> list[dict[str, Any]]:
    runs = stage.get("workflow_runs")
    if not isinstance(runs, list) or not runs:
        raise TrainVerificationError(f"{label}.workflow_runs must be non-empty")
    seen_ids: set[int] = set()
    seen_names: set[str] = set()
    normalized: list[dict[str, Any]] = []
    for index, raw in enumerate(runs):
        run = require_object(raw, f"{label}.workflow_runs[{index}]")
        if set(run) != {"name", "run_id"}:
            raise TrainVerificationError(f"{label}.workflow_runs[{index}] keys invalid")
        name = require_text(run["name"], f"{label}.workflow_runs[{index}].name")
        run_id = require_positive_int(
            run["run_id"], f"{label}.workflow_runs[{index}].run_id"
        )
        if name in seen_names or run_id in seen_ids:
            raise TrainVerificationError(f"{label} workflow run pins must be unique")
        seen_names.add(name)
        seen_ids.add(run_id)
        normalized.append({"name": name, "run_id": run_id})
    return normalized


def verify_receipt(
    stage: dict[str, Any], runs: list[dict[str, Any]], label: str
) -> dict[str, Any]:
    receipt = require_object(stage.get("receipt"), f"{label}.receipt")
    if set(receipt) != {
        "workflow_run_id",
        "artifact_id",
        "artifact_name",
        "artifact_digest",
    }:
        raise TrainVerificationError(f"{label}.receipt keys invalid")
    run_id = require_positive_int(
        receipt["workflow_run_id"], f"{label}.receipt.workflow_run_id"
    )
    if run_id not in {run["run_id"] for run in runs}:
        raise TrainVerificationError(f"{label} receipt run is not pinned in workflow_runs")
    require_positive_int(receipt["artifact_id"], f"{label}.receipt.artifact_id")
    require_text(receipt["artifact_name"], f"{label}.receipt.artifact_name")
    require_digest(receipt["artifact_digest"], f"{label}.receipt.artifact_digest")
    return receipt


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
    if manifest.get("schema_version") != 1 or manifest.get("profile") != PROFILE:
        raise TrainVerificationError("manifest schema/profile mismatch")
    require_text(manifest["candidate_id"], "candidate_id")
    if require_bool(manifest["release_ready"], "release_ready"):
        raise TrainVerificationError("release candidate must not claim final release readiness")
    if require_text(manifest["repository"], "repository") != "safal207/Liminal":
        raise TrainVerificationError("repository pin mismatch")

    target = require_object(manifest["target"], "target")
    if set(target) != {"branch", "initial_head", "expected_head", "drift_policy"}:
        raise TrainVerificationError("target keys invalid")
    if target["branch"] != "main" or target["drift_policy"] != "REFRESH_REQUIRED":
        raise TrainVerificationError("target branch/drift policy mismatch")
    initial_head = require_sha(target["initial_head"], "target.initial_head")
    expected_target_head = require_sha(target["expected_head"], "target.expected_head")

    policy = require_object(manifest["merge_policy"], "merge_policy")
    if policy != {
        "required_method": "merge_commit",
        "forbidden_methods": ["squash", "rebase"],
        "preserve_successor_head": True,
        "retarget_immediate_successor_only": True,
        "rerun_after_retarget": True,
        "final_main_manifest_required": True,
    }:
        raise TrainVerificationError("merge policy must preserve stack ancestry")

    review = require_object(manifest["review_policy"], "review_policy")
    if set(review) != {
        "coderabbit_required",
        "coderabbit_status",
        "codex_required",
        "codex_status",
    }:
        raise TrainVerificationError("review policy keys invalid")
    if review["coderabbit_required"] is not True or review["codex_required"] is not True:
        raise TrainVerificationError("CodeRabbit and Codex must remain mandatory")
    coderabbit_status = require_text(
        review["coderabbit_status"], "review_policy.coderabbit_status"
    )
    codex_status = require_text(review["codex_status"], "review_policy.codex_status")
    if coderabbit_status not in ALLOWED_REVIEW_STATES:
        raise TrainVerificationError("review_policy.coderabbit_status is invalid")
    if codex_status not in ALLOWED_REVIEW_STATES:
        raise TrainVerificationError("review_policy.codex_status is invalid")

    boundary = require_object(manifest["claim_boundary"], "claim_boundary")
    if boundary != {
        "release_candidate_only": True,
        "final_main_manifest": False,
        "semantic_conformance_replaced": False,
        "code_review_replaced": False,
        "branch_protection_replaced": False,
        "human_merge_decision_replaced": False,
    }:
        raise TrainVerificationError("claim boundary mismatch")

    invalidation = set(
        require_string_list(manifest["invalidation_rules"], "invalidation_rules")
    )
    if invalidation != REQUIRED_INVALIDATION_RULES:
        raise TrainVerificationError("invalidation rule coverage mismatch")
    next_action = require_text(manifest["next_action"], "next_action")

    raw_stages = manifest["stages"]
    if not isinstance(raw_stages, list) or len(raw_stages) != 3:
        raise TrainVerificationError("stages must contain exactly three entries")

    stages: list[dict[str, Any]] = []
    seen_prs: set[int] = set()
    seen_heads: set[str] = set()
    open_seen = False
    for index, raw in enumerate(raw_stages):
        label = f"stages[{index}]"
        stage = require_object(raw, label)
        required = {
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
        optional = {"merge_commit_sha"}
        if required - set(stage) or set(stage) - required - optional:
            raise TrainVerificationError(f"{label} keys invalid")
        order = require_positive_int(stage["order"], f"{label}.order")
        if order != index + 1:
            raise TrainVerificationError("stage order must be contiguous")
        require_text(stage["layer"], f"{label}.layer")
        pr_number = require_positive_int(stage["pull_request"], f"{label}.pull_request")
        if pr_number in seen_prs:
            raise TrainVerificationError("stage PR numbers must be unique")
        seen_prs.add(pr_number)

        state = require_text(stage["train_state"], f"{label}.train_state")
        if state not in {"OPEN_STACKED", "MERGED"}:
            raise TrainVerificationError(f"{label}.train_state is invalid")
        if state == "OPEN_STACKED":
            open_seen = True
            if "merge_commit_sha" in stage:
                raise TrainVerificationError("open stage must not pin merge_commit_sha")
        else:
            if open_seen:
                raise TrainVerificationError("merged stages must form a prefix")
            require_sha(stage.get("merge_commit_sha"), f"{label}.merge_commit_sha")

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

        if index == 0:
            if stage["predecessor_pull_request"] is not None:
                raise TrainVerificationError("first stage must not have a predecessor")
            if base_branch != "main":
                raise TrainVerificationError("first stage base branch must be main")
            if base_sha != initial_head:
                raise TrainVerificationError("first stage base SHA must equal initial main")
            if ancestry_parent != initial_head:
                raise TrainVerificationError("first stage ancestry parent must be initial main")
        else:
            previous = stages[index - 1]
            if stage["predecessor_pull_request"] != previous["pull_request"]:
                raise TrainVerificationError("successor predecessor PR mismatch")
            if ancestry_parent != previous["head_sha"]:
                raise TrainVerificationError("successor ancestry parent mismatch")
            if state == "OPEN_STACKED":
                if previous["train_state"] == "OPEN_STACKED":
                    if base_branch != previous["head_branch"] or base_sha != previous["head_sha"]:
                        raise TrainVerificationError(
                            "open successor must target its open predecessor branch and head"
                        )
                else:
                    if base_branch != "main" or base_sha != expected_target_head:
                        raise TrainVerificationError(
                            "successor of merged stage must target current main head"
                        )
            else:
                if base_branch != "main":
                    raise TrainVerificationError("merged successor must have been retargeted to main")
                if base_sha != previous["merge_commit_sha"]:
                    raise TrainVerificationError(
                        "merged successor base SHA must equal predecessor merge commit"
                    )

        runs = verify_stage_runs(stage, label)
        statuses = require_string_list(
            stage["required_commit_statuses"], f"{label}.required_commit_statuses"
        )
        if statuses != ["CodeRabbit"]:
            raise TrainVerificationError("each stage must require CodeRabbit success")
        receipt = verify_receipt(stage, runs, label)

        stages.append(
            {
                **stage,
                "head_branch": head_branch,
                "head_sha": head_sha,
                "expected_base_branch": base_branch,
                "expected_base_sha": base_sha,
                "ancestry_parent_sha": ancestry_parent,
                "delta_commits": delta_commits,
                "workflow_runs": runs,
                "receipt": receipt,
            }
        )

    if [stage["pull_request"] for stage in stages] != [110, 113, 116]:
        raise TrainVerificationError("canonical train must pin PRs 110, 113, and 116")

    merged_count = sum(stage["train_state"] == "MERGED" for stage in stages)
    expected_phase = PHASE_BY_MERGED_COUNT[merged_count]
    if manifest["candidate_state"] != expected_phase:
        raise TrainVerificationError(
            f"candidate_state must be {expected_phase} for {merged_count} merged stages"
        )
    if merged_count == 0:
        if expected_target_head != initial_head:
            raise TrainVerificationError("pre-merge target head must equal initial head")
    else:
        last_merged = stages[merged_count - 1]
        if expected_target_head != last_merged["merge_commit_sha"]:
            raise TrainVerificationError("target head must equal latest merged-stage commit")

    blockers = set(require_string_list(manifest["release_blockers"], "release_blockers"))
    if blockers != expected_blockers(coderabbit_status, codex_status, merged_count):
        raise TrainVerificationError("release blockers do not match train/review state")
    expected_action = expected_next_action(coderabbit_status, codex_status, merged_count)
    if next_action != expected_action:
        raise TrainVerificationError(
            f"next_action must be {expected_action} for the verified review/train state"
        )
    return stages


class GitHubClient:
    def __init__(self, token: str, api_url: str) -> None:
        parsed = urllib.parse.urlparse(api_url)
        allowed_hosts = {
            host.strip().lower()
            for host in os.environ.get(
                "TRUSTWORTHY_TRAIN_ALLOWED_GITHUB_API_HOSTS", "api.github.com"
            ).split(",")
            if host.strip()
        }
        host = (parsed.hostname or "").lower()
        if (
            parsed.scheme != "https"
            or host not in allowed_hosts
            or parsed.username is not None
            or parsed.password is not None
            or parsed.query
            or parsed.fragment
        ):
            raise TrainVerificationError(
                "GITHUB_API_URL must be HTTPS and use an explicitly allowed GitHub API host"
            )
        self.token = token
        self.api_url = api_url.rstrip("/")

    def get(self, path: str) -> Any:
        request = urllib.request.Request(
            f"{self.api_url}/{path.lstrip('/')}",
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


def latest_status(statuses: list[dict[str, Any]], context: str) -> dict[str, Any] | None:
    matching = [status for status in statuses if status.get("context") == context]
    if not matching:
        return None
    return max(matching, key=lambda status: status.get("updated_at") or "")


def verify_live(
    manifest: dict[str, Any], stages: list[dict[str, Any]], client: GitHubClient
) -> dict[str, Any]:
    repository = manifest["repository"]
    encoded_repo = "/".join(urllib.parse.quote(part, safe="") for part in repository.split("/"))
    target = manifest["target"]
    branch = client.get(
        f"repos/{encoded_repo}/branches/{urllib.parse.quote(target['branch'], safe='')}"
    )
    target_head = branch.get("commit", {}).get("sha")
    if target_head != target["expected_head"]:
        raise TrainVerificationError(
            f"target head drifted: {target_head} != {target['expected_head']}"
        )

    live_stages: list[dict[str, Any]] = []
    for stage in stages:
        pr_number = stage["pull_request"]
        pr = client.get(f"repos/{encoded_repo}/pulls/{pr_number}")
        if pr.get("head", {}).get("ref") != stage["head_branch"]:
            raise TrainVerificationError(f"PR #{pr_number} head branch drifted")
        if pr.get("head", {}).get("sha") != stage["head_sha"]:
            raise TrainVerificationError(f"PR #{pr_number} head SHA drifted")
        if pr.get("base", {}).get("ref") != stage["expected_base_branch"]:
            raise TrainVerificationError(f"PR #{pr_number} base branch drifted")

        if stage["train_state"] == "OPEN_STACKED":
            if pr.get("state") != "open" or pr.get("merged") is True:
                raise TrainVerificationError(f"PR #{pr_number} is not open and unmerged")
            if pr.get("base", {}).get("sha") != stage["expected_base_sha"]:
                raise TrainVerificationError(f"PR #{pr_number} base SHA drifted")
        else:
            if pr.get("state") != "closed" or pr.get("merged") is not True:
                raise TrainVerificationError(f"PR #{pr_number} is not merged")
            merge_sha = stage["merge_commit_sha"]
            if pr.get("merge_commit_sha") != merge_sha:
                raise TrainVerificationError(f"PR #{pr_number} merge commit drifted")
            merge_commit = client.get(f"repos/{encoded_repo}/commits/{merge_sha}")
            parents = [parent.get("sha") for parent in merge_commit.get("parents", [])]
            if len(parents) != 2 or set(parents) != {
                stage["expected_base_sha"],
                stage["head_sha"],
            }:
                raise TrainVerificationError(
                    f"PR #{pr_number} was not merged with the pinned two-parent merge commit"
                )

        compare = client.get(
            f"repos/{encoded_repo}/compare/{stage['ancestry_parent_sha']}...{stage['head_sha']}"
        )
        if compare.get("status") != "ahead" or compare.get("behind_by") != 0:
            raise TrainVerificationError(f"PR #{pr_number} ancestry is not a strict descendant")
        if compare.get("ahead_by") != stage["delta_commits"]:
            raise TrainVerificationError(f"PR #{pr_number} delta commit count changed")
        if compare.get("merge_base_commit", {}).get("sha") != stage["ancestry_parent_sha"]:
            raise TrainVerificationError(f"PR #{pr_number} merge base changed")

        normalized_runs: list[dict[str, Any]] = []
        for run_pin in stage["workflow_runs"]:
            run = client.get(f"repos/{encoded_repo}/actions/runs/{run_pin['run_id']}")
            if run.get("name") != run_pin["name"]:
                raise TrainVerificationError(f"workflow run {run_pin['run_id']} name changed")
            if run.get("head_sha") != stage["head_sha"]:
                raise TrainVerificationError(f"workflow run {run_pin['run_id']} head changed")
            if run.get("event") != "pull_request":
                raise TrainVerificationError(f"workflow run {run_pin['run_id']} event changed")
            if run.get("status") != "completed" or run.get("conclusion") != "success":
                raise TrainVerificationError(f"workflow run {run_pin['run_id']} is not green")
            normalized_runs.append(
                {"name": run_pin["name"], "run_id": run_pin["run_id"], "conclusion": "success"}
            )

        receipt = stage["receipt"]
        artifact = client.get(f"repos/{encoded_repo}/actions/artifacts/{receipt['artifact_id']}")
        if artifact.get("name") != receipt["artifact_name"]:
            raise TrainVerificationError(f"PR #{pr_number} artifact name changed")
        if artifact.get("digest") != receipt["artifact_digest"]:
            raise TrainVerificationError(f"PR #{pr_number} artifact digest changed")
        if artifact.get("expired") is not False:
            raise TrainVerificationError(f"PR #{pr_number} artifact expired")
        artifact_run = artifact.get("workflow_run") or {}
        if artifact_run.get("id") != receipt["workflow_run_id"]:
            raise TrainVerificationError(f"PR #{pr_number} artifact run changed")
        if artifact_run.get("head_sha") != stage["head_sha"]:
            raise TrainVerificationError(f"PR #{pr_number} artifact head binding changed")

        combined = client.get(f"repos/{encoded_repo}/commits/{stage['head_sha']}/status")
        normalized_statuses: list[dict[str, str]] = []
        for context in stage["required_commit_statuses"]:
            status = latest_status(combined.get("statuses") or [], context)
            if status is None or status.get("state") != "success":
                raise TrainVerificationError(
                    f"PR #{pr_number} required status {context} is not successful"
                )
            normalized_statuses.append({"context": context, "state": "success"})

        live_stages.append(
            {
                "pull_request": pr_number,
                "train_state": stage["train_state"],
                "state": pr.get("state"),
                "draft": bool(pr.get("draft")),
                "head_sha": stage["head_sha"],
                "base_branch": stage["expected_base_branch"],
                "base_sha": stage["expected_base_sha"],
                "ancestry_parent_sha": stage["ancestry_parent_sha"],
                "delta_commits": stage["delta_commits"],
                "workflow_runs": normalized_runs,
                "statuses": normalized_statuses,
                "artifact": {
                    "artifact_id": receipt["artifact_id"],
                    "name": receipt["artifact_name"],
                    "digest": receipt["artifact_digest"],
                },
            }
        )

    live_state = {
        "target": {"branch": target["branch"], "head_sha": target_head},
        "stages": live_stages,
    }
    return {"live_state": live_state, "live_state_digest": digest_json(live_state)}


def build_receipt(
    manifest: dict[str, Any], stages: list[dict[str, Any]], live: dict[str, Any] | None
) -> dict[str, Any]:
    receipt: dict[str, Any] = {
        "schema": RECEIPT_SCHEMA,
        "profile": PROFILE,
        "verdict": "PASS",
        "mode": "LIVE" if live else "OFFLINE",
        "candidate_id": manifest["candidate_id"],
        "candidate_state": manifest["candidate_state"],
        "manifest_digest": digest_json(manifest),
        "initial_target_head": manifest["target"]["initial_head"],
        "current_target_head": manifest["target"]["expected_head"],
        "merged_stage_count": sum(stage["train_state"] == "MERGED" for stage in stages),
        "stage_heads": {str(stage["pull_request"]): stage["head_sha"] for stage in stages},
        "receipt_artifacts": {
            str(stage["pull_request"]): stage["receipt"]["artifact_digest"]
            for stage in stages
        },
        "merge_method": manifest["merge_policy"]["required_method"],
        "review_policy": manifest["review_policy"],
        "release_ready": manifest["release_ready"],
        "release_blockers": manifest["release_blockers"],
        "next_action": manifest["next_action"],
        "claim_boundary": manifest["claim_boundary"],
    }
    if live:
        receipt.update(live)
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
        live_result = verify_live(
            manifest,
            stages,
            GitHubClient(token, os.environ.get("GITHUB_API_URL", "https://api.github.com")),
        )

    rendered = json.dumps(
        build_receipt(manifest, stages, live_result),
        ensure_ascii=False,
        indent=2,
        sort_keys=True,
    ) + "\n"
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(rendered, encoding="utf-8")
    print(rendered, end="")


if __name__ == "__main__":
    try:
        main()
    except TrainVerificationError as error:
        raise SystemExit(f"release train verification failed: {error}") from error
