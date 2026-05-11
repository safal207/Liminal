#!/usr/bin/env python3
"""Run the Liminal agent audit demo.

Usage:
    python examples/agent_audit_demo/run_demo.py
    python examples/agent_audit_demo/run_demo.py --mode static
    python examples/agent_audit_demo/run_demo.py --mode integrated

Integrated mode can optionally call external tools through environment command
templates:

    LTP_INSPECT_CMD="ltp inspect trace --input {trace}"
    CML_VALIDATE_CMD="cml validate --input {cml}"
    CAPU_EVALUATE_CMD="capu evaluate --input {capu}"

If those commands are not configured or unavailable, integrated mode falls back
to the deterministic local artifact checks.
"""

from __future__ import annotations

import argparse
import json
import os
import shlex
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import Any

ROOT = Path(__file__).resolve().parent
TRACE_PATH = ROOT / "trace.jsonl"
CML_PATH = ROOT / "cml_records.json"
CAPU_PATH = ROOT / "capu_decision.json"
REPORT_PATH = ROOT / "audit_report.md"
COMMAND_TIMEOUT_SECONDS = 20


@dataclass(frozen=True)
class DemoArtifacts:
    trace: list[dict[str, Any]]
    cml_records: dict[str, Any]
    capu_decision: dict[str, Any]
    target_transition_id: str
    target_transition: dict[str, Any] | None
    target_cml_record: dict[str, Any] | None


@dataclass(frozen=True)
class ToolResult:
    name: str
    status: str
    command: str | None
    output: str


def load_jsonl(path: Path) -> list[dict[str, Any]]:
    records: list[dict[str, Any]] = []
    with path.open("r", encoding="utf-8") as file:
        for line_number, line in enumerate(file, start=1):
            stripped = line.strip()
            if not stripped:
                continue
            try:
                records.append(json.loads(stripped))
            except json.JSONDecodeError as exc:
                raise ValueError(
                    f"Invalid JSONL at {path}:{line_number}: {exc}"
                ) from exc
    return records


def load_json(path: Path) -> dict[str, Any]:
    with path.open("r", encoding="utf-8") as file:
        return json.load(file)


def find_transition(
    trace: list[dict[str, Any]], transition_id: str
) -> dict[str, Any] | None:
    return next(
        (item for item in trace if item.get("transition_id") == transition_id),
        None,
    )


def find_cml_record(
    cml_records: dict[str, Any], transition_id: str
) -> dict[str, Any] | None:
    records = cml_records.get("records", [])
    return next(
        (item for item in records if item.get("transition_id") == transition_id),
        None,
    )


def load_artifacts() -> DemoArtifacts:
    trace = load_jsonl(TRACE_PATH)
    cml_records = load_json(CML_PATH)
    capu_decision = load_json(CAPU_PATH)
    target_transition_id = str(capu_decision.get("target_transition_id", ""))

    return DemoArtifacts(
        trace=trace,
        cml_records=cml_records,
        capu_decision=capu_decision,
        target_transition_id=target_transition_id,
        target_transition=find_transition(trace, target_transition_id),
        target_cml_record=find_cml_record(cml_records, target_transition_id),
    )


def render_section(title: str) -> None:
    print("\n" + title)
    print("=" * len(title))


def compact_output(output: str, max_lines: int = 12) -> str:
    lines = output.strip().splitlines()
    if not lines:
        return "<no output>"
    if len(lines) <= max_lines:
        return "\n".join(lines)
    visible = "\n".join(lines[:max_lines])
    hidden = len(lines) - max_lines
    return f"{visible}\n... truncated {hidden} line(s) ..."


def build_command(template: str) -> list[str]:
    formatted = template.format(
        trace=str(TRACE_PATH),
        cml=str(CML_PATH),
        capu=str(CAPU_PATH),
        report=str(REPORT_PATH),
    )
    return shlex.split(formatted)


def run_optional_tool(name: str, env_var: str) -> ToolResult:
    template = os.environ.get(env_var)
    if not template:
        return ToolResult(
            name=name,
            status="SKIPPED",
            command=None,
            output=f"{env_var} is not configured.",
        )

    try:
        command = build_command(template)
        completed = subprocess.run(
            command,
            check=False,
            capture_output=True,
            text=True,
            timeout=COMMAND_TIMEOUT_SECONDS,
        )
    except FileNotFoundError as exc:
        return ToolResult(
            name=name,
            status="UNAVAILABLE",
            command=template,
            output=str(exc),
        )
    except subprocess.TimeoutExpired:
        return ToolResult(
            name=name,
            status="TIMEOUT",
            command=template,
            output=f"Command exceeded {COMMAND_TIMEOUT_SECONDS}s timeout.",
        )

    combined_output = "\n".join(
        part for part in [completed.stdout, completed.stderr] if part.strip()
    )
    status = "OK" if completed.returncode == 0 else "FAILED"
    return ToolResult(
        name=name,
        status=status,
        command=template,
        output=compact_output(combined_output),
    )


def render_intro(mode: str) -> None:
    render_section("Liminal Agent Audit Demo")
    print(f"Mode: {mode}")
    print("Scenario: support agent attempts an external refund action.")
    print(
        "Goal: detect whether the action is traceable, causally valid, "
        "and safe to execute."
    )


def render_trace_summary(artifacts: DemoArtifacts) -> None:
    render_section("Trace Summary")
    print(f"Transitions loaded: {len(artifacts.trace)}")
    for item in artifacts.trace:
        transition_id = item.get("transition_id", "<unknown>")
        action = item.get("action", "<unknown>")
        anchors = item.get("anchors", "n/a")
        print(f"- {transition_id}: {action} | anchors={anchors}")


def render_target_action(artifacts: DemoArtifacts) -> int:
    render_section("Target Action")
    if not artifacts.target_transition:
        print(
            f"ERROR: target transition {artifacts.target_transition_id!r} "
            "was not found in trace."
        )
        return 1

    print(json.dumps(artifacts.target_transition, indent=2, ensure_ascii=False))
    return 0


def render_ltp_finding(artifacts: DemoArtifacts) -> None:
    render_section("LTP-style Trace Finding")
    target_transition = artifacts.target_transition or {}
    anchors = target_transition.get("anchors", [])
    if not anchors:
        print("Decision: BLOCK")
        print("Reason: NO_ANCHORS_DECLARED")
        print(
            "Message: External refund execution has no declared anchor "
            "to approval or committed permission."
        )
    else:
        print("Decision: PROCEED")
        print("Reason: anchors declared")


def render_cml_finding(artifacts: DemoArtifacts) -> int:
    render_section("CML Finding")
    if not artifacts.target_cml_record:
        print(
            f"ERROR: no CML record found for transition "
            f"{artifacts.target_transition_id!r}."
        )
        return 1

    print(f"Causal status: {artifacts.target_cml_record.get('causal_status')}")
    violations = artifacts.target_cml_record.get("violations", [])
    if violations:
        print("Violations:")
        for violation in violations:
            print(f"- {violation}")
    print(f"Reason: {artifacts.target_cml_record.get('reason')}")
    return 0


def render_capu_decision(artifacts: DemoArtifacts) -> None:
    render_section("CaPU Decision")
    capu_decision = artifacts.capu_decision
    print(f"Decision: {capu_decision.get('decision')}")
    print(f"Reason code: {capu_decision.get('reason_code')}")
    print(f"Message: {capu_decision.get('message')}")

    required_preconditions = capu_decision.get("required_preconditions", [])
    if required_preconditions:
        print("Required preconditions:")
        for item in required_preconditions:
            print(f"- {item}")


def render_integrated_tool_checks() -> None:
    render_section("Integrated Tool Checks")
    print("External tools are optional and configured through env vars.")

    results = [
        run_optional_tool("LTP", "LTP_INSPECT_CMD"),
        run_optional_tool("CML", "CML_VALIDATE_CMD"),
        run_optional_tool("CaPU", "CAPU_EVALUATE_CMD"),
    ]

    for result in results:
        print(f"\n[{result.name}] {result.status}")
        if result.command:
            print(f"Command template: {result.command}")
        print(result.output)

    if all(result.status == "SKIPPED" for result in results):
        print("\nNo external tools configured. Using local artifact checks.")


def render_final_verdict() -> None:
    render_section("Final Verdict")
    print(
        "HOLD: the refund action must not execute until approval and "
        "permission records are committed."
    )
    print(f"Human-readable report: {REPORT_PATH.relative_to(ROOT.parent.parent)}")


def render_artifact_checks(mode: str) -> int:
    artifacts = load_artifacts()

    render_intro(mode)
    render_trace_summary(artifacts)

    if render_target_action(artifacts) != 0:
        return 1

    render_ltp_finding(artifacts)

    if render_cml_finding(artifacts) != 0:
        return 1

    render_capu_decision(artifacts)
    render_final_verdict()

    return 0


def run_static() -> int:
    return render_artifact_checks("static")


def run_integrated() -> int:
    render_integrated_tool_checks()
    return render_artifact_checks("integrated")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Run the Liminal agent audit demo."
    )
    parser.add_argument(
        "--mode",
        choices=("static", "integrated"),
        default="static",
        help="Run deterministic static checks or optional external tool checks.",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    if args.mode == "integrated":
        return run_integrated()
    return run_static()


if __name__ == "__main__":
    raise SystemExit(main())
