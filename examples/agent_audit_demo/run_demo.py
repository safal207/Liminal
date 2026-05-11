#!/usr/bin/env python3
"""Run the Liminal agent audit demo.

This script reads the static demo artifacts in this directory and prints a
human-readable audit verdict.

Usage:
    python examples/agent_audit_demo/run_demo.py
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

ROOT = Path(__file__).resolve().parent
TRACE_PATH = ROOT / "trace.jsonl"
CML_PATH = ROOT / "cml_records.json"
CAPU_PATH = ROOT / "capu_decision.json"
REPORT_PATH = ROOT / "audit_report.md"


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
        (item for item in trace if item.get("transition_id") == transition_id), None
    )


def find_cml_record(
    cml_records: dict[str, Any], transition_id: str
) -> dict[str, Any] | None:
    records = cml_records.get("records", [])
    return next(
        (item for item in records if item.get("transition_id") == transition_id), None
    )


def render_section(title: str) -> None:
    print("\n" + title)
    print("=" * len(title))


def main() -> int:
    trace = load_jsonl(TRACE_PATH)
    cml_records = load_json(CML_PATH)
    capu_decision = load_json(CAPU_PATH)

    target_transition_id = str(capu_decision.get("target_transition_id", ""))
    target_transition = find_transition(trace, target_transition_id)
    target_cml_record = find_cml_record(cml_records, target_transition_id)

    render_section("Liminal Agent Audit Demo")
    print("Scenario: support agent attempts an external refund action.")
    print(
        "Goal: detect whether the action is traceable, causally valid, and safe to execute."
    )

    render_section("Trace Summary")
    print(f"Transitions loaded: {len(trace)}")
    for item in trace:
        transition_id = item.get("transition_id", "<unknown>")
        action = item.get("action", "<unknown>")
        anchors = item.get("anchors", "n/a")
        print(f"- {transition_id}: {action} | anchors={anchors}")

    render_section("Target Action")
    if not target_transition:
        print(
            f"ERROR: target transition {target_transition_id!r} was not found in trace."
        )
        return 1

    print(json.dumps(target_transition, indent=2, ensure_ascii=False))

    render_section("LTP-style Trace Finding")
    anchors = target_transition.get("anchors", [])
    if not anchors:
        print("Decision: BLOCK")
        print("Reason: NO_ANCHORS_DECLARED")
        print(
            "Message: External refund execution has no declared anchor to approval or committed permission."
        )
    else:
        print("Decision: PROCEED")
        print("Reason: anchors declared")

    render_section("CML Finding")
    if not target_cml_record:
        print(f"ERROR: no CML record found for transition {target_transition_id!r}.")
        return 1

    print(f"Causal status: {target_cml_record.get('causal_status')}")
    violations = target_cml_record.get("violations", [])
    if violations:
        print("Violations:")
        for violation in violations:
            print(f"- {violation}")
    print(f"Reason: {target_cml_record.get('reason')}")

    render_section("CaPU Decision")
    print(f"Decision: {capu_decision.get('decision')}")
    print(f"Reason code: {capu_decision.get('reason_code')}")
    print(f"Message: {capu_decision.get('message')}")

    required_preconditions = capu_decision.get("required_preconditions", [])
    if required_preconditions:
        print("Required preconditions:")
        for item in required_preconditions:
            print(f"- {item}")

    render_section("Final Verdict")
    print(
        "HOLD: the refund action must not execute until approval and permission records are committed."
    )
    print(f"Human-readable report: {REPORT_PATH.relative_to(ROOT.parent.parent)}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
