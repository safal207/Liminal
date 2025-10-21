#!/usr/bin/env python3
"""CLI wrapper for the Consciousness Cell Prefect workflow."""
from __future__ import annotations

import argparse
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SRC = ROOT / "src"
if str(SRC) not in sys.path:
    sys.path.insert(0, str(SRC))

from liminal.workflows.prefect_flows import consciousness_cell_flow


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Run the Consciousness Cell workflow to generate insights."
    )
    parser.add_argument(
        "project_root",
        nargs="?",
        default=str(ROOT),
        help="Root of the project to analyse (defaults to repository root).",
    )
    parser.add_argument(
        "--no-persist",
        action="store_true",
        help="Do not write the generated report to disk.",
    )
    parser.add_argument(
        "--output-path",
        help="Explicit path for the generated report (defaults to scripts/consciousness_insights_agent.md).",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    result = consciousness_cell_flow(
        project_root=args.project_root,
        persist_report=not args.no_persist,
        output_path=args.output_path,
    )

    report_path = result["report_path"]
    report_text = result["report"]
    totals = {k: len(v) for k, v in result["insights"].items()}

    print(report_text)
    print("\n📄 Report saved to:", report_path)
    print("📊 Insight totals:", totals)


if __name__ == "__main__":
    main()
