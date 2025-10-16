#!/usr/bin/env python3
"""CLI wrapper for the Top At-Risk Prefect workflow."""
from __future__ import annotations

import argparse
import sys
from pathlib import Path
from typing import Any, List

ROOT = Path(__file__).resolve().parents[1]
SRC = ROOT / "src"
if str(SRC) not in sys.path:
    sys.path.insert(0, str(SRC))

from liminal.workflows.prefect_flows import top_at_risk_flow


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Show top at-risk edges using the declarative Prefect workflow."
    )
    parser.add_argument(
        "--limit",
        "-l",
        type=int,
        default=5,
        help="Number of pairs to show (default: 5).",
    )
    parser.add_argument(
        "--threshold",
        "-t",
        type=float,
        default=None,
        help="Override LIMINAL_HEALTH_THRESHOLD for this run.",
    )
    parser.add_argument(
        "--json",
        action="store_true",
        help="Emit raw JSON instead of a table.",
    )
    return parser.parse_args()


def render_table(rows: List[dict[str, Any]]) -> None:
    print("Top at-risk edges")
    print("-" * 72)
    print(f"{'Source':<12} {'Target':<12} {'Score':<8} Advice")
    print("-" * 72)
    for row in rows:
        advice = ", ".join(row.get("advice", []))
        print(f"{row['sourceId']:<12} {row['targetId']:<12} {row['score']:<8.3f} {advice}")


def main() -> None:
    args = parse_args()
    rows = top_at_risk_flow(limit=args.limit, threshold=args.threshold)

    if not rows:
        print("No pairs found. Add nodes via GraphQL or code to see at-risk edges.")
        return

    if args.json:
        import json

        print(json.dumps(rows, ensure_ascii=False, indent=2))
    else:
        render_table(rows)


if __name__ == "__main__":
    main()
