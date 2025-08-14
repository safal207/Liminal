#!/usr/bin/env python3
from __future__ import annotations

import argparse
import datetime as dt
import json
import sys
from pathlib import Path
from typing import Dict, List

# Ensure src/ is importable when running from repository root
ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))

from liminal.reince import InMemoryREINCE  # type: ignore  # noqa: E402


def parse_meta(meta_items: List[str]) -> Dict[str, str]:
    meta: Dict[str, str] = {}
    for item in meta_items:
        if "=" in item:
            k, v = item.split("=", 1)
            meta[k.strip()] = v.strip()
    return meta


def cmd_record(args: argparse.Namespace) -> None:
    rein = InMemoryREINCE()
    event = rein.record_emotional_event(args.text, parse_meta(args.meta or []))
    print(
        json.dumps(
            {
                "timestamp": event.timestamp.isoformat(),
                "text": event.text,
                "meta": event.meta,
            },
            ensure_ascii=False,
        )
    )


def cmd_map(args: argparse.Namespace) -> None:
    rein = InMemoryREINCE()
    # For demo purposes, optionally seed with provided events
    for t in args.seed or []:
        rein.record_emotional_event(t)
    m = rein.get_resonance_map(top_n=args.top)
    print(json.dumps(m, ensure_ascii=False))


def cmd_list(args: argparse.Namespace) -> None:
    rein = InMemoryREINCE()
    for t in args.seed or []:
        rein.record_emotional_event(t)
    events = rein.list_recent(limit=args.limit)
    out = [
        {
            "timestamp": e.timestamp.isoformat(),
            "text": e.text,
            "meta": e.meta,
        }
        for e in events
    ]
    print(json.dumps(out, ensure_ascii=False, indent=2))


def cmd_recommend(args: argparse.Namespace) -> None:
    rein = InMemoryREINCE()
    print(rein.recommend_intervention(args.text))


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        prog="reince-cli",
        description="REINCE: Reflective Integrative Neural Self‑Evolver (offline demo CLI)",
    )
    sub = p.add_subparsers(dest="cmd", required=True)

    p_record = sub.add_parser("record", help="Record an emotional event")
    p_record.add_argument("text", help="Event text")
    p_record.add_argument("--meta", nargs="*", help="key=value pairs", default=[])
    p_record.set_defaults(func=cmd_record)

    p_map = sub.add_parser("map", help="Show resonance map (keyword counts)")
    p_map.add_argument("--top", type=int, default=10, help="Top N tokens")
    p_map.add_argument("--seed", nargs="*", help="Seed events (optional)")
    p_map.set_defaults(func=cmd_map)

    p_list = sub.add_parser("list", help="List recent events")
    p_list.add_argument("--limit", type=int, default=5)
    p_list.add_argument("--seed", nargs="*", help="Seed events (optional)")
    p_list.set_defaults(func=cmd_list)

    p_rec = sub.add_parser("recommend", help="Recommend a gentle intervention")
    p_rec.add_argument("text", help="Input text")
    p_rec.set_defaults(func=cmd_recommend)

    return p


def main(argv: List[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    args.func(args)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
