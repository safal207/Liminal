from __future__ import annotations

import argparse
import json
import os
import pathlib
import sys
from typing import Any

# Ensure `src` is on sys.path for local execution
_ROOT = pathlib.Path(__file__).resolve().parents[1]
_SRC = _ROOT / "src"
if str(_SRC) not in sys.path:
    sys.path.insert(0, str(_SRC))

# Try to import schema for in-process GraphQL execution; fall back to HTTP if unavailable
_schema = None
try:  # pragma: no cover - import path may vary in local dev
    from liminal.graphql_schema import schema as _schema  # type: ignore
except Exception:
    _schema = None


def gql(query: str, variables: dict[str, Any] | None = None) -> dict[str, Any]:
    if _schema is not None:
        res = _schema.execute_sync(query, variable_values=variables)
        if res.errors:
            raise SystemExit(f"GraphQL errors: {res.errors}")
        assert res.data is not None
        return res.data  # type: ignore[return-value]
    # HTTP fallback to running server
    import urllib.error
    import urllib.request

    url = os.getenv("LIMINAL_GRAPHQL_URL", "http://127.0.0.1:8000/graphql")
    payload = json.dumps({"query": query, "variables": variables or {}}).encode("utf-8")
    req = urllib.request.Request(
        url, data=payload, headers={"Content-Type": "application/json"}, method="POST"
    )
    try:
        with urllib.request.urlopen(req, timeout=10) as resp:
            body = resp.read().decode("utf-8")
    except urllib.error.URLError:  # pragma: no cover
        # Fall back to REST when GraphQL unreachable
        return {"__rest_fallback__": True}
    try:
        js = json.loads(body)
    except Exception as e:  # pragma: no cover
        raise SystemExit(f"Invalid JSON from GraphQL endpoint: {e}\nRaw: {body[:300]}") from e
    if js.get("errors"):
        # Indicate failure so caller can fallback to REST
        return {"__rest_fallback__": True}
    return js.get("data", {})


def main() -> None:
    parser = argparse.ArgumentParser(description="Show top at-risk edges (weak relationships)")
    parser.add_argument(
        "--limit",
        "-l",
        type=int,
        default=5,
        help="Number of pairs to show (default: 5)",
    )
    parser.add_argument(
        "--threshold",
        "-t",
        type=float,
        default=None,
        help="Override LIMINAL_HEALTH_THRESHOLD for this run",
    )
    args = parser.parse_args()

    if args.threshold is not None:
        os.environ["LIMINAL_HEALTH_THRESHOLD"] = str(args.threshold)

    data = gql(
        "query($n:Int!){ topAtRiskEdges(limit:$n){ sourceId targetId score advice } }",
        {"n": int(args.limit)},
    )
    rows: list[dict[str, Any]]
    if data.get("__rest_fallback__"):
        # REST fallback
        import urllib.error
        import urllib.request

        rest_base = os.getenv("LIMINAL_REST_URL", "http://127.0.0.1:8000")
        rest_url = f"{rest_base}/api/top-at-risk?limit={int(args.limit)}"
        try:
            with urllib.request.urlopen(rest_url, timeout=10) as resp:
                body = resp.read().decode("utf-8")
                js = json.loads(body)
        except Exception as e:
            raise SystemExit(f"REST fallback failed at {rest_url}: {e}") from e
        rows = js.get("topAtRiskEdges", [])
    else:
        rows = data["topAtRiskEdges"]

    if not rows:
        print("No pairs found. Add nodes via GraphQL or code to see at-risk edges.")
        return

    # Pretty table
    print("Top at-risk edges")
    print("-" * 72)
    print(f"{'Source':<12} {'Target':<12} {'Score':<8} Advice")
    print("-" * 72)
    for r in rows:
        advice = ", ".join(r.get("advice", []))
        print(f"{r['sourceId']:<12} {r['targetId']:<12} {r['score']:<8.3f} {advice}")


if __name__ == "__main__":
    main()
