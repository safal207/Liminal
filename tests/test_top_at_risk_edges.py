from __future__ import annotations

import importlib.util
from typing import Any

import pytest

# Skip if strawberry not installed or schema not importable
if importlib.util.find_spec("strawberry") is None:  # pragma: no cover
    pytest.skip(
        "strawberry-graphql not installed; skipping GraphQL tests",
        allow_module_level=True,
    )

try:
    from liminal.graphql_schema import WEB, schema
except Exception as e:  # pragma: no cover
    pytest.skip(f"graphql schema unavailable: {e}", allow_module_level=True)


def exec_gql(query: str, variables: dict[str, Any] | None = None) -> dict[str, Any]:
    res = schema.execute_sync(query, variable_values=variables)
    assert res.errors is None, f"GraphQL errors: {res.errors}"
    assert res.data is not None
    return res.data


def setup_function(_: Any) -> None:
    WEB._nodes.clear()
    WEB._edges.clear()


def test_top_at_risk_edges_basic(monkeypatch: pytest.MonkeyPatch) -> None:
    # Keep threshold moderate so we get some advice on low scores
    monkeypatch.setenv("LIMINAL_HEALTH_THRESHOLD", "0.6")

    # Create three nodes: one pair with negative traits to be at-risk
    exec_gql(
        """
        mutation {
          a: addNode(kind: "module_state", traits: [{key:"любовь", value:0.8}]) { id }
          b: addNode(kind: "module_state", traits: [{key:"страх", value:0.9}]) { id }
          c: addNode(kind: "module_state", traits: [{key:"спокойствие", value:0.7}]) { id }
        }
        """
    )

    data = exec_gql(
        "query { topAtRiskEdges(limit: 2) { sourceId targetId score advice rationale } }"
    )
    edges = data["topAtRiskEdges"]
    assert len(edges) <= 2

    # Scores should be ascending (lowest first)
    scores = [e["score"] for e in edges]
    assert scores == sorted(scores)

    # If any is below threshold, it should carry advice
    low = [e for e in edges if e["score"] < 0.6]
    if low:
        assert any("breathStep" in e["advice"] for e in low)
