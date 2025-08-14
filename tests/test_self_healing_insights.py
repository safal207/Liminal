from __future__ import annotations

import importlib.util
import json
import os
from pathlib import Path
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
    return res.data  # type: ignore[return-value]


def setup_function(_: Any) -> None:
    WEB._nodes.clear()
    WEB._edges.clear()


def test_low_health_writes_insight(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    # Arrange: set low threshold high so almost any pair triggers, and set insights path
    insights_path = tmp_path / "insights.jsonl"
    monkeypatch.setenv("LIMINAL_HEALTH_THRESHOLD", "0.99")
    monkeypatch.setenv("LIMINAL_INSIGHTS_PATH", str(insights_path))

    exec_gql(
        """
        mutation {
          a: addNode(kind: "module_state", traits: [{key:"гнев", value:0.9}]) { id }
          b: addNode(kind: "module_state", traits: [{key:"страх", value:0.8}]) { id }
        }
        """
    )
    ids = exec_gql("query { nodes { id } }")
    a_id, b_id = ids["nodes"][0]["id"], ids["nodes"][1]["id"]

    # Act
    exec_gql(
        """
        mutation($a: String!, $b: String!) {
          h: computeRelationshipHealth(sourceId: $a, targetId: $b) { score }
        }
        """,
        {"a": a_id, "b": b_id},
    )

    # Assert: file created and contains JSON line with expected fields
    assert insights_path.exists()
    content = insights_path.read_text(encoding="utf-8").strip().splitlines()
    assert len(content) >= 1
    rec = json.loads(content[-1])
    assert rec["type"] == "relationship_low_health"
    assert set(
        ["ts", "source_id", "target_id", "score", "threshold", "rationale", "advice"]
    ).issubset(rec.keys())
