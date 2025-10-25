from __future__ import annotations

import importlib.util
from typing import Any

import pytest

# Skip tests if strawberry is not installed locally
strawberry_spec = importlib.util.find_spec("strawberry")
if strawberry_spec is None:  # pragma: no cover - environment guard
    pytest.skip(
        "strawberry-graphql not installed; skipping GraphQL tests",
        allow_module_level=True,
    )

try:
    from liminal.graphql_schema import (WEB,  # use module-level singletons
                                        schema)
except Exception as e:  # pragma: no cover - environment guard
    pytest.skip(f"graphql schema unavailable: {e}", allow_module_level=True)


def exec_gql(query: str, variables: dict[str, Any] | None = None) -> dict[str, Any]:
    res = schema.execute_sync(query, variable_values=variables)
    assert res.errors is None, f"GraphQL errors: {res.errors}"
    assert res.data is not None
    return res.data  # type: ignore[return-value]


def setup_function(_: Any) -> None:
    # reset in-memory web between tests
    WEB._nodes.clear()
    WEB._edges.clear()


def test_relationship_health_bounded_and_breakdown_present() -> None:
    # create two nodes
    create = exec_gql(
        """
        mutation {
          a: addNode(kind: "module_state", traits: [
            {key:"любовь", value:0.8}, {key:"спокойствие", value:0.6}
          ]) { id }
          b: addNode(kind: "module_state", traits: [
            {key:"страх", value:0.3}, {key:"гнев", value:0.2}
          ]) { id }
        }
        """
    )
    a_id = create["a"]["id"]
    b_id = create["b"]["id"]

    data = exec_gql(
        """
        mutation($a: String!, $b: String!) {
          h: computeRelationshipHealth(sourceId: $a, targetId: $b) {
            score
            rationale
            breakdown { key weight contribution }
          }
        }
        """,
        {"a": a_id, "b": b_id},
    )

    h = data["h"]
    assert 0.0 <= h["score"] <= 1.0
    assert any("base_similarity=" in r for r in h["rationale"])  # sanity
    assert isinstance(h["breakdown"], list) and len(h["breakdown"]) >= 1


def test_relationship_health_improves_with_positive_traits() -> None:
    # nodes with neutral overlap
    res1 = exec_gql(
        """
        mutation {
          a: addNode(kind: "module_state", traits: [{key:"дом", value:0.5}]) { id }
          b: addNode(kind: "module_state", traits: [{key:"дом", value:0.5}]) { id }
        }
        """
    )
    a = res1["a"]["id"]
    b = res1["b"]["id"]

    h1 = exec_gql(
        """
        mutation($a: String!, $b: String!) {
          h: computeRelationshipHealth(sourceId: $a, targetId: $b) { score }
        }
        """,
        {"a": a, "b": b},
    )["h"]["score"]

    # add extra nodes with positive traits and recompute on those
    WEB._nodes.clear()
    WEB._edges.clear()
    res2 = exec_gql(
        """
        mutation {
          a: addNode(kind: "module_state", traits: [
            {key:"дом", value:0.5}, {key:"любовь", value:0.9}, {key:"спокойствие", value:0.8}
          ]) { id }
          b: addNode(kind: "module_state", traits: [
            {key:"дом", value:0.5}
          ]) { id }
        }
        """
    )
    a2 = res2["a"]["id"]
    b2 = res2["b"]["id"]
    h2 = exec_gql(
        """
        mutation($a: String!, $b: String!) {
          h: computeRelationshipHealth(sourceId: $a, targetId: $b) { score }
        }
        """,
        {"a": a2, "b": b2},
    )["h"]["score"]

    assert h2 >= h1  # добавление любви/спокойствия не должно ухудшать здоровье
