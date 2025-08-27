"""Integration tests for at-risk REST endpoints.

These tests require a running server. Run manually:
1. python scripts/at_risk_offline_server.py
2. python -m pytest tests/test_at_risk_rest.py
"""

from typing import Any, Dict, List

import pytest
import requests

# Test against a running server
BASE_URL = "http://127.0.0.1:8000"


@pytest.mark.integration
def test_html_at_risk_served() -> None:
    """Test that /at-risk HTML page is served with expected UI elements."""
    try:
        resp = requests.get(f"{BASE_URL}/at-risk", timeout=5)
        assert resp.status_code == 200
        text = resp.text
        assert "Top At-Risk Edges" in text
        assert 'id="limit"' in text
        assert 'id="threshold"' in text
        assert 'id="refresh"' in text
        assert 'id="seed"' in text
        assert 'id="exportJson"' in text
        assert 'id="exportCsv"' in text
    except requests.exceptions.ConnectionError:
        pytest.skip("Server not running at {BASE_URL}")


@pytest.mark.integration
def test_seed_demo_then_query_top_at_risk() -> None:
    """Test seeding demo data and querying top at-risk edges."""
    try:
        # Seed demo data
        r = requests.post(f"{BASE_URL}/api/seed-demo", timeout=5)
        assert r.status_code == 200
        body = r.json()
        assert "created" in body  # Should return list of created node IDs

        # Query top at risk edges
        r2 = requests.get(f"{BASE_URL}/api/top-at-risk", params={"limit": 5}, timeout=5)
        assert r2.status_code == 200
        j = r2.json()
        assert "topAtRiskEdges" in j
        edges: List[Dict[str, Any]] = j["topAtRiskEdges"]
        assert isinstance(edges, list)
        assert len(edges) >= 1  # Should have at least one pair after seeding

        # Validate shape
        sample = edges[0]
        for key in ("sourceId", "targetId", "score", "advice"):
            assert key in sample
        assert isinstance(sample["advice"], list)
        assert isinstance(sample["score"], (int, float))
    except requests.exceptions.ConnectionError:
        pytest.skip("Server not running at {BASE_URL}")


@pytest.mark.integration
def test_add_node_and_fetch_again() -> None:
    """Test adding a node and fetching edges again."""
    try:
        # Add a simple node with traits
        payload = {"kind": "module_state", "traits": {"любовь": 0.7, "страх": 0.2}}
        r = requests.post(f"{BASE_URL}/api/add-node", json=payload, timeout=5)
        assert r.status_code == 200
        j = r.json()
        # Should return node info with id
        assert "id" in j

        # Now fetch top-at-risk again and ensure no error
        r2 = requests.get(f"{BASE_URL}/api/top-at-risk", params={"limit": 3}, timeout=5)
        assert r2.status_code == 200
        j2 = r2.json()
        edges = j2.get("topAtRiskEdges", [])
        assert isinstance(edges, list)
        # Shape should be consistent if edges exist
        if edges:
            sample = edges[0]
            for key in ("sourceId", "targetId", "score", "advice"):
                assert key in sample
    except requests.exceptions.ConnectionError:
        pytest.skip("Server not running at {BASE_URL}")


@pytest.mark.integration
@pytest.mark.parametrize("limit", [1, 2, 5])
def test_limit_parameter_effect(limit: int) -> None:
    """Test that limit parameter controls number of returned edges."""
    try:
        r = requests.get(
            f"{BASE_URL}/api/top-at-risk", params={"limit": limit}, timeout=5
        )
        assert r.status_code == 200
        j = r.json()
        edges = j.get("topAtRiskEdges", [])
        assert isinstance(edges, list)
        # Not strict equality because total number of pairs may be < limit
        assert len(edges) <= limit
    except requests.exceptions.ConnectionError:
        pytest.skip("Server not running at {BASE_URL}")
