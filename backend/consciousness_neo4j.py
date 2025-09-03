"""
Neo4j Writer for Consciousness Temporal Data Lake.

This module provides a writer class to interact with a Neo4j database
for storing and retrieving consciousness state data.
"""
import os
import uuid
from datetime import datetime
from typing import Any, Dict, List, Optional

import requests
from consciousness_schema import (PHILOSOPHICAL_THRESHOLDS, ConsciousnessNode,
                                  ConsciousnessQueries, ConsciousnessState,
                                  StateTransition, TransitionTrigger)
from neo4j import GraphDatabase


class ConsciousnessNeo4jWriter:
    """Writer for consciousness states in Neo4j."""

    def __init__(self, uri: str = None, user: str = None, password: str = None):
        self.uri = uri or os.getenv("NEO4J_URI", "bolt://localhost:7687")
        self.user = user or os.getenv("NEO4J_USER", "neo4j")
        self.password = password or os.getenv("NEO4J_PASSWORD", "password")
        self.driver = GraphDatabase.driver(self.uri, auth=(self.user, self.password))
        self.queries = ConsciousnessQueries()
        self.ws_relay_url = os.getenv(
            "WS_RELAY_URL", "http://localhost:8080/events"
        )

    def close(self):
        """Close the Neo4j connection."""
        if self.driver:
            self.driver.close()

    def create_consciousness_state(self, node: ConsciousnessNode) -> Dict[str, Any]:
        """Create a consciousness state node."""
        with self.driver.session() as session:
            result = session.run(
                self.queries.create_consciousness_node(), **node.to_neo4j_dict()
            )
            return result.single()["c"]

    def create_state_transition(
        self, transition: StateTransition, from_node_id: str, to_node_id: str
    ) -> Dict[str, Any]:
        """Create a transition between states and publish an event."""
        with self.driver.session() as session:
            params = transition.to_neo4j_dict()
            params.update(
                {
                    "from_id": from_node_id,
                    "to_id": to_node_id,
                    "transition_id": transition.id,
                }
            )
            result = session.run(self.queries.create_state_transition(), **params)
            created_transition = result.single()["t"]

            try:
                event = {
                    "type": "state_transition",
                    "from_state": transition.from_state.value,
                    "to_state": transition.to_state.value,
                    "transition_id": transition.id,
                    "timestamp": transition.timestamp.isoformat(),
                    "trigger": transition.trigger.value,
                    "presence_delta": transition.presence_delta,
                    "harmony_delta": transition.harmony_delta,
                    "authenticity_delta": transition.authenticity_delta,
                    "trigger_data": transition.trigger_data,
                }
                headers = {"Content-Type": "application/json"}
                response = requests.post(
                    self.ws_relay_url, json=event, headers=headers
                )
                if response.status_code in [200, 202]:
                    print(
                        f"WebSocket event sent: {transition.from_state.value} -> "
                        f"{transition.to_state.value}"
                    )
                else:
                    print(
                        "Error sending WebSocket event: "
                        f"HTTP {response.status_code}"
                    )
            except Exception as e:
                print(f"Error sending WebSocket event: {str(e)}")

            return created_transition

    def find_home_states(self) -> List[Dict[str, Any]]:
        """Find 'home' states with high authenticity."""
        with self.driver.session() as session:
            result = session.run(self.queries.find_home_states())
            return [record["c"] for record in result]

    def analyze_temporal_patterns(self, days: int = 7) -> List[Dict[str, Any]]:
        """Analyze temporal patterns of transitions for the last N days."""
        with self.driver.session() as session:
            query = """
            MATCH (s:ConsciousnessState)<-[:FROM]-(t:StateTransition)-[:TO]->(e:ConsciousnessState)
            WHERE t.timestamp > datetime() - duration({days: $days})
            WITH s.state as src, e.state as tgt, t
            RETURN src, tgt, count(t) as freq, collect(t.timestamp) as timestamps
            ORDER BY freq DESC
            """
            result = session.run(query, days=days)
            return [dict(record) for record in result]

    def get_consciousness_timeline(
        self, hours: int = 24, user_id: Optional[str] = None
    ) -> Dict[str, Any]:
        """Get the consciousness timeline for a given period."""
        with self.driver.session() as session:
            params = {"hours": hours}
            user_filter = ""
            if user_id:
                user_filter = (
                    "AND exists((u:User)-[:EXPERIENCED]->(t) "
                    "WHERE u.id = $user_id)"
                )
                params["user_id"] = user_id
            query = f"""
            MATCH (start:ConsciousnessState)<-[:FROM]-(t:StateTransition)-[:TO]->(end:ConsciousnessState)
            WHERE t.timestamp > datetime() - duration({{hours: $hours}})
            {user_filter}
            RETURN start, t, end
            ORDER BY t.timestamp
            """
            result = session.run(query, **params)
            nodes, links = set(), []
            for record in result:
                start_node, transition, end_node = record.values()
                nodes.add(start_node)
                nodes.add(end_node)
                links.append({"source": start_node.id, "target": end_node.id, **dict(transition)})
            return {"nodes": [dict(n) for n in nodes], "links": links}
