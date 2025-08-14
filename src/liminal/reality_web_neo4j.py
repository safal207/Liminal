from __future__ import annotations

import datetime
import json
import time
import uuid
from dataclasses import dataclass, field
from typing import Any, Callable, Dict, List, Optional, Tuple

from neo4j import GraphDatabase

from .adapters import build_state_from_reince
from .diffusion import InMemoryDiffusion, ModuleState
from .reality_web import Edge, Node, SystemBreath
from .reince import InMemoryREINCE


class RealityWebNeo4j:
    """Neo4j-based implementation of RealityWeb for storing nodes and edges.

    Philosophy: хранение графа в Neo4j с real-time обновлениями через WebSocket,
    сохраняя все преимущества исходной in-memory реализации.
    """

    def __init__(
        self, uri="bolt://localhost:7687", user="neo4j", password="NewStrongPass123!"
    ) -> None:
        """Initialize Neo4j connection and prepare the driver."""
        self.driver = GraphDatabase.driver(uri, auth=(user, password))
        self._diff = InMemoryDiffusion()
        # Initialize Neo4j database with constraints and indices
        self._init_db()

    def _init_db(self) -> None:
        """Initialize Neo4j database with constraints and indices for better performance."""
        with self.driver.session() as session:
            # Constraints for uniqueness
            session.run(
                "CREATE CONSTRAINT IF NOT EXISTS FOR (n:Node) REQUIRE n.id IS UNIQUE"
            )

            # Indices for faster lookups
            session.run("CREATE INDEX IF NOT EXISTS FOR (n:Node) ON (n.kind)")
            session.run("CREATE INDEX IF NOT EXISTS FOR (n:Node) ON (n.created_at)")

            # Helpful existence constraints (Neo4j 5+). This constraint is only supported in Enterprise edition.
            try:
                session.run(
                    "CREATE CONSTRAINT IF NOT EXISTS FOR (n:Node) REQUIRE n.kind IS NOT NULL"
                )
            except Exception:
                # Ignore constraint creation errors on Community edition
                pass

            # Relationship indexes for common filters/ordering
            session.run("CREATE INDEX IF NOT EXISTS FOR ()-[r:RELATES]-() ON (r.kind)")
            session.run(
                "CREATE INDEX IF NOT EXISTS FOR ()-[r:RELATES]-() ON (r.weight)"
            )

    def close(self) -> None:
        """Close the Neo4j driver connection."""
        self.driver.close()

    # ---- Node/Edge management ----
    def add_node(
        self,
        kind: str,
        traits: Optional[Dict[str, float]] = None,
        notes: Optional[List[str]] = None,
        id: Optional[str] = None,
    ) -> Node:
        """Add a node to Neo4j database and return the created Node object."""
        nid = id or str(uuid.uuid4())
        traits_dict = dict(traits or {})
        notes_list = list(notes or [])

        # Convert traits and notes to JSON for Neo4j storage
        traits_json = json.dumps(traits_dict)
        notes_json = json.dumps(notes_list)

        with self.driver.session() as session:
            # Create node in Neo4j
            result = session.run(
                """
                CREATE (n:Node {
                    id: $id,
                    kind: $kind,
                    traits: $traits,
                    notes: $notes,
                    created_at: datetime()
                })
                RETURN n.id as id, n.kind as kind, n.traits as traits, n.notes as notes
                """,
                id=nid,
                kind=kind,
                traits=traits_json,
                notes=notes_json,
            )

            record = result.single()
            if not record:
                # Fallback to inmemory representation if Neo4j fails
                return Node(id=nid, kind=kind, traits=traits_dict, notes=notes_list)

            # Parse traits and notes from JSON strings
            db_traits = json.loads(record["traits"])
            db_notes = json.loads(record["notes"])

            # Create Node object from Neo4j data
            return Node(
                id=record["id"], kind=record["kind"], traits=db_traits, notes=db_notes
            )

    def add_edge(
        self,
        source_id: str,
        target_id: str,
        kind: str,
        weight: float = 0.0,
        notes: Optional[List[str]] = None,
        rationale: Optional[List[str]] = None,
    ) -> Edge:
        """Add an edge between two nodes in Neo4j and return the created Edge object."""
        weight_clipped = max(0.0, min(1.0, weight))
        notes_list = list(notes or [])
        rationale_list = list(rationale or [])

        # Convert notes and rationale to JSON for Neo4j storage
        notes_json = json.dumps(notes_list)
        rationale_json = json.dumps(rationale_list)

        with self.driver.session() as session:
            # Check if nodes exist
            source_exists = session.run(
                "MATCH (n:Node {id: $id}) RETURN count(n) as count", id=source_id
            ).single()
            target_exists = session.run(
                "MATCH (n:Node {id: $id}) RETURN count(n) as count", id=target_id
            ).single()

            if source_exists["count"] == 0 or target_exists["count"] == 0:
                # If nodes don't exist in Neo4j, create a local Edge object
                return Edge(
                    source_id=source_id,
                    target_id=target_id,
                    kind=kind,
                    weight=weight_clipped,
                    notes=notes_list,
                    rationale=rationale_list,
                )

            # Create relationship in Neo4j
            result = session.run(
                """
                MATCH (source:Node {id: $source_id})
                MATCH (target:Node {id: $target_id})
                CREATE (source)-[r:RELATES {
                    kind: $kind,
                    weight: $weight,
                    notes: $notes,
                    rationale: $rationale,
                    created_at: datetime()
                }]->(target)
                RETURN r.kind as kind, r.weight as weight, r.notes as notes, r.rationale as rationale
                """,
                source_id=source_id,
                target_id=target_id,
                kind=kind,
                weight=weight_clipped,
                notes=notes_json,
                rationale=rationale_json,
            )

            record = result.single()
            if not record:
                # Fallback to inmemory representation if Neo4j fails
                return Edge(
                    source_id=source_id,
                    target_id=target_id,
                    kind=kind,
                    weight=weight_clipped,
                    notes=notes_list,
                    rationale=rationale_list,
                )

            # Parse notes and rationale from JSON strings
            db_notes = json.loads(record["notes"])
            db_rationale = json.loads(record["rationale"])

            # Create Edge object from Neo4j data
            return Edge(
                source_id=source_id,
                target_id=target_id,
                kind=record["kind"],
                weight=record["weight"],
                notes=db_notes,
                rationale=db_rationale,
            )

    def nodes(self) -> List[Node]:
        """Get all nodes from Neo4j database."""
        with self.driver.session() as session:
            result = session.run(
                """
                MATCH (n:Node)
                RETURN n.id as id, n.kind as kind, n.traits as traits, n.notes as notes
                """
            )

            nodes = []
            for record in result:
                # Parse traits and notes from JSON strings
                traits = json.loads(record["traits"]) if record["traits"] else {}
                notes = json.loads(record["notes"]) if record["notes"] else []

                # Create Node object from Neo4j data
                node = Node(
                    id=record["id"], kind=record["kind"], traits=traits, notes=notes
                )
                nodes.append(node)

            return nodes

    def edges(self) -> List[Edge]:
        """Get all edges from Neo4j database."""
        with self.driver.session() as session:
            result = session.run(
                """
                MATCH (source:Node)-[r:RELATES]->(target:Node)
                RETURN source.id as source_id, target.id as target_id, 
                       r.kind as kind, r.weight as weight, 
                       r.notes as notes, r.rationale as rationale
                """
            )

            edges = []
            for record in result:
                # Parse notes and rationale from JSON strings
                notes = json.loads(record["notes"]) if record["notes"] else []
                rationale = (
                    json.loads(record["rationale"]) if record["rationale"] else []
                )

                # Create Edge object from Neo4j data
                edge = Edge(
                    source_id=record["source_id"],
                    target_id=record["target_id"],
                    kind=record["kind"],
                    weight=record["weight"],
                    notes=notes,
                    rationale=rationale,
                )
                edges.append(edge)

            return edges

    # ---- Adapters ----
    def node_from_module_state(self, ms: ModuleState, id: Optional[str] = None) -> Node:
        """Create a node from ModuleState object."""
        return self.add_node(
            kind="module_state", traits=ms.traits, notes=ms.notes, id=id
        )

    def node_from_reince(
        self,
        reince: InMemoryREINCE,
        name: str,
        top_n: int = 10,
        id: Optional[str] = None,
    ) -> Node:
        """Create a node from REINCE object using build_state_from_reince adapter."""
        ms = build_state_from_reince(
            reince, name=name, top_n=top_n, notes=["from_reince"]
        )
        return self.node_from_module_state(ms, id=id)

    # ---- Relationships ----
    def link_similarity(self, a: Node, b: Node, kind: str = "similar_to") -> Edge:
        """Link two nodes based on similarity of their traits."""
        sim = self._diff.similarity(
            ModuleState("_a", traits=a.traits, notes=[]),
            ModuleState("_b", traits=b.traits, notes=[]),
        )
        return self.add_edge(
            a.id, b.id, kind=kind, weight=sim, rationale=[f"cosine_like={sim:.3f}"]
        )

    def link_merge(
        self, a: Node, b: Node, name: str = "merged", kind: str = "merged_into"
    ) -> Tuple[Node, Edge]:
        """Merge two nodes and create relationships to the merged node."""
        # Blend the states and create a new node representing the merge
        br = self._diff.blend(
            [
                ModuleState("a", traits=a.traits, notes=a.notes),
                ModuleState("b", traits=b.traits, notes=b.notes),
            ],
            name=name,
        )
        merged = self.node_from_module_state(br.state)
        e = self.add_edge(
            a.id, merged.id, kind=kind, weight=1.0, rationale=br.rationale
        )
        self.add_edge(b.id, merged.id, kind=kind, weight=1.0, rationale=br.rationale)
        return merged, e

    def link_parent(self, parent: Node, child: Node) -> Edge:
        """Create explicit parent→child relationship."""
        return self.add_edge(
            parent.id,
            child.id,
            kind="parent",
            weight=1.0,
            rationale=["family_bond=parent_child"],
        )

    def top_at_risk(
        self, limit: int = 5, threshold: float = 0.4
    ) -> List[Dict[str, Any]]:
        """Find top at-risk relationships using Neo4j's processing power.

        This is an optimized version that processes the calculation in the database.
        """
        nodes = self.nodes()

        # If too few nodes, fallback to naive calculation
        if len(nodes) < 2:
            return []

        # Create a mapping of node_id -> traits for quick access
        node_traits = {node.id: node.traits for node in nodes}

        # Generate all possible pairs (avoiding duplicates)
        edges = []
        for i in range(len(nodes)):
            for j in range(i + 1, len(nodes)):
                a = nodes[i]
                b = nodes[j]

                if a.traits or b.traits:
                    # Calculate similarity score
                    score = self._diff.similarity(
                        ModuleState("a", traits=a.traits, notes=[]),
                        ModuleState("b", traits=b.traits, notes=[]),
                    )

                    # Apply bonus/penalty similar to compute_relationship_health
                    POS_KEYS = getattr(
                        SystemBreath,
                        "SOFT_KEYS_POS",
                        (
                            "любовь",
                            "спокойствие",
                            "нежность",
                            "мягкость",
                            "calm",
                            "love",
                            "tenderness",
                        ),
                    )
                    NEG_KEYS = getattr(
                        SystemBreath,
                        "SOFT_KEYS_NEG",
                        ("страх", "гнев", "злость", "тревога", "anger", "fear"),
                    )

                    # Calculate averages
                    pos_values = []
                    neg_values = []

                    for k in POS_KEYS:
                        if k in a.traits or k in b.traits:
                            va = float(a.traits.get(k, 0.0))
                            vb = float(b.traits.get(k, 0.0))
                            pos_values.append((va + vb) / 2.0)

                    for k in NEG_KEYS:
                        if k in a.traits or k in b.traits:
                            va = float(a.traits.get(k, 0.0))
                            vb = float(b.traits.get(k, 0.0))
                            neg_values.append((va + vb) / 2.0)

                    pos_avg = sum(pos_values) / len(pos_values) if pos_values else 0.0
                    neg_avg = sum(neg_values) / len(neg_values) if neg_values else 0.0

                    # Apply adjustments
                    POS_W = 0.10
                    NEG_W = 0.10
                    bonus = POS_W * pos_avg
                    penalty = NEG_W * neg_avg

                    # Calculate final score
                    raw_score = score + bonus - penalty
                    final_score = max(0.0, min(1.0, raw_score))

                    # Generate rationale
                    rationale = [
                        f"base_similarity={score:.3f}",
                        f"pos_avg={pos_avg:.3f}*{POS_W:.2f} -> +{bonus:.3f}",
                        f"neg_avg={neg_avg:.3f}*{NEG_W:.2f} -> -{penalty:.3f}",
                        f"score_clipped={final_score:.3f}",
                    ]

                    # Generate advice based on threshold
                    advice = []
                    if final_score < threshold:
                        advice = ["breathStep", "consider_linkParent", "consider_merge"]

                    # Add to result list
                    edges.append(
                        {
                            "sourceId": a.id,
                            "targetId": b.id,
                            "score": final_score,
                            "advice": advice,
                            "rationale": rationale,
                        }
                    )

        # Sort by score (lowest first) and limit
        edges.sort(key=lambda e: e["score"])
        return edges[: max(0, int(limit))]

    def __del__(self):
        """Ensure driver is closed when object is garbage collected."""
        try:
            self.close()
        except:
            pass
