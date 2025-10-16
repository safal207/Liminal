#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Neo4j Temporal Data Lake Diagnostic
----------------------------------
Diagnostic script for Philosophy First approach
"""

import json
import os
import sys
import traceback
from datetime import datetime

# Create log file
log_file = os.path.join(os.path.dirname(__file__), "..", "neo4j_docker_diagnostic.log")
print(f"Writing diagnostic log to {log_file}")

with open(log_file, "w", encoding="utf-8") as f:
    f.write(f"=== Neo4j Temporal Data Lake Diagnostic Log ===\n")
    f.write(f"Time: {datetime.now().isoformat()}\n\n")

    # System info
    f.write("=== System Info ===\n")
    f.write(f"Python: {sys.version}\n")
    f.write(f"Platform: {sys.platform}\n")
    f.write(f"File encoding: {sys.getfilesystemencoding()}\n\n")

    # Test Neo4j connection
    f.write("=== Testing Neo4j Connection ===\n")
    try:
        from neo4j import GraphDatabase

        f.write("Neo4j driver imported successfully\n")

        # Connection parameters for Docker
        uri = "bolt://localhost:7687"
        user = "neo4j"
        password = "NewStrongPass123!"

        f.write(f"Connecting to: {uri}\n")

        with GraphDatabase.driver(uri, auth=(user, password)) as driver:
            f.write("Neo4j connection established\n")

            # Test simple query
            with driver.session() as session:
                f.write("Testing simple query...\n")
                result = session.run("MATCH (n) RETURN count(n) as count")
                for record in result:
                    count = record["count"]
                    f.write(f"Node count in database: {count}\n")

                # Get consciousness states
                f.write("\n=== Consciousness States in Database ===\n")
                result = session.run(
                    """
                    MATCH (n:ConsciousnessNode)
                    RETURN n.state as state, count(n) as count
                    ORDER BY count DESC
                """
                )

                states = []
                for record in result:
                    state = record["state"]
                    count = record["count"]
                    states.append({"state": state, "count": count})
                    f.write(f"State: {state} - Count: {count}\n")

                # Get state transitions
                f.write("\n=== State Transitions in Database ===\n")
                result = session.run(
                    """
                    MATCH (source:ConsciousnessNode)-[t:TRANSITIONS_TO]->(target:ConsciousnessNode)
                    RETURN source.state as source_state, target.state as target_state, 
                           count(t) as count
                    ORDER BY count DESC
                    LIMIT 10
                """
                )

                transitions = []
                for record in result:
                    source_state = record["source_state"]
                    target_state = record["target_state"]
                    count = record["count"]
                    transitions.append(
                        {
                            "source_state": source_state,
                            "target_state": target_state,
                            "count": count,
                        }
                    )
                    f.write(
                        f"Transition: {source_state} → {target_state} - Count: {count}\n"
                    )

                # Create new philosophical transition if none exist
                if len(transitions) == 0:
                    f.write("\n=== Creating New Philosophical Transitions ===\n")

                    # Import necessary modules
                    try:
                        import uuid

                        try:
                            from backend.consciousness_neo4j import (
                                ConsciousnessNeo4jWriter,
                            )
                        except ModuleNotFoundError:
                            from consciousness_neo4j import (  # type: ignore[no-redef]
                                ConsciousnessNeo4jWriter,
                            )
                        from consciousness_schema import (
                            ConsciousnessNode,
                            ConsciousnessState,
                            StateTransition,
                            TransitionTrigger,
                        )

                        writer = ConsciousnessNeo4jWriter(
                            uri=uri, user=user, password=password
                        )

                        # Philosophy First transition sequence
                        transitions_to_create = [
                            (
                                "TRANSITION_LIMINAL",
                                "PRESENCE_NOW",
                                "Transition to full presence in the now",
                            ),
                            (
                                "PRESENCE_NOW",
                                "HARMONY_BALANCE",
                                "Achieving harmony through meditation",
                            ),
                            (
                                "HARMONY_BALANCE",
                                "HOME_AUTHENTIC",
                                "Home is where you're authentic with yourself",
                            ),
                            (
                                "HOME_AUTHENTIC",
                                "QUESTION_SPACE",
                                "We've learned to ask the right questions",
                            ),
                            (
                                "QUESTION_SPACE",
                                "RESONANCE_MOMENT",
                                "Moment of consciousness resonance",
                            ),
                        ]

                        # Create users for testing resonance
                        users = [
                            "philosopher_1",
                            "philosopher_2",
                            "consciousness_explorer",
                            "authentic_self",
                        ]

                        for (
                            source_name,
                            target_name,
                            description,
                        ) in transitions_to_create:
                            for user_id in users:
                                try:
                                    # Map string names to enum values
                                    source_state_enum = getattr(
                                        ConsciousnessState, source_name
                                    )
                                    target_state_enum = getattr(
                                        ConsciousnessState, target_name
                                    )

                                    # Create source node
                                    source_node = ConsciousnessNode(
                                        id=str(uuid.uuid4()),
                                        state=source_state_enum,
                                        timestamp=datetime.now(),
                                        home_resonance=0.5,
                                        presence_level=0.7,
                                        authenticity_score=0.6,
                                        harmony_index=0.5,
                                        user_id=user_id,
                                    )

                                    # Create target node with improved metrics
                                    target_node = ConsciousnessNode(
                                        id=str(uuid.uuid4()),
                                        state=target_state_enum,
                                        timestamp=datetime.now(),
                                        home_resonance=0.7,
                                        presence_level=0.8,
                                        authenticity_score=0.7,
                                        harmony_index=0.6,
                                        user_id=user_id,
                                    )

                                    # Create transition
                                    transition = StateTransition(
                                        id=str(uuid.uuid4()),
                                        source_id=source_node.id,
                                        target_id=target_node.id,
                                        trigger=TransitionTrigger.PHILOSOPHICAL_INSIGHT,
                                        timestamp=datetime.now(),
                                        presence_delta=0.1,
                                        harmony_delta=0.1,
                                        authenticity_delta=0.1,
                                        philosophical_significance=description,
                                        user_id=user_id,
                                    )

                                    # Save to Neo4j
                                    writer.create_consciousness_node(source_node)
                                    writer.create_consciousness_node(target_node)
                                    writer.create_state_transition(transition)

                                    f.write(
                                        f"Created transition for {user_id}: {source_name} → {target_name}\n"
                                    )
                                    f.write(f"  Description: {description}\n")

                                except Exception as e:
                                    f.write(f"Error creating transition: {str(e)}\n")
                                    f.write(traceback.format_exc() + "\n")

                        # Close Neo4j connection
                        writer.close()
                        f.write("Transitions created successfully\n")

                    except Exception as e:
                        f.write(
                            f"Error importing modules or creating transitions: {str(e)}\n"
                        )
                        f.write(traceback.format_exc() + "\n")

                # Try to analyze temporal patterns
                f.write("\n=== Analyzing Temporal Patterns ===\n")
                try:
                    try:
                        from backend.consciousness_neo4j import (
                            ConsciousnessNeo4jWriter,
                        )
                    except ModuleNotFoundError:
                        from consciousness_neo4j import (  # type: ignore[no-redef]
                            ConsciousnessNeo4jWriter,
                        )

                    writer = ConsciousnessNeo4jWriter(
                        uri=uri, user=user, password=password
                    )

                    # Analyze patterns
                    patterns = writer.analyze_temporal_patterns(days=7)
                    f.write(f"Found {len(patterns)} temporal patterns\n")

                    for i, pattern in enumerate(patterns[:5], 1):
                        f.write(
                            f"Pattern #{i}: {pattern['source']} → {pattern['target']}\n"
                        )
                        f.write(f"  Frequency: {pattern.get('frequency', 'N/A')}\n")
                        f.write(f"  Type: {pattern.get('pattern_type', 'N/A')}\n")
                        f.write(
                            f"  Significance: {pattern.get('philosophical_significance', 'N/A')}\n"
                        )

                    # Find resonance moments
                    f.write("\n=== Finding Resonance Moments ===\n")
                    resonance = writer.find_resonance_moments(hours=24)
                    f.write(f"Found {len(resonance)} resonance moments\n")

                    for i, moment in enumerate(resonance[:3], 1):
                        f.write(
                            f"Resonance #{i}: {moment.get('user1', 'N/A')} and {moment.get('user2', 'N/A')}\n"
                        )
                        f.write(f"  State: {moment.get('to_state', 'N/A')}\n")
                        f.write(f"  Type: {moment.get('resonance_type', 'N/A')}\n")
                        f.write(
                            f"  Meaning: {moment.get('philosophical_meaning', 'N/A')}\n"
                        )

                    # Get timeline
                    f.write("\n=== Getting Consciousness Timeline ===\n")
                    timeline = writer.get_consciousness_timeline(hours=24)
                    nodes = timeline.get("nodes", [])
                    links = timeline.get("links", [])
                    f.write(
                        f"Timeline contains {len(nodes)} states and {len(links)} transitions\n"
                    )

                    # Close writer
                    writer.close()

                except Exception as e:
                    f.write(f"Error analyzing patterns: {str(e)}\n")
                    f.write(traceback.format_exc() + "\n")

                # Try to broadcast transition to WebSocket
                f.write("\n=== Broadcasting to WebSocket ===\n")
                try:
                    import requests

                    # Create test event
                    event = {
                        "event_type": "consciousness_transition",
                        "timestamp": datetime.now().isoformat(),
                        "user_id": "philosopher_1",
                        "source_state": "QUESTION_SPACE",
                        "target_state": "RESONANCE_MOMENT",
                        "meta": {
                            "description": "We've learned to ask the right questions leading to resonance",
                            "philosophical": True,
                            "source_description": "Question-Driven Architecture",
                            "target_description": "Resonance Broadcasting",
                            "presence_delta": 0.2,
                            "harmony_delta": 0.1,
                            "authenticity_delta": 0.3,
                        },
                    }

                    # Send to WebSocket relay
                    f.write(
                        f"Sending event to WebSocket relay at http://localhost:8080/events\n"
                    )
                    response = requests.post(
                        "http://localhost:8080/events",
                        json=event,
                        headers={"Content-Type": "application/json"},
                    )

                    f.write(f"WebSocket response: {response.status_code}\n")
                    if response.text:
                        f.write(f"Response content: {response.text}\n")

                except Exception as e:
                    f.write(f"Error broadcasting to WebSocket: {str(e)}\n")
                    f.write(traceback.format_exc() + "\n")

    except ImportError as e:
        f.write(f"Error importing Neo4j driver: {str(e)}\n")
        f.write(traceback.format_exc() + "\n")
    except Exception as e:
        f.write(f"Error connecting to Neo4j: {str(e)}\n")
        f.write(traceback.format_exc() + "\n")

    # Done
    f.write("\n=== Diagnostic Complete ===\n")
    f.write(f"Time: {datetime.now().isoformat()}\n")

print(f"Diagnostic complete. Check {log_file} for results.")
