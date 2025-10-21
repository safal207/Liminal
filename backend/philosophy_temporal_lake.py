#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Philosophy First - Neo4j Temporal Data Lake Integration
------------------------------------------------------
Integration of philosophical concepts with Neo4j and WebSocket relay
English-only version for Windows compatibility
"""

import argparse
import json
import os
import random
import sys
import time
import traceback
import uuid
from datetime import datetime

import requests

try:
    from backend.philosophy_neo4j import PhilosophyNeo4jWriter
    from backend.philosophy_schema import (
        ConsciousnessNode,
        ConsciousnessState,
        StateTransition,
        TransitionTrigger,
        create_consciousness_event,
    )
except ModuleNotFoundError:  # pragma: no cover - legacy fallback when run as script
    from philosophy_neo4j import PhilosophyNeo4jWriter
    from philosophy_schema import (  # type: ignore[no-redef]
        ConsciousnessNode,
        ConsciousnessState,
        StateTransition,
        TransitionTrigger,
        create_consciousness_event,
    )


def log_message(title, content=None):
    """Print formatted message"""
    print(f"\n=== {title} ===")
    if content:
        if isinstance(content, (dict, list)):
            try:
                print(json.dumps(content, indent=2, ensure_ascii=True))
            except:
                print(str(content))
        else:
            print(str(content))


def create_philosophical_transitions(writer, users=None):
    """
    Create philosophical consciousness state transitions.

    Args:
        writer: Neo4j Writer
        users: List of users for transition creation
    """
    log_message("CREATING PHILOSOPHICAL CONSCIOUSNESS TRANSITIONS")

    # Philosophy First transition sequence
    transitions_to_create = [
        (
            ConsciousnessState.TRANSITION_LIMINAL,
            ConsciousnessState.PRESENCE_NOW,
            "Transition to full presence in the moment",
            TransitionTrigger.PRESENCE_PRACTICE,
        ),
        (
            ConsciousnessState.PRESENCE_NOW,
            ConsciousnessState.HARMONY_BALANCE,
            "Achieving harmony through meditation",
            TransitionTrigger.MEDITATION,
        ),
        (
            ConsciousnessState.HARMONY_BALANCE,
            ConsciousnessState.HOME_AUTHENTIC,
            "Home is where you're authentic with yourself",
            TransitionTrigger.HOME_RECOGNITION,
        ),
        (
            ConsciousnessState.HOME_AUTHENTIC,
            ConsciousnessState.QUESTION_SPACE,
            "We've learned to ask the right questions",
            TransitionTrigger.QUESTION_REFRAMING,
        ),
        (
            ConsciousnessState.QUESTION_SPACE,
            ConsciousnessState.RESONANCE_MOMENT,
            "Moment of consciousness resonance",
            TransitionTrigger.RESONANCE_ALIGNMENT,
        ),
        (
            ConsciousnessState.RESONANCE_MOMENT,
            ConsciousnessState.REFLECTION_SELF,
            "Self-reflection after resonance moment",
            TransitionTrigger.PHILOSOPHICAL_INSIGHT,
        ),
        (
            ConsciousnessState.REFLECTION_SELF,
            ConsciousnessState.INTEGRATION_WISDOM,
            "Integration of acquired wisdom",
            TransitionTrigger.SYNCHRONICITY,
        ),
    ]

    # Create users for testing resonance
    if not users:
        users = [
            "philosopher_1",
            "philosopher_2",
            "consciousness_explorer",
            "authentic_self",
        ]

    created_transitions = []

    for source_state, target_state, description, trigger in transitions_to_create:
        for user_id in users:
            try:
                # Create source node with philosophical metrics
                source_node = ConsciousnessNode(
                    id=str(uuid.uuid4()),
                    state=source_state,
                    timestamp=datetime.now(),
                    presence_level=random.uniform(0.4, 0.6),
                    harmony_index=random.uniform(0.4, 0.6),
                    authenticity_score=random.uniform(0.4, 0.6),
                    home_resonance=random.uniform(0.4, 0.6),
                    question_clarity=random.uniform(0.4, 0.6),
                    resonance_strength=random.uniform(0.4, 0.6),
                    user_id=user_id,
                    meta={
                        "philosophical": True,
                        "description": f"{source_state.name} state",
                    },
                )

                # Create target node with improved philosophical metrics
                target_node = ConsciousnessNode(
                    id=str(uuid.uuid4()),
                    state=target_state,
                    timestamp=datetime.now(),
                    presence_level=random.uniform(0.6, 0.9),
                    harmony_index=random.uniform(0.6, 0.9),
                    authenticity_score=random.uniform(0.6, 0.9),
                    home_resonance=random.uniform(0.6, 0.9),
                    question_clarity=random.uniform(0.6, 0.9),
                    resonance_strength=random.uniform(0.6, 0.9),
                    user_id=user_id,
                    meta={
                        "philosophical": True,
                        "description": f"{target_state.name} state",
                    },
                )

                # Create transition between nodes
                transition = StateTransition(
                    id=str(uuid.uuid4()),
                    source_id=source_node.id,
                    target_id=target_node.id,
                    trigger=trigger,
                    timestamp=datetime.now(),
                    presence_delta=target_node.presence_level
                    - source_node.presence_level,
                    harmony_delta=target_node.harmony_index - source_node.harmony_index,
                    authenticity_delta=target_node.authenticity_score
                    - source_node.authenticity_score,
                    resonance_delta=target_node.resonance_strength
                    - source_node.resonance_strength,
                    philosophical_significance=description,
                    user_id=user_id,
                    meta={"philosophical": True},
                )

                # Save to Neo4j
                writer.create_consciousness_node(source_node)
                writer.create_consciousness_node(target_node)
                writer.create_state_transition(transition)

                log_message(
                    f"Created transition for {user_id}",
                    f"{source_state.name} -> {target_state.name}",
                )
                print(f"  Trigger: {trigger.name}")
                print(f"  Description: {description}")

                created_transitions.append(transition.id)

                # Small pause for time dispersion
                time.sleep(0.2)

            except Exception as e:
                log_message("Error creating transition", str(e))
                traceback.print_exc()

    return created_transitions


def broadcast_transitions_to_websocket(writer, transition_ids):
    """
    Send consciousness state transitions to WebSocket relay.

    Args:
        writer: Neo4j Writer
        transition_ids: List of transition IDs to broadcast
    """
    log_message("BROADCASTING TRANSITIONS TO WEBSOCKET RELAY")

    websocket_url = "http://localhost:8080/events"
    log_message("WebSocket URL", websocket_url)

    successful = 0

    for transition_id in transition_ids:
        try:
            # Get transition details
            transition_details = writer.get_transition_details(transition_id)
            if not transition_details:
                log_message("Transition not found", f"ID: {transition_id}")
                continue

            # Create event for WebSocket
            event = {
                "event_type": "consciousness_transition",
                "timestamp": datetime.now().isoformat(),
                "user_id": transition_details.get("user_id", "unknown"),
                "source_state": transition_details.get("source_state", "unknown"),
                "target_state": transition_details.get("target_state", "unknown"),
                "trigger": transition_details.get("trigger", "UNKNOWN"),
                "meta": {
                    "description": transition_details.get(
                        "philosophical_significance", ""
                    ),
                    "philosophical": True,
                    "presence_delta": transition_details.get("presence_delta", 0),
                    "harmony_delta": transition_details.get("harmony_delta", 0),
                    "authenticity_delta": transition_details.get(
                        "authenticity_delta", 0
                    ),
                    "resonance_delta": transition_details.get("resonance_delta", 0),
                },
            }

            # Send to WebSocket relay via HTTP
            log_message(f"Sending event to WebSocket", f"ID: {transition_id}")
            print(json.dumps(event, indent=2, ensure_ascii=True))

            response = requests.post(
                websocket_url, json=event, headers={"Content-Type": "application/json"}
            )

            if response.status_code == 202:  # Accepted
                log_message(
                    "Event sent successfully", f"Response code: {response.status_code}"
                )
                successful += 1
            else:
                log_message(
                    "Error sending event", f"Response code: {response.status_code}"
                )
                print(f"Response: {response.text}")

            # Pause between sends
            time.sleep(1)

        except Exception as e:
            log_message("Error broadcasting event", str(e))
            traceback.print_exc()

    log_message(
        "BROADCAST SUMMARY", f"{successful} of {len(transition_ids)} events sent"
    )


def analyze_philosophy_patterns(writer):
    """
    Analyze philosophical patterns in Temporal Data Lake.

    Args:
        writer: Neo4j Writer
    """
    log_message("ANALYZING PHILOSOPHICAL PATTERNS")

    try:
        # Temporal pattern analysis
        patterns = writer.analyze_temporal_patterns(days=7)
        log_message(f"Found {len(patterns)} patterns")

        for i, pattern in enumerate(patterns[:5], 1):
            log_message(
                f"Pattern #{i}: {pattern['source']} -> {pattern['target']}",
                f"Frequency: {pattern.get('frequency')}, Type: {pattern.get('pattern_type')}",
            )
            print(
                f"  Philosophical significance: {pattern.get('philosophical_significance', 'N/A')}"
            )
            print(f"  First occurrence: {pattern.get('first_occurrence', 'N/A')}")
            print(f"  Last occurrence: {pattern.get('last_occurrence', 'N/A')}")

    except Exception as e:
        log_message("Error analyzing patterns", str(e))
        traceback.print_exc()


def find_philosophy_resonance(writer):
    """
    Find moments of philosophical resonance between users.

    Args:
        writer: Neo4j Writer
    """
    log_message("FINDING PHILOSOPHICAL RESONANCE MOMENTS")

    try:
        # Find resonance moments
        resonance_moments = writer.find_resonance_moments(hours=24)
        log_message(f"Found {len(resonance_moments)} resonance moments")

        for i, moment in enumerate(resonance_moments[:5], 1):
            log_message(
                f"Resonance #{i}: {moment.get('user1')} and {moment.get('user2')}",
                f"State: {moment.get('to_state')}, Type: {moment.get('resonance_type')}",
            )
            print(
                f"  Philosophical meaning: {moment.get('philosophical_meaning', 'N/A')}"
            )
            print(
                f"  Time difference: {moment.get('time_diff_seconds', 'N/A')} seconds"
            )
            print(f"  Timestamp 1: {moment.get('timestamp1', 'N/A')}")
            print(f"  Timestamp 2: {moment.get('timestamp2', 'N/A')}")

    except Exception as e:
        log_message("Error finding resonance", str(e))
        traceback.print_exc()


def get_philosophy_timeline(writer):
    """
    Get philosophical consciousness timeline.

    Args:
        writer: Neo4j Writer
    """
    log_message("PHILOSOPHICAL CONSCIOUSNESS TIMELINE")

    try:
        # Get timeline
        timeline = writer.get_consciousness_timeline(hours=24)
        nodes = timeline.get("nodes", [])
        links = timeline.get("links", [])

        log_message(
            f"Timeline contains {len(nodes)} states and {len(links)} transitions"
        )

        if nodes and links:
            log_message("Example transitions:", "")
            for link in links[:3]:
                source = link.get("source", "N/A")
                target = link.get("target", "N/A")
                source_state = next(
                    (n.get("state", "unknown") for n in nodes if n.get("id") == source),
                    "unknown",
                )
                target_state = next(
                    (n.get("state", "unknown") for n in nodes if n.get("id") == target),
                    "unknown",
                )

                print(f"  {source_state} -> {target_state}")
                print(f"    Trigger: {link.get('trigger', 'N/A')}")
                print(
                    f"    Philosophical significance: {link.get('philosophical_significance', 'N/A')}"
                )
                print(f"    Time: {link.get('timestamp', 'N/A')}")

            log_message("Example states:", "")
            for node in nodes[:3]:
                print(f"  {node.get('state', 'N/A')} ({node.get('label', 'N/A')})")
                print(f"    Presence: {node.get('presenceLevel', 0):.2f}")
                print(f"    Harmony: {node.get('harmonyIndex', 0):.2f}")
                print(f"    Authenticity: {node.get('authenticityScore', 0):.2f}")
                print(f"    Home resonance: {node.get('homeResonance', 0):.2f}")
                print(f"    Question clarity: {node.get('questionClarity', 0):.2f}")
        else:
            log_message("Timeline is empty", "No states or transitions found")

    except Exception as e:
        log_message("Error getting timeline", str(e))
        traceback.print_exc()


def check_websocket_relay():
    """Check WebSocket relay server availability"""
    log_message("CHECKING WEBSOCKET RELAY")

    try:
        # Check API endpoint
        response = requests.get("http://localhost:8080/api/consciousness/graph")

        if response.status_code == 200:
            log_message("WebSocket API is available", "Retrieved philosophical graph")
            try:
                data = response.json()
                print(f"  Nodes: {len(data.get('nodes', []))}")
                print(f"  Links: {len(data.get('links', []))}")
                print(json.dumps(data, indent=2, ensure_ascii=True)[:500] + "...")
            except:
                print("  Error parsing JSON")
                print(response.text[:200])
        else:
            log_message("API request error", f"Response code: {response.status_code}")

        # Check events endpoint
        test_event = {
            "event_type": "test_event",
            "timestamp": datetime.now().isoformat(),
            "message": "Philosophy First WebSocket test",
        }

        response = requests.post(
            "http://localhost:8080/events",
            json=test_event,
            headers={"Content-Type": "application/json"},
        )

        if response.status_code == 202:
            log_message(
                "Events endpoint is available", f"Response code: {response.status_code}"
            )
        else:
            log_message(
                "Error sending test event", f"Response code: {response.status_code}"
            )

    except Exception as e:
        log_message("Error connecting to WebSocket relay", str(e))
        print("Make sure Go WebSocket relay is running on port 8080")


def main():
    """Main function"""
    print("=== Philosophy First - Neo4j Temporal Data Lake Integration ===")

    parser = argparse.ArgumentParser(description="Philosophy First Neo4j Integration")
    parser.add_argument(
        "--uri", type=str, default="bolt://localhost:7687", help="Neo4j URI"
    )
    parser.add_argument("--user", type=str, default="neo4j", help="Neo4j user")
    parser.add_argument(
        "--password", type=str, default="NewStrongPass123!", help="Neo4j password"
    )
    parser.add_argument(
        "--create", action="store_true", help="Create philosophical transitions"
    )
    parser.add_argument(
        "--broadcast", action="store_true", help="Broadcast transitions to WebSocket"
    )
    parser.add_argument(
        "--analyze", action="store_true", help="Analyze philosophical patterns"
    )
    parser.add_argument(
        "--resonance", action="store_true", help="Find resonance moments"
    )
    parser.add_argument(
        "--timeline", action="store_true", help="Get consciousness timeline"
    )
    parser.add_argument("--check", action="store_true", help="Check WebSocket relay")
    parser.add_argument("--all", action="store_true", help="Run all operations")
    args = parser.parse_args()

    # Check WebSocket relay if requested
    if args.check or args.all:
        check_websocket_relay()

    # Initialize Neo4j writer
    try:
        log_message("CONNECTING TO NEO4J", f"URI: {args.uri}")
        writer = PhilosophyNeo4jWriter(
            uri=args.uri, user=args.user, password=args.password
        )
        log_message("Neo4j connected successfully")
    except Exception as e:
        log_message("Error connecting to Neo4j", str(e))
        traceback.print_exc()
        sys.exit(1)

    try:
        transition_ids = []

        # Create philosophical transitions
        if args.create or args.all:
            transition_ids = create_philosophical_transitions(writer)

        # Broadcast transitions via WebSocket
        if transition_ids and (args.broadcast or args.all):
            broadcast_transitions_to_websocket(writer, transition_ids)

        # Analyze philosophical patterns
        if args.analyze or args.all:
            analyze_philosophy_patterns(writer)

        # Find resonance moments
        if args.resonance or args.all:
            find_philosophy_resonance(writer)

        # Get timeline
        if args.timeline or args.all:
            get_philosophy_timeline(writer)

    finally:
        # Close Neo4j connection
        writer.close()
        log_message("Neo4j connection closed")


if __name__ == "__main__":
    main()
