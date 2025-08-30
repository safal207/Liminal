#!/usr/bin/env python3

"""
Neo4j Temporal Data Lake Docker Test
-----------------------------------
Philosophy First approach for consciousness states analysis
"""

import argparse
import io
import json
import sys
import traceback
import uuid
from datetime import datetime

# Set UTF-8 encoding for stdout
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8")

# Import Neo4j components
try:
    from consciousness_neo4j import (
        ConsciousnessNeo4jWriter,
    )
    from consciousness_schema import (
        ConsciousnessNode,
        ConsciousnessState,
        StateTransition,
        TransitionTrigger,
    )
except ImportError as e:
    print(f"Error importing modules: {e}")
    print("Make sure all required modules are installed")
    sys.exit(1)


def log_message(title, content=None, style="info"):
    """Simple log message without unicode styling issues"""
    print(f"\n== {title} ==")
    if content:
        if isinstance(content, dict | list):
            try:
                print(json.dumps(content, indent=2, ensure_ascii=False))
            except:
                print(str(content))
        else:
            print(str(content))


def simulate_transition_example(writer):
    """Create example transitions for testing"""
    log_message("SIMULATING CONSCIOUSNESS TRANSITIONS")

    # Generate philosophical consciousness states and transitions
    state_transitions = [
        (
            "TRANSITION_LIMINAL",
            "PRESENCE_NOW",
            "Transition to full presence in the moment",
        ),
        ("PRESENCE_NOW", "HARMONY_BALANCE", "Finding harmony through meditation"),
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
        ("QUESTION_SPACE", "RESONANCE_MOMENT", "Moment of consciousness resonance"),
    ]

    # Create transitions with different users for resonance testing
    users = [
        "philosopher_1",
        "philosopher_2",
        "consciousness_explorer",
        "authentic_self",
    ]

    for _i, (source_state_name, target_state_name, description) in enumerate(state_transitions):
        # Map string names to enum values
        try:
            source_state_enum = getattr(ConsciousnessState, source_state_name)
            target_state_enum = getattr(ConsciousnessState, target_state_name)
        except AttributeError:
            log_message(
                "Invalid state name",
                f"Source: {source_state_name}, Target: {target_state_name}",
                "error",
            )
            continue

        try:
            # Create for multiple users with slight time variations for resonance detection
            for user_id in users:
                # Create source state
                current_state = ConsciousnessNode(
                    id=str(uuid.uuid4()),
                    state=source_state_enum,
                    timestamp=datetime.now(),
                    home_resonance=0.5,
                    presence_level=0.7,
                    authenticity_score=0.6,
                    harmony_index=0.5,
                    user_id=user_id,
                )

                # Create target state with increased philosophical metrics
                target_state = ConsciousnessNode(
                    id=str(uuid.uuid4()),
                    state=target_state_enum,
                    timestamp=datetime.now(),
                    home_resonance=0.7,
                    presence_level=0.8,
                    authenticity_score=0.7,
                    harmony_index=0.6,
                    user_id=user_id,
                )

                # Create transition between states
                transition = StateTransition(
                    id=str(uuid.uuid4()),
                    source_id=current_state.id,
                    target_id=target_state.id,
                    trigger=TransitionTrigger.PHILOSOPHICAL_INSIGHT,
                    timestamp=datetime.now(),
                    presence_delta=0.1,
                    harmony_delta=0.1,
                    authenticity_delta=0.1,
                    philosophical_significance=description,
                    user_id=user_id,
                )

                # Save to Neo4j
                writer.create_consciousness_node(current_state)
                writer.create_consciousness_node(target_state)
                writer.create_state_transition(transition)

                log_message(
                    f"Created transition for {user_id}",
                    f"{source_state_name} → {target_state_name}",
                )
                print(f"  Trigger: {transition.trigger.value}")
                print(f"  Description: {description}")

        except Exception as e:
            log_message("Error creating transition", str(e), "error")
            traceback.print_exc()


def analyze_temporal_patterns(writer, days=7):
    """Analyze temporal patterns in consciousness transitions"""
    log_message("ANALYZING TEMPORAL PATTERNS", f"Over the last {days} days")

    patterns = writer.analyze_temporal_patterns(days=days)

    if not patterns:
        log_message("No patterns found", "Not enough transition data in database")
        return

    log_message(f"Found {len(patterns)} patterns")

    for i, pattern in enumerate(patterns[:5], 1):
        log_message(
            f"Pattern #{i}: {pattern['source']} → {pattern['target']}",
            f"Frequency: {pattern['frequency']}, Type: {pattern['pattern_type']}",
        )
        print(f"  Philosophical significance: {pattern.get('philosophical_significance', 'N/A')}")
        print(f"  Median time: {pattern.get('median_hour', 'N/A')} hours")
        print(f"  First occurrence: {pattern.get('first_occurrence', 'N/A')}")
        print(f"  Last occurrence: {pattern.get('last_occurrence', 'N/A')}")


def find_resonance_moments(writer, hours=24):
    """Find moments of resonance between users"""
    log_message("FINDING CONSCIOUSNESS RESONANCE MOMENTS", f"Over the last {hours} hours")

    resonance_moments = writer.find_resonance_moments(hours=hours)

    if not resonance_moments:
        log_message("No resonance moments found", "Not enough data on synchronous transitions")
        return

    log_message(f"Found {len(resonance_moments)} resonance moments")

    for i, moment in enumerate(resonance_moments[:5], 1):
        log_message(
            f"Resonance #{i}: {moment['user1']} and {moment['user2']}",
            f"State: {moment['to_state']}, Type: {moment['resonance_type']}",
        )
        print(f"  Philosophical meaning: {moment.get('philosophical_meaning', 'N/A')}")
        print(f"  Time difference: {moment.get('time_diff_seconds', 'N/A')} seconds")
        print(f"  Timestamp 1: {moment.get('timestamp1', 'N/A')}")
        print(f"  Timestamp 2: {moment.get('timestamp2', 'N/A')}")


def get_consciousness_timeline(writer, hours=24, user_id=None):
    """Get and analyze consciousness timeline"""
    log_message("CONSCIOUSNESS TIMELINE", f"Over the last {hours} hours")

    timeline = writer.get_consciousness_timeline(hours=hours, user_id=user_id)

    if not timeline or not timeline.get("nodes") or not timeline.get("links"):
        log_message("Timeline is empty", "Not enough consciousness state data")
        return

    nodes = timeline.get("nodes", [])
    links = timeline.get("links", [])

    log_message(f"Timeline contains {len(nodes)} states and {len(links)} transitions")

    log_message("Example transitions:", "")
    for link in links[:3]:
        source_id = link.get("source", "N/A")
        target_id = link.get("target", "N/A")
        source_state = next(
            (n.get("state", "unknown") for n in nodes if n.get("id") == source_id),
            "unknown",
        )
        target_state = next(
            (n.get("state", "unknown") for n in nodes if n.get("id") == target_id),
            "unknown",
        )

        print(f"  {source_state} → {target_state}")
        print(f"    Trigger: {link.get('trigger', 'N/A')}")
        print(f"    Philosophical significance: {link.get('philosophical_significance', 'N/A')}")
        print(f"    Time: {link.get('timestamp', 'N/A')}")

    log_message("Example states:", "")
    for node in nodes[:3]:
        print(f"  {node.get('state', 'N/A')} ({node.get('label', 'N/A')})")
        print(f"    Presence: {node.get('presenceLevel', 0):.2f}")
        print(f"    Harmony: {node.get('harmonyIndex', 0):.2f}")
        print(f"    Authenticity: {node.get('authenticityScore', 0):.2f}")


def broadcast_events_to_websocket(writer, state_transition_id):
    """Broadcast state transitions to WebSocket relay"""
    log_message("BROADCASTING TO WEBSOCKET", f"Transition ID: {state_transition_id}")

    try:
        # Get transition details from Neo4j
        transition_details = writer.get_transition_details(state_transition_id)
        if not transition_details:
            log_message("Transition not found", f"ID: {state_transition_id}", "error")
            return False

        # Create event for WebSocket broadcasting
        event = {
            "event_type": "consciousness_transition",
            "timestamp": datetime.now().isoformat(),
            "user_id": transition_details.get("user_id", "unknown"),
            "source_state": transition_details.get("source_state", "unknown"),
            "target_state": transition_details.get("target_state", "unknown"),
            "meta": {
                "description": transition_details.get("philosophical_significance", ""),
                "philosophical": True,
                "presence_delta": transition_details.get("presence_delta", 0),
                "harmony_delta": transition_details.get("harmony_delta", 0),
                "authenticity_delta": transition_details.get("authenticity_delta", 0),
            },
        }

        # Send to WebSocket relay via HTTP
        import requests

        response = requests.post(
            "http://localhost:8080/events",
            json=event,
            headers={"Content-Type": "application/json"},
        )

        if response.status_code == 202:  # Accepted
            log_message(
                "Event sent to WebSocket",
                f"Response: {response.status_code}",
                "success",
            )
            return True
        else:
            log_message(
                "Error sending to WebSocket",
                f"Status code: {response.status_code}",
                "error",
            )
            print(f"Response: {response.text}")
            return False

    except Exception as e:
        log_message("Error broadcasting event", str(e), "error")
        traceback.print_exc()
        return False


def main():
    print("Starting Neo4j Temporal Data Lake Docker Test...")
    parser = argparse.ArgumentParser(description="Neo4j Temporal Data Lake Docker Test")
    parser.add_argument("--days", type=int, default=7, help="Analysis period in days")
    parser.add_argument("--hours", type=int, default=24, help="Analysis period in hours")
    parser.add_argument("--user", type=str, help="User ID for filtering")
    parser.add_argument("--simulate", action="store_true", help="Create example transitions")
    parser.add_argument("--analyze", action="store_true", help="Analyze temporal patterns")
    parser.add_argument("--resonance", action="store_true", help="Find resonance moments")
    parser.add_argument("--timeline", action="store_true", help="Get consciousness timeline")
    parser.add_argument("--broadcast", action="store_true", help="Broadcast to WebSocket")
    parser.add_argument("--all", action="store_true", help="Run all functions")
    parser.add_argument("--docker", action="store_true", default=True, help="Use Docker Neo4j")
    parser.add_argument("--uri", type=str, default="bolt://localhost:7687", help="Neo4j URI")
    args = parser.parse_args()
    print(f"Arguments: {vars(args)}")

    # Initialize Neo4j Writer
    try:
        print("Connecting to Neo4j...")
        print(f"URI: {args.uri}")

        # Use explicit connection parameters for Docker
        if args.docker:
            # Settings for Docker container connection
            writer = ConsciousnessNeo4jWriter(
                uri=args.uri, user="neo4j", password="NewStrongPass123!"
            )
            print("Using Docker Neo4j container")
        else:
            # Default settings from environment variables
            writer = ConsciousnessNeo4jWriter()
            print("Using default settings")

        print("Neo4j Writer created")
        log_message("Neo4j connection", "Established successfully")
    except Exception as e:
        print(f"ERROR: {e}")
        log_message("Neo4j connection error", str(e), "error")
        print("\nCheck that Neo4j Docker container is running:")
        print("docker ps | findstr neo4j")
        traceback.print_exc()
        sys.exit(1)

    try:
        if args.simulate or args.all:
            simulate_transition_example(writer)

        if args.analyze or args.all:
            analyze_temporal_patterns(writer, days=args.days)

        if args.resonance or args.all:
            find_resonance_moments(writer, hours=args.hours)

        if args.timeline or args.all:
            get_consciousness_timeline(writer, hours=args.hours, user_id=args.user)

        if args.broadcast or args.all:
            # Get last created transition and broadcast it
            last_transition = writer.get_last_transition()
            if last_transition:
                log_message(
                    "Broadcasting last transition",
                    f"ID: {last_transition.get('id', 'unknown')}",
                )
                broadcast_events_to_websocket(writer, last_transition.get("id"))
            else:
                log_message(
                    "No transitions found for broadcasting",
                    "Create transitions first",
                    "warning",
                )

    finally:
        writer.close()
        log_message("Neo4j connection", "Closed")


if __name__ == "__main__":
    main()
