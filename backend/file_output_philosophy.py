#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Philosophy First - File Output Diagnostics
-----------------------------------------
Full diagnostics with output to file for Windows compatibility
"""

import json
import os
import random
import sys
import time
import traceback
import uuid
from datetime import datetime

import requests

from neo4j import GraphDatabase

# Create output file
OUTPUT_FILE = os.path.join(
    os.path.dirname(os.path.dirname(__file__)), "philosophy_diagnostics.log"
)


def log_to_file(message, append=True):
    """Write message to file"""
    mode = "a" if append else "w"
    with open(OUTPUT_FILE, mode, encoding="utf-8") as f:
        f.write(f"{message}\n")


# Initialize log file
log_to_file("=== PHILOSOPHY FIRST TEMPORAL DATA LAKE DIAGNOSTICS ===\n", append=False)
log_to_file(f"Started: {datetime.now().isoformat()}")
log_to_file(f"Output file: {OUTPUT_FILE}")


def check_neo4j_connection(
    uri="bolt://localhost:7687", user="neo4j", password="NewStrongPass123!"
):
    """Test Neo4j connection and database state"""

    log_to_file("\n=== NEO4J CONNECTION TEST ===")
    log_to_file(f"Connecting to: {uri}")

    try:
        driver = GraphDatabase.driver(uri, auth=(user, password))
        with driver.session() as session:
            # Test connection
            result = session.run("RETURN 'Connection successful' as message").single()
            log_to_file(f"Connection test: {result['message']}")

            # Neo4j version
            result = session.run(
                "CALL dbms.components() YIELD name, versions RETURN name, versions"
            ).single()
            if result:
                log_to_file(f"Neo4j version: {result['name']} {result['versions'][0]}")

            # Count nodes
            result = session.run("MATCH (n) RETURN count(n) as node_count").single()
            log_to_file(f"Total nodes in database: {result['node_count']}")

            # Count consciousness nodes
            result = session.run(
                "MATCH (n:ConsciousnessNode) RETURN count(n) as count"
            ).single()
            log_to_file(f"ConsciousnessNode count: {result['count']}")

            # Count transitions
            result = session.run(
                "MATCH ()-[r:TRANSITIONS_TO]->() RETURN count(r) as count"
            ).single()
            log_to_file(f"State transitions count: {result['count']}")

            # Check consciousness states
            log_to_file("\n=== CONSCIOUSNESS STATES COUNT ===")
            result = session.run(
                """
                MATCH (n:ConsciousnessNode) 
                RETURN n.state as state, count(n) as count 
                ORDER BY count DESC
            """
            )
            for record in result:
                log_to_file(f"State: {record['state']} - Count: {record['count']}")

            # Sample transitions
            log_to_file("\n=== SAMPLE TRANSITIONS ===")
            result = session.run(
                """
                MATCH (source:ConsciousnessNode)-[t:TRANSITIONS_TO]->(target:ConsciousnessNode)
                RETURN source.state as source_state, 
                       target.state as target_state,
                       t.trigger as trigger,
                       t.timestamp as timestamp,
                       t.philosophical_significance as significance
                LIMIT 5
            """
            )
            for i, record in enumerate(result):
                log_to_file(f"\nTransition #{i+1}:")
                log_to_file(
                    f"  From: {record['source_state']} To: {record['target_state']}"
                )
                log_to_file(f"  Trigger: {record['trigger']}")
                log_to_file(f"  Time: {record['timestamp']}")
                log_to_file(f"  Significance: {record['significance']}")

        driver.close()
        log_to_file("\nNeo4j connection test complete!")
        return True

    except Exception as e:
        log_to_file(f"\nERROR: Failed to connect to Neo4j: {str(e)}")
        traceback.print_exc(file=open(OUTPUT_FILE, "a"))
        return False


def check_resonance_moments(
    uri="bolt://localhost:7687", user="neo4j", password="NewStrongPass123!"
):
    """Find resonance moments between users"""

    log_to_file("\n=== RESONANCE MOMENTS ANALYSIS ===")

    try:
        driver = GraphDatabase.driver(uri, auth=(user, password))
        with driver.session() as session:
            # Find cases where different users transitioned to the same state within a short time
            query = """
            MATCH (n1:ConsciousnessNode)-[:TRANSITIONS_TO]->(m1:ConsciousnessNode)
            MATCH (n2:ConsciousnessNode)-[:TRANSITIONS_TO]->(m2:ConsciousnessNode)
            WHERE n1.user_id <> n2.user_id
            AND m1.state = m2.state
            AND abs(datetime(m1.timestamp).epochMillis - datetime(m2.timestamp).epochMillis) < 5000
            RETURN n1.user_id as user1, n2.user_id as user2, 
                  m1.state as state, m1.timestamp as time1, m2.timestamp as time2,
                  abs(datetime(m1.timestamp).epochMillis - datetime(m2.timestamp).epochMillis) as time_diff_ms
            ORDER BY time_diff_ms ASC
            LIMIT 10
            """

            result = session.run(query)
            records = list(result)

            log_to_file(f"Found {len(records)} resonance moments")

            for i, record in enumerate(records):
                log_to_file(f"\nResonance #{i+1}:")
                log_to_file(f"  Users: {record['user1']} and {record['user2']}")
                log_to_file(f"  State: {record['state']}")
                log_to_file(f"  Time difference: {record['time_diff_ms']} ms")
                log_to_file(f"  Time 1: {record['time1']}")
                log_to_file(f"  Time 2: {record['time2']}")

        driver.close()
        return True

    except Exception as e:
        log_to_file(f"\nERROR: Failed to analyze resonance moments: {str(e)}")
        traceback.print_exc(file=open(OUTPUT_FILE, "a"))
        return False


def get_user_timeline(
    user_id, uri="bolt://localhost:7687", user="neo4j", password="NewStrongPass123!"
):
    """Get timeline for a specific user"""

    log_to_file(f"\n=== USER TIMELINE: {user_id} ===")

    try:
        driver = GraphDatabase.driver(uri, auth=(user, password))
        with driver.session() as session:
            query = """
            MATCH (source:ConsciousnessNode)-[t:TRANSITIONS_TO]->(target:ConsciousnessNode)
            WHERE source.user_id = $user_id
            RETURN source.state as source_state, 
                  target.state as target_state,
                  t.trigger as trigger,
                  t.philosophical_significance as significance,
                  t.timestamp as timestamp
            ORDER BY t.timestamp ASC
            """

            result = session.run(query, user_id=user_id)
            records = list(result)

            log_to_file(f"Found {len(records)} transitions for user {user_id}")

            for i, record in enumerate(records):
                log_to_file(f"\nTransition #{i+1}:")
                log_to_file(f"  {record['source_state']} → {record['target_state']}")
                log_to_file(f"  Trigger: {record['trigger']}")
                log_to_file(f"  Time: {record['timestamp']}")
                log_to_file(f"  Significance: {record['significance']}")

        driver.close()
        return True

    except Exception as e:
        log_to_file(f"\nERROR: Failed to get user timeline: {str(e)}")
        traceback.print_exc(file=open(OUTPUT_FILE, "a"))
        return False


def check_websocket_relay():
    """Check WebSocket relay server availability"""

    log_to_file("\n=== WEBSOCKET RELAY TEST ===")

    try:
        # Check API endpoint
        response = requests.get("http://localhost:8080/api/consciousness/graph")

        if response.status_code == 200:
            log_to_file("WebSocket API is available")
            try:
                data = response.json()
                log_to_file(f"  Nodes: {len(data.get('nodes', []))}")
                log_to_file(f"  Links: {len(data.get('links', []))}")
            except:
                log_to_file("  Error parsing JSON")
        else:
            log_to_file(f"API request error: {response.status_code}")

        # Test event endpoint
        test_event = {
            "event_type": "philosophy_test_event",
            "timestamp": datetime.now().isoformat(),
            "message": "Philosophy First WebSocket test",
            "source_state": "REFLECTION_SELF",
            "target_state": "HOME_AUTHENTIC",
        }

        response = requests.post(
            "http://localhost:8080/events",
            json=test_event,
            headers={"Content-Type": "application/json"},
        )

        if response.status_code == 202:
            log_to_file(f"Event endpoint is available: Status {response.status_code}")
            log_to_file("Test event sent successfully!")
        else:
            log_to_file(f"Event endpoint error: Status {response.status_code}")

    except Exception as e:
        log_to_file(f"ERROR: Failed to connect to WebSocket relay: {str(e)}")
        traceback.print_exc(file=open(OUTPUT_FILE, "a"))


def main():
    """Main diagnostics function"""
    try:
        log_to_file("\n--- RUNNING COMPLETE DIAGNOSTICS ---")

        # Check Neo4j
        check_neo4j_connection()

        # Check resonance moments
        check_resonance_moments()

        # Check user timelines
        get_user_timeline("philosopher_1")
        get_user_timeline("philosopher_2")

        # Check WebSocket relay
        check_websocket_relay()

        log_to_file("\n=== DIAGNOSTICS COMPLETE ===")
        log_to_file(f"Finished: {datetime.now().isoformat()}")
        log_to_file(f"Results saved to: {OUTPUT_FILE}")

        # Print completion message to console
        print(f"Diagnostics complete! Results saved to: {OUTPUT_FILE}")

    except Exception as e:
        log_to_file(f"\nFATAL ERROR: {str(e)}")
        traceback.print_exc(file=open(OUTPUT_FILE, "a"))


if __name__ == "__main__":
    main()
