#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Simple Neo4j Connection Check
----------------------------
Basic diagnostic script to test Neo4j connection and verify schema
"""

import json
import os
import sys

from neo4j import GraphDatabase

# Set up UTF-8 encoding for all IO
sys.stdout.reconfigure(encoding="utf-8")


def check_neo4j_connection(
    uri="bolt://localhost:7687", user="neo4j", password="NewStrongPass123!"
):
    """Test Neo4j connection and run basic queries"""

    print("=== SIMPLE NEO4J CONNECTION TEST ===")
    print(f"Connecting to: {uri}")

    try:
        driver = GraphDatabase.driver(uri, auth=(user, password))
        with driver.session() as session:
            # Test basic connection
            result = session.run("RETURN 'Connection successful' as message").single()
            print(f"Connection test: {result['message']}")

            # Check Neo4j version
            result = session.run(
                "CALL dbms.components() YIELD name, versions RETURN name, versions"
            ).single()
            if result:
                print(f"Neo4j version: {result['name']} {result['versions'][0]}")

            # Check for existing nodes
            result = session.run("MATCH (n) RETURN count(n) as node_count").single()
            print(f"Total nodes in database: {result['node_count']}")

            # Check for consciousness nodes specifically
            result = session.run(
                "MATCH (n:ConsciousnessNode) RETURN count(n) as count"
            ).single()
            print(f"ConsciousnessNode count: {result['count']}")

            # Check for state transitions
            result = session.run(
                "MATCH ()-[r:TRANSITIONS_TO]->() RETURN count(r) as count"
            ).single()
            print(f"State transitions count: {result['count']}")

            # Sample a few nodes if they exist
            print("\n=== SAMPLE NODES ===")
            result = session.run("MATCH (n:ConsciousnessNode) RETURN n LIMIT 3")
            for record in result:
                node = record["n"]
                properties = dict(node.items())
                print(f"Node ID: {properties.get('id', 'N/A')}")
                print(f"State: {properties.get('state', 'N/A')}")
                print(f"User ID: {properties.get('user_id', 'N/A')}")
                print("---")

        driver.close()
        print("\nDiagnostic complete - Neo4j connection successful!")
        return True

    except Exception as e:
        print(f"\nERROR: Failed to connect to Neo4j: {str(e)}")
        return False


if __name__ == "__main__":
    # Write output to both console and file
    with open("neo4j_diagnostics.txt", "w", encoding="utf-8") as f:
        # Save original stdout
        original_stdout = sys.stdout

        # Create a function to write to both console and file
        def write_both(text):
            print(text, end="")
            f.write(text)

        # Create a class that mimics stdout but writes to both
        class DualOutput:
            def write(self, text):
                original_stdout.write(text)
                f.write(text)

            def flush(self):
                original_stdout.flush()
                f.flush()

        # Redirect stdout to our custom class
        sys.stdout = DualOutput()

        # Run the diagnostic
        success = check_neo4j_connection()

        # Restore stdout
        sys.stdout = original_stdout

        print(f"Diagnostics saved to neo4j_diagnostics.txt")
