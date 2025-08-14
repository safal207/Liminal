#!/usr/bin/env python3
"""
Utility script to generate sample test data for load testing.
"""
import json
import os
import random
import uuid
from datetime import datetime, timedelta
from pathlib import Path
from typing import Any, Dict, List

# Sample data for generating realistic test data
MEMORY_TYPES = ["event", "fact", "preference", "reminder"]
MEMORY_CATEGORIES = ["work", "personal", "shopping", "health", "finance"]
MEMORY_TAGS = ["urgent", "important", "someday", "reference"]
MEMORY_CONTENT_TEMPLATES = [
    "Remember to {action} for {subject}",
    "{subject} needs to be {action} by {time}",
    "Don't forget about {subject}",
    "{subject} is important because {reason}",
    "Action item: {action} for {subject}",
]


def generate_memory_content() -> Dict[str, Any]:
    """Generate a sample memory content."""
    template = random.choice(MEMORY_CONTENT_TEMPLATES)

    # Generate random values for the template
    substitutions = {
        "action": random.choice(["call", "email", "meet with", "review", "update"]),
        "subject": random.choice(["John", "project X", "the team", "the report"]),
        "time": (datetime.now() + timedelta(days=random.randint(1, 30))).strftime(
            "%Y-%m-%d"
        ),
        "reason": random.choice(
            ["it's due soon", "it's important", "it affects other tasks"]
        ),
    }

    # Apply substitutions to the template
    content = template.format(**substitutions)

    return {
        "id": str(uuid.uuid4()),
        "type": random.choice(MEMORY_TYPES),
        "content": content,
        "timestamp": datetime.utcnow().isoformat() + "Z",
        "metadata": {
            "category": random.choice(MEMORY_CATEGORIES),
            "priority": random.randint(1, 5),
            "tags": random.sample(MEMORY_TAGS, k=random.randint(1, 3)),
            "source": "generated",
            "confidence": round(random.uniform(0.7, 1.0), 2),
        },
    }


def generate_test_data(num_memories: int = 100, output_dir: str = "test_data") -> None:
    """Generate sample test data for load testing."""
    # Create output directory if it doesn't exist
    output_path = Path(output_dir)
    output_path.mkdir(exist_ok=True)

    # Generate memories
    memories = [generate_memory_content() for _ in range(num_memories)]

    # Save to a JSON file
    output_file = output_path / "sample_memories.json"
    with open(output_file, "w") as f:
        json.dump({"memories": memories}, f, indent=2)

    print(f"Generated {num_memories} sample memories in {output_file}")


def main():
    """Main entry point for the test data generator."""
    import argparse

    parser = argparse.ArgumentParser(
        description="Generate sample test data for load testing"
    )
    parser.add_argument(
        "-n",
        "--num-memories",
        type=int,
        default=100,
        help="Number of sample memories to generate (default: 100)",
    )
    parser.add_argument(
        "-o",
        "--output-dir",
        type=str,
        default="test_data",
        help="Output directory for generated data (default: test_data)",
    )

    args = parser.parse_args()
    generate_test_data(args.num_memories, args.output_dir)


if __name__ == "__main__":
    main()
