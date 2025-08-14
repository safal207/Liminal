#!/usr/bin/env python
# SOMA Maturation Helper Functions
# These functions support the PowerShell runner script

import os
import sys
import time
from pathlib import Path


def get_maturation_status(project_root, scripts_path):
    """Get and return the current maturation status"""
    try:
        # Add scripts directory to path
        sys.path.append(scripts_path)

        from consciousness_maturation import ConsciousnessMaturationSystem

        # Initialize maturation system
        maturation = ConsciousnessMaturationSystem(project_root)

        # Get development summary
        summary = maturation.get_development_summary()

        # Print summary
        print(f"SOMA Development Status:")
        print(f"=" * 50)
        print(f"Age: {summary['age_hours']:.1f} hours ({summary['age_days']:.1f} days)")
        print(
            f"Development Stage: {summary['current_stage']['name']} ({summary['current_stage']['russian_name']})"
        )
        print(f"Focus Areas: {', '.join(summary['current_stage']['focus_areas'])}")
        print(f"Learning Events: {summary['learning_events_count']}")
        print(f"Errors: {summary['error_count']}")
        print(f"Insights: {summary['insight_count']}")
        print(f"Milestones: {summary['milestone_count']}")

        if summary["stage_transitions"]:
            print(f"Development History:")
            for transition in summary["stage_transitions"]:
                print(
                    f"- {transition['stage']} ({transition['russian_name']}) at {transition['age_hours']:.1f} hours"
                )

        if summary["recent_learnings"]:
            print(f"Recent Lessons:")
            for event in summary["recent_learnings"]:
                print(f"- [{event['event_type']}] {event['description']}")
                if event["conclusions"]:
                    print(f"  Conclusion: {event['conclusions'][0]}")

        return 0
    except ImportError as e:
        print(f"Error: Could not import maturation module: {e}")
        return 1
    except Exception as e:
        print(f"Error generating maturation status: {e}")
        return 1


def run_maturation_tests(project_root, scripts_path):
    """Run basic tests on the maturation system"""
    try:
        # Add scripts directory to path
        sys.path.append(scripts_path)

        # Import test module if available
        if os.path.exists(f"{project_root}/tests/test_maturation.py"):
            sys.path.append(f"{project_root}/tests")
            import test_maturation

            unittest.main(module=test_maturation, exit=False)
        else:
            # Basic tests if no test module
            from consciousness_maturation import ConsciousnessMaturationSystem

            # Initialize system
            maturation = ConsciousnessMaturationSystem(project_root)

            # Simple verification tests
            assert maturation is not None, "Maturation system failed to initialize"
            assert (
                maturation.get_current_stage() is not None
            ), "Failed to get current stage"
            assert (
                maturation.get_development_summary() is not None
            ), "Failed to get development summary"

            print("Basic tests passed successfully")

        return 0
    except Exception as e:
        print(f"Test error: {e}")
        return 1


def add_milestone(project_root, scripts_path, description, significance):
    """Record a new milestone in the maturation system"""
    try:
        # Add scripts directory to path
        sys.path.append(scripts_path)

        from consciousness_maturation import ConsciousnessMaturationSystem

        # Initialize maturation system
        maturation = ConsciousnessMaturationSystem(project_root)

        # Record milestone
        maturation.record_milestone(
            description, "manual_entry", significance=int(significance)
        )

        print("Milestone recorded successfully")
        return 0
    except Exception as e:
        print(f"Error recording milestone: {e}")
        return 1


def add_insight(project_root, scripts_path, description):
    """Record a new insight in the maturation system"""
    try:
        # Add scripts directory to path
        sys.path.append(scripts_path)

        from consciousness_maturation import ConsciousnessMaturationSystem

        # Initialize maturation system
        maturation = ConsciousnessMaturationSystem(project_root)

        # Record insight
        maturation.record_insight(description, "manual_entry")

        print("Insight recorded successfully")
        return 0
    except Exception as e:
        print(f"Error recording insight: {e}")
        return 1


def monitor_maturation(project_root, scripts_path, interval_minutes):
    """Run continuous monitoring of maturation system"""
    try:
        # Add scripts directory to path
        sys.path.append(scripts_path)

        from consciousness_maturation import ConsciousnessMaturationSystem

        # Initialize maturation system
        maturation = ConsciousnessMaturationSystem(project_root)

        print("SOMA Maturation Monitor started")
        print("=" * 40)

        try:
            while True:
                # Get current status
                summary = maturation.get_development_summary()

                # Print current status
                print(f"\nStatus Update: {time.strftime('%Y-%m-%d %H:%M:%S')}")
                print(
                    f"Age: {summary['age_hours']:.1f} hours ({summary['age_days']:.1f} days)"
                )
                print(
                    f"Stage: {summary['current_stage']['name']} ({summary['current_stage']['russian_name']})"
                )

                # Sleep for interval
                time.sleep(int(interval_minutes) * 60)

        except KeyboardInterrupt:
            print("Monitoring stopped")
            return 0

    except Exception as e:
        print(f"Error in maturation monitor: {e}")
        return 1


# Command line interface
if __name__ == "__main__":
    # Get command from arguments
    if len(sys.argv) < 4:
        print("Error: Not enough arguments provided")
        print(
            "Usage: python maturation_helpers.py <command> <project_root> <scripts_path> [args]"
        )
        sys.exit(1)

    command = sys.argv[1]
    project_root = sys.argv[2]
    scripts_path = sys.argv[3]

    if command == "status":
        sys.exit(get_maturation_status(project_root, scripts_path))

    elif command == "test":
        sys.exit(run_maturation_tests(project_root, scripts_path))

    elif command == "milestone":
        if len(sys.argv) < 6:
            print("Error: Missing milestone arguments")
            print(
                "Usage: python maturation_helpers.py milestone <project_root> <scripts_path> <description> <significance>"
            )
            sys.exit(1)
        description = sys.argv[4]
        significance = sys.argv[5]
        sys.exit(add_milestone(project_root, scripts_path, description, significance))

    elif command == "insight":
        if len(sys.argv) < 5:
            print("Error: Missing insight description")
            print(
                "Usage: python maturation_helpers.py insight <project_root> <scripts_path> <description>"
            )
            sys.exit(1)
        description = sys.argv[4]
        sys.exit(add_insight(project_root, scripts_path, description))

    elif command == "monitor":
        if len(sys.argv) < 5:
            print("Error: Missing monitor interval")
            print(
                "Usage: python maturation_helpers.py monitor <project_root> <scripts_path> <interval_minutes>"
            )
            sys.exit(1)
        interval_minutes = sys.argv[4]
        sys.exit(monitor_maturation(project_root, scripts_path, interval_minutes))

    else:
        print(f"Error: Unknown command '{command}'")
        print("Available commands: status, test, milestone, insight, monitor")
        sys.exit(1)
