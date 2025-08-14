#!/usr/bin/env python
"""
Debug script to diagnose stage_transitions issues in ConsciousnessMaturationSystem
"""

import inspect
import os
import sys
from pathlib import Path

# Print current working directory and script location
print(f"Working directory: {os.getcwd()}")
script_path = Path(__file__).resolve()
print(f"Script path: {script_path}")
scripts_dir = script_path.parent
print(f"Scripts directory: {scripts_dir}")

# Add scripts directory to path
if str(scripts_dir) not in sys.path:
    sys.path.insert(0, str(scripts_dir))
    print(f"Added {scripts_dir} to sys.path")

# Try to import consciousness_maturation
print("\nImporting consciousness_maturation module...")
try:
    import consciousness_maturation

    print(f"Module location: {inspect.getfile(consciousness_maturation)}")
    print(f"Module content:\n{dir(consciousness_maturation)}")

    # Import ConsciousnessMaturationSystem class
    from consciousness_maturation import ConsciousnessMaturationSystem

    print(
        f"Class definition location: {inspect.getfile(ConsciousnessMaturationSystem)}"
    )

    # Check class attributes
    print("\nClass attributes:")
    print(
        f"{[attr for attr in dir(ConsciousnessMaturationSystem) if not attr.startswith('__')]}"
    )

    # Initialize system
    print("\nInitializing maturation system...")
    project_root = str(scripts_dir.parent)
    maturation = ConsciousnessMaturationSystem(project_root)

    # Check instance attributes
    print("\nInstance attributes:")
    instance_attrs = [attr for attr in dir(maturation) if not attr.startswith("__")]
    print(f"{instance_attrs}")

    # Check if stage_transitions attribute exists
    if hasattr(maturation, "stage_transitions"):
        print("\nstage_transitions attribute exists!")
        print(f"Type: {type(maturation.stage_transitions)}")
        print(f"Value: {maturation.stage_transitions}")
    else:
        print("\nWARNING: stage_transitions attribute does NOT exist!")

    # Try to call get_development_summary
    print("\nCalling get_development_summary()...")
    try:
        summary = maturation.get_development_summary()
        print("Summary keys:")
        print(f"{list(summary.keys())}")
        if "stage_transitions" in summary:
            print("stage_transitions exists in summary!")
        else:
            print("WARNING: stage_transitions NOT found in summary!")
    except Exception as e:
        print(f"Error calling get_development_summary(): {e}")

except ImportError as e:
    print(f"Import error: {e}")
except Exception as e:
    print(f"Error: {e}")
    import traceback

    traceback.print_exc()
