#!/usr/bin/env python
"""
Simplified diagnostic script for SOMA maturation system
"""
import os
import sys
import traceback
from pathlib import Path

# Current script path
script_dir = Path(__file__).parent
sys.path.append(str(script_dir))

try:
    # Import the class
    print("Importing ConsciousnessMaturationSystem...")
    from consciousness_maturation import ConsciousnessMaturationSystem

    # Initialize system
    print("Initializing system...")
    project_root = str(script_dir.parent)
    system = ConsciousnessMaturationSystem(project_root)

    # Check attributes directly
    print("\n--- ATTRIBUTE CHECK ---")
    has_attr = hasattr(system, "stage_transitions")
    print(f"Has stage_transitions attribute: {has_attr}")

    # Try to access specific methods
    print("\n--- METHOD EXECUTION ---")
    print("Calling get_development_summary()...")
    summary = system.get_development_summary()
    print(f"Summary keys: {list(summary.keys())}")

    # Print specific information about the errors
    print("\n--- ERROR DIAGNOSIS ---")
    print("Checking for transitions reference in calculate_current_stage...")
    source_code = inspect.getsource(system.calculate_current_stage)
    print(f"Uses stage_transitions: {'stage_transitions' in source_code}")

except Exception as e:
    print(f"\nERROR: {type(e).__name__}: {e}")
    print("Traceback:")
    traceback.print_exc()
