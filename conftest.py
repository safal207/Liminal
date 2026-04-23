"""Pytest collection guards for generated artifacts and manual smoke scripts."""

from pathlib import Path

collect_ignore = [
    "pytest_log.txt",
    "test_localsystem.txt",
    "test_output.txt",
    "test_output_simple.txt",
    "test_results.txt",
    "backend/test_errors.txt",
]

_BACKEND_ROOT = Path(__file__).parent / "backend"


def pytest_ignore_collect(collection_path, config):
    """Skip root-level backend smoke scripts that are not real pytest modules."""
    path = Path(str(collection_path))

    if path.suffix == ".txt" and path.name.startswith("test"):
        return True

    if path.suffix != ".py" or path.parent != _BACKEND_ROOT:
        return False

    if not path.name.startswith("test_"):
        return False

    try:
        source = path.read_text(encoding="utf-8", errors="ignore")
    except OSError:
        return False

    has_manual_entrypoint = (
        'if __name__ == "__main__":' in source
        or "def main(" in source
        or "async def main(" in source
    )
    has_assertions = "assert " in source
    has_custom_runner = "run_all_tests(" in source
    has_boolean_test_protocol = "return True" in source or "return False" in source

    return has_custom_runner or (
        has_manual_entrypoint and (not has_assertions or has_boolean_test_protocol)
    )
