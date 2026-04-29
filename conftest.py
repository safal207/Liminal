"""Pytest collection guards for generated artifacts and manual smoke scripts."""

import socket
from pathlib import Path

import pytest

collect_ignore = [
    "pytest_log.txt",
    "test_localsystem.txt",
    "test_output.txt",
    "test_output_simple.txt",
    "test_results.txt",
    "backend/test_errors.txt",
]

_BACKEND_ROOT = Path(__file__).parent / "backend"
_REPO_ROOT = Path(__file__).parent
_LIVE_SERVER_TESTS = {
    "backend/tests/test_connection_limits.py",
    "backend/tests/test_debug_channels.py",
    "backend/tests/test_jwt_auth.py",
    "backend/tests/test_jwt_edge_cases.py",
    "backend/tests/test_metrics_integration.py",
    "backend/tests/test_metrics_websocket.py",
    "backend/tests/test_metrics_with_auth.py",
    "backend/tests/test_websocket.py",
    "backend/tests/test_websocket_channels.py",
    "backend/tests/test_websocket_limits_simple.py",
    "backend/tests/test_websocket_python_client.py",
    "backend/tests/test_websocket_simple.py",
}


def _live_server_available(host: str = "127.0.0.1", port: int = 8000) -> bool:
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        sock.settimeout(0.5)
        return sock.connect_ex((host, port)) == 0


def pytest_ignore_collect(collection_path, config):
    """Skip root-level backend smoke scripts that are not real pytest modules."""
    path = Path(str(collection_path))

    if path.suffix == ".txt" and path.name.startswith("test"):
        return True

    if path.suffix != ".py":
        return False

    is_backend_manual_smoke_test = (
        path.parent == _BACKEND_ROOT and path.name.startswith("test_")
    )
    is_backend_tests_script = (
        path.parent == _BACKEND_ROOT / "tests" and path.name.endswith("_test.py")
    )

    if not (is_backend_manual_smoke_test or is_backend_tests_script):
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


def pytest_collection_modifyitems(config, items):
    """Skip live-server smoke suites when no local backend is running."""

    if _live_server_available():
        return

    skip_live_server = pytest.mark.skip(
        reason="Requires a running local backend server on 127.0.0.1:8000"
    )

    for item in items:
        try:
            relative_path = (
                Path(str(item.fspath)).resolve().relative_to(_REPO_ROOT).as_posix()
            )
        except ValueError:
            continue

        if relative_path in _LIVE_SERVER_TESTS:
            item.add_marker(skip_live_server)
