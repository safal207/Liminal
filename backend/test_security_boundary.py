"""Regression checks for the LIMINAL credential boundary."""
from __future__ import annotations

import subprocess
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
_BINARY_SUFFIXES = {".a", ".bin", ".class", ".dll", ".dylib", ".exe", ".gif", ".ico", ".jpg", ".jpeg", ".o", ".pdf", ".png", ".pyd", ".so", ".wasm", ".woff", ".woff2", ".zip"}
_FORBIDDEN = (
    "NewStrong" + "Pass123!",
    "test-jwt-" + "secret-key-for-local-development-only",
    "resonance-liminal-" + "secret-key-change-in-production",
    "rgl_" + "production_password",
    "test_" + "neo4j_password_123",
    "test_" + "postgres_password_123",
    "test_" + "grafana_password_123",
)

def _tracked_files() -> list[Path]:
    output = subprocess.check_output(["git", "ls-files", "-z"], cwd=ROOT)
    return [Path(item) for item in output.decode().split("\0") if item]

class CredentialBoundaryTest(unittest.TestCase):
    def test_environment_file_is_not_tracked(self) -> None:
        self.assertNotIn(Path(".env"), _tracked_files())

    def test_known_runtime_credentials_are_absent(self) -> None:
        violations = []
        for relative_path in _tracked_files():
            if relative_path.name == ".env.example" or relative_path.suffix.lower() in _BINARY_SUFFIXES:
                continue
            path = ROOT / relative_path
            try:
                raw = path.read_bytes()
            except OSError:
                continue
            if b"\\0" in raw[:8192]:
                continue
            text = raw.decode("utf-8", errors="ignore")
            for forbidden in _FORBIDDEN:
                if forbidden in text:
                    violations.append(f"{relative_path}: {forbidden[:8]}…")
        self.assertEqual([], violations)

if __name__ == "__main__":
    unittest.main()
