from pathlib import Path
import subprocess


def test_root_env_is_ignored_and_not_tracked():
    gitignore = Path(".gitignore").read_text(encoding="utf-8").splitlines()
    assert ".env" in {line.strip() for line in gitignore}

    result = subprocess.run(
        ["git", "ls-files", "--error-unmatch", ".env"],
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode != 0, "root .env must never be tracked by git"
