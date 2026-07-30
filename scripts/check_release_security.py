#!/usr/bin/env python3
"""Fail CI when release configuration regresses to unsafe defaults."""

from __future__ import annotations

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
ENV_EXAMPLE = ROOT / ".env.example"
PRODUCTION_COMPOSE = ROOT / "backend/production/docker-compose.production.yml"

REQUIRED_ENV_KEYS = {
    "ENV",
    "JWT_SECRET_KEY",
    "NEO4J_PASSWORD",
    "WS_MAX_CONNECTIONS",
    "WS_MAX_CONNECTIONS_PER_IP",
    "WS_RATE_LIMIT_PER_SECOND",
    "WS_RATE_LIMIT_BURST",
    "WS_MAX_MESSAGE_BYTES",
}

BANNED_COMPOSE_PATTERNS = {
    r"rgl_production_password": "fixed Neo4j password",
    r"rgl_admin_password": "fixed Grafana password",
    r"image:\s*[^\n]*:latest": "mutable latest image",
    r"xpack\.security\.enabled\s*[:=]\s*[\"']?false": "disabled Elasticsearch security",
    r"\"6379:6379\"": "public Redis port",
    r"\"7474:7474\"": "public Neo4j HTTP port",
    r"\"7687:7687\"": "public Neo4j Bolt port",
    r"\"3000:3000\"": "public Grafana port",
    r"\"9090:9090\"": "public Prometheus port",
}

REQUIRED_COMPOSE_SNIPPETS = {
    "JWT_SECRET_KEY: ${JWT_SECRET_KEY:?",
    "NEO4J_PASSWORD: ${NEO4J_PASSWORD:?",
    "GF_SECURITY_ADMIN_PASSWORD: ${GRAFANA_ADMIN_PASSWORD:?",
    "xpack.security.enabled: \"true\"",
    "internal: true",
    "http://localhost:8000/ready",
}


def parse_env_keys(path: Path) -> set[str]:
    keys: set[str] = set()
    for raw_line in path.read_text(encoding="utf-8").splitlines():
        line = raw_line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        keys.add(line.split("=", 1)[0].strip())
    return keys


def main() -> int:
    errors: list[str] = []

    if (ROOT / ".env").exists():
        errors.append("tracked .env must not exist; keep only .env.example")

    if not ENV_EXAMPLE.exists():
        errors.append(".env.example is missing")
    else:
        missing = REQUIRED_ENV_KEYS - parse_env_keys(ENV_EXAMPLE)
        if missing:
            errors.append(f".env.example is missing runtime keys: {sorted(missing)}")

    if not PRODUCTION_COMPOSE.exists():
        errors.append("production compose file is missing")
    else:
        compose = PRODUCTION_COMPOSE.read_text(encoding="utf-8")
        for pattern, description in BANNED_COMPOSE_PATTERNS.items():
            if re.search(pattern, compose, flags=re.IGNORECASE):
                errors.append(f"production compose contains {description}")
        for snippet in REQUIRED_COMPOSE_SNIPPETS:
            if snippet not in compose:
                errors.append(f"production compose missing guard: {snippet}")

    if errors:
        print("Release security checks failed:", file=sys.stderr)
        for error in errors:
            print(f"- {error}", file=sys.stderr)
        return 1

    print("Release security configuration checks passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
