#!/usr/bin/env python3
"""Fail CI when release configuration regresses to unsafe defaults."""

from __future__ import annotations

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
ENV_EXAMPLE = ROOT / ".env.example"
CORE_REQUIREMENTS = ROOT / "requirements-core.txt"
PRODUCTION_DOCKERFILE = ROOT / "backend/Dockerfile"
PRODUCTION_COMPOSE = ROOT / "backend/production/docker-compose.production.yml"

REQUIRED_ENV_KEYS = {
    "ENV",
    "JWT_SECRET_KEY",
    "NEO4J_PASSWORD",
    "REDIS_PASSWORD",
    "GRAFANA_ADMIN_PASSWORD",
    "ELASTIC_PASSWORD",
    "KIBANA_SYSTEM_PASSWORD",
    "WS_MAX_CONNECTIONS",
    "WS_MAX_CONNECTIONS_PER_IP",
    "WS_RATE_LIMIT_PER_SECOND",
    "WS_RATE_LIMIT_BURST",
    "WS_MAX_MESSAGE_BYTES",
    "PYTHON_IMAGE",
    "REDIS_IMAGE_DIGEST",
    "NEO4J_IMAGE_DIGEST",
    "PROMETHEUS_IMAGE_DIGEST",
    "GRAFANA_IMAGE_DIGEST",
    "NGINX_IMAGE_DIGEST",
    "ELASTICSEARCH_IMAGE_DIGEST",
    "LOGSTASH_IMAGE_DIGEST",
    "KIBANA_IMAGE_DIGEST",
    "NEURAL_ANALYTICS_IMAGE_REPOSITORY",
    "NEURAL_ANALYTICS_IMAGE_DIGEST",
    "WEBSOCKET_GATEWAY_IMAGE_REPOSITORY",
    "WEBSOCKET_GATEWAY_IMAGE_DIGEST",
}

BANNED_CORE_PACKAGES = {
    "aiohttp",
    "flask",
    "nltk",
    "numpy",
    "pandas",
    "prefect",
    "python-jose",
    "scikit-learn",
    "selenium",
    "torch",
    "transformers",
    "webdriver-manager",
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
    r"production/Dockerfile\.rgl-core": "missing legacy core Dockerfile",
    r"production/Dockerfile\.analytics": "missing legacy analytics Dockerfile",
    r"production/Dockerfile\.websocket": "missing legacy WebSocket Dockerfile",
}

REQUIRED_COMPOSE_SNIPPETS = {
    "context: ../..",
    "dockerfile: backend/Dockerfile",
    "PYTHON_IMAGE: ${PYTHON_IMAGE:?",
    "JWT_SECRET_KEY: ${JWT_SECRET_KEY:?",
    "NEO4J_PASSWORD: ${NEO4J_PASSWORD:?",
    "GF_SECURITY_ADMIN_PASSWORD: ${GRAFANA_ADMIN_PASSWORD:?",
    "ELASTIC_PASSWORD: ${ELASTIC_PASSWORD:?",
    "ELASTICSEARCH_PASSWORD: ${KIBANA_SYSTEM_PASSWORD:?",
    'xpack.security.enabled: "true"',
    "internal: true",
    "http://localhost:8000/ready",
    "redis@${REDIS_IMAGE_DIGEST:?",
    "neo4j@${NEO4J_IMAGE_DIGEST:?",
    "prom/prometheus@${PROMETHEUS_IMAGE_DIGEST:?",
    "grafana/grafana@${GRAFANA_IMAGE_DIGEST:?",
    "nginx@${NGINX_IMAGE_DIGEST:?",
    "elasticsearch@${ELASTICSEARCH_IMAGE_DIGEST:?",
    "logstash@${LOGSTASH_IMAGE_DIGEST:?",
    "kibana@${KIBANA_IMAGE_DIGEST:?",
    "@${NEURAL_ANALYTICS_IMAGE_DIGEST:?",
    "@${WEBSOCKET_GATEWAY_IMAGE_DIGEST:?",
}


def parse_env_keys(path: Path) -> set[str]:
    keys: set[str] = set()
    for raw_line in path.read_text(encoding="utf-8").splitlines():
        line = raw_line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        keys.add(line.split("=", 1)[0].strip())
    return keys


def parse_requirement_names(path: Path) -> set[str]:
    names: set[str] = set()
    for raw_line in path.read_text(encoding="utf-8").splitlines():
        line = raw_line.strip()
        if not line or line.startswith("#") or line.startswith("-"):
            continue
        name = re.split(r"[<>=!~\[]", line, maxsplit=1)[0].strip().lower()
        names.add(name)
    return names


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

    if not CORE_REQUIREMENTS.exists():
        errors.append("requirements-core.txt is missing")
    else:
        core_packages = parse_requirement_names(CORE_REQUIREMENTS)
        forbidden = BANNED_CORE_PACKAGES & core_packages
        if forbidden:
            errors.append(f"core requirements include optional packages: {sorted(forbidden)}")
        if "pyjwt" not in core_packages:
            errors.append("core requirements must use PyJWT")

    if not PRODUCTION_DOCKERFILE.exists():
        errors.append("production API Dockerfile is missing")
    else:
        dockerfile = PRODUCTION_DOCKERFILE.read_text(encoding="utf-8")
        if "-r requirements-core.txt" not in dockerfile:
            errors.append("production Dockerfile must install requirements-core.txt")
        if "requirements.txt" in dockerfile.replace("requirements-core.txt", ""):
            errors.append("production Dockerfile must not install aggregate requirements.txt")
        if "test-requirements" in dockerfile or "requirements-dev" in dockerfile:
            errors.append("production Dockerfile must not install test/dev dependencies")
        if "ARG PYTHON_IMAGE" not in dockerfile or "FROM ${PYTHON_IMAGE}" not in dockerfile:
            errors.append("production Dockerfile must require an external immutable base image")

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

        for line in compose.splitlines():
            stripped = line.strip()
            if stripped.startswith("image:") and (
                "@${" not in stripped or "_IMAGE_DIGEST" not in stripped
            ):
                errors.append(f"mutable production image reference: {stripped}")

    if errors:
        print("Release security checks failed:", file=sys.stderr)
        for error in errors:
            print(f"- {error}", file=sys.stderr)
        return 1

    print("Release security configuration checks passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
