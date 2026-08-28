#!/usr/bin/env python3
"""Fail CI when release configuration regresses to unsafe defaults."""

from __future__ import annotations

import os
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
ENV_EXAMPLE = ROOT / ".env.example"
CORE_REQUIREMENTS = ROOT / "requirements-core.txt"
PRODUCTION_DOCKERFILE = ROOT / "backend/Dockerfile"
PRODUCTION_COMPOSE = ROOT / "backend/production/docker-compose.production.yml"
RESEARCH_COMPOSE = ROOT / "backend/production/docker-compose.research.yml"
GATEWAY_COMPOSE = ROOT / "backend/production/docker-compose.gateway.yml"
OBSERVABILITY_COMPOSE = ROOT / "backend/production/docker-compose.observability.yml"
PRODUCTION_NGINX_CONFIG = ROOT / "backend/nginx/nginx.conf"
OBSERVABILITY_NGINX_CONFIG = ROOT / "backend/nginx/observability.conf"

REQUIRED_ENV_KEYS = {
    "ENV",
    "JWT_SECRET_KEY",
    "ML_METRICS_SERVICE_TOKEN",
    "NEO4J_PASSWORD",
    "REDIS_PASSWORD",
    "LIMINAL_TLS_CERT_DIR",
    "LIMINAL_OBSERVABILITY_BASE_URL",
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
    r"xpack\.security\.http\.ssl\.enabled\s*[:=]\s*[\"']?false": (
        "disabled Elasticsearch HTTP TLS"
    ),
    r"\bhttp://elasticsearch:9200": "cleartext Elasticsearch credentials",
    r"\bredis://": "cleartext Redis credentials",
    r"\brediss://[^\s\"']*@": "Redis credentials embedded in a URI",
    r"\"6379:6379\"": "public Redis port",
    r"\"7474:7474\"": "public Neo4j HTTP port",
    r"\"7687:7687\"": "public Neo4j Bolt port",
    r"\"3000:3000\"": "public Grafana port",
    r"\"9090:9090\"": "public Prometheus port",
    r'(?m)^\s*-\s*["\']?(?:0\.0\.0\.0:)?8080:8080["\']?\s*$': (
        "public observability ingress port"
    ),
    r"production/Dockerfile\.rgl-core": "missing legacy core Dockerfile",
    r"production/Dockerfile\.analytics": "missing legacy analytics Dockerfile",
    r"production/Dockerfile\.websocket": "missing legacy WebSocket Dockerfile",
}

REQUIRED_COMPOSE_SNIPPETS = {
    PRODUCTION_COMPOSE: {
        "context: ../..",
        "dockerfile: backend/Dockerfile",
        "PYTHON_IMAGE: ${PYTHON_IMAGE:?",
        "JWT_SECRET_KEY: ${JWT_SECRET_KEY:?",
        "ML_METRICS_SERVICE_TOKEN: ${ML_METRICS_SERVICE_TOKEN:?",
        'REDIS_URL: "rediss://redis:6379/0?',
        "--tls-port 6379",
        "--tls-ca-cert-file /etc/liminal/tls/ca.crt",
        "WS_MAX_MESSAGE_BYTES: ${WS_MAX_MESSAGE_BYTES:-16384}",
        "NEO4J_PASSWORD: ${NEO4J_PASSWORD:?",
        "NEO4J_CA_CERT: /etc/liminal/tls/neo4j-ca.crt",
        "NEO4J_server_bolt_tls__level: REQUIRED",
        'NEO4J_dbms_ssl_policy_bolt_enabled: "true"',
        "neo4j/bolt/ca.crt:/etc/liminal/tls/neo4j-ca.crt:ro",
        "neo4j:/ssl:ro",
        'FORWARDED_ALLOW_IPS: "172.30.0.10"',
        "egress:\n        gw_priority: 1",
        "egress:\n    internal: false",
        "ipv4_address: 172.30.0.10",
        "subnet: 172.30.0.0/24",
        "internal: true",
        "http://localhost:8000/ready",
        "../nginx/nginx.conf:/etc/nginx/nginx.conf:ro",
        "redis@${REDIS_IMAGE_DIGEST:?",
        "neo4j@${NEO4J_IMAGE_DIGEST:?",
        "nginx@${NGINX_IMAGE_DIGEST:?",
    },
    RESEARCH_COMPOSE: {
        "@${NEURAL_ANALYTICS_IMAGE_DIGEST:?",
        'REDIS_URL: "rediss://redis:6379/0?',
        "REDIS_PASSWORD: ${REDIS_PASSWORD:?",
        "ssl_cert_reqs=required",
    },
    GATEWAY_COMPOSE: {
        "@${WEBSOCKET_GATEWAY_IMAGE_DIGEST:?",
        'REDIS_URL: "rediss://redis:6379/0?',
        "REDIS_PASSWORD: ${REDIS_PASSWORD:?",
        "ssl_cert_reqs=required",
    },
    OBSERVABILITY_COMPOSE: {
        '"127.0.0.1:8080:8080"',
        "GF_SECURITY_ADMIN_PASSWORD: ${GRAFANA_ADMIN_PASSWORD:?",
        "ELASTIC_PASSWORD: ${ELASTIC_PASSWORD:?",
        "ELASTICSEARCH_PASSWORD: ${KIBANA_SYSTEM_PASSWORD:?",
        "KIBANA_SYSTEM_PASSWORD: ${KIBANA_SYSTEM_PASSWORD:?",
        'xpack.security.enabled: "true"',
        'xpack.security.http.ssl.enabled: "true"',
        "xpack.security.http.ssl.certificate_authorities: certs/ca.crt",
        "https://elasticsearch:9200/_security/user/kibana_system/_password",
        "--cacert /usr/share/elasticsearch/config/certs/ca.crt",
        "ELASTICSEARCH_HOSTS: https://elasticsearch:9200",
        "ELASTICSEARCH_SSL_VERIFICATIONMODE: full",
        "--web.route-prefix=/",
        'GF_SERVER_SERVE_FROM_SUB_PATH: "true"',
        "GF_SERVER_PROTOCOL: https",
        "GF_SERVER_CERT_FILE: /etc/grafana/tls/server.crt",
        "GF_SERVER_CERT_KEY: /etc/grafana/tls/server.key",
        "SERVER_BASEPATH: /kibana",
        'SERVER_SSL_ENABLED: "true"',
        "SERVER_SSL_CERTIFICATE: /usr/share/kibana/config/tls/server.crt",
        "SERVER_SSL_KEY: /usr/share/kibana/config/tls/server.key",
        "observability/ca.crt:/etc/nginx/tls/observability-ca.crt:ro",
        "SERVER_PUBLICBASEURL: ${LIMINAL_OBSERVABILITY_BASE_URL:?",
        "../monitoring/prometheus.yml:/etc/prometheus/prometheus.yml:ro",
        "../../grafana/provisioning/datasources:/etc/grafana/provisioning/datasources:ro",
        "../monitoring/logstash.conf:/usr/share/logstash/pipeline/logstash.conf:ro",
        "condition: service_completed_successfully",
        "../nginx/observability.conf:/etc/nginx/liminal.d/observability.conf:ro",
        "prom/prometheus@${PROMETHEUS_IMAGE_DIGEST:?",
        "grafana/grafana@${GRAFANA_IMAGE_DIGEST:?",
        "elasticsearch@${ELASTICSEARCH_IMAGE_DIGEST:?",
        "logstash@${LOGSTASH_IMAGE_DIGEST:?",
        "kibana@${KIBANA_IMAGE_DIGEST:?",
    },
}

PROFILE_ONLY_BASE_MARKERS = {
    "NEURAL_ANALYTICS_IMAGE_",
    "WEBSOCKET_GATEWAY_IMAGE_",
    "PROMETHEUS_IMAGE_DIGEST",
    "GRAFANA_IMAGE_DIGEST",
    "ELASTICSEARCH_IMAGE_DIGEST",
    "LOGSTASH_IMAGE_DIGEST",
    "KIBANA_IMAGE_DIGEST",
    "GRAFANA_ADMIN_PASSWORD",
    "ELASTIC_PASSWORD",
    "KIBANA_SYSTEM_PASSWORD",
    "LIMINAL_OBSERVABILITY_BASE_URL",
}

PYTHON_IMAGE_PATTERN = re.compile(r"^[^\s@]+@sha256:[0-9A-Fa-f]{64}$")


def dockerfile_instructions(dockerfile: str) -> list[str]:
    """Return logical Dockerfile instructions without blank/comment-only lines."""
    instructions: list[str] = []
    current = ""
    for raw_line in dockerfile.splitlines():
        stripped = raw_line.strip()
        if not stripped or stripped.startswith("#"):
            continue
        current = f"{current} {stripped}".strip()
        if current.endswith("\\"):
            current = current[:-1].rstrip()
            continue
        instructions.append(current)
        current = ""
    if current:
        instructions.append(current)
    return instructions


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


def production_dockerfile_errors(
    dockerfile: str,
    python_image: str | None = None,
) -> list[str]:
    """Return release-boundary violations in the production API image."""
    errors: list[str] = []
    if "-r requirements-core.txt" not in dockerfile:
        errors.append("production Dockerfile must install requirements-core.txt")
    if "requirements.txt" in dockerfile.replace("requirements-core.txt", ""):
        errors.append(
            "production Dockerfile must not install aggregate requirements.txt"
        )
    if any(
        marker in dockerfile
        for marker in (
            "test-requirements",
            "requirements-dev",
            "requirements-research",
        )
    ):
        errors.append(
            "production Dockerfile must not install test/dev/research dependencies"
        )
    if "ARG PYTHON_IMAGE" not in dockerfile or "FROM ${PYTHON_IMAGE}" not in dockerfile:
        errors.append(
            "production Dockerfile must require an external immutable base image"
        )
    supplied_image = (
        os.getenv("PYTHON_IMAGE", "") if python_image is None else python_image
    ).strip()
    if PYTHON_IMAGE_PATTERN.fullmatch(supplied_image) is None:
        errors.append("PYTHON_IMAGE must be an immutable @sha256 reference")
    launch = "\n".join(
        instruction
        for instruction in dockerfile_instructions(dockerfile)
        if re.match(r"^(?:CMD|ENTRYPOINT)\s", instruction, flags=re.IGNORECASE)
    )
    if "--ws-max-size" not in launch or "${WS_MAX_MESSAGE_BYTES:-16384}" not in launch:
        errors.append(
            "production ASGI launch must enforce WS_MAX_MESSAGE_BYTES at transport"
        )
    return errors


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
            errors.append(
                f"core requirements include optional packages: {sorted(forbidden)}"
            )
        if "pyjwt" not in core_packages:
            errors.append("core requirements must use PyJWT")

    if not PRODUCTION_DOCKERFILE.exists():
        errors.append("production API Dockerfile is missing")
    else:
        dockerfile = PRODUCTION_DOCKERFILE.read_text(encoding="utf-8")
        errors.extend(production_dockerfile_errors(dockerfile))

    compose_documents: dict[Path, str] = {}
    for compose_path, required_snippets in REQUIRED_COMPOSE_SNIPPETS.items():
        if not compose_path.exists():
            errors.append(f"production compose file is missing: {compose_path.name}")
            continue
        compose = compose_path.read_text(encoding="utf-8")
        compose_documents[compose_path] = compose
        for pattern, description in BANNED_COMPOSE_PATTERNS.items():
            if re.search(pattern, compose, flags=re.IGNORECASE):
                errors.append(f"{compose_path.name} contains {description}")
        for snippet in required_snippets:
            if snippet not in compose:
                errors.append(f"{compose_path.name} missing guard: {snippet}")

        for line in compose.splitlines():
            stripped = line.strip()
            if stripped.startswith("image:") and (
                "@${" not in stripped or "_IMAGE_DIGEST" not in stripped
            ):
                errors.append(f"mutable image in {compose_path.name}: {stripped}")

    base_compose = compose_documents.get(PRODUCTION_COMPOSE, "")
    for marker in PROFILE_ONLY_BASE_MARKERS:
        if marker in base_compose:
            errors.append(
                f"base production compose contains optional-only setting: {marker}"
            )

    if not PRODUCTION_NGINX_CONFIG.exists():
        errors.append("production nginx config is missing")
    else:
        nginx_config = PRODUCTION_NGINX_CONFIG.read_text(encoding="utf-8")
        if "server rgl-core:8000" not in nginx_config:
            errors.append("production nginx must target the rgl-core service")
        if "server liminal-backend:8000" in nginx_config:
            errors.append("production nginx targets an undefined legacy service")
        if (
            "http://prometheus:9090" in nginx_config
            or "http://grafana:3000" in nginx_config
        ):
            errors.append(
                "base production nginx resolves optional observability services"
            )
        for snippet in (
            "location /ws/",
            "proxy_http_version 1.1",
            "proxy_set_header Upgrade $http_upgrade",
            "proxy_set_header Connection $connection_upgrade",
        ):
            if snippet not in nginx_config:
                errors.append(f"production nginx missing WebSocket guard: {snippet}")

    if not OBSERVABILITY_NGINX_CONFIG.exists():
        errors.append("optional observability nginx config is missing")
    else:
        observability_nginx = OBSERVABILITY_NGINX_CONFIG.read_text(encoding="utf-8")
        for upstream in (
            "http://prometheus:9090",
            "https://grafana:3000",
            "https://kibana:5601",
        ):
            if upstream not in observability_nginx:
                errors.append(
                    f"observability nginx config missing upstream: {upstream}"
                )
        for plaintext_upstream in (
            "http://grafana:3000",
            "http://kibana:5601",
        ):
            if plaintext_upstream in observability_nginx:
                errors.append(
                    f"observability nginx contains plaintext upstream: {plaintext_upstream}"
                )
        for snippet in (
            "proxy_ssl_trusted_certificate /etc/nginx/tls/observability-ca.crt",
            "proxy_ssl_verify on",
            "proxy_ssl_server_name on",
            "proxy_ssl_name grafana",
            "proxy_ssl_name kibana",
        ):
            if snippet not in observability_nginx:
                errors.append(
                    f"observability nginx missing TLS verification guard: {snippet}"
                )

    if errors:
        print("Release security checks failed:", file=sys.stderr)
        for error in errors:
            print(f"- {error}", file=sys.stderr)
        return 1

    print("Release security configuration checks passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
