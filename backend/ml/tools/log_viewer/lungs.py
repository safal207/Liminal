"""Simple watchdog service ("Lungs") that periodically checks health of configured services.
Writes heartbeat results to logs/health/heartbeat.log in markdown+JSON format
and exposes the latest heartbeat via a tiny HTTP server (optional).
"""

import json
import socket
import time
from datetime import datetime
from http.server import BaseHTTPRequestHandler, HTTPServer
from pathlib import Path
from threading import Thread

import requests
import yaml
from endocrine import thyroid

CONFIG_PATH = Path(__file__).parent / "config" / "services.yml"
LOG_DIR = Path(__file__).parent / "logs" / "health"
LOG_DIR.mkdir(parents=True, exist_ok=True)
HEARTBEAT_FILE = LOG_DIR / "heartbeat.log"
INTERVAL = 30  # seconds

# Load service list
with open(CONFIG_PATH, encoding="utf-8") as f:
    SERVICES = yaml.safe_load(f)

last_heartbeat = {}


def check_http(url: str, timeout: float = 3.0):
    start = time.time()
    try:
        r = requests.get(url, timeout=timeout)
        latency = (time.time() - start) * 1000
        return r.status_code == 200, latency
    except Exception:
        return False, None


def check_tcp(host: str, port: int, timeout: float = 3.0):
    start = time.time()
    try:
        with socket.create_connection((host, port), timeout=timeout):
            latency = (time.time() - start) * 1000
            return True, latency
    except Exception:
        return False, None


def classify(ok: bool, latency):
    if not ok:
        return "yin"  # down
    if latency is None:
        return "yan"  # no latency measured but ok
    if latency < 500:
        return "yan"
    if latency < 3000:
        return "yin-yan"
    return "yin"


EXPERIENCE_LOG = Path(__file__).parent / "logs" / "adapter" / "experience.log"
INSIGHTS_LOG = Path(__file__).parent / "logs" / "adapter" / "insights.log"


def _count_errors() -> int:
    if EXPERIENCE_LOG.exists():
        text = EXPERIENCE_LOG.read_text(encoding="utf-8")
        return text.count("```")  # rough count JSON blocks
    return 0


def _log_insight(event: dict):
    INSIGHTS_LOG.parent.mkdir(parents=True, exist_ok=True)
    with open(INSIGHTS_LOG, "a", encoding="utf-8") as f:
        f.write(
            f"## Insight {event['timestamp']}\n```${json.dumps(event, ensure_ascii=False)}```\n\n"
        )


def heartbeat():
    global last_heartbeat
    hb = {"timestamp": datetime.utcnow().isoformat(), "services": {}}
    for name, endpoint in SERVICES.items():
        if endpoint.startswith("http"):
            ok, lat = check_http(endpoint)
        elif endpoint.startswith("tcp://"):
            host_port = endpoint[6:]
            host, port = host_port.split(":")
            ok, lat = check_tcp(host, int(port))
        else:
            ok, lat = False, None
        state = classify(ok, lat)
        hb["services"][name] = {
            "endpoint": endpoint,
            "ok": ok,
            "latency_ms": lat,
            "state": state,
        }
    last_heartbeat = hb

    # ---- Endocrine integration ----
    total_errors = _count_errors()
    thyroid.update_from_error_count(total_errors)
    if thyroid.should_release():
        insight_event = thyroid.release()
        _log_insight(insight_event)
    # Append to markdown log
    with open(HEARTBEAT_FILE, "a", encoding="utf-8") as f:
        f.write(f"## Heartbeat {hb['timestamp']}\n```${json.dumps(hb, ensure_ascii=False)}```\n\n")


def loop():
    while True:
        heartbeat()
        time.sleep(INTERVAL)


class Handler(BaseHTTPRequestHandler):
    def do_GET(self):
        if self.path == "/healthmap":
            self.send_response(200)
            self.send_header("Content-Type", "application/json; charset=utf-8")
            self.end_headers()
            self.wfile.write(json.dumps(last_heartbeat).encode("utf-8"))
        else:
            self.send_response(404)
            self.end_headers()


def start_http_server():
    server = HTTPServer(("0.0.0.0", 5555), Handler)
    server.serve_forever()


if __name__ == "__main__":
    Thread(target=loop, daemon=True).start()
    print("Lungs watchdog started; heartbeat every", INTERVAL, "seconds")
    start_http_server()
