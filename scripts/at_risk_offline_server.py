from __future__ import annotations

import argparse
import json
import os

# Make src importable when running from repo root
import sys
from http.server import BaseHTTPRequestHandler, HTTPServer
from pathlib import Path
from typing import Any, List, Tuple
from urllib.parse import parse_qs, urlparse

ROOT = Path(__file__).resolve().parents[1]
SRC = ROOT / "src"
if str(SRC) not in sys.path:
    sys.path.insert(0, str(SRC))

from liminal.diffusion import InMemoryDiffusion, ModuleState
from liminal.reality_web import RealityWebInMemory, SystemBreath

WEB = RealityWebInMemory()
BREATH = SystemBreath()


def _clip01(x: float) -> float:
    return 0.0 if x < 0.0 else 1.0 if x > 1.0 else x


def compute_relationship_health(
    a_traits: dict[str, float], b_traits: dict[str, float]
) -> Tuple[float, List[str]]:
    d = InMemoryDiffusion()
    base = float(
        d.similarity(
            ModuleState("a", traits=a_traits, notes=[]),
            ModuleState("b", traits=b_traits, notes=[]),
        )
    )
    POS_KEYS = getattr(
        SystemBreath,
        "SOFT_KEYS_POS",
        ("любовь", "спокойствие", "нежность", "мягкость", "calm", "love", "tenderness"),
    )
    NEG_KEYS = getattr(
        SystemBreath,
        "SOFT_KEYS_NEG",
        ("страх", "гнев", "злость", "тревога", "anger", "fear"),
    )

    def avg(keys: Tuple[str, ...]) -> float:
        vals: List[float] = []
        for k in keys:
            if k in a_traits or k in b_traits:
                va = float(a_traits.get(k, 0.0))
                vb = float(b_traits.get(k, 0.0))
                vals.append((va + vb) / 2.0)
        return sum(vals) / len(vals) if vals else 0.0

    pos_avg = avg(tuple(POS_KEYS))
    neg_avg = avg(tuple(NEG_KEYS))
    POS_W = 0.10
    NEG_W = 0.10
    bonus = POS_W * pos_avg
    penalty = NEG_W * neg_avg
    score = _clip01(base + bonus - penalty)
    rationale = [
        f"base_similarity={base:.3f}",
        f"pos_avg={pos_avg:.3f}*{POS_W:.2f} -> +{bonus:.3f}",
        f"neg_avg={neg_avg:.3f}*{NEG_W:.2f} -> -{penalty:.3f}",
        f"score_clipped={score:.3f}",
    ]
    return score, rationale


def compute_top_at_risk(limit: int) -> List[dict[str, Any]]:
    nodes = WEB.nodes()
    edges: List[dict[str, Any]] = []
    try:
        threshold = float(os.getenv("LIMINAL_HEALTH_THRESHOLD", "0.4"))
    except ValueError:
        threshold = 0.4
    for i in range(len(nodes)):
        for j in range(i + 1, len(nodes)):
            a = nodes[i]
            b = nodes[j]
            if a.traits or b.traits:
                score, rationale = compute_relationship_health(a.traits, b.traits)
                advice: List[str] = []
                if score < threshold:
                    advice = ["breathStep", "consider_linkParent", "consider_merge"]
                edges.append(
                    {
                        "sourceId": a.id,
                        "targetId": b.id,
                        "score": score,
                        "advice": advice,
                        "rationale": rationale,
                    }
                )
    edges.sort(key=lambda e: e["score"])  # lowest first
    return edges[: max(0, int(limit))]


HTML_TEMPLATE = """<!doctype html>
<html>
  <head>
    <meta charset='utf-8'/>
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <title>Top At-Risk Edges</title>
    <style>
      :root { --accent: #5b7cff; --danger: #d32f2f; --bg: #0b1020; --card: #121a2e; --muted: #94a3b8; }
      * { box-sizing: border-box; }
      body { font-family: system-ui, -apple-system, Segoe UI, Roboto, Arial, sans-serif; margin: 0; background: #0f172a; color: #e2e8f0; }
      .wrap { max-width: 1080px; margin: 24px auto; padding: 0 16px; }
      h1 { margin: 8px 0 16px; font-weight: 700; letter-spacing: 0.3px; }
      .controls { display: flex; flex-wrap: wrap; gap: 10px; align-items: center; margin-bottom: 16px; }
      .controls .group { display: flex; gap: 8px; align-items: center; background: #0b1224; padding: 8px 10px; border-radius: 8px; border: 1px solid #1f2a44; }
      label { color: var(--muted); font-size: 14px; }
      input[type=number] { width: 90px; background: #0a1326; color: #e2e8f0; border: 1px solid #1f2a44; padding: 6px 8px; border-radius: 6px; }
      input[type=text] { width: 220px; background: #0a1326; color: #e2e8f0; border: 1px solid #1f2a44; padding: 6px 8px; border-radius: 6px; }
      button { background: linear-gradient(135deg, #3b82f6, #6366f1); border: 0; color: white; padding: 8px 12px; border-radius: 8px; cursor: pointer; font-weight: 600; box-shadow: 0 4px 14px rgba(59,130,246,0.35); }
      button.secondary { background: #15213b; border: 1px solid #243b64; color: #cbd5e1; box-shadow: none; }
      .card { background: #0b1224; border: 1px solid #1f2a44; border-radius: 12px; overflow: hidden; }
      .card h3 { margin: 0; padding: 12px 14px; background: #0d1731; border-bottom: 1px solid #1f2a44; font-size: 15px; color: #cbd5e1; }
      table { border-collapse: collapse; width: 100%; }
      th, td { border-bottom: 1px solid #1e293b; padding: 10px 12px; font-size: 14px; }
      th { text-align: left; color: #94a3b8; background: #0b1224; position: sticky; top: 0; }
      tr:hover td { background: #0d1731; }
      .score { font-variant-numeric: tabular-nums; }
      .risk td { background: rgba(211,47,47,0.10); }
      .risk .score { color: #ff6b6b; font-weight: 700; }
      .muted { color: #94a3b8; font-size: 13px; }
      .form-inline { display: flex; flex-wrap: wrap; gap: 8px; align-items: center; padding: 10px 12px; }
    </style>
  </head>
  <body>
    <div class="wrap">
      <h1>Top At-Risk Edges</h1>
      <div class="controls">
        <div class="group">
          <label for="limit">limit</label>
          <input id="limit" type="number" min="1" value="{limit}" />
        </div>
        <div class="group">
          <label for="threshold">threshold</label>
          <input id="threshold" type="number" step="0.01" min="0" max="1" value="{threshold}" />
        </div>
        <button id="refresh" class="secondary">Refresh</button>
        <button id="seed" class="secondary">Seed demo</button>
      </div>

      <div class="card">
        <h3>Edges</h3>
        <div class="form-inline">
          <label for="traits">Add node (traits)</label>
          <input id="traits" type="text" placeholder="любовь:0.7, страх:0.2" />
          <button id="addnode">Add node</button>
          <span class="muted">Формат: ключ:значение через запятую. Значения 0..1</span>
        </div>
        <table>
          <thead>
            <tr><th>Source</th><th>Target</th><th>Score</th><th>Advice</th></tr>
          </thead>
          <tbody id="tbody">
            <tr><td colspan="4" class="muted">Loading…</td></tr>
          </tbody>
        </table>
      </div>

      <p class="muted">Offline server (stdlib). GraphQL not available.</p>
    </div>

    <script>
      const $ = (id) => document.getElementById(id);
      const LS_LIMIT = 'liminal_atrisk_limit';
      const LS_THRESH = 'liminal_atrisk_threshold';

      function savePrefs() {
        localStorage.setItem(LS_LIMIT, String($('#limit').value));
        localStorage.setItem(LS_THRESH, String($('#threshold').value));
      }

      function loadPrefs() {
        const l = localStorage.getItem(LS_LIMIT);
        const t = localStorage.getItem(LS_THRESH);
        if (l) $('#limit').value = l;
        if (t) $('#threshold').value = t;
      }

      async function fetchEdges() {
        const limit = parseInt($('#limit').value || '5');
        const res = await fetch(`/api/top-at-risk?limit=${limit}`);
        const js = await res.json();
        return js.topAtRiskEdges || [];
      }

      function renderRows(rows) {
        const tb = $('#tbody');
        const thr = parseFloat($('#threshold').value || '0.4');
        if (!rows.length) {
          tb.innerHTML = '<tr><td colspan="4" class="muted">No pairs found. Add nodes or seed demo.</td></tr>';
          return;
        }
        tb.innerHTML = rows.map(r => {
          const isRisk = (r.score || 0) < thr;
          const advice = (r.advice || []).join(', ');
          return `<tr class="${isRisk ? 'risk' : ''}"><td>${r.sourceId}</td><td>${r.targetId}</td><td class="score">${(r.score||0).toFixed(3)}</td><td>${advice}</td></tr>`;
        }).join('');
      }

      async function refresh() {
        savePrefs();
        const rows = await fetchEdges();
        renderRows(rows);
      }

      async function seedDemo() {
        await fetch('/api/seed-demo', { method: 'POST' });
        await refresh();
      }

      function parseTraits(str) {
        const obj = {};
        const parts = (str || '').split(',');
        for (const p of parts) {
          const [k0, v0] = p.split(':');
          if (!k0) continue;
          const k = k0.trim();
          const v = parseFloat((v0||'').trim());
          if (!isNaN(v)) obj[k] = v;
        }
        return obj;
      }

      async function addNode() {
        const traits = parseTraits($('#traits').value);
        await fetch('/api/add-node', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ kind: 'module_state', traits })
        });
        $('#traits').value = '';
        await refresh();
      }

      window.addEventListener('DOMContentLoaded', async () => {
        loadPrefs();
        $('#refresh').addEventListener('click', refresh);
        $('#seed').addEventListener('click', seedDemo);
        $('#addnode').addEventListener('click', addNode);
        await refresh();
      });
    </script>
  </body>
</html>
"""


class Handler(BaseHTTPRequestHandler):
    def _send_json(self, obj: Any, code: int = 200) -> None:
        data = json.dumps(obj, ensure_ascii=False).encode("utf-8")
        self.send_response(code)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def _send_html(self, html: str, code: int = 200) -> None:
        data = html.encode("utf-8")
        self.send_response(code)
        self.send_header("Content-Type", "text/html; charset=utf-8")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def do_GET(self) -> None:  # noqa: N802
        parsed = urlparse(self.path)
        qs = parse_qs(parsed.query)
        if parsed.path == "/api/top-at-risk":
            limit = int(qs.get("limit", ["5"])[0])
            edges = compute_top_at_risk(limit)
            self._send_json({"topAtRiskEdges": edges})
            return
        if parsed.path == "/at-risk":
            try:
                threshold = float(os.getenv("LIMINAL_HEALTH_THRESHOLD", "0.4"))
            except ValueError:
                threshold = 0.4
            limit = int(qs.get("limit", ["5"])[0])
            edges = compute_top_at_risk(limit)
            rows = []
            for r in edges:
                advice = ", ".join(r.get("advice", []))
                rows.append(
                    f"<tr><td>{r['sourceId']}</td><td>{r['targetId']}</td><td>{r['score']:.3f}</td><td>{advice}</td></tr>"
                )
            html = HTML_TEMPLATE.format(
                limit=limit,
                threshold=threshold,
                rows=(
                    "".join(rows)
                    if rows
                    else '<tr><td colspan="4">No pairs found. Seed demo first.</td></tr>'
                ),
            )
            self._send_html(html)
            return
        # default
        self.send_response(404)
        self.end_headers()

    def do_POST(self) -> None:  # noqa: N802
        parsed = urlparse(self.path)
        if parsed.path == "/api/seed-demo":
            WEB._nodes.clear()
            WEB._edges.clear()
            a = WEB.add_node(kind="module_state", traits={"любовь": 0.8})
            b = WEB.add_node(kind="module_state", traits={"страх": 0.9})
            c = WEB.add_node(kind="module_state", traits={"спокойствие": 0.7})
            WEB.link_parent(parent=a, child=c)
            self._send_json({"created": [a.id, b.id, c.id]})
            return
        if parsed.path == "/api/add-node":
            length = int(self.headers.get("Content-Length", "0"))
            raw = self.rfile.read(length) if length > 0 else b"{}"
            try:
                payload = json.loads(raw.decode("utf-8"))
            except Exception:
                payload = {}
            kind = str(payload.get("kind", "module_state"))
            traits = payload.get("traits") or {}
            notes = payload.get("notes") or []
            node_id = payload.get("id")
            n = WEB.add_node(kind=kind, traits=traits, notes=notes, id=node_id)
            self._send_json(
                {"id": n.id, "kind": n.kind, "traits": n.traits, "notes": n.notes}
            )
            return
        self.send_response(404)
        self.end_headers()


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Offline stdlib server for at-risk edges (no external deps)"
    )
    parser.add_argument("--port", type=int, default=8000)
    args = parser.parse_args()
    httpd = HTTPServer(("127.0.0.1", args.port), Handler)
    print(
        f"Offline at-risk server running on http://127.0.0.1:{args.port}  (endpoints: /at-risk, /api/top-at-risk, /api/seed-demo)"
    )
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        httpd.server_close()


if __name__ == "__main__":
    main()
