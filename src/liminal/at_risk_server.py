from __future__ import annotations

import os
from typing import Any, Optional

try:
    from starlette.applications import Starlette
    from starlette.responses import (
        HTMLResponse,
        JSONResponse,
        PlainTextResponse,
        RedirectResponse,
        Response,
    )
    from starlette.routing import Mount, Route
    from strawberry.asgi import GraphQL
except Exception as e:  # pragma: no cover
    raise SystemExit(f"Starlette is required for this server: {e}") from e

from .diffusion import InMemoryDiffusion, ModuleState
from .reality_web import RealityWebInMemory, SystemBreath

# graphql_app (ASGI) может отсутствовать — делаем импорт опциональным и оффлайн-совместимым
_graphql_app: Optional[GraphQL]
try:  # pragma: no cover
    from .graphql_schema import graphql_app
    _graphql_app = graphql_app
except ImportError:  # pragma: no cover
    _graphql_app = None


# ---- Minimal in-process state (offline, без GraphQL) ----
WEB = RealityWebInMemory()
BREATH = SystemBreath()


def _clip01(x: float) -> float:
    return 0.0 if x < 0.0 else 1.0 if x > 1.0 else x


def compute_relationship_health(
    a_traits: dict[str, float], b_traits: dict[str, float]
) -> tuple[float, list[str]]:
    """Mirror GraphQL logic without requiring strawberry.

    Returns (score, rationale).
    """
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

    def avg(keys: tuple[str, ...]) -> float:
        vals: list[float] = []
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

    raw_score = base + bonus - penalty
    score = _clip01(raw_score)

    rationale = [
        f"base_similarity={base:.3f}",
        f"pos_avg={pos_avg:.3f}*{POS_W:.2f} -> +{bonus:.3f}",
        f"neg_avg={neg_avg:.3f}*{NEG_W:.2f} -> -{penalty:.3f}",
        f"score_clipped={score:.3f}",
    ]
    return score, rationale


def _compute_top_at_risk(limit: int) -> list[dict[str, Any]]:
    nodes = WEB.nodes()
    edges: list[dict[str, Any]] = []
    try:
        threshold = float(os.getenv("LIMINAL_HEALTH_THRESHOLD", "0.4"))
    except ValueError:
        threshold = 0.4

    # naive all-pairs scoring (small, deterministic)
    for i in range(len(nodes)):
        for j in range(i + 1, len(nodes)):
            a = nodes[i]
            b = nodes[j]
            if a.traits or b.traits:
                score, rationale = compute_relationship_health(a.traits, b.traits)
                advice: list[str] = []
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

    # lowest first
    edges.sort(key=lambda e: e["score"])
    return edges[: max(0, int(limit))]


async def at_risk(request) -> HTMLResponse:
    # Read limit from query params
    try:
        limit = int(request.query_params.get("limit", "5"))
    except ValueError:
        limit = 5

    try:
        _compute_top_at_risk(limit)
    except Exception as e:
        return HTMLResponse(f"<h1>Error</h1><pre>{e}</pre>", status_code=500)

    # Dynamic HTML UI with controls and JS-powered rendering
    # NOTE: avoid f-string here to prevent parser issues with backslashes inside expressions.
    # Use simple placeholders (__LIMIT__, __THRESH__) and replace them below.
    html = """
    <!doctype html>
    <html>
      <head>
         <meta charset='utf-8'/>
         <meta name="viewport" content="width=device-width, initial-scale=1" />
         <title>Top At-Risk Edges</title>
        <style>
          /* Zen-inspired harmonious palette */
          :root {
            --sage: #a7c957;      /* мудрый зелёный */
            --lotus: #f2e9e4;     /* нежный лотос */
            --stone: #4a4e69;     /* камень */
            --water: #9a8c98;     /* вода */
            --earth: #22223b;     /* земля */
            --sky: #f8f5f0;       /* небо */
            --shadow: 0 8px 25px rgba(34, 34, 59, 0.15);
            --glow: 0 0 20px rgba(167, 201, 87, 0.2);
          }

          * { box-sizing: border-box; }

          body {
            font-family: 'Inter', system-ui, -apple-system, sans-serif;
            margin: 0;
            background: linear-gradient(135deg, #f8f5f0 0%, #f2e9e4 100%);
            color: var(--earth);
            line-height: 1.6;
            min-height: 100vh;
          }

          .wrap {
            max-width: 1100px;
            margin: 0 auto;
            padding: 2rem 1.5rem;
          }

          h1 {
            font-size: 2.5rem;
            font-weight: 300;
            margin: 0 0 2rem;
            color: var(--stone);
            text-align: center;
            letter-spacing: 0.5px;
          }

          .controls {
            display: flex;
            flex-wrap: wrap;
            gap: 1rem;
            align-items: center;
            margin-bottom: 2rem;
            padding: 1.5rem;
            background: white;
            border-radius: 16px;
            box-shadow: var(--shadow);
            border: 1px solid rgba(167, 201, 87, 0.1);
          }

          .controls .group {
            display: flex;
            gap: 0.5rem;
            align-items: center;
          }

          label {
            color: var(--water);
            font-size: 0.9rem;
            font-weight: 500;
          }

          input, select {
            padding: 0.6rem 0.8rem;
            border: 2px solid rgba(167, 201, 87, 0.2);
            border-radius: 8px;
            background: white;
            color: var(--earth);
            font-size: 0.9rem;
            transition: all 0.2s ease;
          }

          input:focus, select:focus {
            outline: none;
            border-color: var(--sage);
            box-shadow: var(--glow);
          }

          input[type=number] { width: 80px; }
          input[type=text] { width: 240px; }

          button {
            padding: 0.7rem 1.2rem;
            border: none;
            border-radius: 10px;
            font-weight: 500;
            cursor: pointer;
            transition: all 0.2s ease;
            font-size: 0.9rem;
          }

          button {
            background: linear-gradient(135deg, var(--sage), #8fb339);
            color: white;
            box-shadow: var(--shadow);
          }

          button:hover {
            transform: translateY(-2px);
            box-shadow: 0 12px 35px rgba(167, 201, 87, 0.3);
          }

          button.secondary {
            background: white;
            color: var(--stone);
            border: 2px solid rgba(167, 201, 87, 0.3);
            box-shadow: none;
          }

          button.secondary:hover {
            background: rgba(167, 201, 87, 0.1);
            border-color: var(--sage);
          }

          .card {
            background: white;
            border-radius: 20px;
            overflow: hidden;
            box-shadow: var(--shadow);
            border: 1px solid rgba(167, 201, 87, 0.1);
          }

          .card h3 {
            margin: 0;
            padding: 1.2rem 1.5rem;
            background: linear-gradient(135deg, #f8f5f0, #f2e9e4);
            border-bottom: 1px solid rgba(167, 201, 87, 0.2);
            color: var(--stone);
            font-weight: 600;
          }

          table {
            width: 100%;
            border-collapse: collapse;
          }

          th, td {
            padding: 1rem 1.2rem;
            text-align: left;
            border-bottom: 1px solid rgba(167, 201, 87, 0.1);
          }

          th {
            background: rgba(248, 245, 240, 0.8);
            color: var(--water);
            font-weight: 600;
            font-size: 0.9rem;
            text-transform: uppercase;
            letter-spacing: 0.5px;
          }

          tbody tr:hover td {
            background: rgba(167, 201, 87, 0.05);
          }

          .risk td {
            background: linear-gradient(90deg, rgba(231, 111, 81, 0.1), transparent);
          }

          .risk .score {
            color: #e76f51;
            font-weight: 700;
          }

          .score {
            font-variant-numeric: tabular-nums;
            font-weight: 600;
          }

          .muted {
            color: var(--water);
            font-size: 0.85rem;
          }

          .form-inline {
            display: flex;
            flex-wrap: wrap;
            gap: 1rem;
            align-items: center;
            padding: 1.5rem;
            background: rgba(248, 245, 240, 0.5);
          }

          /* Медитативные тени */
          .card, .controls {
            position: relative;
          }

          .card::before {
            content: '';
            position: absolute;
            top: -1px;
            left: -1px;
            right: -1px;
            bottom: -1px;
            background: linear-gradient(45deg, transparent, rgba(167, 201, 87, 0.1), transparent);
            border-radius: 20px;
            z-index: -1;
          }
        </style>
          </style>
      </head>
      <body>
        <div class="wrap">
          <h1>Top At-Risk Edges</h1>

          <div class="controls">
            <div class="group">
              <label for="limit">limit</label>
              <input id="limit" type="number" min="1" value="__LIMIT__" />
            </div>
            <div class="group">
              <label for="threshold">threshold</label>
              <input id="threshold" type="number" step="0.01" min="0" max="1" value="__THRESH__" />
            </div>
            <div class="group">
              <label for="sort">sort</label>
              <select id="sort" style="background:#0a1326;color:#e2e8f0;border:1px solid #1f2a44;padding:6px 8px;border-radius:6px;">
                <option value="asc">score ↑</option>
                <option value="desc">score ↓</option>
              </select>
            </div>
            <button id="refresh" class="secondary">Refresh</button>
            <button id="seed" class="secondary">Seed demo</button>
            <button id="exportJson" class="secondary">Export JSON</button>
            <button id="exportCsv" class="secondary">Export CSV</button>
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

          <p class='muted'>GraphQL Playground is __GRAPHQL_STATUS__.</p>
        </div>

        <script>
          const $ = (id) => document.getElementById(id);
          const LS_LIMIT = 'liminal_atrisk_limit';
          const LS_THRESH = 'liminal_atrisk_threshold';
          const LS_SORT = 'liminal_atrisk_sort';

          function savePrefs() {
            localStorage.setItem(LS_LIMIT, String($('#limit').value));
            localStorage.setItem(LS_THRESH, String($('#threshold').value));
            localStorage.setItem(LS_SORT, String($('#sort').value));
          }

          function loadPrefs() {
            const l = localStorage.getItem(LS_LIMIT);
            const t = localStorage.getItem(LS_THRESH);
            if (l) $('#limit').value = l;
            if (t) $('#threshold').value = t;
            const s = localStorage.getItem(LS_SORT);
            if (s) $('#sort').value = s;
          }

          async function fetchEdges() {
            const limit = parseInt($('#limit').value || '5');
            const res = await fetch('/api/top-at-risk?limit=' + limit);
            const js = await res.json();
            return js.topAtRiskEdges || [];
          }

          function renderRows(rows) {
            const tb = $('#tbody');
            const thr = parseFloat($('#threshold').value || '0.4');
            const sort = ($('#sort').value || 'asc');
            const sorted = [...rows].sort((a,b)=> (a.score||0)-(b.score||0));
            if (sort === 'desc') sorted.reverse();
            if (!sorted.length) {
              tb.innerHTML = '<tr><td colspan="4" class="muted">No pairs found. Add nodes or seed demo.</td></tr>';
              return;
            }
            tb.innerHTML = sorted.map(function(r) {
              var isRisk = (r.score || 0) < thr;
              var advice = (r.advice || []).join(', ');
              var riskClass = isRisk ? 'risk' : '';
              var score = (r.score || 0).toFixed(3);
              return '<tr class="' + riskClass + '"><td>' + r.sourceId + '</td><td>' + r.targetId + '</td><td class="score">' + score + '</td><td>' + advice + '</td></tr>';
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
            var obj = {};
            var parts = (str || '').split(',');
            for (var i = 0; i < parts.length; i++) {
              var p = parts[i];
              var colonIndex = p.indexOf(':');
              if (colonIndex === -1) continue;
              var k0 = p.substring(0, colonIndex);
              var v0 = p.substring(colonIndex + 1);
              if (!k0) continue;
              var k = k0.trim();
              var v = parseFloat((v0 || '').trim());
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

          function download(filename, text) {
            const blob = new Blob([text], {type: 'text/plain;charset=utf-8'});
            const url = URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url; a.download = filename; a.click();
            URL.revokeObjectURL(url);
          }

          async function exportJson() {
            const rows = await fetchEdges();
            download('top-at-risk.json', JSON.stringify(rows, null, 2));
          }

          function toCsv(rows) {
            const header = ['sourceId','targetId','score','advice'];
            const lines = [header.join(',')];
            for (var i = 0; i < rows.length; i++) {
              var r = rows[i];
              var adv = (r.advice || []).join(' ');
              var advEscaped = adv.replace(/"/g, '""');
              lines.push([r.sourceId, r.targetId, (r.score || 0).toFixed(3), '"' + advEscaped + '"'].join(','));
            }
            return lines.join('\n');
          }

          async function exportCsv() {
            const rows = await fetchEdges();
            download('top-at-risk.csv', toCsv(rows));
          }

          // Events
          window.addEventListener('DOMContentLoaded', async () => {
            loadPrefs();
            $('#refresh').addEventListener('click', refresh);
            $('#seed').addEventListener('click', seedDemo);
            $('#addnode').addEventListener('click', addNode);
            await refresh();
          });
          $('#exportJson').addEventListener('click', exportJson);
          $('#exportCsv').addEventListener('click', exportCsv);
        </script>
      </body>
    </html>
    """
    # Substitute placeholders
    html = html.replace("__LIMIT__", str(limit))
    html = html.replace("__THRESH__", os.getenv("LIMINAL_HEALTH_THRESHOLD", "0.4"))

    # GraphQL status message
    if _graphql_app is not None:
        graphql_status = 'available at <a href="/graphql">/graphql</a>'
    else:
        graphql_status = "not available in offline mode"
    html = html.replace("__GRAPHQL_STATUS__", graphql_status)
    return HTMLResponse(html)


async def api_top_at_risk(request) -> JSONResponse:
    try:
        limit = int(request.query_params.get("limit", "5"))
    except ValueError:
        limit = 5
    edges = _compute_top_at_risk(limit)
    return JSONResponse({"topAtRiskEdges": edges})


async def api_add_node(request) -> JSONResponse:
    """Add a node: POST JSON {"kind":"module_state","traits": {..}, "notes": [..], "id": "optional"}"""
    try:
        payload = await request.json()
    except Exception:
        payload = {}
    kind = str(payload.get("kind", "module_state"))
    traits = payload.get("traits") or {}
    notes = payload.get("notes") or []
    node_id = payload.get("id")
    n = WEB.add_node(kind=kind, traits=traits, notes=notes, id=node_id)
    return JSONResponse({"id": n.id, "kind": n.kind, "traits": n.traits, "notes": n.notes})


async def api_seed_demo(request) -> JSONResponse:
    """Create a tiny deterministic demo: love vs fear vs calm nodes."""
    WEB.clear()  # Clear nodes and edges
    a = WEB.add_node(kind="module_state", traits={"любовь": 0.8})
    b = WEB.add_node(kind="module_state", traits={"страх": 0.9})
    c = WEB.add_node(kind="module_state", traits={"спокойствие": 0.7})
    # Create a parent bond to slightly improve one relation (a->c)
    WEB.link_parent(parent=a, child=c)
    return JSONResponse({"created": [a.id, b.id, c.id]})


async def favicon(request):
    """Simple favicon response to avoid 404"""
    # Return a simple 1x1 transparent PNG as favicon
    import base64

    tiny_png = base64.b64decode(
        "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg=="
    )
    return Response(tiny_png, media_type="image/png")


routes = [
    # Redirect root to main UI
    Route("/", lambda req: RedirectResponse(url="/at-risk", status_code=307)),
    Route("/favicon.ico", favicon),
    Route("/at-risk", at_risk),
    Route("/api/top-at-risk", api_top_at_risk),
    Route("/api/add-node", api_add_node, methods=["POST"]),
    Route("/api/seed-demo", api_seed_demo, methods=["POST"]),
    (
        Mount("/graphql", app=_graphql_app)
        if _graphql_app is not None
        else Route(
            "/graphql",
            lambda req: PlainTextResponse("GraphQL app not available", status_code=503),
        )
    ),
]

app = Starlette(debug=True, routes=routes)

# For uvicorn: uvicorn liminal.at_risk_server:app --reload --port 8000
