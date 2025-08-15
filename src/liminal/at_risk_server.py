from __future__ import annotations

import os
from typing import Any, List

try:
    from starlette.applications import Starlette
    from starlette.responses import HTMLResponse, JSONResponse, PlainTextResponse
    from starlette.routing import Mount, Route
except Exception as e:  # pragma: no cover
    raise SystemExit(f"Starlette is required for this server: {e}")

from .diffusion import InMemoryDiffusion, ModuleState
from .reality_web import RealityWebInMemory, SystemBreath

# graphql_app (ASGI) может отсутствовать — делаем импорт опциональным и оффлайн-совместимым
try:  # pragma: no cover
    from .graphql_schema import graphql_app as _graphql_app  # type: ignore
except Exception:  # pragma: no cover
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

    raw_score = base + bonus - penalty
    score = _clip01(raw_score)

    rationale = [
        f"base_similarity={base:.3f}",
        f"pos_avg={pos_avg:.3f}*{POS_W:.2f} -> +{bonus:.3f}",
        f"neg_avg={neg_avg:.3f}*{NEG_W:.2f} -> -{penalty:.3f}",
        f"score_clipped={score:.3f}",
    ]
    return score, rationale


def _compute_top_at_risk(limit: int) -> List[dict[str, Any]]:
    nodes = WEB.nodes()
    edges: List[dict[str, Any]] = []
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

    # lowest first
    edges.sort(key=lambda e: e["score"])  # type: ignore[index]
    return edges[: max(0, int(limit))]


async def at_risk(request) -> HTMLResponse:
    # Read limit from query params
    try:
        limit = int(request.query_params.get("limit", "5"))
    except ValueError:
        limit = 5

    try:
        edges: List[dict[str, Any]] = _compute_top_at_risk(limit)
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
          :root {{ --accent: #5b7cff; --danger: #d32f2f; --bg: #0b1020; --card: #121a2e; --muted: #94a3b8; }}
          * {{ box-sizing: border-box; }}
          body {{ font-family: system-ui, -apple-system, Segoe UI, Roboto, Arial, sans-serif; margin: 0; background: #0f172a; color: #e2e8f0; }}
          .wrap {{ max-width: 1080px; margin: 24px auto; padding: 0 16px; }}
          h1 {{ margin: 8px 0 16px; font-weight: 700; letter-spacing: 0.3px; }}
          .controls {{ display: flex; flex-wrap: wrap; gap: 10px; align-items: center; margin-bottom: 16px; }}
          .controls .group {{ display: flex; gap: 8px; align-items: center; background: #0b1224; padding: 8px 10px; border-radius: 8px; border: 1px solid #1f2a44; }}
          label {{ color: var(--muted); font-size: 14px; }}
          input[type=number] {{ width: 90px; background: #0a1326; color: #e2e8f0; border: 1px solid #1f2a44; padding: 6px 8px; border-radius: 6px; }}
          input[type=text] {{ width: 220px; background: #0a1326; color: #e2e8f0; border: 1px solid #1f2a44; padding: 6px 8px; border-radius: 6px; }}
          button {{ background: linear-gradient(135deg, #3b82f6, #6366f1); border: 0; color: white; padding: 8px 12px; border-radius: 8px; cursor: pointer; font-weight: 600; box-shadow: 0 4px 14px rgba(59,130,246,0.35); }}
          button.secondary {{ background: #15213b; border: 1px solid #243b64; color: #cbd5e1; box-shadow: none; }}
          button.danger {{ background: linear-gradient(135deg, #ef4444, #dc2626); box-shadow: 0 4px 14px rgba(239,68,68,0.35); }}
          .row {{ display: grid; grid-template-columns: 1fr; gap: 14px; }}
          .card {{ background: #0b1224; border: 1px solid #1f2a44; border-radius: 12px; overflow: hidden; }}
          .card h3 {{ margin: 0; padding: 12px 14px; background: #0d1731; border-bottom: 1px solid #1f2a44; font-size: 15px; color: #cbd5e1; }}
          table {{ border-collapse: collapse; width: 100%; }}
          th, td {{ border-bottom: 1px solid #1e293b; padding: 10px 12px; font-size: 14px; }}
          th {{ text-align: left; color: #94a3b8; background: #0b1224; position: sticky; top: 0; }}
          tr:hover td {{ background: #0d1731; }}
          .score {{ font-variant-numeric: tabular-nums; }}
          .risk td {{ background: rgba(211,47,47,0.10); }}
          .risk .score {{ color: #ff6b6b; font-weight: 700; }}
          .muted {{ color: #94a3b8; font-size: 13px; }}
          .form-inline {{ display: flex; flex-wrap: wrap; gap: 8px; align-items: center; padding: 10px 12px; }}
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

          <p class='muted'>GraphQL Playground is {('available at <a href=\'/graphql\'>/graphql</a>') if _graphql_app is not None else 'not available in offline mode'}.</p>
        </div>

        <script>
          const $ = (id) => document.getElementById(id);
          const LS_LIMIT = 'liminal_atrisk_limit';
          const LS_THRESH = 'liminal_atrisk_threshold';
          const LS_SORT = 'liminal_atrisk_sort';

          function savePrefs() {{
            localStorage.setItem(LS_LIMIT, String($('#limit').value));
            localStorage.setItem(LS_THRESH, String($('#threshold').value));
            localStorage.setItem(LS_SORT, String($('#sort').value));
          }}

          function loadPrefs() {{
            const l = localStorage.getItem(LS_LIMIT);
            const t = localStorage.getItem(LS_THRESH);
            if (l) $('#limit').value = l;
            if (t) $('#threshold').value = t;
            const s = localStorage.getItem(LS_SORT);
            if (s) $('#sort').value = s;
          }}

          async function fetchEdges() {{
            const limit = parseInt($('#limit').value || '5');
            const res = await fetch(`/api/top-at-risk?limit=${{limit}}`);
            const js = await res.json();
            return js.topAtRiskEdges || [];
          }}

          function renderRows(rows) {{
            const tb = $('#tbody');
            const thr = parseFloat($('#threshold').value || '0.4');
            const sort = ($('#sort').value || 'asc');
            const sorted = [...rows].sort((a,b)=> (a.score||0)-(b.score||0));
            if (sort === 'desc') sorted.reverse();
            if (!sorted.length) {{
              tb.innerHTML = '<tr><td colspan="4" class="muted">No pairs found. Add nodes or seed demo.</td></tr>';
              return;
            }}
            tb.innerHTML = sorted.map(r => {{
              const isRisk = (r.score || 0) < thr;
              const advice = (r.advice || []).join(', ');
              return `<tr class="${{isRisk ? 'risk' : ''}}"><td>${{r.sourceId}}</td><td>${{r.targetId}}</td><td class="score">${{(r.score||0).toFixed(3)}}</td><td>${{advice}}</td></tr>`;
            }}).join('');
          }}

          async function refresh() {{
            savePrefs();
            const rows = await fetchEdges();
            renderRows(rows);
          }}

          async function seedDemo() {{
            await fetch('/api/seed-demo', {{ method: 'POST' }});
            await refresh();
          }}

          function parseTraits(str) {{
            const obj = {{}};
            const parts = (str || '').split(',');
            for (const p of parts) {{
              const [k0, v0] = p.split(':');
              if (!k0) continue;
              const k = k0.trim();
              const v = parseFloat((v0||'').trim());
              if (!isNaN(v)) obj[k] = v;
            }}
            return obj;
          }}

          async function addNode() {{
            const traits = parseTraits($('#traits').value);
            await fetch('/api/add-node', {{
              method: 'POST',
              headers: {{ 'Content-Type': 'application/json' }},
              body: JSON.stringify({{ kind: 'module_state', traits }})
            }});
            $('#traits').value = '';
            await refresh();
          }}

          function download(filename, text) {{
            const blob = new Blob([text], {{type: 'text/plain;charset=utf-8'}});
            const url = URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url; a.download = filename; a.click();
            URL.revokeObjectURL(url);
          }}

          async function exportJson() {{
            const rows = await fetchEdges();
            download('top-at-risk.json', JSON.stringify(rows, null, 2));
          }}

          function toCsv(rows) {{
            const header = ['sourceId','targetId','score','advice'];
            const lines = [header.join(',')];
            for (const r of rows) {{
              const adv = (r.advice||[]).join(' ');
              lines.push([r.sourceId, r.targetId, (r.score||0).toFixed(3), '"'+adv.replaceAll('"','""')+'"'].join(','));
            }}
            return lines.join('\n');
          }}

          async function exportCsv() {{
            const rows = await fetchEdges();
            download('top-at-risk.csv', toCsv(rows));
          }}

          // Events
          window.addEventListener('DOMContentLoaded', async () => {{
            loadPrefs();
            $('#refresh').addEventListener('click', refresh);
            $('#seed').addEventListener('click', seedDemo);
            $('#addnode').addEventListener('click', addNode);
            await refresh();
          }});
          $('#exportJson').addEventListener('click', exportJson);
          $('#exportCsv').addEventListener('click', exportCsv);
        </script>
      </body>
    </html>
    """
    # Substitute placeholders
    html = html.replace("__LIMIT__", str(limit))
    html = html.replace("__THRESH__", os.getenv("LIMINAL_HEALTH_THRESHOLD", "0.4"))
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
    return JSONResponse(
        {"id": n.id, "kind": n.kind, "traits": n.traits, "notes": n.notes}
    )


async def api_seed_demo(request) -> JSONResponse:
    """Create a tiny deterministic demo: love vs fear vs calm nodes."""
    WEB._nodes.clear()
    WEB._edges.clear()  # reset demo space only
    a = WEB.add_node(kind="module_state", traits={"любовь": 0.8})
    b = WEB.add_node(kind="module_state", traits={"страх": 0.9})
    c = WEB.add_node(kind="module_state", traits={"спокойствие": 0.7})
    # Create a parent bond to slightly improve one relation (a->c)
    WEB.link_parent(parent=a, child=c)
    return JSONResponse({"created": [a.id, b.id, c.id]})


routes = [
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
