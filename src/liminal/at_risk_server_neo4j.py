from __future__ import annotations

import json
import logging
import os
from typing import Any, List, Optional

try:
    from starlette.applications import Starlette
    from starlette.responses import (HTMLResponse, JSONResponse,
                                     PlainTextResponse)
    from starlette.routing import Mount, Route
except Exception as e:  # pragma: no cover
    raise SystemExit(f"Starlette is required for this server: {e}")

from .diffusion import InMemoryDiffusion, ModuleState
from .reality_web import RealityWebInMemory, SystemBreath
from .reality_web_neo4j import RealityWebNeo4j

# Setup logger
logger = logging.getLogger("at_risk_server")

# graphql_app (ASGI) может отсутствовать — делаем импорт опциональным и оффлайн-совместимым
try:  # pragma: no cover
    from .graphql_schema import graphql_app as _graphql_app  # type: ignore
except Exception:  # pragma: no cover
    _graphql_app = None


# ---- Server configuration ----
USE_NEO4J = os.getenv("LIMINAL_USE_NEO4J", "0") == "1"
NEO4J_URI = os.getenv("LIMINAL_NEO4J_URI", "bolt://localhost:7687")
NEO4J_USER = os.getenv("LIMINAL_NEO4J_USER", "neo4j")
NEO4J_PASSWORD = os.getenv("LIMINAL_NEO4J_PASSWORD", "NewStrongPass123!")

# ---- State initialization ----
NEO4J_INIT_ERROR: Optional[str] = None
if USE_NEO4J:
    try:
        logger.info(f"Initializing Neo4j reality web with URI: {NEO4J_URI}")
        WEB = RealityWebNeo4j(uri=NEO4J_URI, user=NEO4J_USER, password=NEO4J_PASSWORD)
        logger.info("Successfully connected to Neo4j database")
    except Exception as e:
        NEO4J_INIT_ERROR = str(e)
        logger.error(f"Failed to initialize Neo4j reality web: {NEO4J_INIT_ERROR}")
        # Do NOT silently flip to Neo4j=false; keep server running with in-memory for availability,
        # but surface the error in the UI and status endpoint.
        WEB = RealityWebInMemory()
else:
    # Minimal in-process state (offline, без GraphQL)
    logger.info("Using in-memory reality web")
    WEB = RealityWebInMemory()

# Initialize system breath
BREATH = SystemBreath()


# ---- Helper functions ----
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
    try:
        threshold = float(os.getenv("LIMINAL_HEALTH_THRESHOLD", "0.4"))
    except ValueError:
        threshold = 0.4

    # Use Neo4j implementation if available
    if isinstance(WEB, RealityWebNeo4j):
        try:
            return WEB.top_at_risk(limit=limit, threshold=threshold)
        except Exception as e:
            logger.error(f"Error using Neo4j top_at_risk: {e}")
            logger.info("Falling back to standard calculation")

    # Standard calculation
    nodes = WEB.nodes()
    edges: List[dict[str, Any]] = []

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

    # Get the database type for display
    db_type = "Neo4j" if isinstance(WEB, RealityWebNeo4j) else "In-Memory"

    # Dynamic HTML UI with controls and JS-powered rendering
    error_banner = ""
    if NEO4J_INIT_ERROR:
        # Show explicit error if Neo4j was requested but failed
        error_safe = NEO4J_INIT_ERROR.replace("<", "&lt;").replace(">", "&gt;")
        error_banner = f"<div style='background:#fff3cd;color:#856404;padding:10px;border:1px solid #ffeeba;margin-bottom:10px;'>Neo4j init error: {error_safe}</div>"

    html = f"""
    <!DOCTYPE html>
    <html lang="en">
      <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>At-Risk Emotional Connections</title>
        <style>
          :root {{
            --brand-primary: #4f46e5; /* indigo-600 */
            --brand-primary-dark: #4338ca; /* indigo-700 */
            --brand-accent: #22c55e; /* green-500 */
            --text-primary: #1f2937; /* gray-800 */
            --text-secondary: #6b7280; /* gray-500 */
            --card-bg: #ffffff;
            --bg-grad-1: #f8fafc; /* slate-50 */
            --bg-grad-2: #eef2ff; /* indigo-50 */
          }}
          body {{
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            line-height: 1.6;
            color: var(--text-primary);
            max-width: 1200px;
            margin: 0 auto;
            padding: 20px;
            background: radial-gradient(1200px 600px at 10% 0%, var(--bg-grad-2), transparent),
                        linear-gradient(180deg, var(--bg-grad-1), #ffffff);
          }}
          h1 {{
            color: #2c3e50;
            border-bottom: 2px solid #3498db;
            padding-bottom: 10px;
          }}
          .controls {{
            display: flex;
            flex-wrap: wrap;
            gap: 10px;
            align-items: center;
          }}
          button {{
            background: var(--brand-primary);
            color: #fff;
            border: none;
            padding: 10px 16px;
            border-radius: 999px; /* pill */
            cursor: pointer;
            transition: background 0.2s ease, transform 0.05s ease;
            box-shadow: 0 6px 14px rgba(79, 70, 229, 0.18);
          }}
          button:hover {{ background: var(--brand-primary-dark); }}
          button:active {{ transform: translateY(1px); }}
          .btn-secondary {{ background: #e5e7eb; color: #111827; box-shadow: none; }}
          .btn-success {{ background: var(--brand-accent); box-shadow: 0 6px 14px rgba(34,197,94,0.18); }}
          /* Simple Mode scaling */
          body.simple button {{
            font-size: 18px;
            padding: 14px 20px;
            border-radius: 12px;
          }}
          label {{
            margin-right: 5px;
          }}
          .row {{
            background: var(--card-bg);
            padding: 15px;
            margin-bottom: 10px;
            border-radius: 14px;
            box-shadow: 0 8px 24px rgba(2, 6, 23, 0.08);
            display: flex;
            flex-wrap: wrap;
            gap: 15px;
            justify-content: space-between;
          }}
          .row .icon {{
            font-size: 28px;
            margin-right: 10px;
          }}
          body.simple .row {{
            padding: 18px;
            border-radius: 14px;
          }}
          body.simple .row .icon {{
            font-size: 36px;
          }}
          .danger {{ border-left: 6px solid #ef4444; }}
          .warning {{ border-left: 6px solid #f59e0b; }}
          .good {{ border-left: 6px solid #10b981; }}
          .badge {{ display:inline-block; padding:4px 10px; border-radius:999px; font-size:12px; }}
          .badge.danger {{ background:#fee2e2; color:#991b1b; }}
          .badge.warning {{ background:#fef3c7; color:#92400e; }}
          .badge.good {{ background:#d1fae5; color:#065f46; }}
          .export-btns {{
            margin-top: 10px;
            display: flex;
            gap: 10px;
          }}
          .db-type {{
            background: #2c3e50;
            color: white;
            padding: 5px 10px;
            border-radius: 4px;
            margin-left: 10px;
            font-size: 0.8em;
          }}
          /* Sales trigger chips */
          .chips {{ display:flex; flex-wrap:wrap; gap:8px; }}
          .chip {{ padding:8px 12px; border-radius:999px; background:#eef2ff; color:#3730a3; cursor:pointer; user-select:none; border:1px solid #e0e7ff; }}
          .chip.active {{ background:#c7d2fe; border-color:#a5b4fc; }}
          /* Toast for short status messages */
          #toast {{
            position: fixed;
            bottom: 20px;
            right: 20px;
            background: rgba(17, 24, 39, 0.92);
            color: #fff;
            padding: 10px 14px;
            border-radius: 6px;
            box-shadow: 0 2px 8px rgba(0,0,0,0.2);
            opacity: 0;
            transform: translateY(8px);
            transition: opacity 0.25s ease, transform 0.25s ease;
            pointer-events: none;
            z-index: 9999;
            font-size: 14px;
          }}
          #toast.show {{
            opacity: 1;
            transform: translateY(0);
          }}
          /* Onboarding banner */
          #onboarding {{
            display: none;
            background: #eef9ff;
            border: 1px solid #bde5ff;
            color: #1b5e87;
            padding: 12px 14px;
            border-radius: 8px;
            margin: 10px 0 20px;
          }}
          #onboarding.show {{ display: block; }}
          #onboarding .actions {{ margin-top: 8px; }}
          #onboarding button {{ background: #2ecc71; }}
          /* Navbar */
          .navbar {{ display:flex; align-items:center; justify-content:space-between; padding:12px 16px; background:#fff; border-radius:12px; box-shadow:0 6px 20px rgba(2,6,23,0.06); margin-bottom:16px; }}
          .brand {{ display:flex; align-items:center; gap:10px; font-weight:700; color: var(--brand-primary); }}
          .brand .logo {{ width:28px; height:28px; display:inline-flex; align-items:center; justify-content:center; background:var(--brand-primary); color:#fff; border-radius:8px; font-size:16px; }}
          .nav-actions {{ display:flex; gap:8px; }}
          /* Cards grid */
          #results {{ display:grid; grid-template-columns: repeat(auto-fit, minmax(280px, 1fr)); gap:14px; }}
        </style>
      </head>
      <body>
        <header class="navbar" role="navigation" aria-label="Top Navigation">
          <div class="brand"><span class="logo">L</span> Liminal At‑Risk</div>
          <div class="nav-actions">
            <button class="btn-secondary" onclick="location.href='/at-risk'" title="Home">Home</button>
          </div>
        </header>
        <h1>At-Risk Emotional Connections <span class="db-type">{db_type}</span></h1>
        {error_banner}
        
        <div class="controls">
          <button id="refresh" title="Refresh data / Обновить данные — получить актуальные пары риска с сервера">Refresh Data</button>
          <button id="seed" title="Seed demo / Засеять демо — создать несколько примерных узлов (любовь/страх/спокойствие)">Seed Demo</button>
          <button id="toggle" title="Toggle Neo4j / Переключить Neo4j — сменить режим между Neo4j и встроенной памятью">Toggle Neo4j</button>
          <button id="reconnect" title="Reconnect / Переподключить — заново установить соединение с Neo4j без смены режима">Reconnect</button>
          <button id="simpleMode" title="Simple Mode / Простой режим — крупные кнопки, простые подсказки">Simple Mode: Off</button>
          <span id="status" style="margin-left:8px;font-size:0.9em;color:#666;">status: ...</span>
          
          <!-- Sales triggers (Samcart-like) -->
          <div class="chips" id="triggerChips" aria-label="Sales Triggers"></div>
          <div class="advanced">
            <label for="limit">Limit:</label>
            <input id="limit" type="number" min="1" max="20" value="5">
          </div>
          <div class="advanced">
            <label for="threshold">Threshold:</label>
            <input id="threshold" type="number" min="0" max="1" step="0.05" value="0.4">
          </div>
          <div class="export-btns advanced">
            <button id="exportJson" title="Export JSON / Экспорт JSON — выгрузить текущий список рисковых связей в файл JSON">Export JSON</button>
            <button id="exportCsv" title="Export CSV / Экспорт CSV — выгрузить текущий список рисковых связей в CSV">Export CSV</button>
          </div>
        </div>
        
        <div id="onboarding" aria-live="polite">
          <strong>🎈 Simple Mode:</strong> Big friendly buttons, easy tips.
          <div>Tap Seed to create example feelings. Tap Refresh to see connections. Red cards mean “needs care” 💔, green are “doing well” 💚.</div>
          <div class="actions"><button id="hideOnboarding">Got it!</button></div>
        </div>

        <div id="add-node" class="controls advanced">
          <h3>Add Node</h3>
          <div>
            <label for="traits">Traits JSON:</label>
            <input id="traits" style="width:50%" placeholder='{{"любовь": 0.8, "спокойствие": 0.5}}'>
          </div>
          <button id="addnode" title="Add Node / Добавить узел — создаёт узел с указанными признаками (traits)">Add Node</button>
        </div>
        
        <div id="results"></div>
        <div id="toast" role="status" aria-live="polite"></div>
        
        <script>
          const $ = document.querySelector.bind(document);
          
          async function updateStatus() {{
            try {{
              const st = await (await fetch('/api/neo4j/status')).json();
              const el = $('#status');
              el.textContent = `status: ${{st.status || 'unknown'}}, using_neo4j=${{st.using_neo4j}} nodes=${{st.node_count}} edges=${{st.edge_count}}`;
            }} catch (e) {{
              console.error('status error', e);
            }}
          }}

          // Save preferences to localStorage
          function savePrefs() {{
            const prefs = {{
              limit: $('#limit').value,
              threshold: $('#threshold').value,
              simple_mode: document.body.classList.contains('simple'),
              triggers: getSelectedTriggers()
            }};
            localStorage.setItem('at_risk_prefs', JSON.stringify(prefs));
          }}
          
          // Load preferences from localStorage
          function loadPrefs() {{
            try {{
              const prefs = JSON.parse(localStorage.getItem('at_risk_prefs')) || {{}};
              if (prefs.limit) $('#limit').value = prefs.limit;
              if (prefs.threshold) $('#threshold').value = prefs.threshold;
              applySimpleMode(!!prefs.simple_mode, /*skipToast*/ true);
              renderTriggerChips(prefs.triggers || []);
            }} catch (e) {{
              console.error('Error loading prefs', e);
            }}
          }}

          // --- Sales Triggers helpers (frontend-only) ---
          const SALES_TRIGGERS = [
            {{ id:'urgency', label:'Срочность ⏳' }},
            {{ id:'scarcity', label:'Дефицит 📉' }},
            {{ id:'social', label:'Соц.доказательство 👥' }},
            {{ id:'guarantee', label:'Гарантия ✅' }},
            {{ id:'bonus', label:'Бонус 🎁' }},
            {{ id:'discount', label:'Скидка 💸' }},
          ];
          function getSelectedTriggers() {{
            return Array.from(document.querySelectorAll('.chip.active')).map(ch=>ch.dataset.id);
          }}
          function renderTriggerChips(selected) {{
            const cont = document.getElementById('triggerChips');
            if (!cont) return;
            cont.innerHTML = '';
            SALES_TRIGGERS.forEach(t => {{
              const el = document.createElement('div');
              el.className = 'chip' + (selected.includes(t.id)?' active':'');
              el.textContent = t.label;
              el.dataset.id = t.id;
              el.role = 'button';
              el.tabIndex = 0;
              el.addEventListener('click', () => {{ el.classList.toggle('active'); savePrefs(); refresh(); }});
              el.addEventListener('keypress', (e)=>{{ if(e.key==='Enter'||e.key===' ') {{ e.preventDefault(); el.click(); }} }});
              cont.appendChild(el);
            }});
          }}
          function triggerAdvice(selected) {{
            const tips = [];
            if (selected.includes('urgency')) tips.push('Сделай шаг сегодня — маленькое действие сейчас лучше, чем большое завтра.');
            if (selected.includes('scarcity')) tips.push('Редкие тёплые разговоры ценнее — запланируй один уже сейчас.');
            if (selected.includes('social')) tips.push('Другие справлялись — и у тебя получится: начни с простого “привет”.');
            if (selected.includes('guarantee')) tips.push('Это безопасно: 5 минут тишины и глубокого дыхания — мягкий старт.');
            if (selected.includes('bonus')) tips.push('Добавь бонус: маленькая похвала или стикер за смелость.');
            if (selected.includes('discount')) tips.push('Сними «цену входа»: выбери самую лёгкую тему для разговора.');
            return tips;
          }}

          function applySimpleMode(enabled, skipToast=false) {{
            const btn = $('#simpleMode');
            if (enabled) {{
              document.body.classList.add('simple');
              btn.textContent = 'Simple Mode: On';
              const seen = localStorage.getItem('at_risk_onboarding_seen') === '1';
              if (!seen) {{
                $('#onboarding').classList.add('show');
              }}
              if (!skipToast) showToast('Simple Mode: ON / Простой режим: ВКЛ');
            }} else {{
              document.body.classList.remove('simple');
              btn.textContent = 'Simple Mode: Off';
              $('#onboarding').classList.remove('show');
              if (!skipToast) showToast('Simple Mode: OFF / Простой режим: ВЫКЛ');
            }}
            savePrefs();
          }}
          
          async function fetchEdges() {{
            const limit = $('#limit').value || 5;
            savePrefs();
            const response = await fetch(`/api/top-at-risk?limit=${{limit}}`);
            const data = await response.json();
            return data.topAtRiskEdges || [];
          }}
          
          async function refresh() {{
            const results = $('#results');
            results.innerHTML = '<p>Loading...</p>';
            
            try {{
              const rows = await fetchEdges();
              if (rows.length === 0) {{
                results.innerHTML = '<p>No results found. Add some nodes and relationships.</p>';
                return;
              }}
              
              let html = '';
              const threshold = parseFloat($('#threshold').value || 0.4);
              const simple = document.body.classList.contains('simple');
              const selectedTriggers = getSelectedTriggers();
              
              for (const row of rows) {{
                const score = row.score || 0;
                let rowClass = 'row good';
                if (score < threshold) rowClass = 'row danger';
                else if (score < threshold + 0.2) rowClass = 'row warning';
                const icon = (score < threshold) ? '💔' : (score < threshold + 0.2 ? '⚠️' : '💚');
                
                const adviceList = (row.advice || []).slice();
                // augment with sales triggers
                adviceList.push(...triggerAdvice(selectedTriggers));
                const advice = adviceList.join(', ');
                const rationale = simple ? '' : (row.rationale || []).map(r => `<li>${{r}}</li>`).join('');
                
                html += `
                  <div class="${{rowClass}}">
                    <div style="display:flex;align-items:center;gap:8px;">
                      <span class="icon" aria-hidden="true">${{icon}}</span>
                      <div>
                        <div><strong>Source:</strong> ${{row.sourceId}}</div>
                        <div><strong>Target:</strong> ${{row.targetId}}</div>
                        <div><span class="badge ${{score < threshold ? 'danger' : (score < threshold + 0.2 ? 'warning' : 'good')}}">Score: ${{score.toFixed(3)}}</span></div>
                      </div>
                    </div>
                    <div>
                      <div><strong>${{simple ? 'Tip' : 'Advice'}}:</strong> ${{advice || 'None'}}</div>
                      ${{selectedTriggers.length ? `<div style="margin-top:6px;display:flex;flex-wrap:wrap;gap:6px;">${{selectedTriggers.map(id=>`<span class='badge'>#${id}</span>`).join('')}}</div>` : ''}}
                      ${{simple ? '' : '<div><strong>Rationale:</strong></div>'}}
                      ${{simple ? '' : `<ul>${{rationale}}</ul>`}}
                    </div>
                  </div>
                `;
              }}
              
              results.innerHTML = html;
            }} catch (error) {{
              results.innerHTML = `<p>Error loading data: ${{error.message}}</p>`;
            }}
          }}
          
          async function seedDemo() {{
            await fetch('/api/seed-demo', {{ method: 'POST' }});
            await refresh();
            await updateStatus();
            showToast('Seed Demo: done / Демоданные: добавлены');
          }}
          
          function parseTraits(input) {{
            try {{
              const parsed = JSON.parse(input);
              // Ensure we always return an object for traits
              if (parsed === null || Array.isArray(parsed) || typeof parsed !== 'object') {{
                return {{ value: parsed }};
              }}
              return parsed;
            }} catch (e) {{
              // If not valid JSON, try to parse as simplified format
              const obj = {{}};
              const pairs = input.split(',');
              for (const pair of pairs) {{
                const [k, v] = pair.trim().split(':');
                if (!k || !v) continue;
                obj[k.trim().replace(/["']/g, '')] = parseFloat(v);
              }}
              // Filter NaN values
              for (const k in obj) {{
                const v = obj[k];
                if (isNaN(v)) obj[k] = v;
              }}
              return obj;
            }}
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
            await updateStatus();
            showToast('Add Node: done / Добавить узел: готово');
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
            showToast('Export JSON: saved / Экспорт JSON: сохранено');
          }}

          function toCsv(rows) {{
            const header = ['sourceId','targetId','score','advice'];
            const lines = [header.join(',')];
            for (const r of rows) {{
              const adv = (r.advice||[]).join(' ');
              lines.push([r.sourceId, r.targetId, (r.score||0).toFixed(3), '"'+adv.replaceAll('"','""')+'"'].join(','));
            }}
            return lines.join('\\n');
          }}

          async function exportCsv() {{
            const rows = await fetchEdges();
            download('top-at-risk.csv', toCsv(rows));
            showToast('Export CSV: saved / Экспорт CSV: сохранено');
          }}

          async function toggleNeo4j() {{
            try {{
              await fetch('/api/neo4j/toggle', {{ method: 'POST' }});
              await updateStatus();
              await refresh();
              showToast('Toggle Neo4j: done / Переключить Neo4j: готово');
            }} catch (e) {{ console.error('toggle error', e); }}
          }}

          async function reconnectNeo4j() {{
            try {{
              const resp = await fetch('/api/neo4j/reconnect', {{ method: 'POST' }});
              const data = await resp.json();
              console.log('reconnect', data);
              await updateStatus();
              await refresh();
              var msg = (data && data.status) ? data.status : 'reconnect done';
              showToast('Reconnect: ' + msg + ' / Переподключение: ' + msg);
            }} catch (e) {{ console.error('reconnect error', e); }}
          }}

          // Simple toast utility (EN/RU friendly)
          let toastTimer = null;
          function showToast(message, timeoutMs = 2000) {{
            const t = $('#toast');
            if (!t) return;
            t.textContent = message;
            t.classList.add('show');
            if (toastTimer) clearTimeout(toastTimer);
            toastTimer = setTimeout(() => {{
              t.classList.remove('show');
            }}, timeoutMs);
          }}

          // Events
          window.addEventListener('DOMContentLoaded', async () => {{
            loadPrefs();
            $('#refresh').addEventListener('click', refresh);
            $('#seed').addEventListener('click', seedDemo);
            $('#toggle').addEventListener('click', toggleNeo4j);
            $('#reconnect').addEventListener('click', reconnectNeo4j);
            $('#simpleMode').addEventListener('click', () => applySimpleMode(!document.body.classList.contains('simple')));
            $('#exportJson').addEventListener('click', exportJson);
            $('#exportCsv').addEventListener('click', exportCsv);
            $('#addnode').addEventListener('click', addNode);
            $('#hideOnboarding').addEventListener('click', () => {{ localStorage.setItem('at_risk_onboarding_seen','1'); $('#onboarding').classList.remove('show'); }});
            loadPrefs();
            updateStatus();
            refresh();
          }});
        </script>
      </body>
    </html>
    """
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
    # Coerce non-dict traits into a dict to avoid TypeError in RealityWeb
    if not isinstance(traits, dict):
        traits = {"value": traits}
    notes = payload.get("notes") or []
    node_id = payload.get("id")
    n = WEB.add_node(kind=kind, traits=traits, notes=notes, id=node_id)
    return JSONResponse(
        {"id": n.id, "kind": n.kind, "traits": n.traits, "notes": n.notes}
    )


async def api_seed_demo(request) -> JSONResponse:
    """Create a tiny deterministic demo: love vs fear vs calm nodes."""
    if isinstance(WEB, RealityWebNeo4j):
        # For Neo4j, we need to clean all nodes
        try:
            with WEB.driver.session() as session:
                session.run("MATCH (n) DETACH DELETE n")
        except Exception as e:
            logger.error(f"Error clearing Neo4j database: {e}")
    else:
        # For in-memory, just clear the collections
        WEB._nodes.clear()
        WEB._edges.clear()

    # Create demo nodes
    a = WEB.add_node(kind="module_state", traits={"любовь": 0.8})
    b = WEB.add_node(kind="module_state", traits={"страх": 0.9})
    c = WEB.add_node(kind="module_state", traits={"спокойствие": 0.7})

    # Create a parent bond to slightly improve one relation (a->c)
    WEB.link_parent(parent=a, child=c)

    return JSONResponse({"created": [a.id, b.id, c.id]})


async def api_neo4j_status(request) -> JSONResponse:
    """Return the status of Neo4j integration."""
    is_neo4j = isinstance(WEB, RealityWebNeo4j)

    status = {
        "using_neo4j": is_neo4j,
        "uri": NEO4J_URI if is_neo4j else None,
        "node_count": 0,
        "edge_count": 0,
        "init_error": NEO4J_INIT_ERROR,
    }

    if is_neo4j:
        try:
            nodes = WEB.nodes()
            edges = WEB.edges()
            status["node_count"] = len(nodes)
            status["edge_count"] = len(edges)
            status["status"] = "connected"
        except Exception as e:
            status["status"] = f"error: {str(e)}"
    else:
        status["status"] = "using in-memory database"

    return JSONResponse(status)


async def api_toggle_neo4j(request) -> JSONResponse:
    """Toggle between Neo4j and in-memory database."""
    global WEB
    is_neo4j = isinstance(WEB, RealityWebNeo4j)

    try:
        if is_neo4j:
            # Switch to in-memory
            WEB.close()
            WEB = RealityWebInMemory()
            return JSONResponse(
                {"status": "switched to in-memory", "using_neo4j": False}
            )
        else:
            # Switch to Neo4j
            WEB = RealityWebNeo4j(
                uri=NEO4J_URI, user=NEO4J_USER, password=NEO4J_PASSWORD
            )
            return JSONResponse({"status": "switched to Neo4j", "using_neo4j": True})
    except Exception as e:
        return JSONResponse(
            {
                "status": f"error: {str(e)}",
                "using_neo4j": isinstance(WEB, RealityWebNeo4j),
            }
        )


async def api_neo4j_reconnect(request) -> JSONResponse:
    """Attempt to reconnect Neo4j without changing current mode.

    - If using Neo4j: close driver and reinitialize connection.
    - If using in-memory: do nothing and return informative status.
    """
    global WEB
    if isinstance(WEB, RealityWebNeo4j):
        try:
            # Close old connection and re-initialize
            WEB.close()
            new_web = RealityWebNeo4j(
                uri=NEO4J_URI,
                user=NEO4J_USER,
                password=NEO4J_PASSWORD,
            )
            WEB = new_web
            return JSONResponse({"status": "reconnected", "using_neo4j": True})
        except Exception as e:
            return JSONResponse({"status": f"error: {str(e)}", "using_neo4j": True})
    else:
        # Not using Neo4j right now; explicitly state no mode change
        return JSONResponse({"status": "no-op: using in-memory", "using_neo4j": False})


async def api_neo4j_health(request) -> JSONResponse:
    """Health check for Neo4j: returns status and counts.

    - If using Neo4j: run lightweight queries to validate connectivity and provide counts.
    - If using in-memory: return status indicating in-memory mode.
    """
    if isinstance(WEB, RealityWebNeo4j):
        try:
            # Quick ping and counts
            with WEB.driver.session() as session:  # type: ignore[attr-defined]
                ok = session.run("RETURN 1 as ok").single()["ok"]
                node_count = session.run(
                    "MATCH (n:Node) RETURN count(n) as c"
                ).single()["c"]
                rel_count = session.run(
                    "MATCH ()-[r:RELATES]->() RETURN count(r) as c"
                ).single()["c"]
            return JSONResponse(
                {
                    "status": "ok" if ok == 1 else "unknown",
                    "using_neo4j": True,
                    "node_count": int(node_count),
                    "edge_count": int(rel_count),
                }
            )
        except Exception as e:
            return JSONResponse(
                {
                    "status": f"error: {str(e)}",
                    "using_neo4j": True,
                }
            )
    else:
        return JSONResponse(
            {
                "status": "using in-memory database",
                "using_neo4j": False,
            }
        )


routes = [
    Route("/at-risk", at_risk),
    Route("/api/top-at-risk", api_top_at_risk),
    Route("/api/add-node", api_add_node, methods=["POST"]),
    Route("/api/seed-demo", api_seed_demo, methods=["POST"]),
    Route("/api/neo4j/status", api_neo4j_status),
    Route("/api/neo4j/toggle", api_toggle_neo4j, methods=["POST"]),
    Route("/api/neo4j/health", api_neo4j_health),
    Route("/api/neo4j/reconnect", api_neo4j_reconnect, methods=["POST"]),
    (
        Mount("/graphql", app=_graphql_app)
        if _graphql_app is not None
        else Route(
            "/graphql",
            lambda req: PlainTextResponse("GraphQL app not available", status_code=503),
        )
    ),
]

import os

# Используем переменную окружения для debug режима
debug_mode = os.getenv('DEBUG', 'false').lower() == 'true'
app = Starlette(debug=debug_mode, routes=routes)

# For uvicorn: uvicorn liminal.at_risk_server_neo4j:app --reload --port 8000
