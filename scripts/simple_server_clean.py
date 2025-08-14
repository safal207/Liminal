#!/usr/bin/env python3
"""Simple working HTTP server for at-risk edges visualization."""

import json
import sys
from http.server import BaseHTTPRequestHandler, HTTPServer
from pathlib import Path
from urllib.parse import parse_qs, urlparse

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from liminal.reality_web import RealityWebInMemory

# Global web instance
WEB = RealityWebInMemory()

HTML_PAGE = """<!doctype html>
<html>
<head>
    <meta charset='utf-8'/>
    <title>At-Risk Edges</title>
    <style>
        :root {
            --bg: #f5f7fb;
            --card: #ffffff;
            --text: #222;
            --subtle: #6b7280;
            --primary: #0a84ff;
            --primary-dark: #0060d6;
            --green: #34c759;
            --yellow: #ffd60a;
            --red: #ff3b30;
            --shadow: 0 8px 24px rgba(0,0,0,0.08);
            --radius: 16px;
        }
        * { box-sizing: border-box; }
        body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif; margin: 0; background: var(--bg); color: var(--text); }
        .container { max-width: 1040px; margin: 0 auto; padding: 20px; }
        .card { background: var(--card); border-radius: var(--radius); box-shadow: var(--shadow); padding: 20px; }
        h1 { color: #111; font-weight: 800; letter-spacing: -0.02em; margin: 0 0 12px; }
        .sub { color: var(--subtle); margin: 0 0 16px; }
        .toolbar { display: flex; align-items: center; justify-content: space-between; gap: 12px; margin: 16px 0; }
        .toggle { display: flex; align-items: center; gap: 10px; font-weight: 600; }
        .switch { position: relative; width: 54px; height: 32px; background: #e5e7eb; border-radius: 999px; cursor: pointer; transition: background .2s; }
        .switch.on { background: var(--primary); }
        .knob { position: absolute; top: 3px; left: 3px; width: 26px; height: 26px; background: white; border-radius: 50%; transition: left .2s; box-shadow: 0 2px 6px rgba(0,0,0,0.15); }
        .switch.on .knob { left: 25px; }
        .simple-hero { display: grid; grid-template-columns: 1.2fr .8fr; gap: 18px; align-items: center; }
        .hero-text { padding: 8px 4px; }
        .steps { display: flex; gap: 12px; flex-wrap: wrap; margin-top: 8px; }
        .chip { background: #f1f5f9; color: #111; padding: 10px 12px; border-radius: 999px; font-weight: 600; box-shadow: inset 0 -1px 0 rgba(0,0,0,0.06); }
        .big-buttons { display: grid; grid-template-columns: repeat(3, 1fr); gap: 12px; }
        .big-btn { display: flex; flex-direction: column; align-items: center; gap: 10px; padding: 22px 16px; border-radius: 20px; background: #fff; box-shadow: var(--shadow); border: 2px solid #eef2f7; cursor: pointer; transition: transform .08s ease, border-color .2s; text-align: center; }
        .big-btn:hover { transform: translateY(-1px); border-color: var(--primary); }
        .emoji { font-size: 36px; line-height: 1; }
        .btn-title { font-weight: 800; letter-spacing: -0.02em; }
        .btn-sub { color: var(--subtle); font-size: 13px; }
        .chips { display:flex; flex-wrap:wrap; gap:8px; margin: 8px 0 12px; }
        .chip-toggle { padding:8px 12px; border-radius:999px; background:#eef2ff; color:#3730a3; cursor:pointer; user-select:none; border:1px solid #e0e7ff; font-weight:600; }
        .chip-toggle.active { background:#c7d2fe; border-color:#a5b4fc; }
        .advanced-controls { margin: 16px 0; display: flex; flex-wrap: wrap; align-items: center; gap: 8px; }
        .advanced-controls button { padding: 10px 14px; background: var(--primary); color: white; border: none; border-radius: 12px; cursor: pointer; font-weight: 700; }
        .advanced-controls button:hover { background: var(--primary-dark); }
        .advanced-controls input { padding: 10px 12px; border: 1px solid #e5e7eb; border-radius: 12px; }
        .hint { color: var(--subtle); font-size: 13px; margin-left: 6px; }
        table { width: 100%; border-collapse: collapse; margin: 12px 0 0; background: #fff; border-radius: 16px; overflow: hidden; box-shadow: var(--shadow); }
        th, td { padding: 14px 16px; text-align: left; border-bottom: 1px solid #f1f5f9; }
        th { background: #f8fafc; font-weight: 800; letter-spacing: .02em; }
        .risk { background: #fff7f7; }
        .score { font-weight: 800; }
        .empty { padding: 18px; color: var(--subtle); }
        .footer-note { color: var(--subtle); font-size: 12px; margin-top: 10px; }
        body.simple .advanced-controls { display: none; }
        @media (max-width: 800px) {
            .simple-hero { grid-template-columns: 1fr; }
            .big-buttons { grid-template-columns: 1fr; }
        }
    </style>
</head>
<body>
    <div class="container">
        <div class="toolbar">
            <div>
                <h1>Слабые ниточки</h1>
                <p class="sub">Шарики — это состояния. Ниточки — связи. Мы помогаем заметить те, что нуждаются в заботе.</p>
            </div>
            <div class="toggle" title="Простой режим — всё крупно и понятно">
                <span>Simple Mode</span>
                <div id="simpleSwitch" class="switch on" onclick="window.toggleSimpleMode()"><div class="knob"></div></div>
            </div>
        </div>

        <div class="card simple-hero">
            <div class="hero-text">
                <div class="steps">
                    <div class="chip">1️⃣ Посеять</div>
                    <div class="chip">2️⃣ Обновить</div>
                    <div class="chip">3️⃣ Посмотреть советы</div>
                </div>
                <p class="sub" style="margin-top:10px">Советы подскажут: подышать, связать с «родителем» или объединить похожие шарики.</p>
            </div>
            <div class="big-buttons">
                <div class="big-btn" onclick="window.seedDemo()">
                    <div class="emoji">🌱</div>
                    <div class="btn-title">Посеять</div>
                    <div class="btn-sub">Создать 3 шарика</div>
                </div>
                <div class="big-btn" onclick="window.refresh()">
                    <div class="emoji">🔄</div>
                    <div class="btn-title">Обновить</div>
                    <div class="btn-sub">Показать слабые ниточки</div>
                </div>
                <div class="big-btn" onclick="window.addNodeSimple()">
                    <div class="emoji">🟣</div>
                    <div class="btn-title">Добавить</div>
                    <div class="btn-sub">Быстрый шарик</div>
                </div>
            </div>
        </div>

        <div class="chips" id="triggerChips" aria-label="Sales Triggers"></div>

        <div class="advanced-controls">
            <button onclick="window.seedDemo()">Seed Demo</button>
            <button onclick="window.refresh()">Refresh</button>
            <input type="number" id="limit" value="5" min="1" placeholder="Limit">
            <input type="text" id="traits" placeholder="любовь:0.8, страх:0.2">
            <button onclick="window.addNode()">Add Node</button>
            <button onclick="window.runUiSelfTest()">Run UI Self-Test</button>
            <span class="hint">Продвинутые настройки (скрыты в Simple Mode)</span>
        </div>
        <table>
            <thead>
                <tr><th>Source</th><th>Target</th><th>Score</th><th>Advice</th></tr>
            </thead>
            <tbody id="tbody">
                <tr><td colspan="4" class="empty">Нажми «Посеять», потом «Обновить» — и мы подсветим слабые ниточки 💡</td></tr>
            </tbody>
        </table>
        <div class="footer-note">Подсказки: breathStep — вдох-выдох; linkParent — свяжи с родителем; merge — склей похожие.</div>
    </div>

    <script>
        (function() {
            'use strict';
            
            console.log('Страница At-Risk загружается...');
            
            // Sales Triggers configuration
            var SALES_TRIGGERS = [
                { id: 'urgency', label: 'Срочность ⏳' },
                { id: 'scarcity', label: 'Дефицит 📉' },
                { id: 'social', label: 'Соц.доказательство 👥' },
                { id: 'guarantee', label: 'Гарантия ✅' },
                { id: 'bonus', label: 'Бонус 🎁' },
                { id: 'discount', label: 'Скидка 💸' }
            ];
            
            // Helper functions
            function getSelectedTriggers() {
                var elements = document.querySelectorAll('.chip-toggle.active');
                var result = [];
                for (var i = 0; i < elements.length; i++) {
                    result.push(elements[i].dataset.id);
                }
                return result;
            }
            
            function saveSelectedTriggers(list) {
                localStorage.setItem('offline_at_risk_triggers', JSON.stringify(list));
            }
            
            function renderTriggerChips(selected) {
                var cont = document.getElementById('triggerChips');
                cont.innerHTML = '';
                
                for (var i = 0; i < SALES_TRIGGERS.length; i++) {
                    var trigger = SALES_TRIGGERS[i];
                    var el = document.createElement('div');
                    el.className = 'chip-toggle' + (selected.indexOf(trigger.id) !== -1 ? ' active' : '');
                    el.textContent = trigger.label;
                    el.dataset.id = trigger.id;
                    el.role = 'button';
                    el.tabIndex = 0;
                    
                    el.addEventListener('click', function() {
                        this.classList.toggle('active');
                        saveSelectedTriggers(getSelectedTriggers());
                        window.refresh();
                    });
                    
                    el.addEventListener('keypress', function(e) {
                        if (e.key === 'Enter' || e.key === ' ') {
                            e.preventDefault();
                            this.click();
                        }
                    });
                    
                    cont.appendChild(el);
                }
            }
            
            function triggerAdvice(selected) {
                var tips = [];
                if (selected.indexOf('urgency') !== -1) tips.push('Сделай маленький шаг сегодня — время имеет значение.');
                if (selected.indexOf('scarcity') !== -1) tips.push('Такие моменты редки — запланируй один сейчас.');
                if (selected.indexOf('social') !== -1) tips.push('Другие справлялись — у тебя тоже получится.');
                if (selected.indexOf('guarantee') !== -1) tips.push('Безопасный старт: 5 минут спокойствия и дыхания.');
                if (selected.indexOf('bonus') !== -1) tips.push('Добавь бонус: похвала или стикер за смелость.');
                if (selected.indexOf('discount') !== -1) tips.push('Снизь порог: выбери самую лёгкую тему для разговора.');
                return tips;
            }
            
            // Main functions
            function toggleSimpleMode() {
                try {
                    var body = document.body;
                    var sw = document.getElementById('simpleSwitch');
                    var on = !body.classList.contains('simple');
                    console.log('Переключение Simple Mode:', on ? 'ON' : 'OFF');
                    if (on) {
                        body.classList.add('simple');
                        sw.classList.add('on');
                    } else {
                        body.classList.remove('simple');
                        sw.classList.remove('on');
                    }
                } catch (error) {
                    console.error('Ошибка при переключении режима:', error);
                }
            }
            
            function seedDemo() {
                console.log('Вызываем /api/seed-demo...');
                fetch('/api/seed-demo', { method: 'POST' })
                    .then(function(response) { return response.json(); })
                    .then(function(data) {
                        console.log('Seed Demo успешно:', data);
                        window.refresh();
                    })
                    .catch(function(error) {
                        console.error('Ошибка при вызове seed-demo:', error);
                        alert('Ошибка при вызове API: ' + error.message);
                    });
            }
            
            function refresh() {
                console.log('Вызываем /api/top-at-risk...');
                var limit = document.getElementById('limit').value || 5;
                fetch('/api/top-at-risk?limit=' + limit)
                    .then(function(response) { return response.json(); })
                    .then(function(data) {
                        console.log('Получены данные:', data);
                        var tbody = document.getElementById('tbody');
                        
                        if (!data.topAtRiskEdges || data.topAtRiskEdges.length === 0) {
                            tbody.innerHTML = '<tr><td colspan="4" class="empty">Пока ниточек нет. Нажми «Посеять», потом «Обновить». 🌈</td></tr>';
                            return;
                        }
                        
                        var html = '';
                        var selected = getSelectedTriggers();
                        
                        for (var i = 0; i < data.topAtRiskEdges.length; i++) {
                            var edge = data.topAtRiskEdges[i];
                            var isRisk = edge.score < 0.4;
                            var list = (edge.advice || []).slice();
                            var triggerTips = triggerAdvice(selected);
                            for (var j = 0; j < triggerTips.length; j++) {
                                list.push(triggerTips[j]);
                            }
                            var advice = list.join(', ');
                            
                            var triggerTags = '';
                            if (selected.length > 0) {
                                triggerTags = '<div style="margin-top:6px;display:flex;flex-wrap:wrap;gap:6px;">';
                                for (var k = 0; k < selected.length; k++) {
                                    triggerTags += '<span class="chip">#' + selected[k] + '</span>';
                                }
                                triggerTags += '</div>';
                            }
                            
                            html += '<tr class="' + (isRisk ? 'risk' : '') + '">' +
                                '<td>' + edge.sourceId + '</td>' +
                                '<td>' + edge.targetId + '</td>' +
                                '<td class="score">' + edge.score.toFixed(3) + '</td>' +
                                '<td>' + advice + triggerTags + '</td>' +
                                '</tr>';
                        }
                        
                        tbody.innerHTML = html;
                    })
                    .catch(function(error) {
                        console.error('Ошибка в refresh():', error);
                        alert('Ошибка при обновлении: ' + (error.message || error));
                    });
            }
            
            function addNode() {
                var traitsEl = document.getElementById('traits');
                var traitsStr = traitsEl.value;
                if (!traitsStr) return;
                
                console.log('Добавляем узел с traits:', traitsStr);
                var traits = {};
                var pairs = traitsStr.split(',');
                for (var i = 0; i < pairs.length; i++) {
                    var parts = pairs[i].split(':');
                    if (parts.length === 2) {
                        var key = parts[0].trim();
                        var value = parseFloat(parts[1].trim()) || 0;
                        if (key) traits[key] = value;
                    }
                }
                
                fetch('/api/add-node', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({ kind: 'module_state', traits: traits })
                })
                .then(function(response) { return response.json(); })
                .then(function(data) {
                    console.log('Узел добавлен:', data);
                    traitsEl.value = '';
                    window.refresh();
                })
                .catch(function(error) {
                    console.error('Ошибка при добавлении узла:', error);
                    alert('Ошибка при добавлении узла: ' + error.message);
                });
            }
            
            function addNodeSimple() {
                var presets = [
                    { label: '😊 радость', key: 'радость', val: 0.8 },
                    { label: '😌 спокойствие', key: 'спокойствие', val: 0.7 },
                    { label: '😟 тревога', key: 'тревога', val: 0.6 }
                ];
                
                var menu = '';
                for (var i = 0; i < presets.length; i++) {
                    menu += (i + 1) + '. ' + presets[i].label + '\\n';
                }
                
                var ans = prompt('Выбери шарик:\\n' + menu + '\\n(или оставь пустым для «радость»)', '1');
                if (ans === null) return;
                
                var idx = Math.max(1, Math.min(3, parseInt(ans || '1', 10))) - 1;
                var pick = presets[idx] || presets[0];
                console.log('Добавляем простой узел:', pick);
                
                var traits = {};
                traits[pick.key] = pick.val;
                
                fetch('/api/add-node', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({ kind: 'module_state', traits: traits })
                })
                .then(function(response) { return response.json(); })
                .then(function(data) {
                    console.log('Простой узел добавлен:', data);
                    window.refresh();
                })
                .catch(function(error) {
                    console.error('Ошибка при добавлении простого узла:', error);
                    alert('Ошибка: ' + error.message);
                });
            }
            
            function runUiSelfTest() {
                console.log('[UI-TEST] Старт теста: seedDemo -> refresh -> проверка DOM');
                
                window.seedDemo();
                setTimeout(function() {
                    window.refresh();
                    setTimeout(function() {
                        var tbody = document.getElementById('tbody');
                        var rows = tbody.querySelectorAll('tr');
                        var hasData = false;
                        for (var i = 0; i < rows.length; i++) {
                            if (!rows[i].querySelector('.empty')) {
                                hasData = true;
                                break;
                            }
                        }
                        
                        if (!hasData) {
                            console.log('[UI-TEST] Провал: таблица пуста');
                            alert('UI Self-Test FAIL: таблица не обновилась');
                            return;
                        }
                        
                        var scoreCells = tbody.querySelectorAll('td.score');
                        var scoresOk = scoreCells.length > 0;
                        for (var j = 0; j < scoreCells.length; j++) {
                            var text = scoreCells[j].textContent || '';
                            if (!/\\d+\\.\\d{3}/.test(text)) {
                                scoresOk = false;
                                break;
                            }
                        }
                        
                        if (!scoresOk) {
                            console.log('[UI-TEST] Провал: ячейки score некорректны');
                            alert('UI Self-Test FAIL: нет корректных ячеек score');
                            return;
                        }
                        
                        console.log('[UI-TEST] Успех: таблица заполнена, score-ячейки найдены');
                        alert('UI Self-Test PASS ✅');
                    }, 300);
                }, 300);
            }
            
            // Export functions to global scope for onclick handlers
            window.toggleSimpleMode = toggleSimpleMode;
            window.seedDemo = seedDemo;
            window.refresh = refresh;
            window.addNode = addNode;
            window.addNodeSimple = addNodeSimple;
            window.runUiSelfTest = runUiSelfTest;

            // Single, clean initialization block, runs once when the DOM is ready.
            document.addEventListener('DOMContentLoaded', function() {
                console.log('DOM fully loaded. Initializing page state...');

                // 1. Set simple mode state from localStorage (or default to true)
                var isSimple = localStorage.getItem('at_risk_simple_mode') !== 'false';
                document.body.classList.toggle('simple', isSimple);
                document.getElementById('simpleSwitch').classList.toggle('on', isSimple);

                // 2. Render trigger chips, safely parsing from localStorage
                var savedTriggers = [];
                try {
                    var stored = localStorage.getItem('offline_at_risk_triggers');
                    if (stored) {
                        savedTriggers = JSON.parse(stored);
                    }
                } catch (e) {
                    console.error('Could not parse triggers from localStorage:', e);
                }
                renderTriggerChips(savedTriggers);

                // 3. Perform the initial data refresh to populate the table
                window.refresh();
            });

        })();
    </script>
</body>
</html>"""


class SimpleHandler(BaseHTTPRequestHandler):
    def log_message(self, format, *args):
        """Suppress default logging"""
        pass

    def do_GET(self):
        parsed = urlparse(self.path)
        print(f"GET request: {parsed.path}")

        if parsed.path == "/" or parsed.path == "/at-risk":
            self.send_response(200)
            self.send_header("Content-Type", "text/html; charset=utf-8")
            self.end_headers()
            self.wfile.write(HTML_PAGE.encode("utf-8"))
            return

        elif parsed.path == "/test-button-clicks.html":
            # Serve the test file
            try:
                with open("test-button-clicks.html", "r", encoding="utf-8") as f:
                    content = f.read()
                self.send_response(200)
                self.send_header("Content-Type", "text/html; charset=utf-8")
                self.send_header("Access-Control-Allow-Origin", "*")
                self.end_headers()
                self.wfile.write(content.encode("utf-8"))
                return
            except FileNotFoundError:
                print(f"Test file not found: test-button-clicks.html")
                self.send_response(404)
                self.end_headers()
                self.wfile.write(b"Test file not found")
                return

        elif parsed.path == "/api/top-at-risk":
            query = parse_qs(parsed.query)
            limit = int(query.get("limit", [5])[0])

            pairs = []
            nodes = list(WEB._nodes.values())
            for i, a in enumerate(nodes):
                for b in nodes[i + 1 :]:
                    edge = WEB.link_similarity(a, b)
                    score = edge.weight
                    advice = []
                    if score < 0.4:
                        advice.append("Consider strengthening connection")
                    if score < 0.2:
                        advice.append("High risk - needs attention")

                    pairs.append(
                        {
                            "sourceId": a.id,
                            "targetId": b.id,
                            "score": score,
                            "advice": advice,
                        }
                    )

            pairs.sort(key=lambda x: x["score"])
            result = {"topAtRiskEdges": pairs[:limit]}

            self.send_response(200)
            self.send_header("Content-Type", "application/json; charset=utf-8")
            self.send_header("Access-Control-Allow-Origin", "*")
            self.end_headers()
            self.wfile.write(json.dumps(result, ensure_ascii=False).encode("utf-8"))
            return

        self.send_error(404, "Not Found")

    def do_POST(self):
        parsed = urlparse(self.path)
        print(f"POST request: {parsed.path}")

        if parsed.path == "/api/seed-demo":
            WEB._nodes.clear()
            WEB._edges.clear()

            a = WEB.add_node(kind="module_state", traits={"любовь": 0.8})
            b = WEB.add_node(kind="module_state", traits={"страх": 0.9})
            c = WEB.add_node(kind="module_state", traits={"спокойствие": 0.7})
            WEB.link_parent(parent=a, child=c)

            result = {"created": [a.id, b.id, c.id]}

            self.send_response(200)
            self.send_header("Content-Type", "application/json; charset=utf-8")
            self.send_header("Access-Control-Allow-Origin", "*")
            self.end_headers()
            self.wfile.write(json.dumps(result, ensure_ascii=False).encode("utf-8"))
            return

        elif parsed.path == "/api/add-node":
            length = int(self.headers.get("Content-Length", "0"))
            if length > 0:
                data = json.loads(self.rfile.read(length).decode("utf-8"))
                kind = data.get("kind", "module_state")
                traits = data.get("traits", {})
                node = WEB.add_node(kind=kind, traits=traits)

                result = {"id": node.id, "kind": node.kind, "traits": node.traits}

                self.send_response(200)
                self.send_header("Content-Type", "application/json; charset=utf-8")
                self.send_header("Access-Control-Allow-Origin", "*")
                self.end_headers()
                self.wfile.write(json.dumps(result, ensure_ascii=False).encode("utf-8"))
                return

        self.send_error(404, "Not Found")

    def do_OPTIONS(self):
        """Handle preflight CORS requests"""
        self.send_response(200)
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
        self.send_header("Access-Control-Allow-Headers", "Content-Type")
        self.end_headers()


def main():
    port = 8080
    server = HTTPServer(("127.0.0.1", port), SimpleHandler)
    print(f"Server running at http://127.0.0.1:{port}/at-risk")
    print(f"Also accessible at http://127.0.0.1:{port}/")
    print("API endpoints:")
    print(f"  GET  http://127.0.0.1:{port}/api/top-at-risk")
    print(f"  POST http://127.0.0.1:{port}/api/seed-demo")
    print(f"  POST http://127.0.0.1:{port}/api/add-node")
    print("\nPress Ctrl+C to stop")
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nShutting down...")
    finally:
        server.server_close()


if __name__ == "__main__":
    main()
