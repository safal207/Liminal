// Основные функции для страницы /at-risk

// Инициализация и отладка
console.log('At-Risk JS loaded');

// Simple Mode toggle
function toggleSimpleMode() {
    try {
        const body = document.body;
        const sw = document.getElementById('simpleSwitch');
        const on = !body.classList.contains('simple');
        console.log('Toggling Simple Mode:', on ? 'ON' : 'OFF');
        if (on) { 
            body.classList.add('simple'); 
            sw.classList.add('on'); 
        } else { 
            body.classList.remove('simple'); 
            sw.classList.remove('on'); 
        }
    } catch (error) {
        console.error('Error toggling mode:', error);
    }
}

// Добавить узел из поля ввода #traits (глобальная функция)
async function addNodeFromInput() {
    try {
        var input = document.getElementById('traits');
        var traits = parseTraitsInput(input ? input.value : '');
        await addNode('module_state', traits);
    } catch (e) {
        console.error('addNodeFromInput error:', e);
    }
}

// Разбор строки признаков формата "любовь:0.8, страх:0.2"
function parseTraitsInput(str) {
    var traits = {};
    if (!str) return traits;
    try {
        // Разбиваем по запятым/точкам с запятой
        var parts = str.split(/[,;]+/);
        for (var i = 0; i < parts.length; i++) {
            var p = parts[i].trim();
            if (!p) continue;
            var kv = p.split(/[:=]+/);
            if (kv.length >= 2) {
                var key = kv[0].trim();
                var valRaw = kv.slice(1).join(':').trim();
                var val = parseFloat(valRaw.replace(',', '.'));
                if (isNaN(val)) val = 0.5;
                traits[key] = val;
            }
        }
    } catch (e) {
        console.warn('parseTraitsInput error:', e);
    }
    return traits;
}

// Sales Triggers logic (idempotent init to avoid double-load SyntaxError)
// Переведено на русский для простоты восприятия
var SALES_TRIGGERS = window.SALES_TRIGGERS || [
    { id:'urgency',   label:'Срочность ⏳' },
    { id:'scarcity',  label:'Дефицит 📉' },
    { id:'social',    label:'Соц.доказательство 👥' },
    { id:'guarantee', label:'Гарантия ✅' },
    { id:'bonus',     label:'Бонус 🎁' },
    { id:'discount',  label:'Скидка 💸' },
];
window.SALES_TRIGGERS = SALES_TRIGGERS;

function getSelectedTriggers() {
    const activeChips = document.querySelectorAll('.chip-toggle.active');
    return Array.from(activeChips).map(chip => chip.dataset.id);
}

function saveSelectedTriggers(list) {
    localStorage.setItem('offline_at_risk_triggers', JSON.stringify(list));
}

function renderTriggerChips(selected) {
    const container = document.getElementById('triggerChips');
    if (!container) return;
    
    container.innerHTML = '';
    SALES_TRIGGERS.forEach(trigger => {
        const chip = document.createElement('div');
        chip.className = 'chip-toggle' + (selected.includes(trigger.id) ? ' active' : '');
        chip.textContent = trigger.label;
        chip.dataset.id = trigger.id;
        chip.addEventListener('click', () => {
            chip.classList.toggle('active');
            saveSelectedTriggers(getSelectedTriggers());
            refresh();
        });
        container.appendChild(chip);
    });
}

// API Functions
async function seedDemo() {
    try {
        const response = await fetch('/api/seed-demo', { method: 'POST' });
        const data = await response.json();
        console.log('Demo seeded:', data);
        refresh();
    } catch (error) {
        console.error('Seed demo error:', error);
    }
}

async function refresh() {
    try {
        const limitInput = document.getElementById('limit');
        const limit = limitInput ? limitInput.value || 5 : 5;
        
        const response = await fetch('/api/top-at-risk?limit=' + limit);
        const data = await response.json();
        renderTable(data);
    } catch (error) {
        console.error('Refresh error:', error);
    }
}

function renderTable(data) {
    const tbody = document.getElementById('tbody');
    if (!tbody) return;

    if (!data.topAtRiskEdges || !data.topAtRiskEdges.length) {
        tbody.innerHTML = '<tr><td colspan="5" class="empty">🔍 Пока связей нет</td></tr>';
        return;
    }

    // Эмоции из Головоломки
    const EMOTION_MAP = {
        'радость': '😊',
        'грусть': '😢', 
        'гнев': '😠',
        'страх': '😨',
        'отвращение': '🤢'
    };

    tbody.innerHTML = data.topAtRiskEdges.map(edge => {
        const isRisk = edge.score < 0.4;
        const emotionIcon = EMOTION_MAP[edge.emotion] || '❓';
        
        // Форматируем советы со смайлами
        const advice = (edge.advice || []).map(function(a) {
            if (a.includes('дыши')) return a + ' 🌬️';
            if (a.includes('родител')) return a + ' 👪';
            if (a.includes('похож')) return a + ' 🔄';
            return a + ' 💡';
        }).join(' ');
        
        return '<tr class="' + (isRisk ? 'risk' : '') + '">' +
               '<td>' + advice + '</td>' +  // Советы в первом столбце
               '<td>' + edge.sourceId + '</td>' +
               '<td>' + edge.targetId + '</td>' +
               '<td class="score">' + edge.score.toFixed(3) + '</td>' +
               '<td class="emotion">' + emotionIcon + ' ' + (edge.emotion || 'неизвестно') + '</td></tr>';
    }).join('');
}

// Glossary Popup Component
class GlossaryPopup {
  constructor() {
    this.terms = {
      'состояние': 'Эмоциональный паттерн модуля (0-1)',
      'связь': 'Взаимодействие с весом (score)'
    };
    this.initPopup();
  }

  initPopup() {
    this.popup = document.createElement('div');
    this.popup.className = 'glossary-popup';
    document.body.appendChild(this.popup);
  }

  showTerm(term) {
    this.popup.innerHTML = `<h3>${term}</h3><p>${this.terms[term]}</p>`;
    this.popup.style.display = 'block';
  }
}

// Initialize on first hover
const glossary = new GlossaryPopup();

// Add tooltips for key terms
const TOOLTIPS = {
  'состояние': 'Эмоциональное/психологическое состояние модуля (например: любовь, страх)',
  'связь': 'Взаимодействие между состояниями, требующее внимания'
};

function showTooltip(term) {
  return TOOLTIPS[term] || '';
}

// Human-readable use cases
const USE_CASES = [
  {
    title: "Добавить состояние",
    steps: [
      "1. Введите эмоцию и силу (0-1) в поле признаков",
      "2. Нажмите кнопку 'Добавить'"
    ]
  },
  {
    title: "Найти слабые связи",
    steps: [
      "1. Нажмите кнопку 'Обновить'",
      "2. Проверьте таблицу 'Топ рисков'"
    ]
  }
];

function renderUseCases() {
  const container = document.createElement('div');
  container.className = 'use-cases';
  
  USE_CASES.forEach(uc => {
    const card = document.createElement('div');
    card.innerHTML = `
      <h4>${uc.title}</h4>
      <ul>
        ${uc.steps.map(s => `<li>${s}</li>`).join('')}
      </ul>
    `;
    container.appendChild(card);
  });
  
  document.body.appendChild(container);
}

// Remove advanced controls and simplify to core flow
function initSimplifiedUI() {
  document.querySelector('.advanced-controls').style.display = 'none';
  
  // Keep only essential buttons
  const essentialBtns = ['seedDemo', 'refresh'];
  document.querySelectorAll('button').forEach(btn => {
    if (!essentialBtns.includes(btn.id)) {
      btn.remove();
    }
  });
}

// Initialize
function init() {
    // Load saved triggers
    const savedTriggers = JSON.parse(localStorage.getItem('offline_at_risk_triggers') || '[]');
    renderTriggerChips(savedTriggers);
    renderUseCases();
    initSimplifiedUI();
}

// Start when DOM is ready
if (document.readyState !== 'loading') {
    init();
} else {
    document.addEventListener('DOMContentLoaded', init);
}

// --- Missing functions implemented ---
// Add a node via API with given kind and traits
async function addNode(kind, traits) {
    try {
        var payload = { kind: kind || 'module_state', traits: traits || {} };
        var resp = await fetch('/api/add-node', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(payload)
        });
        var data = await resp.json();
        console.log('Node added:', data);
        await refresh();
    } catch (e) {
        console.error('addNode error:', e);
    }
}

// Add a simple node based on selected triggers or defaults
async function addNodeSimple() {
    try {
        var selected = getSelectedTriggers();
        var traits = {};
        // Map triggers to simple numeric traits [0..1]
        for (var i = 0; i < selected.length; i++) {
            var key = selected[i];
            traits[key] = 0.8; // simple default weight
        }
        if (selected.length === 0) {
            // fallback trait
            traits['neutral'] = 0.5;
        }
        await addNode('module_state', traits);
    } catch (e) {
        console.error('addNodeSimple error:', e);
    }
}

// Minimal UI self-test to verify functions are bound and API reachable
async function runUiSelfTest() {
    try {
        console.log('[SELFTEST] start');
        if (typeof toggleSimpleMode !== 'function') throw new Error('toggleSimpleMode missing');
        if (typeof seedDemo !== 'function') throw new Error('seedDemo missing');
        if (typeof refresh !== 'function') throw new Error('refresh missing');
        if (typeof addNode !== 'function') throw new Error('addNode missing');
        if (typeof addNodeSimple !== 'function') throw new Error('addNodeSimple missing');
        console.log('[SELFTEST] functions present');
        await refresh();
        console.log('[SELFTEST] refresh ok');
    } catch (e) {
        console.error('[SELFTEST] error', e);
    }
}

// Explicitly export functions to window
window.toggleSimpleMode = toggleSimpleMode;
window.seedDemo = seedDemo;
window.refresh = refresh;
window.addNode = addNode;
window.addNodeSimple = addNodeSimple;
window.runUiSelfTest = runUiSelfTest;
window.addNodeFromInput = addNodeFromInput;

// Enhanced preview display
function showPreview(value) {
  const preview = document.getElementById('preview');
  preview.innerHTML = value ? 
    `<div class="preview-box">
      ${renderPreviewContent(value)}
    </div>` : '';
}

function renderPreviewContent(value) {
  const [emotion, strength] = value.split(' ');
  const icon = EMOTION_MAP[emotion] || '❓';
  
  if (!strength || isNaN(strength)) 
    return 'Формат: эмоция число (0-1)';
    
  const num = parseFloat(strength);
  const strengthColor = 
    num < 0.3 ? 'red' : num < 0.6 ? 'orange' : 'green';
    
  return `
    <div>${icon} Эмоция: <strong>${emotion || '?'}</strong></div>
    <div>Сила: <span style="color:${strengthColor}">${num.toFixed(2)}</span></div>
  `;
}
