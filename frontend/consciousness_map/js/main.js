/**
 * Main application entry point
 * Инициализация компонентов карты сознания
 */

// Глобальная переменная для отслеживания состояния приложения
const app = {
  isConnected: false,
  lastActiveState: null,
  temporalInsights: [],
  maxInsightHistory: 5,
  emotimeMode: null,
  emotimePollInterval: null,
};

// Инициализация при загрузке DOM
document.addEventListener('DOMContentLoaded', () => {
  console.log('🧠 Инициализация карты состояний сознания...');
  
  // Инициализация визуализации
  initGraphRenderer();

  // Инициализация контроллера состояний
  initStateController();

  // Подключение к WebSocket
  connectToWebSocket();

  // Попытка загрузки данных из Neo4j
  loadDataFromNeo4j();

  // Дополнительная инициализация временных паттернов
  initTemporalPatterns();

  // Запуск опроса Emotime
  initEmotimePolling();
});

/**
 * Подключение к WebSocket серверу
 */
function connectToWebSocket() {
  try {
    console.log('🔌 Подключение к WebSocket серверу...');
    dataService.connectWebSocket();
    
    // Добавляем индикатор статуса подключения
    const header = document.querySelector('.mit-header .container');
    if (header) {
      const statusIndicator = document.createElement('div');
      statusIndicator.className = 'connection-status';
      statusIndicator.innerHTML = `
        <span class="status-indicator disconnected"></span>
        <span class="status-text">Подключение...</span>
      `;
      header.appendChild(statusIndicator);
      
      // Обновляем индикатор при изменении состояния подключения
      setInterval(() => {
        const indicator = document.querySelector('.status-indicator');
        const statusText = document.querySelector('.status-text');
        if (indicator && statusText) {
          if (dataService.isConnected) {
            indicator.className = 'status-indicator connected';
            statusText.textContent = 'Подключено';
          } else {
            indicator.className = 'status-indicator disconnected';
            statusText.textContent = 'Переподключение...';
          }
        }
      }, 1000);
    }
  } catch (error) {
    console.error('⚠️ Ошибка подключения к WebSocket:', error);
  }
}

/**
 * Загрузка данных из Neo4j
 */
function loadDataFromNeo4j() {
  try {
    console.log('📊 Загрузка данных из Neo4j...');
    dataService.loadDataFromNeo4j()
      .then(() => {
        console.log('✅ Данные успешно загружены из Neo4j');
      })
      .catch(error => {
        console.warn('⚠️ Невозможно загрузить данные из Neo4j, используем демо-данные:', error);
        // Демо-данные уже загружены при инициализации dataService
      });
  } catch (error) {
    console.error('⚠️ Ошибка при загрузке данных:', error);
  }
}

/**
 * Инициализация анализа временных паттернов
 */
function initTemporalPatterns() {
  // Подписка на изменения состояний для анализа временных паттернов
  dataService.subscribe(data => {
    if (data.activeState && data.activeState !== app.lastActiveState) {
      // Сохраняем временную точку перехода
      const timestamp = new Date();
      const fromState = app.lastActiveState ? 
        CONFIG.states[app.lastActiveState]?.label || app.lastActiveState : 
        "Начальное состояние";
      
      const toState = CONFIG.states[data.activeState]?.label || data.activeState;
      
      // Добавляем инсайт в историю
      app.temporalInsights.unshift({
        timestamp,
        fromState,
        toState,
        description: `Переход ${fromState} → ${toState}`
      });
      
      // Ограничиваем размер истории
      if (app.temporalInsights.length > app.maxInsightHistory) {
        app.temporalInsights.pop();
      }
      
      // Обновляем последнее активное состояние
      app.lastActiveState = data.activeState;
      
      // Анализируем паттерны переходов
      analyzeTransitionPatterns();
    }
  });
}

/**
 * Анализ паттернов переходов между состояниями
 * Основа для будущей ML-интеграции
 */
function analyzeTransitionPatterns() {
  if (app.temporalInsights.length < 2) return;
  
  // В реальной реализации здесь был бы анализ паттернов
  // с использованием ML-моделей для предсказания следующего состояния
  
  // Для демо просто логируем последние переходы
  console.log('📈 Анализ временных паттернов:');
  app.temporalInsights.forEach((insight, index) => {
    const timeStr = insight.timestamp.toLocaleTimeString();
    console.log(`${timeStr}: ${insight.fromState} → ${insight.toState}`);
  });
  
  // Добавляем рекомендацию в UI
  const insightPanel = document.getElementById('current-insight');
  if (insightPanel && app.temporalInsights.length >= 2) {
    const lastTransition = app.temporalInsights[0];
    
    // В будущем здесь будет предсказание от ML-модели
    let recommendation = '';
    
    // Демо-рекомендации на основе последнего перехода
    if (lastTransition.toState.includes('Защитный гнев')) {
      recommendation = 'Рекомендуем глубокое дыхание (3 минуты) для снижения интенсивности';
    } else if (lastTransition.toState.includes('Уединение')) {
      recommendation = 'Рекомендуем практику заземления и медитацию для восстановления';
    } else {
      recommendation = 'Проанализируйте причины этого перехода. Что его вызвало?';
    }
    
    // Добавляем секцию рекомендаций, если она еще не существует
    const recSection = insightPanel.querySelector('.recommendation') || 
                      document.createElement('div');
    recSection.className = 'recommendation';
    recSection.innerHTML = `
      <h5>🧬 Анализ паттернов</h5>
      <p>${recommendation}</p>
      <small>Основано на ${app.temporalInsights.length} последних переходах</small>
    `;
    
    // Добавляем секцию в панель инсайтов, если она еще не добавлена
    if (!insightPanel.querySelector('.recommendation')) {
      insightPanel.appendChild(recSection);
    }
  }
}

// ---- Emotime mode polling ----

const EMOTIME_API = 'http://localhost:8000/emotime/status';
const EMOTIME_POLL_MS = 10000; // 10 секунд

const EMOTIME_META = {
  calm:          { label: 'Спокойствие',    color: '#4CAF50', emoji: '🌿' },
  focus:         { label: 'Фокус',          color: '#2196F3', emoji: '🎯' },
  stress:        { label: 'Стресс',         color: '#f44336', emoji: '⚡' },
  joy:           { label: 'Радость',        color: '#FFC107', emoji: '✨' },
  contemplation: { label: 'Созерцание',     color: '#9C27B0', emoji: '🌀' },
  neutral:       { label: 'Нейтральный',    color: '#607D8B', emoji: '○'  },
};

function initEmotimePolling() {
  // Создаём бейдж в шапке
  const header = document.querySelector('.mit-header .container');
  if (header) {
    const badge = document.createElement('div');
    badge.id = 'emotime-badge';
    badge.className = 'emotime-badge';
    badge.innerHTML = '<span class="emotime-dot"></span><span class="emotime-label">Emotime: —</span>';
    header.appendChild(badge);
  }

  // Первый запрос сразу
  pollEmotimeStatus();

  // Периодический опрос
  app.emotimePollInterval = setInterval(pollEmotimeStatus, EMOTIME_POLL_MS);
}

async function pollEmotimeStatus() {
  try {
    const res = await fetch(EMOTIME_API);
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    const data = await res.json();

    // Emotime API возвращает { mode: { name: "Calm" } } или { mode: "calm" }
    const rawMode = (data.mode?.name || data.mode || '').toLowerCase();
    const mode = Object.keys(EMOTIME_META).find(k => rawMode.includes(k)) || 'neutral';

    if (mode !== app.emotimeMode) {
      app.emotimeMode = mode;
      updateEmotimeBadge(mode, true);
      if (graphRendererInstance) {
        graphRendererInstance.applyEmotimeMode(mode);
      }
    }
  } catch {
    // API недоступен — сбрасываем overlay тихо
    if (app.emotimeMode !== null) {
      app.emotimeMode = null;
      updateEmotimeBadge(null, false);
      if (graphRendererInstance) {
        graphRendererInstance.clearEmotimeMode();
      }
    }
  }
}

function updateEmotimeBadge(mode, online) {
  const badge = document.getElementById('emotime-badge');
  if (!badge) return;
  if (!online || !mode) {
    badge.innerHTML = '<span class="emotime-dot" style="background:#555"></span><span class="emotime-label">Emotime: офлайн</span>';
    badge.style.borderColor = '#555';
    return;
  }
  const meta = EMOTIME_META[mode];
  badge.innerHTML = `
    <span class="emotime-dot" style="background:${meta.color};box-shadow:0 0 6px ${meta.color}"></span>
    <span class="emotime-label">${meta.emoji} ${meta.label}</span>
  `;
  badge.style.borderColor = meta.color;
}

// Добавляем CSS для индикатора подключения
const connectionStyle = document.createElement('style');
connectionStyle.textContent = `
  .connection-status {
    display: inline-flex;
    align-items: center;
    margin-left: 20px;
    font-size: 0.9rem;
  }
  
  .status-indicator {
    width: 10px;
    height: 10px;
    border-radius: 50%;
    margin-right: 8px;
  }
  
  .connected {
    background-color: #4CAF50;
    box-shadow: 0 0 5px #4CAF50;
  }
  
  .disconnected {
    background-color: #F44336;
    box-shadow: 0 0 5px #F44336;
  }
  
  .recommendation {
    margin-top: 15px;
    padding: 10px;
    background: rgba(255, 255, 255, 0.1);
    border-left: 3px solid #a31f34;
    font-style: italic;
  }
`;
document.head.appendChild(connectionStyle);
