/**
 * State Controller for Consciousness Map
 * MIT Media Lab inspired state management
 */

class StateController {
  constructor() {
    this.filterElements = {};
    this.initializeFilters();
    this.setupEventListeners();
  }
  
  /**
   * Инициализация фильтров состояний
   */
  initializeFilters() {
    const filterContainer = document.querySelector('.state-filters');
    if (!filterContainer) return;
    
    // Группируем состояния по категориям
    const stateGroups = {
      "Философские состояния": [
        'home_authentic', 'resonance_flow', 'presence_now', 
        'question_space', 'harmony_balance', 'transition_liminal'
      ],
      "Эмоциональные состояния": [
        'doubt_creative', 'curious_flow', 'anger_protection',
        'boundary_integrity', 'withdrawal_protection', 
        'grief_transformation', 'gratitude_presence'
      ]
    };
    
    // Создаем фильтры по группам
    Object.entries(stateGroups).forEach(([groupName, stateIds]) => {
      const groupDiv = document.createElement('div');
      groupDiv.className = 'filter-group';
      
      const groupTitle = document.createElement('h4');
      groupTitle.textContent = groupName;
      groupDiv.appendChild(groupTitle);
      
      // Создаем чекбоксы для каждого состояния
      stateIds.forEach(stateId => {
        const stateInfo = CONFIG.states[stateId];
        if (!stateInfo) return;
        
        const checkboxId = `filter-${stateId}`;
        
        const checkboxContainer = document.createElement('div');
        checkboxContainer.className = 'filter-item';
        
        const checkbox = document.createElement('input');
        checkbox.type = 'checkbox';
        checkbox.id = checkboxId;
        checkbox.checked = true; // По умолчанию все включены
        checkbox.dataset.stateId = stateId;
        
        const label = document.createElement('label');
        label.htmlFor = checkboxId;
        label.textContent = stateInfo.label;
        
        // Добавляем цветовой индикатор
        const colorIndicator = document.createElement('span');
        colorIndicator.className = `color-indicator ${stateInfo.colorClass}`;
        
        // Собираем элементы
        checkboxContainer.appendChild(checkbox);
        checkboxContainer.appendChild(colorIndicator);
        checkboxContainer.appendChild(label);
        groupDiv.appendChild(checkboxContainer);
        
        // Сохраняем ссылку на элемент фильтра
        this.filterElements[stateId] = checkbox;
      });
      
      filterContainer.appendChild(groupDiv);
    });
  }
  
  /**
   * Настройка обработчиков событий
   */
  setupEventListeners() {
    // Обработчики изменения фильтров
    document.querySelectorAll('.filter-item input[type="checkbox"]').forEach(checkbox => {
      checkbox.addEventListener('change', () => this.applyFilters());
    });
    
    // Кнопка для сброса фильтров
    const resetButton = document.createElement('button');
    resetButton.textContent = 'Сбросить фильтры';
    resetButton.className = 'reset-filters';
    resetButton.addEventListener('click', () => this.resetFilters());
    
    const filterContainer = document.querySelector('.filter-panel');
    if (filterContainer) {
      filterContainer.appendChild(resetButton);
    }
    
    // Кнопка для утреннего ритуала
    const morningRitualButton = document.createElement('button');
    morningRitualButton.textContent = 'Утренний путь';
    morningRitualButton.className = 'morning-ritual';
    morningRitualButton.addEventListener('click', () => this.startMorningRitual());
    
    const insightPanel = document.querySelector('.insight-panel');
    if (insightPanel) {
      insightPanel.appendChild(morningRitualButton);
    }
  }
  
  /**
   * Применение выбранных фильтров
   */
  applyFilters() {
    // Собираем список активных состояний
    const activeStates = {};
    Object.entries(this.filterElements).forEach(([stateId, checkbox]) => {
      activeStates[stateId] = checkbox.checked;
    });
    
    // Фильтруем узлы
    const filteredNodes = dataService.nodes.filter(node => 
      activeStates[node.id] !== false);
    
    // Фильтруем связи на основе видимых узлов
    const filteredLinks = dataService.links.filter(link => 
      activeStates[link.source.id] !== false && 
      activeStates[link.target.id] !== false);
    
    // Обновляем данные и уведомляем подписчиков
    dataService.subscribers.forEach(callback => {
      try {
        callback({
          nodes: filteredNodes,
          links: filteredLinks,
          activeState: dataService.activeState,
          activeTransition: dataService.activeTransition
        });
      } catch (error) {
        console.error('Error in subscriber callback:', error);
      }
    });
  }
  
  /**
   * Сброс всех фильтров
   */
  resetFilters() {
    // Включаем все чекбоксы
    Object.values(this.filterElements).forEach(checkbox => {
      checkbox.checked = true;
    });
    
    // Применяем фильтры
    this.applyFilters();
  }
  
  /**
   * Запуск утреннего ритуала - последовательная визуализация пути
   */
  startMorningRitual() {
    // Путь для утреннего ритуала
    const morningPath = [
      {state: 'transition_liminal', duration: 1000},
      {state: 'presence_now', duration: 2000},
      {state: 'harmony_balance', duration: 2000},
      {state: 'home_authentic', duration: 2000},
      {state: 'gratitude_presence', duration: 3000}
    ];
    
    // Сбрасываем текущее активное состояние
    dataService.activeState = null;
    dataService.activeTransition = null;
    
    // Функция для последовательной активации состояний
    const activateSequentially = (index) => {
      if (index >= morningPath.length) return;
      
      const currentStep = morningPath[index];
      
      // Активируем текущее состояние
      dataService.activateState(currentStep.state);
      
      // Показываем информацию о шаге ритуала
      const insightPanel = document.getElementById('current-insight');
      const stateInfo = CONFIG.states[currentStep.state];
      
      insightPanel.innerHTML = `
        <h4>Утренний путь: ${stateInfo.label}</h4>
        <p>${stateInfo.description}</p>
        <p class="ritual-step">Шаг ${index + 1} из ${morningPath.length}</p>
      `;
      
      // Переходим к следующему шагу через указанное время
      setTimeout(() => {
        activateSequentially(index + 1);
      }, currentStep.duration);
    };
    
    // Запускаем последовательность
    activateSequentially(0);
  }
}

// Функция инициализации после загрузки DOM
function initStateController() {
  const stateController = new StateController();
}
