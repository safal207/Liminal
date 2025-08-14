/**
 * Data Service for Consciousness Map
 * MIT Media Lab inspired Observable Pattern implementation
 */

class DataService {
  constructor() {
    this.nodes = [];
    this.links = [];
    this.subscribers = [];
    this.activeState = null;
    this.activeTransition = null;
    this.socket = null;
    this.isConnected = false;
    this.apiType = CONFIG.defaultApiType || 'graphql'; // По умолчанию используем GraphQL
    this.graphqlClient = new GraphQLClient();
    this.neo4jClient = new Neo4jClient();
    this.subscriptions = [];
    this.graphSubscriptionId = null;
    this.activeStateSubscriptionId = null;
    this.transitionSubscriptionId = null;
    
    // Демо-данные для начального состояния
    this.initializeDemoData();
    
    // Если используем GraphQL API, инициализируем подписки
    if (this.apiType === 'graphql') {
      this.initializeGraphQLSubscriptions();
    } else if (this.apiType === 'websocket') {
      // Для обратной совместимости используем старый WebSocket
      this.connectWebSocket();
    }
  }
  
  /**
   * Установка WebSocket соединения
   */
  connectWebSocket() {
    this.socket = new WebSocket(CONFIG.api.wsEndpoint);
    
    this.socket.onopen = () => {
      console.log('WebSocket connection established');
      this.isConnected = true;
    };
    
    this.socket.onclose = () => {
      console.log('WebSocket connection closed');
      this.isConnected = false;
      
      // Переподключение через 3 секунды
      setTimeout(() => this.connectWebSocket(), 3000);
    };
    
    this.socket.onmessage = (event) => {
      try {
        const data = JSON.parse(event.data);
        if (data.type === 'state_transition') {
          this.handleTransitionEvent(data);
        }
      } catch (error) {
        console.error('Error processing WebSocket message:', error);
      }
    };
  }
  
  /**
   * Обработка события перехода состояния
   */
  handleTransitionEvent(event) {
    // Проверяем существование узлов
    let fromNodeIndex = this.nodes.findIndex(node => node.id === event.from_state);
    let toNodeIndex = this.nodes.findIndex(node => node.id === event.to_state);
    
    // Если узлы не существуют, создаем их
    if (fromNodeIndex === -1) {
      const newNode = {
        id: event.from_state,
        state: event.from_state,
        label: CONFIG.states[event.from_state]?.label || event.from_state,
        colorClass: CONFIG.states[event.from_state]?.colorClass || 'node-default',
        description: CONFIG.states[event.from_state]?.description || '',
        metrics: {
          presence_level: event.from_presence_level || 0.5,
          harmony_index: event.from_harmony_index || 0.5,
          authenticity_score: event.from_authenticity_score || 0.5,
          emotional_charge: event.from_emotional_charge || 50
        }
      };
      
      this.nodes.push(newNode);
      fromNodeIndex = this.nodes.length - 1;
    }
    
    if (toNodeIndex === -1) {
      const newNode = {
        id: event.to_state,
        state: event.to_state,
        label: CONFIG.states[event.to_state]?.label || event.to_state,
        colorClass: CONFIG.states[event.to_state]?.colorClass || 'node-default',
        description: CONFIG.states[event.to_state]?.description || '',
        metrics: {
          presence_level: event.to_presence_level || 0.5,
          harmony_index: event.to_harmony_index || 0.5,
          authenticity_score: event.to_authenticity_score || 0.5,
          emotional_charge: event.to_emotional_charge || 50
        }
      };
      
      this.nodes.push(newNode);
      toNodeIndex = this.nodes.length - 1;
    }
    
    // Создаем или обновляем связь между узлами
    const linkKey = `${event.from_state}-${event.to_state}`;
    let linkIndex = this.links.findIndex(link => 
      link.source.id === event.from_state && link.target.id === event.to_state);
    
    if (linkIndex === -1) {
      this.links.push({
        id: linkKey,
        source: this.nodes[fromNodeIndex],
        target: this.nodes[toNodeIndex],
        trigger: event.trigger,
        triggerLabel: CONFIG.triggers[event.trigger] || event.trigger,
        count: 1,
        lastTransitionData: event.trigger_data || {}
      });
    } else {
      this.links[linkIndex].count += 1;
      this.links[linkIndex].lastTransitionData = event.trigger_data || {};
    }
    
    // Анимация перехода
    this.activeTransition = {
      fromState: event.from_state,
      toState: event.to_state,
      trigger: event.trigger
    };
    
    // После анимации перехода активируем новое состояние
    setTimeout(() => {
      this.activeState = event.to_state;
      this.activeTransition = null;
      this.notifySubscribers();
    }, CONFIG.graph.links.transitionSpeed);
    
    // Уведомляем подписчиков
    this.notifySubscribers();
  }
  
  /**
   * Инициализирует GraphQL подписки
   */
  initializeGraphQLSubscriptions() {
    console.log('Инициализация GraphQL подписок...');
    
    // Подписка на изменения графа
    this.graphSubscriptionId = this.graphqlClient.subscribeToGraphChanges(data => {
      console.log('Получено обновление графа:', data);
      this.handleGraphChanges(data.graphChanged);
    });
    
    // Подписка на изменения активного состояния
    this.activeStateSubscriptionId = this.graphqlClient.subscribeToActiveStateChanges(data => {
      console.log('Получено обновление активного состояния:', data);
      this.handleActiveStateChange(data.activeStateChanged);
    });
    
    // Подписка на выполнение переходов
    this.transitionSubscriptionId = this.graphqlClient.subscribeToTransitionExecutions(data => {
      console.log('Получено уведомление о выполнении перехода:', data);
      this.handleTransitionExecution(data.transitionExecuted);
    });
    
    console.log('GraphQL подписки инициализированы');
  }
  
  /**
   * Обработчик изменений графа
   * @param {Object} changeData - Данные об изменении графа
   */
  handleGraphChanges(changeData) {
    if (!changeData) return;
    
    const { event, node, link } = changeData;
    
    switch (event) {
      case 'NODE_CREATED':
      case 'NODE_UPDATED':
        if (node) {
          const existingNodeIndex = this.nodes.findIndex(n => n.id === node.id);
          if (existingNodeIndex >= 0) {
            // Обновляем существующий узел
            this.nodes[existingNodeIndex] = {
              ...this.nodes[existingNodeIndex],
              ...node,
              colorClass: CONFIG.states[node.state]?.colorClass || 'node-default'
            };
          } else {
            // Добавляем новый узел
            this.nodes.push({
              ...node,
              colorClass: CONFIG.states[node.state]?.colorClass || 'node-default',
              metrics: node.metrics || {
                presence_level: 0.5,
                harmony_index: 0.5,
                authenticity_score: 0.5,
                emotional_charge: 50
              }
            });
          }
        }
        break;
        
      case 'NODE_DELETED':
        if (node) {
          this.nodes = this.nodes.filter(n => n.id !== node.id);
          // Удаляем также связи с этим узлом
          this.links = this.links.filter(l => 
            l.source.id !== node.id && l.target.id !== node.id);
        }
        break;
        
      case 'LINK_CREATED':
      case 'LINK_UPDATED':
        if (link) {
          const sourceNode = this.nodes.find(n => n.id === link.source.id);
          const targetNode = this.nodes.find(n => n.id === link.target.id);
          
          if (sourceNode && targetNode) {
            const existingLinkIndex = this.links.findIndex(l => 
              l.source.id === link.source.id && l.target.id === link.target.id);
              
            if (existingLinkIndex >= 0) {
              // Обновляем существующую связь
              this.links[existingLinkIndex] = {
                ...this.links[existingLinkIndex],
                ...link,
                source: sourceNode,
                target: targetNode,
                triggerLabel: CONFIG.triggers[link.trigger] || link.trigger
              };
            } else {
              // Добавляем новую связь
              this.links.push({
                id: `${link.source.id}-${link.target.id}`,
                source: sourceNode,
                target: targetNode,
                trigger: link.trigger,
                triggerLabel: CONFIG.triggers[link.trigger] || link.trigger,
                count: link.count || 1,
                lastTransitionData: link.lastTransitionData || {}
              });
            }
          }
        }
        break;
        
      case 'LINK_DELETED':
        if (link) {
          this.links = this.links.filter(l => 
            !(l.source.id === link.source.id && l.target.id === link.target.id));
        }
        break;
    }
    
    this.notifySubscribers();
  }
  
  /**
   * Обработчик изменения активного состояния
   * @param {Object} stateData - Данные об изменении активного состояния
   */
  handleActiveStateChange(stateData) {
    if (!stateData) return;
    
    const { currentState } = stateData;
    if (currentState) {
      this.activeState = currentState;
      this.notifySubscribers();
    }
  }
  
  /**
   * Обработчик выполнения перехода
   * @param {Object} transitionData - Данные о выполненном переходе
   */
  handleTransitionExecution(transitionData) {
    if (!transitionData) return;
    
    const { sourceState, targetState, trigger, timestamp } = transitionData;
    
    // Создаем активный переход для анимации
    this.activeTransition = {
      fromState: sourceState,
      toState: targetState,
      trigger: trigger
    };
    
    // После анимации перехода активируем новое состояние
    setTimeout(() => {
      this.activeState = targetState;
      this.activeTransition = null;
      this.notifySubscribers();
    }, CONFIG.graph.links.transitionSpeed);
    
    // Уведомляем подписчиков о начале анимации
    this.notifySubscribers();
  }
  
  /**
   * Загрузка данных из Neo4j через прокси-API
   * @param {boolean} useGraphQL - Использовать ли GraphQL API вместо REST
   */
  async loadDataFromNeo4j(useGraphQL = true) {
    try {
      let data;
      
      if (useGraphQL) {
        // Используем GraphQL API для более гибких запросов
        console.log('Загрузка данных через GraphQL API...');
        const graphData = await this.graphqlClient.getConsciousnessGraph();
        
        // Преобразуем данные из GraphQL формата в формат совместимый с существующим кодом
        data = {
          nodes: graphData.nodes,
          transitions: graphData.links.map(link => ({
            from_id: link.source.id,
            to_id: link.target.id,
            trigger: link.trigger,
            count: link.count,
            trigger_data: link.lastTransitionData
          }))
        };
      } else {
        // Используем REST API (старый способ)
        console.log('Загрузка данных через REST API...');
        const response = await fetch(`${CONFIG.api.neo4jEndpoint}/graph`);
        data = await response.json();
      }
      
      this.nodes = data.nodes.map(node => ({
        id: node.id,
        state: node.state,
        label: CONFIG.states[node.state]?.label || node.state,
        colorClass: CONFIG.states[node.state]?.colorClass || 'node-default',
        description: CONFIG.states[node.state]?.description || '',
        metrics: {
          presence_level: node.presence_level || 0.5,
          harmony_index: node.harmony_index || 0.5,
          authenticity_score: node.authenticity_score || 0.5,
          emotional_charge: node.emotional_charge || 50
        }
      }));
      
      this.links = data.transitions.map(transition => {
        const source = this.nodes.find(node => node.id === transition.from_id);
        const target = this.nodes.find(node => node.id === transition.to_id);
        
        return {
          id: `${transition.from_id}-${transition.to_id}`,
          source,
          target,
          trigger: transition.trigger,
          triggerLabel: CONFIG.triggers[transition.trigger] || transition.trigger,
          count: transition.count || 1,
          lastTransitionData: transition.trigger_data || {}
        };
      });
      
      // Уведомляем подписчиков
      this.notifySubscribers();
    } catch (error) {
      console.error('Error loading data from Neo4j:', error);
    }
  }
  
  /**
   * Инициализация демо-данных для начального состояния
   */
  initializeDemoData() {
    // Создаем узлы для всех состояний из конфига
    this.nodes = Object.entries(CONFIG.states).map(([state, info]) => ({
      id: state,
      state: state,
      label: info.label,
      colorClass: info.colorClass,
      description: info.description,
      metrics: {
        presence_level: Math.random() * 0.5 + 0.3,
        harmony_index: Math.random() * 0.5 + 0.3,
        authenticity_score: Math.random() * 0.5 + 0.3,
        emotional_charge: Math.random() * 50 + 30
      }
    }));
    
    // Создаем демо-связи между состояниями
    this.links = [
      this.createDemoLink('doubt_creative', 'curious_flow', 'curiosity_sparked'),
      this.createDemoLink('anger_protection', 'boundary_integrity', 'boundary_consciously_set'),
      this.createDemoLink('harmony_balance', 'withdrawal_protection', 'emotional_overwhelm_detected'),
      this.createDemoLink('grief_transformation', 'home_authentic', 'grief_consciously_felt'),
      this.createDemoLink('home_authentic', 'gratitude_presence', 'gratitude_practice_engaged'),
      this.createDemoLink('transition_liminal', 'presence_now', 'deep_breathing_meditation'),
      this.createDemoLink('presence_now', 'harmony_balance', 'meditation_practice'),
      this.createDemoLink('harmony_balance', 'home_authentic', 'authenticity_insight'),
      this.createDemoLink('home_authentic', 'question_space', 'right_question_asked')
    ];
  }
  
  /**
   * Вспомогательный метод для создания демо-связи
   */
  createDemoLink(sourceId, targetId, triggerId) {
    const source = this.nodes.find(node => node.id === sourceId);
    const target = this.nodes.find(node => node.id === targetId);
    
    if (!source || !target) return null;
    
    return {
      id: `${sourceId}-${targetId}`,
      source,
      target,
      trigger: triggerId,
      triggerLabel: CONFIG.triggers[triggerId] || triggerId,
      count: Math.floor(Math.random() * 5) + 1,
      lastTransitionData: {
        insight: CONFIG.transitionInsights[`${sourceId}_${targetId}`] || 
                "Осознанный переход между состояниями сознания",
        intensity: Math.random(),
        duration_seconds: Math.random() * 10 + 2
      }
    };
  }
  
  /**
   * Активация состояния при клике
   */
  activateState(stateId) {
    // Если используем GraphQL, отправляем мутацию для активации состояния
    if (this.apiType === 'graphql') {
      this.graphqlClient.activateConsciousnessState(stateId)
        .then(result => {
          if (!result.activateConsciousnessState.success) {
            console.error('Ошибка активации состояния:', result.activateConsciousnessState.message);
          }
        })
        .catch(error => {
          console.error('Ошибка при вызове мутации активации состояния:', error);
          // В случае ошибки, все равно обновляем локальное состояние
          this.activeState = stateId;
          this.activeTransition = null;
          this.notifySubscribers();
        });
    } else {
      // Для обратной совместимости просто обновляем локальное состояние
      this.activeState = stateId;
      this.activeTransition = null;
      this.notifySubscribers();
    }
  }
  
  /**
   * Подписка на изменения данных (Observable Pattern)
   */
  subscribe(callback) {
    this.subscribers.push(callback);
    return () => {
      this.subscribers = this.subscribers.filter(cb => cb !== callback);
    };
  }
  
  /**
   * Уведомление всех подписчиков
   */
  notifySubscribers() {
    this.subscribers.forEach(callback => {
      try {
        callback({
          nodes: this.nodes,
          links: this.links,
          activeState: this.activeState,
          activeTransition: this.activeTransition
        });
      } catch (error) {
        console.error('Error in subscriber callback:', error);
      }
    });
  }
  
  /**
   * Создает новое состояние сознания
   * @param {Object} stateData - Данные нового состояния
   * @returns {Promise} - Промис с результатом операции
   */
  async createConsciousnessState(stateData) {
    if (this.apiType === 'graphql') {
      return this.graphqlClient.createConsciousnessState(stateData);
    }
    return Promise.reject(new Error('Метод доступен только при использовании GraphQL API'));
  }
  
  /**
   * Обновляет существующее состояние сознания
   * @param {string} stateId - ID состояния
   * @param {Object} stateData - Новые данные состояния
   * @returns {Promise} - Промис с результатом операции
   */
  async updateConsciousnessState(stateId, stateData) {
    if (this.apiType === 'graphql') {
      return this.graphqlClient.updateConsciousnessState(stateId, stateData);
    }
    return Promise.reject(new Error('Метод доступен только при использовании GraphQL API'));
  }
  
  /**
   * Удаляет состояние сознания
   * @param {string} stateId - ID состояния для удаления
   * @returns {Promise} - Промис с результатом операции
   */
  async deleteConsciousnessState(stateId) {
    if (this.apiType === 'graphql') {
      return this.graphqlClient.deleteConsciousnessState(stateId);
    }
    return Promise.reject(new Error('Метод доступен только при использовании GraphQL API'));
  }
  
  /**
   * Создает новый переход между состояниями
   * @param {Object} transitionData - Данные нового перехода
   * @returns {Promise} - Промис с результатом операции
   */
  async createTransition(transitionData) {
    if (this.apiType === 'graphql') {
      return this.graphqlClient.createTransition(transitionData);
    }
    return Promise.reject(new Error('Метод доступен только при использовании GraphQL API'));
  }
  
  /**
   * Выполняет переход между состояниями
   * @param {string} transitionId - ID перехода для выполнения
   * @returns {Promise} - Промис с результатом операции
   */
  async executeTransition(transitionId) {
    if (this.apiType === 'graphql') {
      return this.graphqlClient.executeTransition(transitionId);
    }
    return Promise.reject(new Error('Метод доступен только при использовании GraphQL API'));
  }
  
  /**
   * Очистка ресурсов при уничтожении сервиса
   */
  destroy() {
    // Отписываемся от всех GraphQL подписок
    if (this.graphSubscriptionId) {
      this.graphqlClient.unsubscribe(this.graphSubscriptionId);
    }
    
    if (this.activeStateSubscriptionId) {
      this.graphqlClient.unsubscribe(this.activeStateSubscriptionId);
    }
    
    if (this.transitionSubscriptionId) {
      this.graphqlClient.unsubscribe(this.transitionSubscriptionId);
    }
    
    // Закрываем WebSocket соединение, если оно открыто
    if (this.socket) {
      this.socket.close();
    }
  }
}

// Создаем глобальный экземпляр сервиса данных
const dataService = new DataService();

// Обеспечиваем корректную очистку ресурсов при закрытии страницы
window.addEventListener('beforeunload', () => {
  dataService.destroy();
});
