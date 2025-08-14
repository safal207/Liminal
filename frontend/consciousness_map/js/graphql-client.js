/**
 * GraphQL Client для Resonance Liminal
 * Реализует принципы "Пути Чистоты" через гибкие запросы данных
 */
class GraphQLClient {
  constructor() {
    this.endpoint = CONFIG.api.graphqlEndpoint;
    this.subscriptionUrl = CONFIG.api.graphqlSubscriptionsEndpoint;
    this.subscriptionClient = null;
  }

  /**
   * Выполняет GraphQL запрос
   * @param {string} query - GraphQL запрос
   * @param {Object} variables - Переменные запроса
   * @returns {Promise<Object>} - Результат запроса
   */
  async query(query, variables = {}) {
    try {
      const response = await fetch(this.endpoint, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Accept': 'application/json',
        },
        body: JSON.stringify({
          query,
          variables
        })
      });

      const result = await response.json();
      
      if (result.errors) {
        console.error('GraphQL ошибки:', result.errors);
        throw new Error(`GraphQL ошибка: ${result.errors[0].message}`);
      }
      
      return result.data;
    } catch (error) {
      console.error('Ошибка GraphQL запроса:', error);
      throw error;
    }
  }

  /**
   * Выполняет GraphQL мутацию
   * @param {string} mutation - GraphQL мутация
   * @param {Object} variables - Переменные для мутации
   * @returns {Promise<Object>} - Результат мутации
   */
  async mutate(mutation, variables = {}) {
    // Мутации используют тот же механизм, что и запросы
    return this.query(mutation, variables);
  }

  /**
   * Инициализирует клиент подписок, если он еще не создан
   * @returns {GraphQLSubscriptionClient} - Клиент подписок
   */
  getSubscriptionClient() {
    if (!this.subscriptionClient && this.subscriptionUrl) {
      this.subscriptionClient = new GraphQLSubscriptionClient(this.subscriptionUrl);
    }
    return this.subscriptionClient;
  }

  /**
   * Подписывается на GraphQL подписку
   * @param {string} subscription - GraphQL подписка
   * @param {Object} variables - Переменные для подписки
   * @param {Function} onData - Обработчик полученных данных
   * @param {Function} onError - Обработчик ошибок
   * @returns {string} - ID подписки для последующей отписки
   */
  subscribe(subscription, variables = {}, onData, onError) {
    const client = this.getSubscriptionClient();
    if (!client) {
      throw new Error('Subscription client not initialized. Check if subscriptionUrl is provided.');
    }
    
    return client.subscribe(
      subscription,
      variables,
      onData,
      onError
    );
  }

  /**
   * Отписывается от GraphQL подписки
   * @param {string} subscriptionId - ID подписки
   */
  unsubscribe(subscriptionId) {
    if (this.subscriptionClient) {
      this.subscriptionClient.unsubscribe(subscriptionId);
    }
  }

  /**
   * Получает граф сознания
   * @returns {Promise<Object>} - Данные графа сознания
   */
  async getConsciousnessGraph() {
    const query = `
      query {
        consciousnessGraph {
          nodes {
            id
            state
            label
            description
            colorClass
            metrics {
              clarity
              depth
              resonance
              purity
            }
            philosophicalAspects {
              pathOfPurity
              homeStateDetection
              resonanceFrequency
              innerTruthfulness
            }
          }
          links {
            id
            source { id }
            target { id }
            trigger
            triggerLabel
            count
            philosophicalSignificance
            temporalPatterns {
              frequency
              averageDuration
              cycleDetection
            }
          }
          activeState
          pathOfPurityScore
        }
      }
    `;

    const data = await this.query(query);
    return data.consciousnessGraph;
  }

  /**
   * Создает новое состояние сознания
   * @param {Object} stateInput - Данные нового состояния
   * @returns {Promise<Object>} - Результат создания
   */
  async createConsciousnessState(stateInput) {
    const mutation = `
      mutation CreateConsciousnessState($input: ConsciousnessStateInput!) {
        createConsciousnessState(input: $input) {
          success
          message
          stateId
        }
      }
    `;
    
    return this.mutate(mutation, { input: stateInput });
  }

  /**
   * Обновляет существующее состояние сознания
   * @param {string} id - ID состояния
   * @param {Object} stateInput - Новые данные состояния
   * @returns {Promise<Object>} - Результат обновления
   */
  async updateConsciousnessState(id, stateInput) {
    const mutation = `
      mutation UpdateConsciousnessState($id: ID!, $input: ConsciousnessStateInput!) {
        updateConsciousnessState(id: $id, input: $input) {
          success
          message
        }
      }
    `;
    
    return this.mutate(mutation, { id, input: stateInput });
  }

  /**
   * Удаляет состояние сознания
   * @param {string} id - ID состояния для удаления
   * @returns {Promise<Object>} - Результат удаления
   */
  async deleteConsciousnessState(id) {
    const mutation = `
      mutation DeleteConsciousnessState($id: ID!) {
        deleteConsciousnessState(id: $id) {
          success
          message
        }
      }
    `;
    
    return this.mutate(mutation, { id });
  }

  /**
   * Активирует состояние сознания
   * @param {string} id - ID состояния для активации
   * @returns {Promise<Object>} - Результат активации
   */
  async activateConsciousnessState(id) {
    const mutation = `
      mutation ActivateConsciousnessState($id: ID!) {
        activateConsciousnessState(id: $id) {
          success
          message
        }
      }
    `;
    
    return this.mutate(mutation, { id });
  }

  /**
   * Создает новый переход между состояниями
   * @param {Object} transitionInput - Данные нового перехода
   * @returns {Promise<Object>} - Результат создания
   */
  async createTransition(transitionInput) {
    const mutation = `
      mutation CreateTransition($input: TransitionInput!) {
        createTransition(input: $input) {
          success
          message
          transitionId
        }
      }
    `;
    
    return this.mutate(mutation, { input: transitionInput });
  }

  /**
   * Выполняет переход между состояниями
   * @param {string} id - ID перехода для выполнения
   * @returns {Promise<Object>} - Результат выполнения
   */
  async executeTransition(id) {
    const mutation = `
      mutation ExecuteTransition($id: ID!) {
        executeTransition(id: $id) {
          success
          message
        }
      }
    `;
    
    return this.mutate(mutation, { id });
  }

  /**
   * Подписывается на изменения графа сознания
   * @param {Function} onData - Обработчик полученных данных
   * @returns {string} - ID подписки
   */
  subscribeToGraphChanges(onData) {
    const subscription = `
      subscription {
        graphChanged {
          event
          node {
            id
            state
            label
          }
          link {
            id
            source { id }
            target { id }
          }
        }
      }
    `;
    
    return this.subscribe(subscription, {}, onData);
  }

  /**
   * Подписывается на изменения активного состояния
   * @param {Function} onData - Обработчик полученных данных
   * @returns {string} - ID подписки
   */
  subscribeToActiveStateChanges(onData) {
    const subscription = `
      subscription {
        activeStateChanged {
          previousState
          currentState
          timestamp
        }
      }
    `;
    
    return this.subscribe(subscription, {}, onData);
  }

  /**
   * Подписывается на выполнение переходов
   * @param {Function} onData - Обработчик полученных данных
   * @returns {string} - ID подписки
   */
  subscribeToTransitionExecutions(onData) {
    const subscription = `
      subscription {
        transitionExecuted {
          transitionId
          sourceState
          targetState
          trigger
          timestamp
        }
      }
    `;
    
    return this.subscribe(subscription, {}, onData);
  }
}

// Экспортируем класс
if (typeof module !== 'undefined') {
  module.exports = GraphQLClient;
}
