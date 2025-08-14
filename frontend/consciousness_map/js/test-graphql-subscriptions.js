/**
 * Тест GraphQL подписок для Resonance Liminal
 * MIT Media Lab inspired testing approach
 */

// Функция для тестирования GraphQL подписок
function testGraphQLSubscriptions() {
  console.log('🧪 Запуск тестов GraphQL подписок...');
  
  // Проверяем, что GraphQL клиент доступен
  if (!window.GraphQLClient) {
    console.error('❌ GraphQLClient не найден!');
    return;
  }
  
  // Проверяем, что GraphQLSubscriptionClient доступен
  if (!window.GraphQLSubscriptionClient) {
    console.error('❌ GraphQLSubscriptionClient не найден!');
    return;
  }
  
  // Создаем тестовый клиент
  const testClient = new GraphQLClient();
  
  // Тест 1: Подписка на изменения графа
  console.log('📊 Тест 1: Подписка на изменения графа');
  let graphChangesReceived = false;
  const graphSubscriptionId = testClient.subscribeToGraphChanges(data => {
    console.log('✅ Получены данные изменения графа:', data);
    graphChangesReceived = true;
    
    // Проверяем структуру данных
    if (data && data.graphChanged) {
      console.log('✅ Структура данных корректна');
    } else {
      console.error('❌ Неверная структура данных');
    }
  });
  
  // Тест 2: Подписка на изменения активного состояния
  console.log('🔄 Тест 2: Подписка на изменения активного состояния');
  let stateChangesReceived = false;
  const stateSubscriptionId = testClient.subscribeToActiveStateChanges(data => {
    console.log('✅ Получены данные изменения активного состояния:', data);
    stateChangesReceived = true;
    
    // Проверяем структуру данных
    if (data && data.activeStateChanged && data.activeStateChanged.currentState) {
      console.log('✅ Структура данных корректна');
    } else {
      console.error('❌ Неверная структура данных');
    }
  });
  
  // Тест 3: Подписка на выполнение переходов
  console.log('🔀 Тест 3: Подписка на выполнение переходов');
  let transitionExecutionsReceived = false;
  const transitionSubscriptionId = testClient.subscribeToTransitionExecutions(data => {
    console.log('✅ Получены данные о выполнении перехода:', data);
    transitionExecutionsReceived = true;
    
    // Проверяем структуру данных
    if (data && data.transitionExecuted && 
        data.transitionExecuted.sourceState && 
        data.transitionExecuted.targetState) {
      console.log('✅ Структура данных корректна');
    } else {
      console.error('❌ Неверная структура данных');
    }
  });
  
  // Тест 4: Симуляция активации состояния через GraphQL мутацию
  console.log('🧠 Тест 4: Активация состояния через GraphQL');
  testClient.activateConsciousnessState('PRESENCE_NOW')
    .then(result => {
      console.log('✅ Активация состояния выполнена:', result);
    })
    .catch(error => {
      console.error('❌ Ошибка активации состояния:', error);
    });
  
  // Проверка результатов через 5 секунд
  setTimeout(() => {
    console.log('📝 Результаты тестов:');
    console.log(`Тест 1 (изменения графа): ${graphChangesReceived ? '✅ PASSED' : '❌ FAILED'}`);
    console.log(`Тест 2 (изменения состояния): ${stateChangesReceived ? '✅ PASSED' : '❌ FAILED'}`);
    console.log(`Тест 3 (выполнение переходов): ${transitionExecutionsReceived ? '✅ PASSED' : '❌ FAILED'}`);
    
    // Отписываемся от всех подписок
    testClient.unsubscribe(graphSubscriptionId);
    testClient.unsubscribe(stateSubscriptionId);
    testClient.unsubscribe(transitionSubscriptionId);
    
    console.log('🧹 Тесты завершены, подписки очищены');
  }, 5000);
}

// Запускаем тесты после загрузки страницы
window.addEventListener('load', () => {
  // Добавляем кнопку для запуска тестов
  const testButton = document.createElement('button');
  testButton.innerText = 'Запустить тесты GraphQL подписок';
  testButton.className = 'test-button';
  testButton.onclick = testGraphQLSubscriptions;
  
  // Добавляем кнопку на страницу
  document.body.appendChild(testButton);
  
  console.log('🔍 Тесты GraphQL подписок готовы к запуску');
});
