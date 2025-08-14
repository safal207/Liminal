/**
 * Configuration for Consciousness Map Visualization
 * MIT Media Lab inspired configuration approach
 */

const CONFIG = {
  // Сетевые настройки
  api: {
    websocketEndpoint: 'ws://localhost:8080/ws',
    neo4jEndpoint: 'http://localhost:8080/neo4j',
    graphqlEndpoint: 'http://localhost:8080/graphql',
    graphqlSubscriptionsEndpoint: 'ws://localhost:8080/graphql/subscriptions'
  },
  
  // Настройки графа
  graph: {
    simulation: {
      strength: -400,         // Сила отталкивания между узлами
      distance: 200,          // Оптимальная дистанция между связанными узлами
      centerForce: 0.03,      // Сила притяжения к центру
      collideRadius: 60       // Радиус коллизии узлов
    },
    nodes: {
      defaultRadius: 40,      // Стандартный размер узла
      activeRadius: 45,       // Размер активного узла
      labelOffset: 15,        // Смещение текстовой метки
      fontSize: 12            // Размер шрифта меток
    },
    links: {
      defaultWidth: 3,        // Стандартная ширина связи
      activeWidth: 5,         // Ширина активной связи
      transitionSpeed: 800    // Скорость анимации перехода в мс
    }
  },
  
  // Соответствие состояний и их метаданных
  states: {
    // Базовые философские состояния
    'home_authentic': {
      label: 'Дом (Аутентичность)',
      description: 'Состояние искренности с собой',
      colorClass: 'node-home_authentic'
    },
    'resonance_flow': {
      label: 'Резонанс',
      description: 'Поток резонанса с другими',
      colorClass: 'node-resonance_flow'
    },
    'presence_now': {
      label: 'Присутствие',
      description: 'Присутствие здесь и сейчас',
      colorClass: 'node-presence_now'
    },
    'question_space': {
      label: 'Пространство вопросов',
      description: 'Пространство правильных вопросов',
      colorClass: 'node-question_space'
    },
    'harmony_balance': {
      label: 'Гармония',
      description: 'Гармония от понимания себя',
      colorClass: 'node-harmony_balance'
    },
    'transition_liminal': {
      label: 'Переход',
      description: 'Переходное состояние',
      colorClass: 'node-transition_liminal'
    },
    
    // Эмоциональные состояния
    'doubt_creative': {
      label: 'Творческое сомнение',
      description: 'Творческое сомнение как ресурс',
      colorClass: 'node-doubt_creative'
    },
    'curious_flow': {
      label: 'Поток любопытства',
      description: 'Состояние открытого исследования',
      colorClass: 'node-curious_flow'
    },
    'anger_protection': {
      label: 'Защитный гнев',
      description: 'Защитный гнев как граница',
      colorClass: 'node-anger_protection'
    },
    'boundary_integrity': {
      label: 'Целостность границ',
      description: 'Целостность личных границ',
      colorClass: 'node-boundary_integrity'
    },
    'withdrawal_protection': {
      label: 'Защитное уединение',
      description: 'Уход для самозащиты',
      colorClass: 'node-withdrawal_protection'
    },
    'grief_transformation': {
      label: 'Трансформация горя',
      description: 'Трансформация через горе',
      colorClass: 'node-grief_transformation'
    },
    'gratitude_presence': {
      label: 'Благодарность',
      description: 'Благодарность в настоящем',
      colorClass: 'node-gratitude_presence'
    }
  },
  
  // Триггеры переходов и их описания
  triggers: {
    'thyroid_insight_release': 'Освобождение тиреоидной системы',
    'moment_of_self_honesty': 'Момент честности с собой',
    'resonance_with_other': 'Резонанс с другим человеком',
    'right_question_asked': 'Задан правильный вопрос',
    'harmony_recognized': 'Распознавание гармонии',
    'presence_anchored': 'Заякоривание присутствия',
    'deep_breathing_meditation': 'Глубокое дыхание',
    'meditation_practice': 'Практика медитации',
    'authenticity_insight': 'Инсайт аутентичности',
    
    // Эмоциональные триггеры
    'allowing_creative_doubt': 'Принятие творческого сомнения',
    'curiosity_sparked': 'Вспышка любопытства',
    'boundary_violation_detected': 'Обнаружение нарушения границ',
    'healthy_anger_released': 'Здоровое выражение гнева',
    'boundary_consciously_set': 'Осознанная установка границ',
    'emotional_overwhelm_detected': 'Обнаружение эмоциональной перегрузки',
    'grief_consciously_felt': 'Осознанное проживание горя',
    'gratitude_practice_engaged': 'Практика благодарности'
  },
  
  // Философские инсайты для каждого перехода
  transitionInsights: {
    'doubt_creative_curious_flow': 'Сомнение, принятое с любопытством, становится мостом к новому пониманию',
    'anger_protection_boundary_integrity': 'Здоровый гнев — это не разрушение, а созидание границ',
    'harmony_balance_withdrawal_protection': 'Иногда отступление — это не бегство, а стратегическая перегруппировка',
    'grief_transformation_home_authentic': 'В глубине горя раскрывается суть нашей подлинности',
    'home_authentic_gratitude_presence': 'Благодарность — это естественное состояние подлинного бытия'
  }
};
