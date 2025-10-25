# 🧠 PersonalityAdapter: персонализация и рекомендации

> «Случайности не случайны» — мастер Угвей

## Обзор

PersonalityAdapter — это универсальный слой персонализации, который позволяет хранить, анализировать и использовать данные о пользователе для создания персонализированного опыта. Модуль интегрируется с DatabaseAdapter для хранения данных в Datomic (временные данные) и Neo4j (связи и графы).

## Возможности

- 🔄 **Хранение эмоциональных состояний** — сохранение истории эмоций пользователя с контекстом
- 🎯 **Управление предпочтениями** — хранение и обновление предпочтений пользователя
- 🧩 **Генерация рекомендаций** — создание персонализированных рекомендаций на основе профиля
- 📊 **Анализ истории** — отслеживание изменений профиля во времени

## Архитектура

PersonalityAdapter использует:
- **Datomic** для хранения временных данных (эмоции, история)
- **Neo4j** для хранения связей (предпочтения, рекомендации)
- **GraphQL API** для гибких клиентских запросов

## GraphQL API

### Эндпоинты

- `/personality/graphql` — основной GraphQL эндпоинт
- `/personality/graphql/playground` — интерактивный GraphiQL интерфейс

### Типы данных

```graphql
type Emotion {
  type: String!
  intensity: Float!
  context: String
  timestamp: DateTime!
}

type Preference {
  category: String!
  value: String!
  strength: Float!
  lastUpdated: DateTime!
}

type Recommendation {
  id: String!
  content: String!
  confidence: Float!
  source: String!
  context: String
  createdAt: DateTime!
}

type PersonalityProfile {
  userId: String!
  emotionalHistory: [Emotion!]!
  preferences: [Preference!]!
  recommendations: [Recommendation!]!
  currentMood: Emotion
  dominantPreferences(limit: Int = 3): [Preference!]!
}
```

### Запросы (Query)

```graphql
# Получение полного профиля
query GetProfile($userId: String!) {
  personalityProfile(userId: $userId) {
    emotionalHistory {
      type
      intensity
      context
      timestamp
    }
    preferences {
      category
      value
      strength
    }
    currentMood {
      type
      intensity
    }
  }
}

# Получение рекомендаций
query GetRecommendations($userId: String!, $limit: Int = 5, $context: String) {
  recommendations(userId: $userId, limit: $limit, context: $context) {
    id
    content
    confidence
    source
  }
}
```

### Мутации (Mutation)

```graphql
# Сохранение эмоции
mutation StoreEmotion($userId: String!, $type: String!, $intensity: Float!, $context: String) {
  storeEmotion(userId: $userId, emotionType: $type, intensity: $intensity, context: $context) {
    type
    intensity
    timestamp
  }
}

# Обновление предпочтения
mutation UpdatePreference($userId: String!, $preference: PreferenceInput!) {
  updatePreference(userId: $userId, preference: $preference) {
    category
    value
    strength
  }
}
```

### Подписки (Subscription)

```graphql
# Подписка на обновления рекомендаций
subscription RecommendationUpdates($userId: String!) {
  recommendationUpdates(userId: $userId) {
    id
    content
    confidence
  }
}
```

## REST API

Для совместимости с существующими системами, PersonalityAdapter также предоставляет REST API:

- `POST /personality/emotion` — сохранение эмоции
- `GET /personality/profile` — получение профиля
- `GET /personality/recommendations` — получение рекомендаций

## Примеры использования

### Python клиент

```python
from liminal.personality import PersonalityAdapter

# Инициализация адаптера
adapter = PersonalityAdapter(user_id="123")

# Сохранение эмоции
await adapter.store_emotion(
    emotion_type="радость",
    intensity=0.8,
    context="завершение задачи"
)

# Получение профиля
profile = await adapter.get_profile()

# Получение рекомендаций
recommendations = await adapter.get_recommendations(limit=3, context="обучение")
```

### GraphQL клиент

```javascript
// Получение профиля
const GET_PROFILE = `
  query GetProfile($userId: String!) {
    personalityProfile(userId: $userId) {
      currentMood {
        type
        intensity
      }
    }
  }
`;

// Выполнение запроса
const response = await fetch('/personality/graphql', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({
    query: GET_PROFILE,
    variables: { userId: '123' }
  })
});

const data = await response.json();
console.log(data.data.personalityProfile.currentMood);
```

## Установка и настройка

### Зависимости

```bash
# Установка через WSL (рекомендуется)
wsl bash -lc "cd /mnt/c/Users/safal/OneDrive/Documente/GitHub/resonance-liminal && \
  python -m pip install -i https://pypi.org/simple --default-timeout 25 --no-cache-dir -vvv strawberry-graphql"
```

### Интеграция в проект

```python
# main.py
from backend.personality import router as personality_router

app.include_router(personality_router)
```

## Устранение неполадок

### Типичные проблемы

| Проблема | Решение |
|----------|---------|
| Ошибка импорта strawberry | Проверьте установку пакета `strawberry-graphql` |
| Ошибка подключения к Neo4j | Проверьте настройки подключения в `database_adapter.py` |
| Пустые рекомендации | Убедитесь, что у пользователя есть предпочтения |

### Логирование

PersonalityAdapter использует структурное логирование:

```python
# Пример логирования
self.logger.info(
    "Рекомендации сгенерированы",
    user_id=self.user_id,
    count=len(recommendations),
    context=context
)
```

## ML-интеграция для анализа эмоций

PersonalityAdapter интегрирован с ML-моделями для анализа эмоций в тексте пользователя:

### Возможности ML-анализа

- 🧠 **Анализ текста** — определение эмоций на основе текста пользователя
- 📊 **Эмоциональные измерения** — анализ по модели VAD (Valence-Arousal-Dominance)
- 🎯 **Предложения эмоций** — получение релевантных эмоций для контекста
- 📈 **Оценка уверенности** — определение точности анализа

### Примеры использования ML-анализа

```python
from liminal.personality import PersonalityAdapter

# Инициализация адаптера
adapter = PersonalityAdapter(user_id="123")

# Анализ эмоций в тексте
emotion_data = await adapter.analyze_text_emotion(
    text="Я очень рад, что всё работает!",
    context="тестирование системы"
)

print(f"Тип эмоции: {emotion_data['type']}")
print(f"Интенсивность: {emotion_data['intensity']}")
print(f"Уверенность: {emotion_data['ml_analysis']['confidence']}")

# Получение предложений по эмоциям
suggestions = await adapter.get_emotion_suggestions(context="обучение")
for suggestion in suggestions:
    print(f"{suggestion['emotion_type']}: {suggestion['description']}")
```

### GraphQL запросы для ML-анализа

```graphql
# Анализ эмоций в тексте
mutation AnalyzeTextEmotion($userId: String!, $text: String!, $context: String) {
  analyzeTextEmotion(userId: $userId, text: $text, context: $context) {
    type
    intensity
    context
    timestamp
    mlAnalysis {
      dimensions {
        valence
        arousal
        dominance
      }
      confidence
    }
  }
}

# Получение предложений по эмоциям
query GetEmotionSuggestions($userId: String!, $context: String) {
  emotionSuggestions(userId: $userId, context: $context) {
    emotionType
    description
  }
}
```

## Дальнейшее развитие

- ✅ Интеграция с ML моделями для анализа эмоций
- 🔄 Реализация real-time обновлений через WebSocket
- 🧩 Расширение типов рекомендаций
- 📊 Аналитика и визуализация профиля

## Принцип «Случайности не случайны»

Любые сбои в работе PersonalityAdapter — это точки роста для системы. Модуль спроектирован с учетом возможных ошибок и включает механизмы восстановления:

- Возврат пустого профиля вместо ошибки
- Логирование всех сбоев для анализа
- Постепенное обучение на основе ошибок
