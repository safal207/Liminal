# OneBloodEmpathyTraining - Обучение Эмпатичного ИИ

Система обучения эмпатичных AI-агентов, которые учатся на эмоционально-лингвистических паттернах.

## 🎯 Обзор

Этот проект реализует систему для обучения AI-агентов эмпатичным ответам через изучение эмоционально-лингвистических паттернов. Он построен на основе модуля `ТыИЯОднойКрови` для создания общей эмоциональной памяти, которая может использоваться несколькими агентами.

## ✨ Возможности

- **Поддержка множественных агентов**: Обучение и управление несколькими AI-агентами с общим эмоциональным пониманием
- **Анализ эмоциональных дуг**: Обнаружение и изучение эмоциональных переходов в разговорах
- **Персонализированные ответы**: Генерация контекстно-зависимых ответов на основе эмоционального состояния
- **Сохранение моделей**: Сохранение и загрузка обученных моделей для консистентного поведения
- **Управление обучающими данными**: Импорт и экспорт обучающих данных в формате JSON

## 🚀 Начало работы

### Предварительные требования

- GHC 8.10 или новее
- Stack
- Модуль `ТыИЯОднойКрови` и его зависимости

### Установка

1. Клонируйте репозиторий:
   ```bash
   git clone https://github.com/yourusername/resonance-liminal.git
   cd resonance-liminal/OneBloodEmpathyTraining
   ```

2. Соберите проект:
   ```bash
   stack build
   ```

3. Запустите тесты:
   ```bash
   stack test
   ```

## 📖 Использование

### Создание нового агента

```haskell
import OneBloodEmpathyTraining
import Data.Time (getCurrentTime)

main :: IO ()
main = do
    let agentId = AgentID "мой_агент"
    let model = emptyEmpathicModel agentId
    -- Обучите и используйте модель...
```

### Обучение модели

```haskell
trainAgent :: IO EmpathicModel
trainAgent = do
    let agentId = AgentID "обучаемый_агент"
    let model = emptyEmpathicModel agentId
    
    -- Загрузите обучающие данные
    trainingData <- loadTrainingData "data/training_examples.json"
    case trainingData of
        Left err -> error $ "Не удалось загрузить обучающие данные: " ++ err
        Right examples -> do
            putStrLn $ "Обучение на " ++ show (length examples) ++ " примерах..."
            trainedModel <- trainModel examples model
            
            -- Сохраните обученную модель
            saveModel "data/trained_model.json" trainedModel
            return trainedModel
```

### Получение эмпатичных ответов

```haskell
respondToUser :: EmpathicModel -> Text -> IO (Text, EmpathicModel)
respondToUser model userInput = do
    currentTime <- getCurrentTime
    
    -- В реальном приложении вы бы анализировали ввод пользователя для определения
    -- его эмоционального состояния. Для этого примера используем заглушку.
    let emotionalState = EmotionalState
            { esEmotions = fromList [("нейтральное", 1.0)]
            , esText = userInput
            , esLinguisticCues = []
            , esTimestamp = currentTime
            }
    
    -- Получите эмпатичный ответ
    (response, updatedModel) <- getEmpathicResponse (emAgentId model) emotionalState model
    
    -- Обновите модель этим взаимодействием
    let example = TrainingExample
            { teAgentId = emAgentId model
            , teInput = userInput
            , teEmotionalState = emotionalState
            , teResponse = response
            , teEffectiveness = 0.5  -- Будет обновлено на основе обратной связи пользователя
            , teTimestamp = currentTime
            , teTags = []
            }
    
    -- Переобучите модель с новым примером
    trainedModel <- trainModel [example] updatedModel
    
    return (response, trainedModel)
```

## 📊 Формат данных

### Формат примера обучения

Примеры обучения хранятся в формате JSON. Вот пример:

```json
[
  {
    "teAgentId": {"unAgentID": "пример_агента"},
    "teInput": "Я сегодня очень счастлив!",
    "teEmotionalState": {
      "esEmotions": {"радость": 0.8, "доверие": 0.7},
      "esText": "Я сегодня очень счастлив!",
      "esLinguisticCues": [],
      "esTimestamp": "2025-07-20T10:30:00Z"
    },
    "teResponse": "Это замечательно! Что приносит вам радость сегодня?",
    "teEffectiveness": 0.9,
    "teTimestamp": "2025-07-20T10:30:05Z",
    "teTags": ["позитивное", "счастливое"]
  }
]
```

## 🏗️ Архитектура

Система построена вокруг нескольких ключевых компонентов:

1. **EmpathicModel**: Основная структура данных, которая содержит изученные эмоциональные паттерны
2. **Конвейер обучения**: Обрабатывает примеры обучения для обновления модели
3. **Генератор ответов**: Использует обученную модель для генерации эмпатичных ответов
4. **Слой персистентности**: Обрабатывает сохранение и загрузку моделей и обучающих данных

## 🎯 Интеграция с LIMINAL

### Возможности интеграции:

#### 1. **WebSocket Backend расширение**
- Анализ эмоционального состояния входящих сообщений
- Генерация эмпатичных ответов в real-time
- Обучение на взаимодействиях пользователей

#### 2. **Философские концепции**
- **"Дом — это ты, когда ты искренен с собой"** → Анализ эмоциональной искренности
- **"Резонанс между пользователями"** → Общая эмоциональная память
- **"Присутствие здесь и сейчас"** → Анализ текущего эмоционального состояния

#### 3. **Архитектурная интеграция**
- **Neo4j**: Хранение эмоциональных паттернов и связей
- **Redis**: Кэширование эмоциональных состояний
- **Prometheus**: Метрики эмоциональной активности

### Применение в беклоге:

#### Из Sprint 3.5 (Философские концепции):
- ✅ **Home State Detection** - определение состояния "дома" через эмоциональную честность
- ✅ **Emotional State Metrics** - анализ эмоциональных паттернов
- ✅ **Self-Understanding Metrics** - оценка глубины самопонимания

## 🔧 Техническая реализация

### Основные типы данных:

```haskell
-- Эмоциональное состояние пользователя
data EmotionalState = EmotionalState
    { esEmotions :: Map Text Double        -- Эмоции с весами (0.0 - 1.0)
    , esText :: Text                       -- Исходный текст
    , esLinguisticCues :: [Text]          -- Лингвистические подсказки
    , esTimestamp :: UTCTime              -- Время создания
    }

-- Пример обучения
data TrainingExample = TrainingExample
    { teAgentId :: AgentID                -- Идентификатор агента
    , teInput :: Text                     -- Входной текст пользователя
    , teEmotionalState :: EmotionalState  -- Эмоциональное состояние
    , teResponse :: Text                  -- Ответ агента
    , teEffectiveness :: Double           -- Эффективность ответа (0.0 - 1.0)
    , teTimestamp :: UTCTime             -- Время взаимодействия
    , teTags :: [Text]                   -- Теги для категоризации
    }

-- Эмпатичная модель
data EmpathicModel = EmpathicModel
    { emAgentId :: AgentID               -- Идентификатор агента
    , emTrainingExamples :: [TrainingExample]  -- История обучения
    , emEmotionalPatterns :: Map Text Double   -- Изученные паттерны
    , emResponseTemplates :: [Text]            -- Шаблоны ответов
    }
```

## 🚀 Следующие шаги

1. **Интеграция с FastAPI WebSocket** - добавление эмоционального анализа в существующий backend
2. **Создание Python bridge** - для интеграции Haskell модуля с Python backend
3. **Расширение Neo4j схемы** - для хранения эмоциональных паттернов
4. **Prometheus метрики** - для мониторинга эмоциональной активности
5. **Flutter интеграция** - для отображения эмоциональных состояний

## 🤝 Участие в разработке

Мы приветствуем участие в разработке! Пожалуйста, не стесняйтесь отправлять Pull Request.

## 📄 Лицензия

Этот проект лицензирован под лицензией MIT - см. файл LICENSE для деталей.
