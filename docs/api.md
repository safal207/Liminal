# API Reference - Resonance Liminal

## WebSocket API

### Основной эндпоинт

```
GET /ws
```

Подключение к WebSocket серверу.

#### Параметры URL
- `token` (обязательный): JWT токен для аутентификации

#### Пример
```
ws://localhost:8000/ws?token=your.jwt.token
```

### События

#### Подключение
После успешного подключения вы получите сообщение:
```json
{
  "type": "connection_established",
  "connection_id": "unique_connection_id"
}
```

#### Подписка на канал
Для подписки на канал отправьте:
```json
{
  "type": "subscribe",
  "channel": "channel_name"
}
```

Ответ при успешной подписке:
```json
{
  "type": "subscribe_success",
  "channel": "channel_name"
}
```

#### Отписка от канала
Для отписки от канала отправьте:
```json
{
  "type": "unsubscribe",
  "channel": "channel_name"
}
```

Ответ при успешной отписке:
```json
{
  "type": "unsubscribe_success",
  "channel": "channel_name"
}
```

#### Отправка сообщения
Для отправки сообщения в канал:
```json
{
  "type": "message",
  "channel": "channel_name",
  "content": "Your message content"
}
```

Получение сообщения из канала:
```json
{
  "type": "message",
  "channel": "channel_name",
  "content": "Message content",
  "sender": "user_id",
  "timestamp": "2025-07-30T08:00:00Z"
}
```

## REST API

### Аутентификация

#### Получение токена

```
POST /token
```

Запрос:
```json
{
  "username": "your_username",
  "password": "your_password"
}
```

Ответ:
```json
{
  "access_token": "your.jwt.token",
  "token_type": "bearer",
  "expires_in": 3600
}
```

### Статистика и мониторинг

#### Метрики Prometheus

```
GET /metrics
```

Возвращает метрики в формате Prometheus.

#### Статистика подключений

```
GET /debug/connections/stats
```

Ответ:
```json
{
  "total_connections": 42,
  "connections_by_ip": {
    "127.0.0.1": 5,
    "192.168.1.10": 2
  },
  "channel_subscriptions": {
    "channel1": 10,
    "channel2": 5
  }
}
```

### Neo4j API

#### Получение данных из графа

```
GET /api/graph/{node_type}/{node_id}
```

Параметры:
- `node_type`: Тип узла (например, "user", "device")
- `node_id`: Идентификатор узла

Ответ:
```json
{
  "id": "node_id",
  "type": "node_type",
  "properties": {
    "name": "Node Name",
    "created_at": "2025-07-01T12:00:00Z"
  },
  "relationships": [
    {
      "type": "CONNECTED_TO",
      "direction": "outgoing",
      "target_id": "related_node_id",
      "properties": {
        "since": "2025-06-01T10:30:00Z"
      }
    }
  ]
}
```

#### Создание связи

```
POST /api/graph/relationship
```

Запрос:
```json
{
  "source_id": "source_node_id",
  "target_id": "target_node_id",
  "type": "RELATIONSHIP_TYPE",
  "properties": {
    "property1": "value1"
  }
}
```

Ответ:
```json
{
  "id": "relationship_id",
  "source_id": "source_node_id",
  "target_id": "target_node_id",
  "type": "RELATIONSHIP_TYPE",
  "created_at": "2025-07-30T08:15:30Z"
}
```

## Коды ошибок

| Код | Описание |
|-----|----------|
| 1000 | Нормальное закрытие |
| 1008 | Нарушение политики (ошибки аутентификации) |
| 1011 | Внутренняя ошибка сервера |
| 4001 | Неверный токен аутентификации |
| 4002 | Токен истек |
| 4003 | Недостаточно прав |
| 4004 | Канал не найден |
| 4005 | Превышен лимит подключений |
| 4006 | Превышен лимит подключений с одного IP |
| 5001 | Внутренняя ошибка сервера |

## Лимиты и ограничения

- Максимум 100 одновременных соединений на сервер
- Максимум 10 соединений с одного IP адреса
- Размер сообщения: до 64 KB
- Rate limiting: 100 сообщений в минуту для одного соединения
