# Руководство по развертыванию - Resonance Liminal

## Требования

- Docker 20.10+
- Docker Compose 2.0+
- 4 GB RAM (минимум)
- 20 GB свободного места на диске
- Публичный IP или домен для production

## Быстрый старт

### 1. Подготовка

Клонируйте репозиторий:
```bash
git clone https://github.com/yourusername/resonance-liminal.git
cd resonance-liminal
```

Создайте файл `.env` с необходимыми переменными окружения:
```bash
cp .env.example .env
```

Отредактируйте файл `.env` и задайте безопасные пароли:
```
# Security
NEO4J_PASSWORD=your_secure_neo4j_password
REDIS_PASSWORD=your_secure_redis_password
GRAFANA_PASSWORD=your_secure_grafana_password

# AWS Configuration (for Datomic)
AWS_ACCESS_KEY_ID=your_aws_access_key
AWS_SECRET_ACCESS_KEY=your_aws_secret_key
AWS_REGION=your_aws_region
AWS_BUCKET=your_aws_bucket
```

### 2. Запуск в режиме разработки

```bash
docker-compose -f docker-compose.dev.yml up -d
```

Это запустит:
- Neo4j на порту 7474 (HTTP), 7473 (HTTPS) и 7687 (Bolt)
- Redis на порту 6379
- Backend в режиме разработки с автоперезагрузкой на порту 8000

### 3. Запуск в production режиме

```bash
docker-compose up -d
```

Это запустит все сервисы в production режиме:
- Neo4j
- Redis
- Backend
- Prometheus для мониторинга
- Grafana для визуализации метрик

## Масштабирование

### Горизонтальное масштабирование

Для запуска нескольких экземпляров backend:

```bash
docker-compose up -d --scale liminal-backend=3
```

Это запустит 3 экземпляра backend контейнера. Синхронизация между экземплярами будет осуществляться через Redis.

### Настройка балансировщика нагрузки

Для production рекомендуется использовать Nginx в качестве балансировщика нагрузки:

```bash
docker-compose -f docker-compose.yml -f docker-compose.nginx.yml up -d
```

## Мониторинг

### Prometheus

Prometheus доступен по адресу:
```
http://your-server:9090
```

### Grafana

Grafana доступна по адресу:
```
http://your-server:3000
```

Начальные учетные данные:
- Username: admin
- Password: значение GRAFANA_PASSWORD из .env файла

### Метрики приложения

Метрики приложения доступны по адресу:
```
http://your-server:8000/metrics
```

## Резервное копирование

### Neo4j

Для резервного копирования Neo4j:

```bash
docker exec -it pythia-neo4j neo4j-admin dump --database=neo4j --to=/var/lib/neo4j/backups/neo4j-$(date +%F).dump
```

Для восстановления:

```bash
docker exec -it pythia-neo4j neo4j-admin load --database=neo4j --from=/var/lib/neo4j/backups/neo4j-YYYY-MM-DD.dump --force
```

### Redis

Для резервного копирования Redis (при включенном RDB):

```bash
docker cp pythia-redis:/data/dump.rdb ./redis-backup-$(date +%F).rdb
```

## Обновление

1. Остановите текущую версию:
   ```bash
   docker-compose down
   ```

2. Обновите код из репозитория:
   ```bash
   git pull
   ```

3. Запустите обновленную версию:
   ```bash
   docker-compose up -d --build
   ```

## Проверка работоспособности

### Health Check API

```bash
curl http://your-server:8000/health
```

Ожидаемый ответ:
```json
{"status":"ok","components":{"database":"ok","redis":"ok"}}
```

### Логирование

Логи backend доступны через Docker:

```bash
docker-compose logs -f liminal-backend
```

Также логи записываются в файл `backend/logs/app.log`.

## Устранение неисправностей

### Проблемы с подключением к Redis

1. Проверьте, что Redis контейнер запущен:
   ```bash
   docker-compose ps redis
   ```

2. Проверьте пароль Redis в файле `.env`

3. Проверьте доступность Redis:
   ```bash
   docker exec -it pythia-redis redis-cli -a $REDIS_PASSWORD ping
   ```
   
   Ожидаемый ответ: `PONG`

### Проблемы с подключением к Neo4j

1. Проверьте, что Neo4j контейнер запущен:
   ```bash
   docker-compose ps neo4j
   ```

2. Проверьте пароль Neo4j в файле `.env`

3. Откройте Neo4j Browser по адресу `http://your-server:7474`

### Проблемы с WebSocket

1. Проверьте логи backend:
   ```bash
   docker-compose logs -f liminal-backend
   ```

2. Проверьте метрики подключений:
   ```bash
   curl http://your-server:8000/debug/connections/stats
   ```

3. Убедитесь, что клиент использует правильный JWT токен
