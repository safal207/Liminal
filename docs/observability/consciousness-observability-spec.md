# Спецификация метрик и событий сознания

## Обзор

Наблюдаемость KPI сознания строится вокруг трёх подсистем:

1. **Таймлайн памяти** — непрерывный поток фрагментов воспоминаний и их трансляции подписчикам.
2. **ML-пайплайны** — обучение, загрузка и инференс моделей, управляющих резонансом.
3. **Neo4j** — графовая память, требующая контроля насыщенности пула соединений.

Настоящая спецификация описывает обязательные Prometheus-метрики и события, а также ожидаемые семантики обновления. Все метрики публикуются FastAPI-приложением и ML-экспортёром по `/_metrics`/`/ml_metrics` и автоматически подтягиваются Prometheus.

## Таймлайн памяти

| Метрика | Тип | Лейблы | Семантика | Обновление |
| --- | --- | --- | --- | --- |
| `memory_timeline_events_total` | Counter | `event_type` ∈ {`memory_added`, `subscriber_joined`, `subscriber_left`, `subscriber_dropped`, `notification_sent`, `notification_failed`, `notification_skipped`} | Счётчик событий, характеризующих жизненный цикл таймлайна | Инкремент при каждом событии соответствующего типа |
| `memory_timeline_processing_seconds` | Histogram | `operation` ∈ {`add_memory`, `notify_subscribers`} | Время выполнения критичных операций | Наблюдение продолжительности каждой операции |
| `memory_timeline_backlog_size` | Gauge | — | Размер буфера воспоминаний (последнее известное значение) | Обновляется после добавления памяти |
| `memory_timeline_subscribers` | Gauge | — | Количество активных подписчиков вебсокета | Обновляется при подписке/отписке и автозачистке |

### Событийный таймлайн

Приоритетные события и их привязка к метрикам:

1. **`memory_added`** — добавление нового фрагмента (`memory_timeline_events_total{event_type="memory_added"}`) и обновление размера буфера (`memory_timeline_backlog_size`).
2. **`subscriber_joined`/`subscriber_left`/`subscriber_dropped`** — изменения подписчиков (гейдж `memory_timeline_subscribers`).
3. **`notification_sent`/`notification_failed`/`notification_skipped`** — результаты рассылки события (`memory_timeline_processing_seconds{operation="notify_subscribers"}`).

## ML-пайплайны

| Метрика | Тип | Лейблы | Семантика | Обновление |
| --- | --- | --- | --- | --- |
| `ml_pipeline_runs_total` | Counter | `pipeline`, `stage` ∈ {`train`, `load`, `inference`}, `status` ∈ {`success`, `error`, `exception`, `missing_artifact`, `skipped`, `unknown`} | Итог выполнения стадий ML-процессов | Инкремент после завершения стадии |
| `ml_pipeline_run_duration_seconds` | Histogram | `pipeline`, `stage` | Длительность стадий обучения/загрузки/инференса | Наблюдение продолжительности каждой стадии |
| `ml_pipeline_queue_depth` | Gauge | `pipeline`, `stage` | Глубина очередей/буферов данных | Для обучения отражает число записей в датасете до запуска и сбрасывается после |
| `ml_pipeline_last_success_timestamp` | Gauge | `pipeline`, `stage` | Unix-время последнего успешного выполнения | Обновляется при статусе `success` |

### Поток событий

- **Подготовка данных** — фиксация глубины очереди (`ml_pipeline_queue_depth`).
- **Запуск и завершение стадии** — `ml_pipeline_runs_total` + `ml_pipeline_run_duration_seconds`.
- **Успешные завершения** — `ml_pipeline_last_success_timestamp`.

## Neo4j и насыщенность

| Метрика | Тип | Лейблы | Семантика | Обновление |
| --- | --- | --- | --- | --- |
| `neo4j_operations_total` | Counter | `operation`, `status` ∈ {`success`, `error`} | Количество операций Neo4j по типам | Инкрементируется после каждого обращения к БД |
| `neo4j_operation_duration_seconds` | Histogram | `operation` | Длительность операций | Наблюдение времени выполнения запросов |
| `neo4j_active_sessions` | Gauge | — | Активные сессии Neo4j в пуле | Увеличивается при старте операции и уменьшается после |
| `neo4j_saturation_ratio` | Gauge | — | Отношение активных сессий к лимиту пула (`NEO4J_MAX_POOL_SIZE`, по умолчанию 100) | Пересчитывается при каждом изменении активных сессий |

### Ожидаемые события

Каждая операция (`create_indexes`, `create_dunewave_node`, `list_*`, `link_dunewave_to_memory`, `create_mentorship`, `find_wisdom_fragments`) оборачивается счётчиком и таймером. Рост `neo4j_saturation_ratio` > 0.85 сигнализирует о приближении к насыщению пула соединений.

## Интеграция с Prometheus и Grafana

- Экспорт метрик доступен по `/metrics` в основном FastAPI и по `/ml_metrics/metrics` в ML-сервисе.
- Prometheus считывает метрики каждые 15 секунд (по умолчанию), конфигурация не меняется.
- Grafana использует панель «Consciousness KPI» и набор алертов из папки `Liminal`:
  - **Memory Timeline Backlog High** — `memory_timeline_backlog_size > 500` в течение 5 минут.
  - **ML Pipeline Success Ratio Low** — доля `status="success"` ниже 80 % за 15 минут.
  - **Neo4j Saturation High** — `neo4j_saturation_ratio > 0.85` в течение 3 минут.

## Автоматизированная проверка

Скрипт `tools/metrics_smoke_test.py` добавлен в CI и гарантирует наличие ключевых метрик в реестре Prometheus при импорте модулей.
