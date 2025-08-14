# 🚀 LIMINAL: Пошаговая дорожная карта развития проекта

## 🔹 Этап 1: Минимально работающий MVP (готово ✅)

* [x] `liminal.py` демонстрирует полный цикл (пульс → инсайт → переход)
* [x] Docker-инстансы: Neo4j, Redis, Datomic
* [x] InfluxDB имитация в CLI-режиме
* [x] Запись пульса в Redis (`send_pulse.py`)

## 🔹 Этап 2: Интеграция с реальными данными 📱

* [x] **Настроить WebSocket сервер на Python (FastAPI + Redis PubSub)** ✅
  * [x] FastAPI WebSocket endpoint `/ws/timeline`
  * [x] DoS protection (лимиты 100/10 подключений)
  * [x] Unified logging system
  * [x] Comprehensive testing suite
* [ ] Подключить Flutter к Redis/WebSocket:
  * [ ] `lib/services/redis_stream.dart`
  * [ ] `lib/screens/resonance_visualizer.dart`
* [ ] Настроить реальные метрики InfluxDB

## 🔹 Этап 3: Бэкенд-интеллект (Pythia)

* [x] **Neo4j интеграция** ✅ (базовая готова)
* [ ] Переписать `pythia_flow`, `pythia_time` в виде модулей, читающих:
  * InfluxDB (метрики)
  * Datomic (факты)
  * [x] Neo4j (граф-связи) ✅
* [ ] Вынести правила анализа в JSON/YAML:
  * `patterns/breathing.yaml`
  * `patterns/alertness.yaml`

## 🔹 Этап 4: Scripts Layer и пользовательские переходы

* [ ] FastAPI endpoint: `/scripts/run/{scenario_id}`
* [ ] Создать YAML-сценарии в `scripts/`
* [ ] Пример: `morning_focus.yaml`, `evening_relax.yaml`

## 🔹 Этап 5: ChronoAnalytics (визуализация времени)

* [ ] Использовать InfluxDB + Neo4j для визуализаций
* [ ] Подключить Vega-Lite или Dash
* [ ] Раздел: `dashboard/patterns_over_time`

## 🔹 Этап 6: Empath UI (реактивный интерфейс)

* [ ] Flutter: связать элементы интерфейса с `psychological_state`
* [ ] UI обновляется при смене резонанса (цвет, звук, вибрация)
* [ ] Использовать `provider` или `riverpod` для потоков данных

## 🔹 Этап 7: Feedback Loop и AI Training

* [ ] Логировать инсайты и переходы в `learning_log.json`
* [ ] Строить наборы данных для последующего обучения моделей
* [ ] Возможна интеграция с HuggingFace для дообучения

---

## 🔸 Последовательность этапов развития:

**Этап 1**: ✅ MVP с Docker и Neo4j - **ГОТОВО**
**Этап 2**: ✅ WebSocket Backend + DoS Protection - **ГОТОВО**
**Этап 3**: 🔧 Бэкенд-интеллект (Pythia) - **В ПРОЦЕССЕ**
**Этап 4**: 📜 Scripts Layer и YAML-сценарии
**Этап 5**: 📊 ChronoAnalytics (визуализация времени)
**Этап 6**: 📱 Empath UI (реактивный интерфейс)
**Этап 7**: 🧠 Feedback Loop и AI Training

📚 **Детальные задачи см. в** [PROJECT_BACKLOG_2025.md](PROJECT_BACKLOG_2025.md)

## ✨ Финальная цель:

Создание живой системы переходов, в которой каждый пользователь получает инсайты и рекомендации в режиме реального времени, а весь путь сохраняется как хроника эволюции сознания.

> "Сознание в потоке. Время в твоих руках. Переходы — осознанны."
