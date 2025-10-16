# 🚀 Спринт 4: Resilient Scale‑Out & Observability (14–21.08.2025)

## 🎯 Цели
1) Надёжный scale‑out WebSocket через Redis shared state
2) Проверка rate limiting под нагрузкой (SLO)
3) Наблюдаемость: дашборды + алерты

## 📊 Метрики успеха
- 2 инстанса backend проходят e2e (connect/send/broadcast/reconnect)
- RL удерживает лимиты при hard burst; p95 < 100 ms, errors < 1%
- Алерт readiness срабатывает ≤ 5 минут

## 📦 Deliverables
- Redis‑реестр подключений (TTL, presence)
- Multi‑instance compose: 2× backend + Redis (broadcast, reconnection)
- CI smoke‑нагрузка (Artillery) с порогами (SLO)
- Grafana панель: WS, rate_limit, HTTP 429
- Prometheus rules: readiness_down, rl_spike
- Документация: HOWTO scale‑out + обновления README

## 🧩 Бэклог задач (оценки)
- Redis presence (set/hash + TTL), lifecycle хуки on_connect/on_disconnect — 0.5д
- Reconnection cross‑instance + e2e тест — 0.5д
- Pub/Sub broadcast e2e (2 инстанса) — 0.5д
- Artillery профили: soft/hard burst; пороги SLO — 0.5д
- CI job: artillery run + fail по SLO — 0.5д
- Grafana dashboard (WS, RL, 429, CPU/lat) — 0.5д
- Prometheus rules (readiness_down, rl_spike) — 0.5д
- docs/LOAD_TESTING.md + README ссылки — 0.5д

## 🗓️ Таймлайн (7 дней)
- Д1: Redis presence + lifecycle хуки
- Д2: Reconnection e2e + тесты
- Д3: Multi‑instance compose + Pub/Sub проверки
- Д4: Профили Artillery + локальные SLO
- Д5: CI smoke‑job с порогами
- Д6: Grafana + Prometheus алерты
- Д7: Документация, DoD, ретро

## ✅ Definition of Done
- 2 инстанса проходят все e2e сценарии
- RL стабилен под burst (нет «проскоков»)
- CI Artillery: p95 < 100 ms, errors < 1%
- Алерты активны, дашборд опубликован
- Документация обновлена (HOWTO + README)

## ⚠️ Риски и смягчение
- Сетевые флейки Redis — retry/backoff, короткие таймауты
- FP в RL — буферные окна, корректный clock sync
- Нагрузка CI — облегчённый smoke‑профиль, nightly full

## 🌟 Стретч‑цели
- WS heartbeat/keepalive + авто‑rejoin
- Персональные лимиты (per user/route)
