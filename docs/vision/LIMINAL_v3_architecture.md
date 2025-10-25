# LIMINAL v3.1 — Архитектурное видение

> **Статус:** Vision Document (стратегическое видение)  
> **Текущая реализация:** Python/FastAPI базовый стек  
> **Дата:** 2025-08-20

## Обзор

Этот документ содержит расширенное архитектурное видение LIMINAL — системы осознанных переходов во времени и резонансе. Включает передовые паттерны: Meta-Liminal Engine, Privacy Engine, нейроволновую синхронизацию, UX-этику и production-ready observability.

## Ключевые концепции из Vision

### 🧠 Meta-Liminal Engine
Система саморефлексии: анализ собственных паттернов, ежедневные сравнения состояний, "цифровое зеркало" для пользователя.

### 🔐 Privacy & Ethics Layer  
- Differential Privacy Engine
- Consent & Transparency Layer
- Recovery UI ("мне тяжело", настройка интенсивности)
- Humility Protocol (система может признать ошибку)

### 🎵 NeuroWave Synth Engine
Бинауральные ритмы (альфа, тета, дельта) синхронизированные с состоянием пользователя и голосовыми помощниками.

### 📊 Advanced Observability
- OpenTelemetry distributed tracing
- Structured logging (ELK Stack)
- Prometheus metrics + Grafana dashboards
- ML drift detection (Evidently)
- Feature Store (Feast)

### 🔄 Message Bus Architecture
RabbitMQ с priority queues, dead letter queues, MessagePack compression, circuit breakers.

## Технологический стек (Vision)

| Компонент | Vision Stack | Текущая реализация |
|-----------|-------------|-------------------|
| **Core Engine** | Haskell (Pythia) + Elixir (Realtime) | Python/FastAPI |
| **Database** | Datomic + Neo4j + InfluxDB + Redis | PostgreSQL + Redis |
| **Message Queue** | RabbitMQ + DLQ + Priority | Опционально |
| **Auth/Security** | OAuth2/JWT + mTLS + Audit | Базовый JWT |
| **Monitoring** | Prometheus + Grafana + ELK + OTEL | Prometheus + базовые логи |
| **ML Infrastructure** | Feast + Evidently + Model Registry | Scikit/PyTorch + базовая ML |
| **Privacy** | Differential Privacy Engine | Планируется |
| **Frontend** | Flutter/Elm + Empath UI | React/HTML + WebSocket |

## Roadmap: Vision → Reality

### Phase 1: Security & Observability MVP
- [ ] JWT authentication
- [ ] Structured logging (structlog)  
- [ ] Prometheus metrics
- [ ] Health + Readiness endpoints
- [ ] Basic audit logging

### Phase 2: UX Ethics & Recovery
- [ ] Recovery UI flags (`intensity_level`, `show_explanation`)
- [ ] Digital Mirror API (показать пользователю его паттерны)
- [ ] Consent management
- [ ] Emotional safety triggers

### Phase 3: ML Quality & Drift
- [ ] Evidently drift detection
- [ ] Simple model registry (файловая)
- [ ] Feature versioning
- [ ] A/B testing framework

### Phase 4: Advanced Features
- [ ] NeuroWave integration (бинауральные ритмы)
- [ ] Meta-Liminal self-reflection
- [ ] Message queue (при необходимости)
- [ ] Differential privacy

## Принципы адаптации

1. **Python-first**: используем Python/FastAPI вместо Haskell/Elixir
2. **Incremental complexity**: добавляем слои по мере роста
3. **Ethics by design**: Recovery UI и прозрачность с самого начала
4. **Observability early**: метрики и логи — приоритет
5. **User safety**: эмоциональная безопасность важнее технических фич

---

*Этот документ служит северной звездой для развития LIMINAL. Текущая реализация фокусируется на MVP с постепенным движением к vision.*
