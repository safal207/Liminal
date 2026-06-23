# LIMINAL Documentation Index

Добро пожаловать в пространство знаний LIMINAL. Документы сгруппированы по назначению, чтобы новые участники могли быстро найти нужный контекст.

## Roadmap — "куда идём"
Эта категория описывает текущее состояние развития продукта, планы спринтов и критические задачи. Используйте её, чтобы синхронизироваться по срокам и приоритетам команды.

- [Обзор и навигация по проекту](roadmap/Liminal_readme.md)
- [Главный бэклог](roadmap/BACKLOG.md)
- [Бэклог 2025](roadmap/PROJECT_BACKLOG_2025.md)
- [Планы спринтов](roadmap/SPRINT_3_PLAN.md) · [Спринт 4](roadmap/SPRINT_4_PLAN.md)
- [Чек-лист критических исправлений](roadmap/CRITICAL_FIXES_CHECKLIST_2025.md)
- [Пошаговая дорожная карта развития](roadmap/legend.md)

## Strategy — "как выигрываем"
Документы о позиционировании, ценности продукта и выводе на рынок. Здесь концентрируется всё, что помогает сформировать устойчивую стратегию роста и монетизации.

- [Обратная связь наставников (RU)](strategy/MENTOR_FEEDBACK_RU.md)
- [Стратегия монетизации](strategy/MONETIZATION_STRATEGY_2025.md)
- [Оценка проекта и финансовые сценарии](strategy/PROJECT_VALUATION_2025.md) · [Финансовая оценка 2025](strategy/FINANCIAL_VALUATION_2025.md)
- [Анализ конкурентов](strategy/COMPETITOR_ANALYSIS_2025.md)
- [Проверка гипотез с пользователями](strategy/CUSTOMER_VALIDATION_2025.md)
- [Продажная воронка и платформа](strategy/SALES_FUNNEL_PLATFORM_2025.md) · [Платформа продаж RU](strategy/%D0%9F%D0%9B%D0%90%D0%A2%D0%A4%D0%9E%D0%A0%D0%9C%D0%90_%D0%9F%D0%A0%D0%9E%D0%94%D0%90%D0%96_2025.md)

## Architecture — "как устроена непрерывность"
Публичные архитектурные модели и общий язык для памяти, доказательств, решений, действий и восстановления AI-агентов.

- [Liminal Agent Continuity Model v0.1](architecture/LIMINAL_AGENT_CONTINUITY_MODEL_V0_1.md)
- [Liminal Architectural Principles v0.1](architecture/LIMINAL_ARCHITECTURAL_PRINCIPLES_V0_1.md)
- [Liminal Memory Helix and Homeostasis Model v0.1](architecture/LIMINAL_MEMORY_HELIX_AND_HOMEOSTASIS_MODEL_V0_1.md)
- [Liminal Causal Stem Model v0.1](architecture/LIMINAL_CAUSAL_STEM_MODEL_V0_1.md)

## Interoperability — "как соединяем экосистемы"
Черновики нейтральных схем и профилей совместимости между LIMINAL и внешними agent/runtime/attestation системами.

- [AetherProof × LIMINAL Agent Chain Context Extension v0.1](interop/AETHERPROOF_AGENT_CHAIN_CONTEXT_V0_1.md)
- [Agent Chain Context JSON Schema v0.1](../schemas/agent-chain-context-v0.1.schema.json)
- [Agent Chain Purpose Registry v0.1](../registries/agent-chain-purpose-v0.1.json)
- [AetherProof Mandatory Replay Fixture v0.1](../fixtures/aetherproof-agent-chain-context-replay-v0.1.json)
- [DEFER and Continuation Conformance v0.1](interop/DEFER_CONTINUATION_CONFORMANCE_V0_1.md)
- [DEFER Continuation Machine-Readable Fixture v0.1](../fixtures/defer-continuation-conformance-v0.1.json)

## Research — "что изучаем"
Глубокие исследования, технические архитектуры и экспериментальные концепции. Изучайте эту секцию, когда нужно принять инженерные решения или вдохновиться будущими направлениями развития.

- [Техническая архитектура и инфраструктура](research/TECHNICAL_ARCHITECTURE_2025.md) · [Экосистема](research/ECOSYSTEM_ARCHITECTURE.md)
- [Риски и безопасность](research/RISK_ANALYSIS_2025.md) · [Расширенный анализ рисков](research/ADVANCED_RISK_ANALYSIS_2025.md) · [Lipsits Risk](research/LIPSITS_RISK_ANALYSIS_2025.md)
- [Этические и DevOps исследования](research/AI_ETHICS_2025.md) · [Совместимость DevOps](research/DEVOPS_COMPATIBILITY_ANALYSIS_2025.md)
- [Квантовая агентная архитектура](research/QUANTUM_AGENT_SPEC.md) · [Строковая теория](research/QUANTUM_STRING_THEORY_ANALYSIS_2025.md) · [Meta Cvants](research/meta_cvants.md)
- [Инженерные руководства](research/DOCKER_DEPLOYMENT.md) · [Multi-LLM Quickstart](research/MULTI-LLM-QUICKSTART.md)
- [OpenAI Adapter Check (03.08.2025)](research/ADAPTER_CHECK_RESULTS_20250803.md) · [Neo4j snapshot note](research/NEO4J_QUERY_NOTE.md) · [Экспорт графа](research/neo4j_query_out.json)
- [SOMA: философия и запуск](research/README_SOMA.md)
- [Дополнительные концепции](research/agent.md) · [Cubit](research/cubit.md) · [Miro проект](research/miro_proect.md)

## Перенесённые из корня артефакты
Список документов, которые находились в корне репозитория и теперь упорядочены по тематическим разделам.

| Артефакт | Новый путь | Назначение |
| --- | --- | --- |
| Обратная связь наставников | `docs/strategy/MENTOR_FEEDBACK_RU.md` | Консолидированные инсайты от ключевых партнёров LIMINAL |
| Лог проверки OpenAI адаптера | `docs/research/ADAPTER_CHECK_RESULTS_20250803.md` | Исторический отчёт о состоянии мульти-провайдерного адаптера |
| Neo4j snapshot | `docs/research/neo4j_query_out.json` · `docs/research/NEO4J_QUERY_NOTE.md` | Пример структуры графа и пояснения к нему |

## Другие полезные материалы
- [Глоссарий терминов](GLOSSARY.md)
- [Документация по SOMA](SOMA_Documentation.md)
- [API Reference](api.md)
- [Deployment Guide](deployment.md)

> 💡 Совет: обновляйте ссылки в этом индексе при добавлении новых ключевых документов, чтобы структура оставалась актуальной.

## Регламент обновления индекса

- **Ответственный:** Safal (Documentation Steward)
- **Аудит индекса:** первый рабочий день каждого месяца — сверка структуры, проверка битых ссылок, добавление новых ключевых артефактов.
- **Оперативные изменения:** вносить сразу после появления новых документов, фиксируя контекст и владельца материалов.
