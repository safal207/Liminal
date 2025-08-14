# 🚀 Multi-LLM Quick Start Guide

## Быстрый запуск системы за 5 минут

### 1. Подготовка API ключей

```bash
# Скопируйте пример конфигурации
cp .env.example .env

# Отредактируйте .env и добавьте ваши API ключи:
# OPENAI_API_KEY=sk-your-openai-key-here
# ANTHROPIC_API_KEY=sk-ant-your-anthropic-key-here
```

### 2. Запуск системы

```powershell
# Windows PowerShell
./scripts/start-multi-llm.ps1
```

```bash
# Linux/Mac
chmod +x scripts/start-multi-llm.sh
./scripts/start-multi-llm.sh
```

### 3. Проверка работы

После запуска система будет доступна по адресам:

- **🔧 ML API**: http://localhost:8000/docs
- **🧠 XAI Service**: http://localhost:8001/docs  
- **📊 Prometheus**: http://localhost:9090
- **📈 Grafana**: http://localhost:3000

## 🧪 Тестирование Multi-LLM

### Быстрый тест OpenAI

```bash
curl -X POST "http://localhost:8000/ml/openai/analyze-logs" \
  -H "Content-Type: application/json" \
  -d '{
    "logs": ["ERROR: Database connection failed", "INFO: Retrying connection"],
    "context": {
      "system_load": 0.8,
      "error_rate": 0.15
    }
  }'
```

### Быстрый тест Claude

```bash
curl -X POST "http://localhost:8000/ml/claude/safety-analysis" \
  -H "Content-Type: application/json" \
  -d '{
    "ml_decision": {
      "model_name": "anomaly_detection",
      "prediction": "anomaly",
      "confidence": 0.95
    },
    "context": {
      "system_load": 0.9,
      "time_of_day": "night"
    }
  }'
```

### Быстрый тест Multi-LLM

```bash
curl -X POST "http://localhost:8000/ml/multi-llm/analyze" \
  -H "Content-Type: application/json" \
  -d '{
    "task_type": "safety",
    "data": {
      "problem_description": "High error rate detected",
      "metrics": {
        "error_rate": 0.3,
        "response_time": 2000
      }
    },
    "preferred_provider": "auto"
  }'
```

## 📊 Load Testing

### Быстрый тест

```powershell
./scripts/run-load-tests.ps1 -TestType quick
```

### Полный тест

```powershell
./scripts/run-load-tests.ps1 -TestType full -Duration 120s -VirtualUsers 20
```

### Стресс-тест

```powershell
./scripts/run-load-tests.ps1 -TestType stress -Duration 300s -VirtualUsers 50
```

### Только AI тесты

```powershell
./scripts/run-load-tests.ps1 -TestType ai-only -Duration 180s -VirtualUsers 15
```

## 🔍 Мониторинг

### Проверка статуса всех AI сервисов

```bash
curl http://localhost:8000/ml/ai-status
```

**Пример ответа:**
```json
{
  "overall_status": "healthy",
  "services": {
    "xai": {
      "status": "healthy",
      "cache_size": 1000,
      "models_loaded": 3
    },
    "openai": {
      "status": "healthy",
      "model": "gpt-4-turbo-preview",
      "requests_today": 150
    },
    "claude": {
      "status": "healthy",
      "model": "claude-3-sonnet-20240229",
      "requests_today": 89
    },
    "multi_llm": {
      "status": "healthy",
      "providers_available": ["openai", "claude"],
      "consensus_requests": 25
    }
  }
}
```

### Статистика использования Multi-LLM

```bash
curl http://localhost:8000/ml/multi-llm/usage-report
```

## 🔧 Troubleshooting

### Проблема: API ключи не работают

```bash
# Проверьте переменные окружения
docker-compose -f docker-compose.ml-production.yml exec backend env | grep API_KEY

# Перезапустите с новыми ключами
docker-compose -f docker-compose.ml-production.yml down
./scripts/start-multi-llm.ps1
```

### Проблема: Сервисы не запускаются

```bash
# Проверьте логи
docker-compose -f docker-compose.ml-production.yml logs backend
docker-compose -f docker-compose.ml-production.yml logs xai-intelligence

# Проверьте порты
netstat -an | findstr :8000
netstat -an | findstr :8001
```

### Проблема: Load тесты падают

```bash
# Проверьте доступность сервисов
curl http://localhost:8000/health
curl http://localhost:8001/health

# Уменьшите нагрузку
./scripts/run-load-tests.ps1 -TestType quick -VirtualUsers 5
```

## 📚 Дополнительная документация

- **Полная документация**: [XAI-OPENAI-INTEGRATION.md](docs/XAI-OPENAI-INTEGRATION.md)
- **API Reference**: http://localhost:8000/docs
- **XAI Service API**: http://localhost:8001/docs
- **Architecture Overview**: [docs/architecture.md](docs/architecture.md)

## 🎯 Основные возможности

### ✨ XAI (Explainable AI)
- SHAP explanations для ML моделей
- LIME local interpretability
- Feature importance analysis
- Counterfactual explanations

### 🤖 OpenAI GPT-4 Integration
- Intelligent log analysis
- Natural language explanations
- Smart alert generation
- Performance pattern recognition

### 🧠 Anthropic Claude Integration
- Constitutional AI для этических решений
- Safety analysis для критических операций
- Detailed reasoning для сложных случаев
- Security threat assessment

### 🎛️ Multi-LLM Orchestrator
- Автоматический выбор лучшего провайдера
- Fallback между OpenAI и Claude
- Consensus analysis для критических решений
- Cost optimization и usage tracking

## 🚀 Production Deployment

### Docker Swarm

```bash
docker swarm init
docker stack deploy -c docker-compose.ml-production.yml resonance-liminal
```

### Kubernetes

```bash
# Конвертация в Kubernetes manifests
kompose convert -f docker-compose.ml-production.yml
kubectl apply -f .
```

### Monitoring Setup

```bash
# Настройка Grafana dashboards
curl -X POST http://admin:admin@localhost:3000/api/dashboards/db \
  -H "Content-Type: application/json" \
  -d @monitoring/grafana-multi-llm-dashboard.json
```

---

## 🎉 Готово!

Ваша Multi-LLM система готова к работе! 

Для получения помощи:
- 📧 Email: support@resonance-liminal.com
- 💬 Discord: [Resonance Liminal Community](https://discord.gg/resonance-liminal)
- 📖 Docs: [Full Documentation](docs/XAI-OPENAI-INTEGRATION.md)
