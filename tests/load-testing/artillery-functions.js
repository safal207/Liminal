/**
 * Artillery.io Custom Functions для XAI + OpenAI Load Testing
 * Дополнительные функции для измерения производительности и валидации
 */

module.exports = {
  // =============================================================================
  // Timing и Performance функции
  // =============================================================================
  
  captureStartTime: function(requestParams, context, ee, next) {
    context.vars.startTime = Date.now();
    context.vars.requestId = generateRequestId();
    return next();
  },

  captureMLLatency: function(requestParams, response, context, ee, next) {
    const latency = Date.now() - context.vars.startTime;
    ee.emit('histogram', 'ml_prediction_latency', latency);
    
    // Логируем медленные запросы
    if (latency > 1000) {
      console.log(`⚠️ Slow ML request: ${latency}ms - ${context.vars.requestId}`);
    }
    
    return next();
  },

  captureXAILatency: function(requestParams, response, context, ee, next) {
    const latency = Date.now() - context.vars.startTime;
    ee.emit('histogram', 'xai_explanation_latency', latency);
    
    // Проверяем качество XAI ответа
    if (response.body) {
      try {
        const body = JSON.parse(response.body);
        if (body.feature_importance && Object.keys(body.feature_importance).length > 0) {
          ee.emit('counter', 'xai_explanations_with_features', 1);
        }
        if (body.natural_explanation && body.natural_explanation.length > 50) {
          ee.emit('counter', 'xai_quality_explanations', 1);
        }
      } catch (e) {
        console.log(`❌ XAI response parsing error: ${e.message}`);
      }
    }
    
    return next();
  },

  captureOpenAILatency: function(requestParams, response, context, ee, next) {
    const latency = Date.now() - context.vars.startTime;
    ee.emit('histogram', 'openai_analysis_latency', latency);
    
    // Анализируем качество OpenAI ответов
    if (response.body) {
      try {
        const body = JSON.parse(response.body);
        
        // Проверяем наличие рекомендаций
        if (body.recommendations && body.recommendations.length > 0) {
          ee.emit('counter', 'openai_responses_with_recommendations', 1);
        }
        
        // Проверяем confidence score
        if (body.confidence && body.confidence > 0.7) {
          ee.emit('counter', 'openai_high_confidence_responses', 1);
        }
        
        // Проверяем длину анализа
        if (body.analysis && body.analysis.length > 100) {
          ee.emit('counter', 'openai_detailed_analysis', 1);
        }
        
      } catch (e) {
        console.log(`❌ OpenAI response parsing error: ${e.message}`);
      }
    }
    
    return next();
  },

  captureClaudeLatency: function(requestParams, response, context, ee, next) {
    const latency = Date.now() - context.vars.startTime;
    ee.emit('histogram', 'claude_analysis_latency', latency);
    
    // Анализируем качество Claude ответов
    if (response.body) {
      try {
        const body = JSON.parse(response.body);
        
        // Проверяем Constitutional AI notes
        if (body.constitutional_ai_notes && body.constitutional_ai_notes.length > 0) {
          ee.emit('counter', 'claude_constitutional_ai_responses', 1);
        }
        
        // Проверяем safety assessment
        if (body.safety_assessment) {
          ee.emit('counter', `claude_safety_${body.safety_assessment}`, 1);
        }
        
        // Проверяем ethical considerations
        if (body.ethical_considerations && body.ethical_considerations.length > 0) {
          ee.emit('counter', 'claude_ethical_analysis_provided', 1);
        }
        
        // Проверяем detailed reasoning
        if (body.reasoning && body.reasoning.length > 2) {
          ee.emit('counter', 'claude_detailed_reasoning', 1);
        }
        
        // Проверяем harm assessment
        if (body.harm_assessment && body.harm_assessment.level) {
          ee.emit('counter', `claude_harm_${body.harm_assessment.level}`, 1);
        }
        
      } catch (e) {
        console.log(`❌ Claude response parsing error: ${e.message}`);
      }
    }
    
    return next();
  },

  captureMultiLLMLatency: function(requestParams, response, context, ee, next) {
    const latency = Date.now() - context.vars.startTime;
    ee.emit('histogram', 'multi_llm_analysis_latency', latency);
    
    // Анализируем Multi-LLM ответы
    if (response.body) {
      try {
        const body = JSON.parse(response.body);
        
        // Отслеживаем используемого провайдера
        if (body.provider_used) {
          ee.emit('counter', `multi_llm_provider_${body.provider_used}`, 1);
        }
        
        // Проверяем использование fallback
        if (body.fallback_used) {
          ee.emit('counter', 'multi_llm_fallback_used', 1);
        }
        
        // Проверяем доступность провайдеров
        if (body.openai_available && body.claude_available) {
          ee.emit('counter', 'multi_llm_both_providers_available', 1);
        } else if (body.openai_available || body.claude_available) {
          ee.emit('counter', 'multi_llm_single_provider_available', 1);
        } else {
          ee.emit('counter', 'multi_llm_no_providers_available', 1);
        }
        
        // Проверяем стоимость
        if (body.cost_estimate && body.cost_estimate > 0) {
          ee.emit('histogram', 'multi_llm_cost_estimate', body.cost_estimate * 1000); // В миллицентах
        }
        
      } catch (e) {
        console.log(`❌ Multi-LLM response parsing error: ${e.message}`);
      }
    }
    
    return next();
  },

  captureConsensusLatency: function(requestParams, response, context, ee, next) {
    const latency = Date.now() - context.vars.startTime;
    ee.emit('histogram', 'consensus_analysis_latency', latency);
    
    // Анализируем консенсус ответы
    if (response.body) {
      try {
        const body = JSON.parse(response.body);
        
        // Проверяем наличие консенсус-анализа
        if (body.consensus_analysis) {
          ee.emit('counter', 'consensus_analysis_provided', 1);
        }
        
        // Проверяем agreement score
        if (body.agreement_score !== undefined) {
          ee.emit('histogram', 'consensus_agreement_score', body.agreement_score * 100);
          
          if (body.agreement_score > 0.8) {
            ee.emit('counter', 'consensus_high_agreement', 1);
          } else if (body.agreement_score < 0.5) {
            ee.emit('counter', 'consensus_low_agreement', 1);
          }
        }
        
        // Проверяем доступность обоих провайдеров для консенсуса
        if (body.openai_available && body.claude_available) {
          ee.emit('counter', 'consensus_both_providers_used', 1);
        }
        
      } catch (e) {
        console.log(`❌ Consensus response parsing error: ${e.message}`);
      }
    }
    
    return next();
  },

  // =============================================================================
  // Data Generation функции
  // =============================================================================
  
  generateRealisticFeatures: function(requestParams, context, ee, next) {
    // Генерируем реалистичные фичи для ML тестирования
    const scenarios = [
      // Нормальная активность
      {
        messages_per_minute: randomBetween(5, 20),
        error_rate: randomBetween(0.01, 0.05),
        connection_duration: randomBetween(300, 1800),
        burstiness_score: randomBetween(0.1, 0.3),
        ip_entropy: randomBetween(1.0, 2.5),
        channels_count: randomBetween(1, 5)
      },
      // Подозрительная активность
      {
        messages_per_minute: randomBetween(50, 100),
        error_rate: randomBetween(0.1, 0.3),
        connection_duration: randomBetween(10, 60),
        burstiness_score: randomBetween(0.7, 1.0),
        ip_entropy: randomBetween(3.0, 4.5),
        channels_count: randomBetween(10, 50)
      },
      // Аномальная активность
      {
        messages_per_minute: randomBetween(100, 500),
        error_rate: randomBetween(0.3, 0.8),
        connection_duration: randomBetween(1, 30),
        burstiness_score: randomBetween(0.9, 1.0),
        ip_entropy: randomBetween(4.0, 5.0),
        channels_count: randomBetween(50, 200)
      }
    ];
    
    const scenario = scenarios[Math.floor(Math.random() * scenarios.length)];
    context.vars.testFeatures = scenario;
    
    return next();
  },

  generateRealisticLogs: function(requestParams, context, ee, next) {
    const logTemplates = [
      "ERROR: WebSocket connection failed for user {user_id} from IP {ip}",
      "WARNING: High latency detected: {latency}ms for endpoint {endpoint}",
      "INFO: Rate limit triggered for IP {ip} - {requests} requests in {window}s",
      "ERROR: Authentication failed for token {token_hash}",
      "DEBUG: Processing message batch of {batch_size} messages",
      "WARNING: Memory usage high: {memory_percent}% on server {server_id}",
      "ERROR: Database connection timeout after {timeout}ms",
      "INFO: User {user_id} connected to channel {channel}",
      "WARNING: Unusual traffic pattern detected from {ip_range}",
      "ERROR: Message validation failed: {error_details}"
    ];
    
    const logs = [];
    const logCount = randomBetween(3, 10);
    
    for (let i = 0; i < logCount; i++) {
      const template = logTemplates[Math.floor(Math.random() * logTemplates.length)];
      const log = template
        .replace('{user_id}', `user_${randomBetween(1000, 9999)}`)
        .replace('{ip}', generateRandomIP())
        .replace('{latency}', randomBetween(100, 2000))
        .replace('{endpoint}', ['/ws', '/api/messages', '/auth'][Math.floor(Math.random() * 3)])
        .replace('{requests}', randomBetween(10, 100))
        .replace('{window}', randomBetween(60, 300))
        .replace('{token_hash}', generateRandomHash())
        .replace('{batch_size}', randomBetween(10, 1000))
        .replace('{memory_percent}', randomBetween(70, 95))
        .replace('{server_id}', `srv-${randomBetween(1, 10)}`)
        .replace('{channel}', `channel_${randomBetween(1, 100)}`)
        .replace('{ip_range}', generateRandomIPRange())
        .replace('{timeout}', randomBetween(5000, 30000))
        .replace('{error_details}', ['Invalid JSON', 'Missing field', 'Type mismatch'][Math.floor(Math.random() * 3)]);
      
      logs.push(log);
    }
    
    context.vars.testLogs = logs;
    return next();
  },

  // =============================================================================
  // Validation функции
  // =============================================================================
  
  validateMLResponse: function(requestParams, response, context, ee, next) {
    if (response.statusCode !== 200) {
      ee.emit('counter', 'ml_api_errors', 1);
      return next();
    }
    
    try {
      const body = JSON.parse(response.body);
      
      // Проверяем обязательные поля
      const requiredFields = ['model_name', 'prediction', 'confidence'];
      const missingFields = requiredFields.filter(field => !(field in body));
      
      if (missingFields.length > 0) {
        ee.emit('counter', 'ml_response_validation_errors', 1);
        console.log(`❌ Missing fields in ML response: ${missingFields.join(', ')}`);
      } else {
        ee.emit('counter', 'ml_response_validation_success', 1);
      }
      
      // Проверяем диапазон confidence
      if (body.confidence && (body.confidence < 0 || body.confidence > 1)) {
        ee.emit('counter', 'ml_invalid_confidence_range', 1);
      }
      
    } catch (e) {
      ee.emit('counter', 'ml_response_parse_errors', 1);
      console.log(`❌ ML response parsing error: ${e.message}`);
    }
    
    return next();
  },

  validateXAIResponse: function(requestParams, response, context, ee, next) {
    if (response.statusCode !== 200) {
      ee.emit('counter', 'xai_api_errors', 1);
      return next();
    }
    
    try {
      const body = JSON.parse(response.body);
      
      // Проверяем наличие объяснений
      const explanationFields = ['feature_importance', 'technical_explanation', 'natural_explanation'];
      const presentFields = explanationFields.filter(field => field in body && body[field]);
      
      if (presentFields.length >= 2) {
        ee.emit('counter', 'xai_complete_explanations', 1);
      } else {
        ee.emit('counter', 'xai_incomplete_explanations', 1);
      }
      
      // Проверяем качество feature importance
      if (body.feature_importance && typeof body.feature_importance === 'object') {
        const featureCount = Object.keys(body.feature_importance).length;
        if (featureCount >= 3) {
          ee.emit('counter', 'xai_rich_feature_importance', 1);
        }
      }
      
    } catch (e) {
      ee.emit('counter', 'xai_response_parse_errors', 1);
      console.log(`❌ XAI response parsing error: ${e.message}`);
    }
    
    return next();
  },

  // =============================================================================
  // Stress Testing функции
  // =============================================================================
  
  simulateHighLoad: function(requestParams, context, ee, next) {
    // Симулируем высокую нагрузку с множественными запросами
    context.vars.isHighLoadTest = true;
    context.vars.concurrentRequests = randomBetween(5, 15);
    
    return next();
  },

  measureEndToEndLatency: function(requestParams, context, ee, next) {
    context.vars.e2eStartTime = Date.now();
    return next();
  },

  captureEndToEndLatency: function(requestParams, response, context, ee, next) {
    if (context.vars.e2eStartTime) {
      const e2eLatency = Date.now() - context.vars.e2eStartTime;
      ee.emit('histogram', 'end_to_end_latency', e2eLatency);
      
      // Проверяем SLA
      if (e2eLatency > 5000) {
        ee.emit('counter', 'sla_violations', 1);
      }
    }
    
    return next();
  },

  // =============================================================================
  // Monitoring и Alerting
  // =============================================================================
  
  checkSystemHealth: function(requestParams, response, context, ee, next) {
    if (response.statusCode === 200) {
      try {
        const body = JSON.parse(response.body);
        
        if (body.overall_status === 'healthy') {
          ee.emit('counter', 'system_health_checks_passed', 1);
        } else {
          ee.emit('counter', 'system_health_checks_failed', 1);
          console.log(`⚠️ System health check failed: ${body.overall_status}`);
        }
        
      } catch (e) {
        ee.emit('counter', 'health_check_parse_errors', 1);
      }
    }
    
    return next();
  }
};

// =============================================================================
// Utility функции
// =============================================================================

function randomBetween(min, max) {
  return Math.random() * (max - min) + min;
}

function generateRequestId() {
  return `req_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
}

function generateRandomIP() {
  return `${randomBetween(1, 255)}.${randomBetween(1, 255)}.${randomBetween(1, 255)}.${randomBetween(1, 255)}`;
}

function generateRandomIPRange() {
  return `${randomBetween(1, 255)}.${randomBetween(1, 255)}.0.0/16`;
}

function generateRandomHash() {
  return Math.random().toString(36).substr(2, 16);
}
