# =============================================================================
# Resonance Liminal Multi-LLM System Startup Script
# =============================================================================

Write-Host "🚀 Starting Resonance Liminal Multi-LLM System..." -ForegroundColor Green

# Check if .env file exists
if (-not (Test-Path ".env")) {
    Write-Host "⚠️  .env file not found. Please copy .env.example to .env and configure your API keys." -ForegroundColor Yellow
    Write-Host "Required API keys:" -ForegroundColor Yellow
    Write-Host "  - OPENAI_API_KEY" -ForegroundColor Yellow
    Write-Host "  - ANTHROPIC_API_KEY" -ForegroundColor Yellow
    exit 1
}

# Check if Docker is running
try {
    docker info | Out-Null
    Write-Host "✅ Docker is running" -ForegroundColor Green
} catch {
    Write-Host "❌ Docker is not running. Please start Docker Desktop." -ForegroundColor Red
    exit 1
}

# Check if Docker Compose is available
try {
    docker-compose --version | Out-Null
    Write-Host "✅ Docker Compose is available" -ForegroundColor Green
} catch {
    Write-Host "❌ Docker Compose not found. Please install Docker Compose." -ForegroundColor Red
    exit 1
}

Write-Host ""
Write-Host "🔧 Building and starting services..." -ForegroundColor Cyan

# Build and start services
try {
    docker-compose -f docker-compose.ml-production.yml up --build -d
    
    Write-Host ""
    Write-Host "⏳ Waiting for services to be ready..." -ForegroundColor Cyan
    Start-Sleep -Seconds 10
    
    # Check service health
    Write-Host ""
    Write-Host "🏥 Checking service health..." -ForegroundColor Cyan
    
    $services = @(
        @{Name="Backend ML API"; Url="http://localhost:8000/health"},
        @{Name="XAI Intelligence"; Url="http://localhost:8001/health"},
        @{Name="Redis"; Url="http://localhost:8000/ml/status"},
        @{Name="Prometheus"; Url="http://localhost:9090/-/healthy"}
    )
    
    foreach ($service in $services) {
        try {
            $response = Invoke-WebRequest -Uri $service.Url -TimeoutSec 5 -UseBasicParsing
            if ($response.StatusCode -eq 200) {
                Write-Host "✅ $($service.Name) is healthy" -ForegroundColor Green
            } else {
                Write-Host "⚠️  $($service.Name) returned status $($response.StatusCode)" -ForegroundColor Yellow
            }
        } catch {
            Write-Host "❌ $($service.Name) is not responding" -ForegroundColor Red
        }
    }
    
    Write-Host ""
    Write-Host "🎯 Testing AI integrations..." -ForegroundColor Cyan
    
    # Test OpenAI integration
    try {
        $openaiTest = @{
            logs = @("INFO: System startup completed", "ERROR: Connection timeout")
            context = @{
                system_load = 0.5
                error_rate = 0.1
            }
        } | ConvertTo-Json -Depth 3
        
        $response = Invoke-WebRequest -Uri "http://localhost:8000/ml/openai/analyze-logs" -Method POST -Body $openaiTest -ContentType "application/json" -TimeoutSec 10 -UseBasicParsing
        if ($response.StatusCode -eq 200) {
            Write-Host "✅ OpenAI integration is working" -ForegroundColor Green
        }
    } catch {
        Write-Host "⚠️  OpenAI integration test failed (check API key)" -ForegroundColor Yellow
    }
    
    # Test Claude integration
    try {
        $claudeTest = @{
            ml_decision = @{
                model_name = "test_model"
                prediction = "normal"
                confidence = 0.8
            }
            context = @{
                system_load = 0.3
            }
        } | ConvertTo-Json -Depth 3
        
        $response = Invoke-WebRequest -Uri "http://localhost:8000/ml/claude/safety-analysis" -Method POST -Body $claudeTest -ContentType "application/json" -TimeoutSec 10 -UseBasicParsing
        if ($response.StatusCode -eq 200) {
            Write-Host "✅ Claude integration is working" -ForegroundColor Green
        }
    } catch {
        Write-Host "⚠️  Claude integration test failed (check API key)" -ForegroundColor Yellow
    }
    
    # Test Multi-LLM orchestrator
    try {
        $multiLLMTest = @{
            task_type = "general"
            data = @{
                problem_description = "Test analysis"
                context = @{
                    environment = "test"
                }
            }
            preferred_provider = "auto"
        } | ConvertTo-Json -Depth 3
        
        $response = Invoke-WebRequest -Uri "http://localhost:8000/ml/multi-llm/analyze" -Method POST -Body $multiLLMTest -ContentType "application/json" -TimeoutSec 15 -UseBasicParsing
        if ($response.StatusCode -eq 200) {
            Write-Host "✅ Multi-LLM orchestrator is working" -ForegroundColor Green
        }
    } catch {
        Write-Host "⚠️  Multi-LLM orchestrator test failed" -ForegroundColor Yellow
    }
    
    Write-Host ""
    Write-Host "🌐 Service URLs:" -ForegroundColor Cyan
    Write-Host "  📊 ML API:           http://localhost:8000" -ForegroundColor White
    Write-Host "  🧠 XAI Service:      http://localhost:8001" -ForegroundColor White
    Write-Host "  📈 Prometheus:       http://localhost:9090" -ForegroundColor White
    Write-Host "  📊 Grafana:          http://localhost:3000" -ForegroundColor White
    Write-Host "  📚 API Docs:         http://localhost:8000/docs" -ForegroundColor White
    Write-Host "  🔍 XAI Docs:         http://localhost:8001/docs" -ForegroundColor White
    
    Write-Host ""
    Write-Host "🧪 Load Testing:" -ForegroundColor Cyan
    Write-Host "  Run load tests: cd tests/load-testing && artillery run artillery-xai-test.yml" -ForegroundColor White
    
    Write-Host ""
    Write-Host "🎉 Multi-LLM system is ready!" -ForegroundColor Green
    Write-Host "   Use 'docker-compose -f docker-compose.ml-production.yml logs -f' to view logs" -ForegroundColor White
    Write-Host "   Use 'docker-compose -f docker-compose.ml-production.yml down' to stop" -ForegroundColor White
    
} catch {
    Write-Host "❌ Failed to start services: $($_.Exception.Message)" -ForegroundColor Red
    Write-Host "🔍 Check logs with: docker-compose -f docker-compose.ml-production.yml logs" -ForegroundColor Yellow
    exit 1
}
