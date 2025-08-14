# Simple Multi-LLM System Test Script
Write-Host "🚀 Starting Resonance Liminal Multi-LLM System Test..." -ForegroundColor Green

# Check if .env file exists
if (-not (Test-Path ".env")) {
    Write-Host "⚠️  .env file not found. Using example configuration." -ForegroundColor Yellow
}

# Check Docker
try {
    docker info | Out-Null
    Write-Host "✅ Docker is running" -ForegroundColor Green
} catch {
    Write-Host "❌ Docker is not running. Please start Docker Desktop." -ForegroundColor Red
    exit 1
}

Write-Host "🔧 Building and starting services..." -ForegroundColor Cyan

# Start services
docker-compose -f docker-compose.ml-production.yml up --build -d

Write-Host "⏳ Waiting for services to start..." -ForegroundColor Cyan
Start-Sleep -Seconds 15

# Test basic endpoints
Write-Host "🧪 Testing basic endpoints..." -ForegroundColor Cyan

try {
    $response = Invoke-WebRequest -Uri "http://localhost:8000/health" -TimeoutSec 10 -UseBasicParsing
    if ($response.StatusCode -eq 200) {
        Write-Host "✅ Backend API is healthy" -ForegroundColor Green
    }
} catch {
    Write-Host "⚠️  Backend API not responding yet" -ForegroundColor Yellow
}

try {
    $response = Invoke-WebRequest -Uri "http://localhost:8001/health" -TimeoutSec 10 -UseBasicParsing
    if ($response.StatusCode -eq 200) {
        Write-Host "✅ XAI Service is healthy" -ForegroundColor Green
    }
} catch {
    Write-Host "⚠️  XAI Service not responding yet" -ForegroundColor Yellow
}

Write-Host ""
Write-Host "🌐 Service URLs:" -ForegroundColor Cyan
Write-Host "  📊 ML API:           http://localhost:8000/docs" -ForegroundColor White
Write-Host "  🧠 XAI Service:      http://localhost:8001/docs" -ForegroundColor White
Write-Host "  📈 Prometheus:       http://localhost:9090" -ForegroundColor White
Write-Host "  📊 Grafana:          http://localhost:3000" -ForegroundColor White

Write-Host ""
Write-Host "📋 To view logs: docker-compose -f docker-compose.ml-production.yml logs -f" -ForegroundColor White
Write-Host "🛑 To stop: docker-compose -f docker-compose.ml-production.yml down" -ForegroundColor White

Write-Host ""
Write-Host "🎉 System started! Check the URLs above." -ForegroundColor Green
