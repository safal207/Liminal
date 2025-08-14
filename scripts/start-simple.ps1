# Simple Multi-LLM System Startup
Write-Host "Starting Resonance Liminal Multi-LLM System..." -ForegroundColor Green

# Check Docker
try {
    docker info | Out-Null
    Write-Host "Docker is running" -ForegroundColor Green
} catch {
    Write-Host "Docker is not running. Please start Docker Desktop." -ForegroundColor Red
    exit 1
}

Write-Host "Building and starting services..." -ForegroundColor Cyan

# Start services
docker-compose -f docker-compose.ml-production.yml up --build -d

Write-Host "Waiting for services to start..." -ForegroundColor Cyan
Start-Sleep -Seconds 20

Write-Host "Testing endpoints..." -ForegroundColor Cyan

# Test backend
try {
    $response = Invoke-WebRequest -Uri "http://localhost:8000/health" -TimeoutSec 10 -UseBasicParsing
    Write-Host "Backend API: OK" -ForegroundColor Green
} catch {
    Write-Host "Backend API: Not ready yet" -ForegroundColor Yellow
}

# Test XAI service
try {
    $response = Invoke-WebRequest -Uri "http://localhost:8001/health" -TimeoutSec 10 -UseBasicParsing
    Write-Host "XAI Service: OK" -ForegroundColor Green
} catch {
    Write-Host "XAI Service: Not ready yet" -ForegroundColor Yellow
}

Write-Host ""
Write-Host "Service URLs:" -ForegroundColor Cyan
Write-Host "  ML API:      http://localhost:8000/docs" -ForegroundColor White
Write-Host "  XAI Service: http://localhost:8001/docs" -ForegroundColor White
Write-Host "  Prometheus:  http://localhost:9090" -ForegroundColor White
Write-Host "  Grafana:     http://localhost:3000" -ForegroundColor White

Write-Host ""
Write-Host "System started successfully!" -ForegroundColor Green
