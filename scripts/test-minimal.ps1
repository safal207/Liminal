# Minimal Multi-LLM System Test
Write-Host "Starting Minimal Multi-LLM Test..." -ForegroundColor Green

# Stop any running containers first
Write-Host "Stopping existing containers..." -ForegroundColor Yellow
docker-compose -f docker-compose.test.yml down

# Start minimal services
Write-Host "Starting Redis and Backend..." -ForegroundColor Cyan
docker-compose -f docker-compose.test.yml up --build -d

Write-Host "Waiting for services..." -ForegroundColor Cyan
Start-Sleep -Seconds 30

# Test Redis
Write-Host "Testing Redis..." -ForegroundColor Cyan
try {
    $redisTest = docker exec resonance-redis-test redis-cli ping
    if ($redisTest -eq "PONG") {
        Write-Host "Redis: OK" -ForegroundColor Green
    }
} catch {
    Write-Host "Redis: Failed" -ForegroundColor Red
}

# Test Backend
Write-Host "Testing Backend API..." -ForegroundColor Cyan
try {
    $response = Invoke-WebRequest -Uri "http://localhost:8000/health" -TimeoutSec 15 -UseBasicParsing
    if ($response.StatusCode -eq 200) {
        Write-Host "Backend API: OK" -ForegroundColor Green
        Write-Host "Response: $($response.Content)" -ForegroundColor White
    }
} catch {
    Write-Host "Backend API: Failed - $($_.Exception.Message)" -ForegroundColor Red
}

# Test ML Status
Write-Host "Testing ML Status..." -ForegroundColor Cyan
try {
    $response = Invoke-WebRequest -Uri "http://localhost:8000/ml/status" -TimeoutSec 10 -UseBasicParsing
    if ($response.StatusCode -eq 200) {
        Write-Host "ML Status: OK" -ForegroundColor Green
        Write-Host "Response: $($response.Content)" -ForegroundColor White
    }
} catch {
    Write-Host "ML Status: Failed - $($_.Exception.Message)" -ForegroundColor Red
}

Write-Host ""
Write-Host "Test URLs:" -ForegroundColor Cyan
Write-Host "  Health:    http://localhost:8000/health" -ForegroundColor White
Write-Host "  ML Status: http://localhost:8000/ml/status" -ForegroundColor White
Write-Host "  API Docs:  http://localhost:8000/docs" -ForegroundColor White

Write-Host ""
Write-Host "To view logs: docker-compose -f docker-compose.test.yml logs -f" -ForegroundColor White
Write-Host "To stop: docker-compose -f docker-compose.test.yml down" -ForegroundColor White
