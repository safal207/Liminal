# Build ML Stack Script
# Этот скрипт пошагово собирает и запускает все компоненты ML-инфраструктуры

# Функция для отображения статуса
function Show-Status {
    param (
        [string]$Message,
        [string]$Status = "INFO"
    )
    
    $color = switch ($Status) {
        "SUCCESS" { "Green" }
        "ERROR" { "Red" }
        "WARNING" { "Yellow" }
        default { "Cyan" }
    }
    
    Write-Host "[$Status] $Message" -ForegroundColor $color
}

# Шаг 1: Остановка и удаление всех контейнеров
Show-Status "Step 1: Cleaning up environment..." -Status "INFO"
docker rm -f ml-redis ml-prometheus ml-backend 2>$null

# Шаг 2: Запуск Redis
Show-Status "Step 2: Starting Redis..." -Status "INFO"
docker run -d --name ml-redis -p 6379:6379 redis:latest
Start-Sleep -Seconds 5

$redisRunning = $false
$redisStatus = docker ps --filter "name=ml-redis" --format "{{.Status}}"
if ($redisStatus -match "Up") {
    $redisResponse = docker exec ml-redis redis-cli ping
    if ($redisResponse -eq "PONG") {
        Show-Status "Redis is running and responding (PONG)" -Status "SUCCESS"
        $redisRunning = $true
    } else {
        Show-Status "Redis container is running but not responding correctly" -Status "ERROR"
    }
} else {
    Show-Status "Redis container failed to start" -Status "ERROR"
}

# Шаг 3: Настройка и запуск Prometheus
if ($redisRunning) {
    Show-Status "Step 3: Setting up Prometheus..." -Status "INFO"
    
    $promDir = "$PSScriptRoot/../prometheus"
    if (-not (Test-Path $promDir)) {
        New-Item -Path $promDir -ItemType Directory -Force | Out-Null
        Show-Status "Created directory: $promDir" -Status "INFO"
    }
    
    $prometheusConfig = @"
global:
  scrape_interval: 15s

scrape_configs:
  - job_name: 'prometheus'
    static_configs:
      - targets: ['localhost:9090']
  
  - job_name: 'backend'
    static_configs:
      - targets: ['ml-backend:8000']
"@
    
    $prometheusConfig | Out-File -FilePath "$promDir/prometheus.yml" -Encoding utf8
    Show-Status "Created prometheus.yml configuration" -Status "SUCCESS"
    
    Show-Status "Starting Prometheus container..." -Status "INFO"
    docker run -d --name ml-prometheus -p 9090:9090 -v "${promDir}/prometheus.yml:/etc/prometheus/prometheus.yml" prom/prometheus:latest
    Start-Sleep -Seconds 5
    
    $prometheusRunning = $false
    $prometheusStatus = docker ps --filter "name=ml-prometheus" --format "{{.Status}}"
    if ($prometheusStatus -match "Up") {
        Show-Status "Prometheus is running" -Status "SUCCESS"
        $prometheusRunning = $true
    } else {
        Show-Status "Prometheus container failed to start" -Status "ERROR"
    }
}

# Шаг 4: Создание временного упрощенного Dockerfile для ML
if ($redisRunning -and $prometheusRunning) {
    Show-Status "Step 4: Creating simplified Dockerfile for ML backend..." -Status "INFO"
    
    $dockerfilePath = "$PSScriptRoot/../backend/Dockerfile.simple"
    $dockerfileContent = @"
FROM python:3.11-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential curl \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY requirements.txt .
RUN pip install --no-cache-dir -U pip wheel setuptools
RUN pip install --no-cache-dir pyyaml prometheus-client redis
RUN pip install --no-cache-dir -r requirements.txt

ENV ML_ENABLED=true

COPY . .
EXPOSE 8000

CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "8000"]
"@

    $dockerfileContent | Out-File -FilePath $dockerfilePath -Encoding utf8
    Show-Status "Created simplified Dockerfile at $dockerfilePath" -Status "SUCCESS"
    
    # Шаг 5: Сборка Docker образа
    Show-Status "Step 5: Building Docker image..." -Status "INFO"
    $buildCommand = "cd $PSScriptRoot/../backend && docker build -t resonance-liminal-backend:latest -f Dockerfile.simple ."
    
    Show-Status "Running build command: $buildCommand" -Status "INFO"
    Invoke-Expression $buildCommand
    
    $imageExists = docker images resonance-liminal-backend:latest --format "{{.Repository}}"
    if ($imageExists) {
        Show-Status "Successfully built backend Docker image" -Status "SUCCESS"
        
        # Шаг 6: Запуск backend контейнера
        Show-Status "Step 6: Starting ML backend container..." -Status "INFO"
        docker run -d --name ml-backend -p 8000:8000 --link ml-redis:redis --link ml-prometheus:prometheus -e REDIS_HOST=ml-redis -e PROMETHEUS_URL=http://ml-prometheus:9090 -e ML_ENABLED=true resonance-liminal-backend:latest
        Start-Sleep -Seconds 10
        
        $backendRunning = $false
        $backendStatus = docker ps --filter "name=ml-backend" --format "{{.Status}}"
        if ($backendStatus -match "Up") {
            Show-Status "Backend container is running" -Status "SUCCESS"
            $backendRunning = $true
        } else {
            Show-Status "Backend container failed to start" -Status "ERROR"
            Show-Status "Checking logs..." -Status "INFO"
            docker logs ml-backend
        }
    } else {
        Show-Status "Failed to build backend Docker image" -Status "ERROR"
    }
}

# Вывод итогового статуса
Show-Status "ML Infrastructure Status:" -Status "INFO"
Write-Host "----------------------------------------"
docker ps
Write-Host "----------------------------------------"

if ($redisRunning) { 
    Write-Host "Redis: " -NoNewline
    Write-Host "RUNNING (localhost:6379)" -ForegroundColor Green 
} else { 
    Write-Host "Redis: " -NoNewline
    Write-Host "FAILED" -ForegroundColor Red 
}

if ($prometheusRunning) { 
    Write-Host "Prometheus: " -NoNewline
    Write-Host "RUNNING (http://localhost:9090)" -ForegroundColor Green 
} else { 
    Write-Host "Prometheus: " -NoNewline
    Write-Host "FAILED" -ForegroundColor Red 
}

if ($backendRunning) { 
    Write-Host "ML Backend: " -NoNewline
    Write-Host "RUNNING (http://localhost:8000)" -ForegroundColor Green 
    Write-Host ""
    Show-Status "ML Endpoints Available:" -Status "SUCCESS"
    Write-Host "• ML Metrics: http://localhost:8000/ml_metrics"
    Write-Host "• Health Check: http://localhost:8000/health"
} else { 
    Write-Host "ML Backend: " -NoNewline
    Write-Host "FAILED" -ForegroundColor Red 
}
