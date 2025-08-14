# Simplified ML Infrastructure Test Script

# Colors
$Green = [System.ConsoleColor]::Green
$Cyan = [System.ConsoleColor]::Cyan
$Yellow = [System.ConsoleColor]::Yellow
$Red = [System.ConsoleColor]::Red

function Write-ColorText {
    param(
        [string]$Message,
        $Color = $Cyan
    )
    Write-Host $Message -ForegroundColor $Color
}

# Clean existing environment
Write-ColorText "[INFO] Cleaning up environment..." $Yellow
docker rm -f ml-redis ml-prometheus ml-backend 2>$null

# Step 1: Start Redis
Write-ColorText "[INFO] Starting Redis..." $Cyan
docker run -d --name ml-redis -p 6379:6379 redis:latest
Start-Sleep -Seconds 5
$redisStatus = docker ps --filter "name=ml-redis" --format "{{.Status}}"
Write-ColorText "[INFO] Redis status: $redisStatus" $(if ($redisStatus) { $Green } else { $Red })

# Check Redis connectivity
Write-ColorText "[INFO] Testing Redis connectivity..." $Cyan
$redisPing = docker exec ml-redis redis-cli ping
Write-ColorText "[INFO] Redis ping response: $redisPing" $(if ($redisPing -eq "PONG") { $Green } else { $Red })

# Step 2: Setup Prometheus
Write-ColorText "[INFO] Setting up Prometheus..." $Cyan
$promPath = "c:/Users/safal/OneDrive/Documente/GitHub/resonance-liminal/prometheus/prometheus.yml"
if (-not (Test-Path -Path $promPath)) {
    Write-ColorText "[INFO] Creating Prometheus config..." $Yellow
    $promConfig = @"
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
    New-Item -Path "c:/Users/safal/OneDrive/Documente/GitHub/resonance-liminal/prometheus" -ItemType Directory -Force | Out-Null
    $promConfig | Out-File -FilePath $promPath -Encoding utf8
}

# Start Prometheus
Write-ColorText "[INFO] Starting Prometheus..." $Cyan
docker run -d --name ml-prometheus -p 9090:9090 -v ${promPath}:/etc/prometheus/prometheus.yml prom/prometheus:latest
Start-Sleep -Seconds 10
$promStatus = docker ps --filter "name=ml-prometheus" --format "{{.Status}}"
Write-ColorText "[INFO] Prometheus status: $promStatus" $(if ($promStatus) { $Green } else { $Red })
Write-ColorText "[INFO] Prometheus URL: http://localhost:9090" $Cyan

# Step 3: Create simplified Dockerfile for Backend
Write-ColorText "[INFO] Creating simplified Dockerfile for ML backend..." $Cyan
$dockerfilePath = "c:/Users/safal/OneDrive/Documente/GitHub/resonance-liminal/backend/Dockerfile.simple"
$dockerfileContent = @"
FROM python:3.11-slim

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    curl \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install core dependencies explicitly
RUN pip install --no-cache-dir pyyaml==6.0 \
    prometheus-client==0.17.1 \
    redis==4.5.5 \
    fastapi==0.100.0 \
    uvicorn==0.22.0

# Set working directory
WORKDIR /app

# Copy application files
COPY . .

# ML environment setup
ENV ML_ENABLED=true \
    REDIS_HOST=ml-redis \
    PROMETHEUS_URL=http://ml-prometheus:9090 \
    PYTHONUNBUFFERED=1

# Expose port
EXPOSE 8000

# Run the application
CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "8000"]
"@

$dockerfileContent | Out-File -FilePath $dockerfilePath -Encoding utf8 -Force
Write-ColorText "[INFO] Dockerfile.simple created successfully" $Green

# Step 4: Build and run backend
Write-ColorText "[INFO] Building ML backend container..." $Cyan
Set-Location -LiteralPath "$PSScriptRoot/../backend"
docker build -t resonance-liminal-backend:ml -f Dockerfile.simple .

# Check if build was successful
$buildSuccess = $?
if (-not $buildSuccess) {
    Write-ColorText "[ERROR] Failed to build backend container!" $Red
    exit 1
}

Write-ColorText "[INFO] Starting ML backend container..." $Cyan
docker run -d --name ml-backend -p 8000:8000 --link ml-redis:redis --link ml-prometheus:prometheus -e REDIS_HOST=ml-redis -e PROMETHEUS_URL=http://ml-prometheus:9090 -e ML_ENABLED=true resonance-liminal-backend:ml

Start-Sleep -Seconds 15
$backendStatus = docker ps --filter "name=ml-backend" --format "{{.Status}}"
Write-ColorText "[INFO] Backend status: $backendStatus" $(if ($backendStatus) { $Green } else { $Red })

# Step 5: Test ML endpoints
Write-ColorText "[INFO] Testing ML endpoints..." $Cyan
try {
    $healthResponse = Invoke-RestMethod -Uri "http://localhost:8000/health" -Method Get -ErrorAction Stop
    Write-ColorText "[SUCCESS] Health endpoint response:" $Green
    $healthResponse | Format-Table -AutoSize
} catch {
    Write-ColorText "[ERROR] Health endpoint failed: $_" $Red
}

try {
    $mlMetricsResponse = Invoke-RestMethod -Uri "http://localhost:8000/ml_metrics" -Method Get -ErrorAction Stop
    Write-ColorText "[SUCCESS] ML metrics endpoint response:" $Green
    $mlMetricsResponse | Format-Table -AutoSize
} catch {
    Write-ColorText "[ERROR] ML metrics endpoint failed: $_" $Red
}

# Summary
Write-ColorText "`n[SUMMARY] ML Infrastructure Test Results" $Green
Write-ColorText "• Redis: $(if ($redisPing -eq 'PONG') { 'SUCCESS' } else { 'FAILED' })" $(if ($redisPing -eq 'PONG') { $Green } else { $Red })
Write-ColorText "• Prometheus: $(if ($promStatus) { 'SUCCESS' } else { 'FAILED' })" $(if ($promStatus) { $Green } else { $Red })
Write-ColorText "• Backend with ML: $(if ($backendStatus) { 'SUCCESS' } else { 'FAILED' })" $(if ($backendStatus) { $Green } else { $Red })

Write-ColorText "`n[INFO] Available endpoints:" $Cyan
Write-ColorText "• Backend API: http://localhost:8000/docs" $Green
Write-ColorText "• Health Check: http://localhost:8000/health" $Green
Write-ColorText "• ML Metrics: http://localhost:8000/ml_metrics" $Green
Write-ColorText "• Prometheus: http://localhost:9090" $Green

Write-ColorText "`n[INFO] To stop all containers, run:" $Yellow
Write-ColorText "docker rm -f ml-redis ml-prometheus ml-backend" $Yellow
