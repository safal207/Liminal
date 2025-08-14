# ML Infrastructure Test Script - Simple Version
# This script tests ML components one by one with proper error handling

# Function to show status message
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

# Function to test container status
function Test-Container {
    param (
        [string]$Name,
        [string]$ExpectedStatus = "running"
    )
    
    $status = docker ps --filter "name=$Name" --format "{{.Status}}"
    
    if ($status -match $ExpectedStatus) {
        Show-Status "$Name is $ExpectedStatus" -Status "SUCCESS"
        return $true
    } else {
        Show-Status "$Name is not $ExpectedStatus" -Status "ERROR"
        return $false
    }
}

# Clear environment
Show-Status "Cleaning previous containers..."
docker rm -f ml-redis ml-prometheus ml-backend ml-kenning 2>$null

# Test 1: Redis
Show-Status "Starting Redis..."
docker run -d --name ml-redis -p 6379:6379 redis:latest

# Wait and check Redis
Start-Sleep -Seconds 5
if (Test-Container "ml-redis") {
    $redisResponse = docker exec ml-redis redis-cli ping
    if ($redisResponse -eq "PONG") {
        Show-Status "Redis is responding correctly (PONG)" -Status "SUCCESS"
    } else {
        Show-Status "Redis is not responding correctly" -Status "ERROR"
    }
} else {
    Show-Status "Redis container failed to start" -Status "ERROR"
}

# Test 2: Prometheus
Show-Status "Creating Prometheus configuration..."
$promDir = "c:/Users/safal/OneDrive/Documente/GitHub/resonance-liminal/prometheus"
if (-not (Test-Path $promDir)) {
    New-Item -Path $promDir -ItemType Directory -Force | Out-Null
}

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

$promConfig | Out-File -FilePath "$promDir/prometheus.yml" -Encoding utf8

Show-Status "Starting Prometheus..."
docker run -d --name ml-prometheus -p 9090:9090 -v "$promDir/prometheus.yml:/etc/prometheus/prometheus.yml" prom/prometheus:latest

# Wait and check Prometheus
Start-Sleep -Seconds 10
if (Test-Container "ml-prometheus") {
    Show-Status "Prometheus started successfully" -Status "SUCCESS"
    Show-Status "Prometheus UI available at: http://localhost:9090" -Status "INFO"
} else {
    Show-Status "Prometheus failed to start" -Status "ERROR"
}

# Summary of Test Results
Show-Status "ML Infrastructure Test Results" -Status "INFO"
Write-Host "----------------------------------------"

$redisRunning = Test-Container "ml-redis"
$prometheusRunning = Test-Container "ml-prometheus"

Write-Host "Redis:      " -NoNewline
if ($redisRunning) { 
    Write-Host "[RUNNING]" -ForegroundColor Green 
} else { 
    Write-Host "[FAILED]" -ForegroundColor Red 
}

Write-Host "Prometheus: " -NoNewline
if ($prometheusRunning) { 
    Write-Host "[RUNNING]" -ForegroundColor Green 
} else { 
    Write-Host "[FAILED]" -ForegroundColor Red 
}

# Backend - Manual Steps Guidance
Show-Status "Backend container requires manual build" -Status "WARNING"
Write-Host @"

To build and run the backend container with ML support:
1. Make sure your Dockerfile.optimized includes:
   - pyyaml
   - prometheus-client
   - redis

2. Build with:
   docker build -t resonance-liminal-backend:latest -f backend/Dockerfile.optimized backend/

3. Run with:
   docker run -d --name ml-backend -p 8000:8000 --link ml-redis:redis --link ml-prometheus:prometheus -e REDIS_HOST=ml-redis -e PROMETHEUS_URL=http://ml-prometheus:9090 -e ML_ENABLED=true resonance-liminal-backend:latest
"@

# Output available services
Show-Status "Available Services:"
if ($redisRunning) { Write-Host "• Redis:      localhost:6379" -ForegroundColor Green }
if ($prometheusRunning) { Write-Host "• Prometheus: http://localhost:9090" -ForegroundColor Green }

# Next steps
Show-Status "Next Steps:" -Status "INFO"
Write-Host "1. Verify Redis and Prometheus are running"
Write-Host "2. Check the Prometheus UI in browser"
Write-Host "3. Build backend with correct dependencies"
Write-Host "4. Test ML API endpoints when backend is ready"
