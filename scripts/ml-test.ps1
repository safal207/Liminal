# ML INFRASTRUCTURE TEST SCRIPT
# Script for step-by-step testing of ML infrastructure components

param(
    [switch]$CleanEnv,          # Clean environment before testing
    [switch]$SkipRedis,         # Skip Redis testing
    [switch]$SkipPrometheus,    # Skip Prometheus testing
    [switch]$SkipBackend,       # Skip Backend testing
    [switch]$SkipKenning        # Skip Kenning ML testing
)

# Colors for output
$Green = [System.ConsoleColor]::Green
$Cyan = [System.ConsoleColor]::Cyan
$Yellow = [System.ConsoleColor]::Yellow
$Red = [System.ConsoleColor]::Red

# Function to output status
function Write-Status($message, $color = $Cyan) {
    Write-Host $message -ForegroundColor $color
}

# Function to start container and check its status
function Test-Container {
    param(
        [string]$Name,
        [string]$Command,
        [string]$HealthCheck,
        [int]$WaitTime = 10
    )

    Write-Status "[*] Testing $Name" $Cyan
    Write-Status "> $Command" $Yellow
    
    try {
        Invoke-Expression $Command | Out-Null
        Start-Sleep -Seconds $WaitTime
        
        $status = docker ps --filter "name=$Name" --format "{{.Status}}"
        
        if ($status -like "*healthy*" -or $status -like "*Up*") {
            Write-Status "[+] Container $Name started successfully" $Green
            return $true
        } else {
            Write-Status "[!] Container $Name started but may have issues: $status" $Yellow
            
            Write-Status "[*] Logs of container $Name:" $Yellow
            docker logs --tail 20 $Name
            
            return $false
        }
    }
    catch {
        Write-Status "[-] Error starting $Name: $_" $Red
        return $false
    }
}

# Check Docker presence
try {
    $dockerVersion = docker --version
    Write-Status "[+] Docker detected: $dockerVersion" $Green
} catch {
    Write-Status "[-] Docker not installed! Please install Docker Desktop." $Red
    exit 1
}

# Clean environment if specified
if ($CleanEnv) {
    Write-Status "[*] Cleaning test environment..." $Yellow
    docker rm -f ml-redis ml-prometheus ml-backend ml-kenning 2>$null
}

$testResults = @()

# 1. Testing Redis
if (-not $SkipRedis) {
    Write-Status "`n[*] TESTING REDIS" $Green
    $redisSuccess = Test-Container -Name "ml-redis" `
                               -Command "docker run -d --name ml-redis -p 6379:6379 redis:latest" `
                               -HealthCheck "redis-cli ping" `
                               -WaitTime 5
    
    if ($redisSuccess) {
        Write-Status "[*] Checking Redis CLI" $Cyan
        docker exec ml-redis redis-cli ping
    }
    
    $testResults += @{
        Component = "Redis"
        Status = if ($redisSuccess) { "SUCCESS" } else { "FAILED" }
    }
}

# 2. Testing Prometheus
if (-not $SkipPrometheus) {
    Write-Status "`n[*] TESTING PROMETHEUS" $Green
    
    # Create temporary config if needed
    if (-not (Test-Path -Path "c:/Users/safal/OneDrive/Documente/GitHub/resonance-liminal/prometheus/prometheus.yml")) {
        Write-Status "[*] Creating temporary config for Prometheus..." $Yellow
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
        New-Item -Path "c:/Users/safal/OneDrive/Documente/GitHub/resonance-liminal/prometheus" -ItemType Directory -Force | Out-Null
        $prometheusConfig | Out-File -FilePath "c:/Users/safal/OneDrive/Documente/GitHub/resonance-liminal/prometheus/prometheus.yml" -Encoding utf8
    }
    
    $prometheusSuccess = Test-Container -Name "ml-prometheus" `
                                    -Command "docker run -d --name ml-prometheus -p 9090:9090 -v c:/Users/safal/OneDrive/Documente/GitHub/resonance-liminal/prometheus/prometheus.yml:/etc/prometheus/prometheus.yml prom/prometheus:latest" `
                                    -HealthCheck "curl http://localhost:9090/-/healthy" `
                                    -WaitTime 10
                                    
    if ($prometheusSuccess) {
        Write-Status "[*] Prometheus available at: http://localhost:9090" $Cyan
    }
    
    $testResults += @{
        Component = "Prometheus"
        Status = if ($prometheusSuccess) { "SUCCESS" } else { "FAILED" }
    }
}

# 3. Testing Backend with ML components
if (-not $SkipBackend) {
    Write-Status "`n[*] TESTING BACKEND WITH ML SUPPORT" $Green
    
    $backendSuccess = Test-Container -Name "ml-backend" `
                                 -Command "docker run -d --name ml-backend -p 8000:8000 --link ml-redis:redis --link ml-prometheus:prometheus -e REDIS_HOST=ml-redis -e PROMETHEUS_URL=http://ml-prometheus:9090 -e TEST_MODE=true -e ML_ENABLED=true -e OPENAI_API_KEY=demo_key -e ANTHROPIC_API_KEY=demo_key resonance-liminal-backend:latest" `
                                 -HealthCheck "curl http://localhost:8000/health" `
                                 -WaitTime 15
    
    if ($backendSuccess) {
        Write-Status "[*] Checking API availability" $Cyan
        try {
            $response = Invoke-WebRequest -Uri "http://localhost:8000/docs" -UseBasicParsing
            if ($response.StatusCode -eq 200) {
                Write-Status "[+] API documentation available" $Green
            }
        } catch {
            Write-Status "[!] API documentation unavailable: $_" $Yellow
        }
        
        Write-Status "[*] Backend Logs:" $Yellow
        docker logs --tail 20 ml-backend
    }
    
    $testResults += @{
        Component = "Backend ML"
        Status = if ($backendSuccess) { "SUCCESS" } else { "FAILED" }
    }
}

# 4. Testing Kenning ML service
if (-not $SkipKenning) {
    Write-Status "`n[*] TESTING KENNING ML SERVICE" $Green
    
    # Check if image exists
    $kenningImageExists = docker images kenning/kenning:latest -q
    
    if (-not $kenningImageExists) {
        Write-Status "[!] Image kenning/kenning:latest not found. Trying backup image..." $Yellow
        $kenningImageExists = docker images kenning/kenning-serving:latest -q
        
        if (-not $kenningImageExists) {
            Write-Status "[-] No Kenning images found. Skipping Kenning testing." $Red
            $kenningSuccess = $false
        }
        else {
            $kenningImage = "kenning/kenning-serving:latest"
        }
    }
    else {
        $kenningImage = "kenning/kenning:latest"
    }
    
    if ($kenningImageExists) {
        $kenningSuccess = Test-Container -Name "ml-kenning" `
                                     -Command "docker run -d --name ml-kenning -p 8001:8000 --link ml-redis:redis --link ml-prometheus:prometheus -e PROMETHEUS_URL=http://ml-prometheus:9090 -e REDIS_HOST=ml-redis -v c:/Users/safal/OneDrive/Documente/GitHub/resonance-liminal/models:/app/models $kenningImage" `
                                     -HealthCheck "curl http://localhost:8001/health" `
                                     -WaitTime 20
        
        if ($kenningSuccess) {
            Write-Status "[*] Checking Kenning API availability" $Cyan
            try {
                $response = Invoke-WebRequest -Uri "http://localhost:8001/docs" -UseBasicParsing
                if ($response.StatusCode -eq 200) {
                    Write-Status "[+] Kenning API documentation available" $Green
                }
            } catch {
                Write-Status "[!] Kenning API documentation unavailable: $_" $Yellow
            }
            
            Write-Status "[*] Kenning Logs:" $Yellow
            docker logs --tail 20 ml-kenning
        }
    }
    
    $testResults += @{
        Component = "Kenning ML"
        Status = if ($kenningSuccess) { "SUCCESS" } else { "FAILED" }
    }
}

# Output results
Write-Status "`n[*] ML INFRASTRUCTURE COMPONENT TEST RESULTS" $Green
$testResults | ForEach-Object {
    $statusColor = if ($_.Status -like "SUCCESS") { $Green } else { $Red }
    Write-Status ("  • {0,-15}: {1}" -f $_.Component, $_.Status) $statusColor
}

# Output summary information
$successCount = ($testResults | Where-Object { $_.Status -like "SUCCESS" }).Count
$totalCount = $testResults.Count

Write-Status "`n[*] SUMMARY: $successCount of $totalCount components working correctly" $(if ($successCount -eq $totalCount) { $Green } else { $Yellow })

if ($successCount -eq $totalCount) {
    Write-Status "`n[+] ML infrastructure is ready!" $Green
    Write-Status "   Available services:" $Cyan
    Write-Status "   • Backend API:    http://localhost:8000/docs" $Green
    Write-Status "   • Prometheus:     http://localhost:9090" $Green
    if (-not $SkipKenning) {
        Write-Status "   • Kenning ML API: http://localhost:8001/docs" $Green
    }
} else {
    Write-Status "`n[!] Some ML infrastructure components are not working properly." $Yellow
    Write-Status "   Check logs and configuration to fix issues." $Yellow
}

Write-Status "`n[*] To stop all test containers, run:" $Cyan
Write-Status "   docker rm -f ml-redis ml-prometheus ml-backend ml-kenning" $Yellow
