# ML Infrastructure Test Script
# Simple version for testing ML components

param(
    [switch]$CleanEnv,
    [switch]$SkipRedis,
    [switch]$SkipPrometheus,
    [switch]$SkipBackend,
    [switch]$SkipKenning
)

# Colors
$Green = [System.ConsoleColor]::Green
$Cyan = [System.ConsoleColor]::Cyan
$Yellow = [System.ConsoleColor]::Yellow
$Red = [System.ConsoleColor]::Red

# Status function
function Write-Status {
    param(
        [string]$Message,
        $Color = $Cyan
    )
    
    Write-Host $Message -ForegroundColor $Color
}

# Container test function
function Test-Container {
    param(
        [string]$Name,
        [string]$Command,
        [string]$Check,
        [int]$Wait = 10
    )
    
    Write-Status -Message "[*] Testing container $Name" -Color $Cyan
    Write-Status -Message "> $Command" -Color $Yellow
    
    try {
        Invoke-Expression $Command | Out-Null
        Start-Sleep -Seconds $Wait
        
        $status = docker ps --filter "name=$Name" --format "{{.Status}}"
        
        if ($status -like "*healthy*" -or $status -like "*Up*") {
            Write-Status -Message "[+] Container $Name started successfully" -Color $Green
            return $true
        } else {
            Write-Status -Message "[!] Container $Name started but may have issues: $status" -Color $Yellow
            Write-Status -Message "[*] Container $Name logs" -Color $Yellow
            docker logs --tail 20 $Name
            return $false
        }
    }
    catch {
        Write-Status -Message "[-] Error starting $Name $_" -Color $Red
        return $false
    }
}

# Check Docker
try {
    $dockerVersion = docker --version
    Write-Status -Message "[+] Docker detected: $dockerVersion" -Color $Green
} catch {
    Write-Status -Message "[-] Docker not installed!" -Color $Red
    exit 1
}

# Clean if needed
if ($CleanEnv) {
    Write-Status -Message "[*] Cleaning test environment..." -Color $Yellow
    docker rm -f ml-redis ml-prometheus ml-backend ml-kenning 2>$null
}

$results = @()

# 1. Test Redis
if (-not $SkipRedis) {
    Write-Status -Message "`n[*] TESTING REDIS" -Color $Green
    $redisOk = Test-Container -Name "ml-redis" `
                          -Command "docker run -d --name ml-redis -p 6379:6379 redis:latest" `
                          -Check "redis-cli ping" `
                          -Wait 5
    
    if ($redisOk) {
        Write-Status -Message "[*] Checking Redis CLI" -Color $Cyan
        docker exec ml-redis redis-cli ping
    }
    
    $results += @{
        Component = "Redis"
        Status = if ($redisOk) { "SUCCESS" } else { "FAILED" }
    }
}

# 2. Test Prometheus
if (-not $SkipPrometheus) {
    Write-Status -Message "`n[*] TESTING PROMETHEUS" -Color $Green
    
    # Create config if needed
    $promPath = "c:/Users/safal/OneDrive/Documente/GitHub/resonance-liminal/prometheus/prometheus.yml"
    if (-not (Test-Path -Path $promPath)) {
        Write-Status -Message "[*] Creating Prometheus config..." -Color $Yellow
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
    
    $promOk = Test-Container -Name "ml-prometheus" `
                         -Command "docker run -d --name ml-prometheus -p 9090:9090 -v $promPath`:/etc/prometheus/prometheus.yml prom/prometheus:latest" `
                         -Check "curl http://localhost:9090/-/healthy" `
                         -Wait 10
                         
    if ($promOk) {
        Write-Status -Message "[*] Prometheus available at http://localhost:9090" -Color $Cyan
    }
    
    $results += @{
        Component = "Prometheus"
        Status = if ($promOk) { "SUCCESS" } else { "FAILED" }
    }
}

# 3. Test Backend
if (-not $SkipBackend) {
    Write-Status -Message "`n[*] TESTING BACKEND WITH ML SUPPORT" -Color $Green
    
    $backendOk = Test-Container -Name "ml-backend" `
                             -Command "docker run -d --name ml-backend -p 8000:8000 --link ml-redis:redis --link ml-prometheus:prometheus -e REDIS_HOST=ml-redis -e PROMETHEUS_URL=http://ml-prometheus:9090 -e TEST_MODE=true -e ML_ENABLED=true -e OPENAI_API_KEY=demo_key -e ANTHROPIC_API_KEY=demo_key resonance-liminal-backend:latest" `
                             -Check "curl http://localhost:8000/health" `
                             -Wait 15
    
    if ($backendOk) {
        Write-Status -Message "[*] Checking API availability" -Color $Cyan
        try {
            $response = Invoke-WebRequest -Uri "http://localhost:8000/docs" -UseBasicParsing
            if ($response.StatusCode -eq 200) {
                Write-Status -Message "[+] API documentation available" -Color $Green
            }
        } catch {
            Write-Status -Message "[!] API documentation unavailable: $_" -Color $Yellow
        }
        
        Write-Status -Message "[*] Backend Logs" -Color $Yellow
        docker logs --tail 20 ml-backend
    }
    
    $results += @{
        Component = "Backend ML"
        Status = if ($backendOk) { "SUCCESS" } else { "FAILED" }
    }
}

# 4. Test Kenning ML
if (-not $SkipKenning) {
    Write-Status -Message "`n[*] TESTING KENNING ML SERVICE" -Color $Green
    
    # Check image
    $kenningImg = docker images kenning/kenning:latest -q
    
    if (-not $kenningImg) {
        Write-Status -Message "[!] Image kenning/kenning:latest not found. Trying backup image..." -Color $Yellow
        $kenningImg = docker images kenning/kenning-serving:latest -q
        
        if (-not $kenningImg) {
            Write-Status -Message "[-] No Kenning images found. Skipping." -Color $Red
            $kenningOk = $false
        }
        else {
            $kenningTag = "kenning/kenning-serving:latest"
        }
    }
    else {
        $kenningTag = "kenning/kenning:latest"
    }
    
    if ($kenningImg) {
        $kenningOk = Test-Container -Name "ml-kenning" `
                                 -Command "docker run -d --name ml-kenning -p 8001:8000 --link ml-redis:redis --link ml-prometheus:prometheus -e PROMETHEUS_URL=http://ml-prometheus:9090 -e REDIS_HOST=ml-redis -v c:/Users/safal/OneDrive/Documente/GitHub/resonance-liminal/models:/app/models $kenningTag" `
                                 -Check "curl http://localhost:8001/health" `
                                 -Wait 20
        
        if ($kenningOk) {
            Write-Status -Message "[*] Checking Kenning API availability" -Color $Cyan
            try {
                $response = Invoke-WebRequest -Uri "http://localhost:8001/docs" -UseBasicParsing
                if ($response.StatusCode -eq 200) {
                    Write-Status -Message "[+] Kenning API documentation available" -Color $Green
                }
            } catch {
                Write-Status -Message "[!] Kenning API documentation unavailable: $_" -Color $Yellow
            }
            
            Write-Status -Message "[*] Kenning Logs" -Color $Yellow
            docker logs --tail 20 ml-kenning
        }
    }
    
    $results += @{
        Component = "Kenning ML"
        Status = if ($kenningOk) { "SUCCESS" } else { "FAILED" }
    }
}

# Results summary
Write-Status -Message "`n[*] ML INFRASTRUCTURE TEST RESULTS" -Color $Green
$results | ForEach-Object {
    $statusColor = if ($_.Status -eq "SUCCESS") { $Green } else { $Red }
    Write-Status -Message ("  • {0,-15}: {1}" -f $_.Component, $_.Status) -Color $statusColor
}

# Summary
$successCount = ($results | Where-Object { $_.Status -eq "SUCCESS" }).Count
$totalCount = $results.Count

Write-Status -Message "`n[*] SUMMARY: $successCount of $totalCount components working correctly" -Color $(if ($successCount -eq $totalCount) { $Green } else { $Yellow })

if ($successCount -eq $totalCount) {
    Write-Status -Message "`n[+] ML infrastructure is ready!" -Color $Green
    Write-Status -Message "   Available services:" -Color $Cyan
    Write-Status -Message "   • Backend API:    http://localhost:8000/docs" -Color $Green
    Write-Status -Message "   • Prometheus:     http://localhost:9090" -Color $Green
    if (-not $SkipKenning) {
        Write-Status -Message "   • Kenning ML API: http://localhost:8001/docs" -Color $Green
    }
} else {
    Write-Status -Message "`n[!] Some ML infrastructure components are not working properly." -Color $Yellow
    Write-Status -Message "   Check logs and configuration to fix issues." -Color $Yellow
}

Write-Status -Message "`n[*] To stop all test containers, run:" -Color $Cyan
Write-Status -Message "   docker rm -f ml-redis ml-prometheus ml-backend ml-kenning" -Color $Yellow
