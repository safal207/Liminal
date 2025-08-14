#!/usr/bin/env pwsh
# =============================================================================
# Resonance Liminal - Optimized startup script with dependency caching
# =============================================================================

param(
    [string]$Mode = "demo",  # demo or production
    [bool]$RunTests = $false,
    [bool]$OpenBrowser = $true
)

$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue" # Makes PowerShell faster in Docker

# ASCII Art banner for Liminal Resonance
Write-Host "=================================" -ForegroundColor Cyan
Write-Host "                                 " -ForegroundColor Cyan
Write-Host "    Advanced AI Orchestration    " -ForegroundColor Cyan
Write-Host "=================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Mode: $Mode" -ForegroundColor White
Write-Host "Run Tests: $RunTests" -ForegroundColor White
Write-Host "Open Browser: $OpenBrowser" -ForegroundColor White
Write-Host ""

# Check for Docker
try {
    docker --version | Out-Null
    Write-Host "[OK] Docker is installed" -ForegroundColor Green
} catch {
    Write-Host "[ERROR] Docker is not installed. Please install Docker and try again." -ForegroundColor Red
    exit 1
}

# Check for docker-compose
try {
    docker-compose --version | Out-Null
    Write-Host "[OK] Docker Compose is installed" -ForegroundColor Green
} catch {
    Write-Host "[ERROR] Docker Compose is not installed. Please install Docker Compose and try again." -ForegroundColor Red
    exit 1
}

# Check and create .env file if missing
if (-not (Test-Path ".env")) {
    Write-Host "[WARNING] .env file not found, creating demo version..." -ForegroundColor Yellow
    
    if (Test-Path ".env.example") {
        Copy-Item ".env.example" ".env"
        Write-Host "[OK] Created .env from example" -ForegroundColor Green
    } else {
        @"
# Demo environment for Resonance Liminal
OPENAI_API_KEY=demo_key
ANTHROPIC_API_KEY=demo_key
MULTI_LLM_MODE=demo
XAI_CACHE_SIZE=1000
XAI_ENABLE_SHAP=true
XAI_ENABLE_LIME=true
"@ | Out-File -FilePath ".env" -Encoding utf8
        Write-Host "[OK] Created minimal .env file" -ForegroundColor Green
    }
}

# Start in demo or production mode
if ($Mode -eq "demo") {
    Write-Host "[STARTING] System in demo mode..." -ForegroundColor Cyan
    $env:MULTI_LLM_MODE = "demo"
    $env:OPENAI_API_KEY = "demo_key"
    $env:ANTHROPIC_API_KEY = "demo_key"
} else {
    Write-Host "[STARTING] System in production mode..." -ForegroundColor Cyan
    
    # Check API keys
    if (-not $env:OPENAI_API_KEY -or $env:OPENAI_API_KEY -eq "demo_key") {
        Write-Host "[WARNING] OPENAI_API_KEY not set or demo. Check .env file." -ForegroundColor Yellow
    }
    
    if (-not $env:ANTHROPIC_API_KEY -or $env:ANTHROPIC_API_KEY -eq "demo_key") {
        Write-Host "[WARNING] ANTHROPIC_API_KEY not set or demo. Check .env file." -ForegroundColor Yellow
    }
}

# Start with optimized Docker Compose
try {
    Write-Host "[STARTING] Containers with dependency caching..." -ForegroundColor Cyan
    docker-compose -f docker-compose.fast.yml up -d
    
    if ($LASTEXITCODE -ne 0) {
        throw "Error starting Docker Compose"
    }
    
    Write-Host "[OK] Containers started successfully" -ForegroundColor Green
} catch {
    Write-Host "[ERROR] Error starting containers: $_" -ForegroundColor Red
    exit 1
}

# Wait for services to be ready
Write-Host "[WAITING] For services to be ready..." -ForegroundColor Cyan
$maxRetries = 30
$retryCount = 0
$allServicesReady = $false

while (-not $allServicesReady -and $retryCount -lt $maxRetries) {
    try {
        $healthResponse = Invoke-RestMethod -Uri "http://localhost:8000/health" -TimeoutSec 2
        
        if ($healthResponse.status -eq "healthy") {
            $allServicesReady = $true
            Write-Host "[OK] All services ready" -ForegroundColor Green
            break
        }
    } catch {
        # Continue waiting
    }
    
    $retryCount++
    Write-Host "[WAITING] For services: attempt $retryCount of $maxRetries" -ForegroundColor Yellow
    Start-Sleep -Seconds 2
}

if (-not $allServicesReady) {
    Write-Host "[WARNING] Timed out waiting for services, but will try to continue..." -ForegroundColor Yellow
}

# Run tests if needed
if ($RunTests) {
    Write-Host "[TESTING] Running tests..." -ForegroundColor Cyan
    
    try {
        Write-Host "[TESTING] Running minimal tests..." -ForegroundColor White
        & "$PSScriptRoot\test-minimal.ps1"
        
        Write-Host "[OK] Minimal tests passed" -ForegroundColor Green
    } catch {
        Write-Host "[ERROR] Error running tests: $_" -ForegroundColor Red
    }
}

# Open browser if needed
if ($OpenBrowser) {
    Write-Host "[BROWSER] Opening browser..." -ForegroundColor Cyan
    Start-Process "http://localhost:8000/docs"
}

# Final message
Write-Host ""
Write-Host "=================================" -ForegroundColor Cyan
Write-Host "    Multi-LLM System Operational " -ForegroundColor Green
Write-Host "=================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "API Documentation: http://localhost:8000/docs" -ForegroundColor White
Write-Host "Health Check: http://localhost:8000/health" -ForegroundColor White
Write-Host "ML Status: http://localhost:8000/ml/status" -ForegroundColor White
Write-Host ""
Write-Host "Run tests:" -ForegroundColor Cyan
Write-Host "   Minimal Test:     powershell -File scripts/test-minimal.ps1" -ForegroundColor White
Write-Host "   API Endpoints:    python scripts/test-api-endpoints.py" -ForegroundColor White
Write-Host "   Detailed Test:    python scripts/detailed-test.py" -ForegroundColor White
Write-Host "   Load Test:        python scripts/run-load-tests.py" -ForegroundColor White
Write-Host ""
Write-Host "To stop the system:" -ForegroundColor Cyan
Write-Host "   docker-compose -f docker-compose.fast.yml down" -ForegroundColor White
Write-Host ""

# Philosophical concepts from LIMINAL materials - ASCII only
Write-Host "Home is not a place. Home is you when you are sincere with yourself." -ForegroundColor Magenta
Write-Host "Resonance begins with presence here and now." -ForegroundColor Magenta
