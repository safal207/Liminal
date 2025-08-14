# TURBO ML BUILD SCRIPT for Resonance Liminal
# Optimized script for fast Docker build

param(
    [switch]$CleanCache,    # Clean Docker caches before build
    [switch]$StartAfterBuild, # Start system after build
    [switch]$TestAfterStart, # Run tests after start
    [switch]$Production      # Use production settings
)

# Set default values
$StartAfterBuild = $StartAfterBuild.IsPresent -or (-not $PSBoundParameters.ContainsKey('StartAfterBuild'))
$CleanCache = $CleanCache.IsPresent
$TestAfterStart = $TestAfterStart.IsPresent
$Production = $Production.IsPresent

# Colors for output
$Green = [System.ConsoleColor]::Green
$Cyan = [System.ConsoleColor]::Cyan
$Yellow = [System.ConsoleColor]::Yellow
$Red = [System.ConsoleColor]::Red

# Function for status output
function Write-Status($message, $color = $Cyan) {
    Write-Host $message -ForegroundColor $color
}

# Check for Docker
try {
    $dockerVersion = docker --version
    Write-Status "Docker detected: $dockerVersion" $Green
} catch {
    Write-Status "Docker not installed! Please install Docker Desktop." $Red
    exit 1
}

# Check for docker-compose
try {
    $composeVersion = docker-compose --version
    Write-Status "Docker Compose detected: $composeVersion" $Green
} catch {
    Write-Status "Docker Compose not installed!" $Red
    exit 1
}

# Project directory
$projectDir = Split-Path -Parent (Split-Path -Parent $MyInvocation.MyCommand.Path)
Set-Location $projectDir
Write-Status "Project directory: $projectDir"

# Set up Docker BuildKit for faster builds
$env:DOCKER_BUILDKIT = 1
$env:COMPOSE_DOCKER_CLI_BUILD = 1
Write-Status "BuildKit activated for faster builds" $Green

# Check for .env file
if (-not (Test-Path ".env")) {
    Write-Status "Creating .env file with default settings..."
    @"
OPENAI_API_KEY=demo_key
ANTHROPIC_API_KEY=demo_key
XAI_CACHE_SIZE=1000
XAI_ENABLE_SHAP=true
XAI_ENABLE_LIME=true
MULTI_LLM_FALLBACK_ENABLED=true
MULTI_LLM_CONSENSUS_THRESHOLD=0.7
MULTI_LLM_MODE=demo
"@ | Out-File -FilePath ".env" -Encoding utf8
}

# Clean Docker cache if needed
if ($CleanCache) {
    Write-Status "Cleaning Docker caches..." $Yellow
    docker builder prune -f
    docker system prune -f
}

# Choose docker-compose file based on mode
$composeFile = if ($Production) { "docker-compose.ml.yml" } else { "docker-compose.ml.yml" }
Write-Status "Using configuration file: $composeFile" $Cyan

# Build images
Write-Status "Starting Docker build (optimized)..." $Cyan
$buildCommand = "docker-compose -f $composeFile build --parallel --progress plain"
Write-Status "> $buildCommand" $Yellow
Invoke-Expression $buildCommand

# Check build status
if ($LASTEXITCODE -ne 0) {
    Write-Status "Error building Docker images!" $Red
    exit 1
}

Write-Status "Docker build completed successfully!" $Green

# Start system after build if flag is set
if ($StartAfterBuild) {
    Write-Status "Starting system..." $Cyan
    $startCommand = "docker-compose -f $composeFile up -d"
    Write-Status "> $startCommand" $Yellow
    Invoke-Expression $startCommand
    
    # Check startup status
    if ($LASTEXITCODE -ne 0) {
        Write-Status "Error starting system!" $Red
        exit 1
    }
    
    Write-Status "Waiting for services to start..." $Yellow
    Start-Sleep -Seconds 10
    
    # Check service status
    $services = docker-compose -f $composeFile ps --services
    foreach ($service in $services) {
        $status = docker-compose -f $composeFile ps $service | Select-String "Up"
        if ($status) {
            Write-Status "Service $service is running" $Green
        } else {
            Write-Status "Service $service might not be ready" $Yellow
        }
    }
    
    # Output service URLs
    Write-Status "`nService access URLs:" $Cyan
    Write-Status "  • Backend API:  http://localhost:8000" $Green
    Write-Status "  • Prometheus:   http://localhost:9090" $Green
    Write-Status "  • Grafana:      http://localhost:3000" $Green
    Write-Status "  • MinIO:        http://localhost:9001" $Green
    Write-Status "  • Jupyter:      http://localhost:8888" $Green
    
    # Run tests if flag is set
    if ($TestAfterStart) {
        Write-Status "`nRunning tests..." $Cyan
        $testCommand = ".\scripts\run-load-tests.ps1"
        Write-Status "> $testCommand" $Yellow
        Invoke-Expression $testCommand
    }
}

Write-Status "`nBuild and startup process completed!" $Green
Write-Status "   To stop the system: docker-compose -f $composeFile down" $Cyan
