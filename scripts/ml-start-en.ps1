# ML-INFRASTRUCTURE START SCRIPT for Resonance Liminal
# Fast startup script for ML infrastructure with optimized settings

param(
    [switch]$Demo,          # Run in demo mode with simplified settings
    [switch]$RunTests,      # Run tests after startup
    [switch]$OpenDashboard  # Open Grafana dashboard after startup
)

# Set default values
$Demo = $Demo.IsPresent
$RunTests = $RunTests.IsPresent
$OpenDashboard = $OpenDashboard.IsPresent

# Colors for output
$Green = [System.ConsoleColor]::Green
$Cyan = [System.ConsoleColor]::Cyan
$Yellow = [System.ConsoleColor]::Yellow
$Red = [System.ConsoleColor]::Red

# Function for status output
function Write-Status($message, $color = $Cyan) {
    Write-Host $message -ForegroundColor $color
}

# Go to project root directory
$projectDir = Split-Path -Parent (Split-Path -Parent $MyInvocation.MyCommand.Path)
Set-Location $projectDir
Write-Status "Project directory: $projectDir"

# Check for Docker
try {
    $dockerVersion = docker --version
    Write-Status "Docker detected: $dockerVersion" $Green
} catch {
    Write-Status "Docker not installed! Please install Docker Desktop." $Red
    exit 1
}

# Choose docker-compose file
$composeFile = "docker-compose.ml.yml"
Write-Status "Using configuration: $composeFile" $Cyan

# Check and create .env file if needed
if (-not (Test-Path ".env")) {
    Write-Status "Creating .env file..."
    
    if ($Demo) {
        Write-Status "Setting up DEMO mode with test API keys" $Yellow
        $apiKeyConfig = @"
OPENAI_API_KEY=demo_key
ANTHROPIC_API_KEY=demo_key
XAI_CACHE_SIZE=1000
XAI_ENABLE_SHAP=true
XAI_ENABLE_LIME=true
MULTI_LLM_FALLBACK_ENABLED=true
MULTI_LLM_CONSENSUS_THRESHOLD=0.7
MULTI_LLM_MODE=demo
"@
    } else {
        Write-Status "Setting up PRODUCTION mode" $Yellow
        $openAiKey = Read-Host "Enter your OpenAI API key (or leave empty for demo_key)"
        if ([string]::IsNullOrWhiteSpace($openAiKey)) { $openAiKey = "demo_key" }
        
        $anthropicKey = Read-Host "Enter your Anthropic API key (or leave empty for demo_key)"
        if ([string]::IsNullOrWhiteSpace($anthropicKey)) { $anthropicKey = "demo_key" }
        
        $apiKeyConfig = @"
OPENAI_API_KEY=$openAiKey
ANTHROPIC_API_KEY=$anthropicKey
XAI_CACHE_SIZE=2000
XAI_ENABLE_SHAP=true
XAI_ENABLE_LIME=true
MULTI_LLM_FALLBACK_ENABLED=true
MULTI_LLM_CONSENSUS_THRESHOLD=0.8
MULTI_LLM_MODE=production
"@
    }
    
    $apiKeyConfig | Out-File -FilePath ".env" -Encoding utf8
    Write-Status ".env file created successfully!" $Green
}

# Start Docker containers
Write-Status "Starting ML infrastructure..." $Cyan
docker-compose -f $composeFile up -d

# Check startup status
if ($LASTEXITCODE -ne 0) {
    Write-Status "Error starting containers!" $Red
    exit 1
}

Write-Status "Waiting for all services to start..." $Yellow
Start-Sleep -Seconds 15

# Check backend service availability
$backend_status = docker-compose -f $composeFile ps backend | Select-String "Up"
if ($backend_status) {
    Write-Status "Backend service started successfully" $Green
} else {
    Write-Status "Backend service might not be ready, checking logs..." $Yellow
    docker-compose -f $composeFile logs --tail=20 backend
}

# Check Kenning ML service
$kenning_status = docker-compose -f $composeFile ps kenning | Select-String "Up"
if ($kenning_status) {
    Write-Status "ML service started successfully" $Green
} else {
    Write-Status "ML service might not be ready, checking logs..." $Yellow
    docker-compose -f $composeFile logs --tail=20 kenning
}

# Output service information
Write-Status "`nML infrastructure access:" $Cyan
Write-Status "  • Backend API:  http://localhost:8000" $Green
Write-Status "  • ML API:       http://localhost:8000/ml_docs" $Green
Write-Status "  • Prometheus:   http://localhost:9090" $Green
Write-Status "  • Grafana:      http://localhost:3000 (login/password: admin/admin)" $Green
Write-Status "  • Jupyter:      http://localhost:8888" $Green
Write-Status "  • MinIO:        http://localhost:9001 (login/password: minioadmin/minioadmin)" $Green

# Run tests if flag is set
if ($RunTests) {
    Write-Status "`nRunning load and integration tests..." $Cyan
    & "$projectDir\scripts\run-load-tests.ps1"
}

# Open Grafana dashboard if flag is set
if ($OpenDashboard) {
    Write-Status "`nOpening Grafana dashboard..." $Cyan
    Start-Process "http://localhost:3000"
}

Write-Status "`nML infrastructure started successfully!" $Green
Write-Status "   API available at http://localhost:8000" $Cyan
Write-Status "   To stop, run: docker-compose -f $composeFile down" $Cyan
