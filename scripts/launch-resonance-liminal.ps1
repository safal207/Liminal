# Resonance Liminal Multi-LLM System Launcher
# Complete system startup and testing script

param(
    [string]$Mode = "demo",  # demo, production, test
    [switch]$RunTests = $false,
    [switch]$OpenBrowser = $true
)

Write-Host ""
Write-Host "=================================" -ForegroundColor Cyan
Write-Host "  RESONANCE LIMINAL MULTI-LLM   " -ForegroundColor Green
Write-Host "    Advanced AI Orchestration    " -ForegroundColor Green  
Write-Host "=================================" -ForegroundColor Cyan
Write-Host ""

Write-Host "Mode: $Mode" -ForegroundColor Yellow
Write-Host "Run Tests: $RunTests" -ForegroundColor Yellow
Write-Host "Open Browser: $OpenBrowser" -ForegroundColor Yellow
Write-Host ""

# Check Python
try {
    $pythonVersion = python --version 2>&1
    Write-Host "Python: $pythonVersion" -ForegroundColor Green
} catch {
    Write-Host "ERROR: Python not found. Please install Python 3.11+" -ForegroundColor Red
    exit 1
}

# Install dependencies if needed
Write-Host "Installing dependencies..." -ForegroundColor Cyan
pip install fastapi uvicorn requests pydantic | Out-Null

if ($Mode -eq "demo") {
    Write-Host ""
    Write-Host "Starting DEMO MODE..." -ForegroundColor Green
    Write-Host "- No external API calls" -ForegroundColor Yellow
    Write-Host "- All responses simulated" -ForegroundColor Yellow
    Write-Host "- Perfect for testing and demonstration" -ForegroundColor Yellow
    Write-Host ""
    
    # Start demo server
    Write-Host "Launching Multi-LLM Demo Server..." -ForegroundColor Cyan
    Start-Process python -ArgumentList "scripts/demo-server.py" -WindowStyle Minimized
    
    # Wait for server to start
    Write-Host "Waiting for server startup..." -ForegroundColor Yellow
    Start-Sleep -Seconds 5
    
    # Test server
    $maxRetries = 10
    $retry = 0
    $serverReady = $false
    
    while ($retry -lt $maxRetries -and -not $serverReady) {
        try {
            $response = Invoke-WebRequest -Uri "http://localhost:8000/health" -TimeoutSec 3 -UseBasicParsing
            if ($response.StatusCode -eq 200) {
                $serverReady = $true
                Write-Host "Server is ready!" -ForegroundColor Green
            }
        } catch {
            $retry++
            Write-Host "Waiting... ($retry/$maxRetries)" -ForegroundColor Yellow
            Start-Sleep -Seconds 2
        }
    }
    
    if (-not $serverReady) {
        Write-Host "ERROR: Server failed to start" -ForegroundColor Red
        exit 1
    }
    
} elseif ($Mode -eq "production") {
    Write-Host ""
    Write-Host "Starting PRODUCTION MODE..." -ForegroundColor Green
    Write-Host "- Real API calls to OpenAI and Claude" -ForegroundColor Yellow
    Write-Host "- Requires valid API keys in .env file" -ForegroundColor Yellow
    Write-Host ""
    
    # Check for API keys
    if (-not (Test-Path ".env")) {
        Write-Host "ERROR: .env file not found. Copy .env.example and add your API keys." -ForegroundColor Red
        exit 1
    }
    
    # Start with Docker
    Write-Host "Starting Docker containers..." -ForegroundColor Cyan
    docker-compose -f docker-compose.ml-production.yml up -d
    
    Write-Host "Waiting for containers..." -ForegroundColor Yellow
    Start-Sleep -Seconds 30
}

# Run tests if requested
if ($RunTests) {
    Write-Host ""
    Write-Host "Running comprehensive tests..." -ForegroundColor Cyan
    python scripts/detailed-test.py
}

# Display URLs and information
Write-Host ""
Write-Host "=================================" -ForegroundColor Cyan
Write-Host "      SYSTEM READY!              " -ForegroundColor Green
Write-Host "=================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Access Points:" -ForegroundColor White
Write-Host "  API Documentation: http://localhost:8000/docs" -ForegroundColor Cyan
Write-Host "  Health Check:      http://localhost:8000/health" -ForegroundColor Cyan
Write-Host "  ML Status:         http://localhost:8000/ml/status" -ForegroundColor Cyan
Write-Host "  AI Status:         http://localhost:8000/ml/ai-status" -ForegroundColor Cyan
Write-Host ""
Write-Host "Key Endpoints:" -ForegroundColor White
Write-Host "  XAI Explanations:  POST /ml/xai/explain-prediction" -ForegroundColor Yellow
Write-Host "  OpenAI Analysis:   POST /ml/openai/analyze-logs" -ForegroundColor Yellow
Write-Host "  Claude Safety:     POST /ml/claude/safety-analysis" -ForegroundColor Yellow
Write-Host "  Multi-LLM:         POST /ml/multi-llm/analyze" -ForegroundColor Yellow
Write-Host "  Consensus:         POST /ml/multi-llm/consensus" -ForegroundColor Yellow
Write-Host ""
Write-Host "Testing Commands:" -ForegroundColor White
Write-Host "  Quick Test:        python scripts/test-api-endpoints.py" -ForegroundColor Green
Write-Host "  Detailed Test:     python scripts/detailed-test.py" -ForegroundColor Green
Write-Host "  Load Test:         python scripts/run-load-tests.ps1" -ForegroundColor Green
Write-Host ""
Write-Host "Documentation:" -ForegroundColor White
Write-Host "  Architecture:      docs/XAI-OPENAI-INTEGRATION.md" -ForegroundColor Magenta
Write-Host "  Quick Start:       MULTI-LLM-QUICKSTART.md" -ForegroundColor Magenta
Write-Host "  Test Report:       docs/TESTING-REPORT.md" -ForegroundColor Magenta
Write-Host "  Snowden Review:    docs/SNOWDEN-TESTIMONIAL.md" -ForegroundColor Magenta
Write-Host ""

# Open browser if requested
if ($OpenBrowser) {
    Write-Host "Opening browser..." -ForegroundColor Cyan
    Start-Process "http://localhost:8000/docs"
}

Write-Host "=================================" -ForegroundColor Cyan
Write-Host "  Multi-LLM System Operational   " -ForegroundColor Green
Write-Host "=================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Press Ctrl+C to stop the system" -ForegroundColor Yellow

# Keep script running
try {
    while ($true) {
        Start-Sleep -Seconds 10
        # Check if server is still running
        try {
            $response = Invoke-WebRequest -Uri "http://localhost:8000/health" -TimeoutSec 3 -UseBasicParsing
            Write-Host "." -NoNewline -ForegroundColor Green
        } catch {
            Write-Host ""
            Write-Host "WARNING: Server not responding" -ForegroundColor Yellow
            break
        }
    }
} catch {
    Write-Host ""
    Write-Host "Shutting down..." -ForegroundColor Yellow
}
