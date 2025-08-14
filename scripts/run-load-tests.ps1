# =============================================================================
# Resonance Liminal Multi-LLM Load Testing Script
# =============================================================================

param(
    [string]$TestType = "full",  # full, quick, stress, ai-only
    [string]$Duration = "60s",
    [int]$VirtualUsers = 10,
    [string]$ReportPath = "./test-results"
)

Write-Host "🧪 Starting Resonance Liminal Multi-LLM Load Tests..." -ForegroundColor Green
Write-Host "   Test Type: $TestType" -ForegroundColor Cyan
Write-Host "   Duration: $Duration" -ForegroundColor Cyan
Write-Host "   Virtual Users: $VirtualUsers" -ForegroundColor Cyan

# Check if Artillery is installed
try {
    artillery --version | Out-Null
    Write-Host "✅ Artillery.io is available" -ForegroundColor Green
} catch {
    Write-Host "❌ Artillery.io not found. Installing..." -ForegroundColor Yellow
    npm install -g artillery
}

# Check if services are running
try {
    $response = Invoke-WebRequest -Uri "http://localhost:8000/health" -TimeoutSec 5 -UseBasicParsing
    if ($response.StatusCode -eq 200) {
        Write-Host "✅ Backend services are running" -ForegroundColor Green
    }
} catch {
    Write-Host "❌ Backend services are not running. Please start them first:" -ForegroundColor Red
    Write-Host "   ./scripts/start-multi-llm.ps1" -ForegroundColor Yellow
    exit 1
}

# Create results directory
if (-not (Test-Path $ReportPath)) {
    New-Item -ItemType Directory -Path $ReportPath -Force | Out-Null
}

$timestamp = Get-Date -Format "yyyy-MM-dd_HH-mm-ss"
$reportFile = "$ReportPath/load-test-$TestType-$timestamp.json"

Write-Host ""
Write-Host "🚀 Running load tests..." -ForegroundColor Cyan

# Change to load testing directory
Set-Location "tests/load-testing"

try {
    switch ($TestType) {
        "quick" {
            Write-Host "⚡ Running quick test (basic endpoints only)..." -ForegroundColor Yellow
            artillery run artillery-xai-test.yml --config "{ `"config`": { `"phases`": [{ `"duration`": `"30`", `"arrivalRate`": 5 }] } }" --output $reportFile
        }
        "stress" {
            Write-Host "💪 Running stress test (high load)..." -ForegroundColor Yellow
            artillery run artillery-xai-test.yml --config "{ `"config`": { `"phases`": [{ `"duration`": `"$Duration`", `"arrivalRate`": $($VirtualUsers * 2) }] } }" --output $reportFile
        }
        "ai-only" {
            Write-Host "🤖 Running AI-focused tests..." -ForegroundColor Yellow
            # Create temporary config for AI-only tests
            $aiConfig = @"
config:
  target: 'http://localhost:8000'
  phases:
    - duration: $Duration
      arrivalRate: $VirtualUsers
  processor: './artillery-functions.js'

scenarios:
  - name: "OpenAI Analysis"
    weight: 30
    flow:
      - post:
          url: "/ml/openai/analyze-logs"
          json:
            logs: ["INFO: Test log entry", "ERROR: Test error"]
            context:
              system_load: 0.5
              error_rate: 0.1
          beforeRequest: "captureStartTime"
          afterResponse: "captureOpenAILatency"

  - name: "Claude Safety Analysis"
    weight: 30
    flow:
      - post:
          url: "/ml/claude/safety-analysis"
          json:
            ml_decision:
              model_name: "test_model"
              prediction: "normal"
              confidence: 0.8
            context:
              system_load: 0.3
          beforeRequest: "captureStartTime"
          afterResponse: "captureClaudeLatency"

  - name: "Multi-LLM Analysis"
    weight: 40
    flow:
      - post:
          url: "/ml/multi-llm/analyze"
          json:
            task_type: "general"
            data:
              problem_description: "Test analysis"
              context:
                environment: "load_test"
            preferred_provider: "auto"
          beforeRequest: "captureStartTime"
          afterResponse: "captureMultiLLMLatency"
"@
            $aiConfig | Out-File -FilePath "ai-only-test.yml" -Encoding UTF8
            artillery run ai-only-test.yml --output $reportFile
            Remove-Item "ai-only-test.yml" -Force
        }
        default {
            Write-Host "🔄 Running full test suite..." -ForegroundColor Yellow
            artillery run artillery-xai-test.yml --config "{ `"config`": { `"phases`": [{ `"duration`": `"$Duration`", `"arrivalRate`": $VirtualUsers }] } }" --output $reportFile
        }
    }
    
    Write-Host ""
    Write-Host "📊 Generating test report..." -ForegroundColor Cyan
    
    # Generate HTML report
    $htmlReport = $reportFile.Replace(".json", ".html")
    artillery report $reportFile --output $htmlReport
    
    Write-Host ""
    Write-Host "✅ Load tests completed!" -ForegroundColor Green
    Write-Host "   📄 JSON Report: $reportFile" -ForegroundColor White
    Write-Host "   🌐 HTML Report: $htmlReport" -ForegroundColor White
    
    # Display quick summary
    if (Test-Path $reportFile) {
        $results = Get-Content $reportFile | ConvertFrom-Json
        $summary = $results.aggregate
        
        Write-Host ""
        Write-Host "📈 Quick Summary:" -ForegroundColor Cyan
        Write-Host "   🎯 Total Requests: $($summary.requestsCompleted)" -ForegroundColor White
        Write-Host "   ✅ Success Rate: $([math]::Round((1 - $summary.errors / $summary.requestsCompleted) * 100, 2))%" -ForegroundColor White
        Write-Host "   ⏱️  Avg Response Time: $([math]::Round($summary.latency.mean, 2))ms" -ForegroundColor White
        Write-Host "   📊 95th Percentile: $([math]::Round($summary.latency.p95, 2))ms" -ForegroundColor White
        
        if ($summary.errors -gt 0) {
            Write-Host "   ⚠️  Errors: $($summary.errors)" -ForegroundColor Yellow
        }
    }
    
    Write-Host ""
    Write-Host "🔍 To view detailed results:" -ForegroundColor Cyan
    Write-Host "   Open: $htmlReport" -ForegroundColor White
    
} catch {
    Write-Host "❌ Load test failed: $($_.Exception.Message)" -ForegroundColor Red
    exit 1
} finally {
    # Return to original directory
    Set-Location "../.."
}

Write-Host ""
Write-Host "🎉 Load testing completed!" -ForegroundColor Green
