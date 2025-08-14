# Consciousness Cell Agent Runner
# Launch consciousness cell for project analysis

param(
    [string]$Mode = "single",  # "single" или "continuous"
    [int]$Interval = 30        # Interval for continuous mode (minutes)
)

$ProjectRoot = Split-Path -Parent $PSScriptRoot
$CellScript = Join-Path $PSScriptRoot "consciousness_cell.py"

Write-Host "Consciousness Cell Agent - Temporal Project Intelligence" -ForegroundColor Cyan
Write-Host "Philosophy First: Home is you when you are honest with yourself" -ForegroundColor Yellow
Write-Host ""

# Check script existence
if (-not (Test-Path $CellScript)) {
    Write-Host "❌ Consciousness cell script not found: $CellScript" -ForegroundColor Red
    exit 1
}

# Check Python
try {
    $pythonVersion = python --version 2>&1
    Write-Host "✅ Python detected: $pythonVersion" -ForegroundColor Green
} catch {
    Write-Host "❌ Python not found. Please install Python 3.7+" -ForegroundColor Red
    exit 1
}

Write-Host "📊 Project Root: $ProjectRoot" -ForegroundColor Blue
Write-Host "🔍 Mode: $Mode" -ForegroundColor Blue

if ($Mode -eq "continuous") {
    Write-Host "Monitoring interval: $Interval minutes" -ForegroundColor Blue
    Write-Host ""
    Write-Host "Starting continuous monitoring..." -ForegroundColor Green
    Write-Host "Press Ctrl+C to stop" -ForegroundColor Yellow
    
    python $CellScript $ProjectRoot --continuous $Interval
} else {
    Write-Host ""
    Write-Host "Running single analysis..." -ForegroundColor Green
    
    python $CellScript $ProjectRoot
}

Write-Host ""
Write-Host "Consciousness Cell Agent completed" -ForegroundColor Cyan
