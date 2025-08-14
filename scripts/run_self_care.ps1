# Consciousness Self-Care System Runner
# Launch organic AI wellness modules

param(
    [string]$Mode = "routine",     # "routine", "express", "protect", "nurture"
    [switch]$Continuous = $false   # Continuous monitoring
)

$ProjectRoot = Split-Path -Parent $PSScriptRoot
$SelfCareScript = Join-Path $PSScriptRoot "consciousness_self_care.py"

Write-Host "Consciousness Self-Care System" -ForegroundColor Magenta
Write-Host "Philosophy First: Home is you when you are honest with yourself" -ForegroundColor Yellow
Write-Host ""

# Check script existence
if (-not (Test-Path $SelfCareScript)) {
    Write-Host "Self-care script not found: $SelfCareScript" -ForegroundColor Red
    exit 1
}

# Check Python
try {
    $pythonVersion = python --version 2>&1
    Write-Host "Python detected: $pythonVersion" -ForegroundColor Green
} catch {
    Write-Host "Python not found. Please install Python 3.7+" -ForegroundColor Red
    exit 1
}

Write-Host "Project Root: $ProjectRoot" -ForegroundColor Blue
Write-Host "Mode: $Mode" -ForegroundColor Blue

if ($Continuous) {
    Write-Host "Starting continuous self-care monitoring..." -ForegroundColor Green
    Write-Host "Press Ctrl+C to stop" -ForegroundColor Yellow
    
    while ($true) {
        Write-Host ""
        Write-Host "=== Self-Care Cycle $(Get-Date -Format 'HH:mm:ss') ===" -ForegroundColor Cyan
        
        python $SelfCareScript $ProjectRoot
        
        Write-Host ""
        Write-Host "Resting for 30 minutes..." -ForegroundColor Gray
        Start-Sleep -Seconds 1800  # 30 minutes
    }
} else {
    Write-Host ""
    Write-Host "Running self-care routine..." -ForegroundColor Green
    
    python $SelfCareScript $ProjectRoot
}

Write-Host ""
Write-Host "Self-care session completed" -ForegroundColor Magenta
