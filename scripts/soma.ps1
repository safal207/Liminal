# SOMA Maturation Module Runner - Minimal Version
# Philosophy First: "Дом - это ты, когда искренен с собой"

param (
    [string]$Command = "help"
)

$ScriptPath = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = Split-Path -Parent $ScriptPath

# Check Python availability
try {
    $pythonVersion = python --version 2>&1
    Write-Host "Python available: $pythonVersion" -ForegroundColor Cyan
}
catch {
    Write-Host "Error: Python is required but not found" -ForegroundColor Red
    exit 1
}

# Process commands
switch ($Command.ToLower()) {
    "status" {
        Write-Host "Getting SOMA maturation status..." -ForegroundColor Cyan
        python "$ScriptPath\maturation_helpers.py" status "$ProjectRoot" "$ScriptPath"
    }
    "test" {
        Write-Host "Running SOMA maturation tests..." -ForegroundColor Cyan
        python "$ScriptPath\maturation_helpers.py" test "$ProjectRoot" "$ScriptPath"
    }
    "milestone" {
        $description = Read-Host "Enter milestone description"
        $significance = Read-Host "Enter significance (1-5)"
        python "$ScriptPath\maturation_helpers.py" milestone "$ProjectRoot" "$ScriptPath" "$description" $significance
    }
    "insight" {
        $description = Read-Host "Enter insight description"
        python "$ScriptPath\maturation_helpers.py" insight "$ProjectRoot" "$ScriptPath" "$description"
    }
    "monitor" {
        $interval = Read-Host "Enter monitoring interval in minutes (default: 60)"
        if (-not $interval) { $interval = 60 }
        
        Write-Host "Starting SOMA maturation monitor (Interval: $interval minutes)" -ForegroundColor Cyan
        Write-Host "Press Ctrl+C to stop monitoring" -ForegroundColor Yellow
        python "$ScriptPath\maturation_helpers.py" monitor "$ProjectRoot" "$ScriptPath" $interval
    }
    default {
        Write-Host "SOMA Maturation Module Manager" -ForegroundColor Cyan
        Write-Host "===============================" -ForegroundColor Cyan
        Write-Host "Philosophy First: 'Дом - это ты, когда искренен с собой'" -ForegroundColor Magenta
        Write-Host ""
        Write-Host "Commands:" -ForegroundColor White
        Write-Host "  status    - Display current developmental status" -ForegroundColor White
        Write-Host "  monitor   - Run continuous maturation monitoring" -ForegroundColor White
        Write-Host "  milestone - Record a new development milestone" -ForegroundColor White
        Write-Host "  insight   - Record a new developmental insight" -ForegroundColor White
        Write-Host "  test      - Run maturation module tests" -ForegroundColor White
        Write-Host ""
        Write-Host "Example:" -ForegroundColor White
        Write-Host "  .\soma.ps1 status" -ForegroundColor White
    }
}
