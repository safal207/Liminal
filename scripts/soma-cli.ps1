# SOMA Maturation Module Runner - Simple ASCII Version

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
if ($Command -eq "status") {
    Write-Host "Getting SOMA maturation status..." -ForegroundColor Cyan
    python "$ScriptPath\maturation_helpers.py" status "$ProjectRoot" "$ScriptPath"
}
elseif ($Command -eq "test") {
    Write-Host "Running SOMA maturation tests..." -ForegroundColor Cyan
    python "$ScriptPath\maturation_helpers.py" test "$ProjectRoot" "$ScriptPath"
}
elseif ($Command -eq "milestone") {
    $description = Read-Host "Enter milestone description"
    $significance = Read-Host "Enter significance (1-5)"
    python "$ScriptPath\maturation_helpers.py" milestone "$ProjectRoot" "$ScriptPath" "$description" $significance
}
elseif ($Command -eq "insight") {
    $description = Read-Host "Enter insight description"
    python "$ScriptPath\maturation_helpers.py" insight "$ProjectRoot" "$ScriptPath" "$description"
}
elseif ($Command -eq "monitor") {
    $interval = Read-Host "Enter monitoring interval in minutes (default: 60)"
    if (-not $interval) { $interval = 60 }
    
    Write-Host "Starting SOMA maturation monitor (Interval: $interval minutes)" -ForegroundColor Cyan
    Write-Host "Press Ctrl+C to stop monitoring" -ForegroundColor Yellow
    python "$ScriptPath\maturation_helpers.py" monitor "$ProjectRoot" "$ScriptPath" $interval
}
else {
    Write-Host "SOMA Maturation Module Manager" -ForegroundColor Cyan
    Write-Host "===============================" -ForegroundColor Cyan
    Write-Host "Philosophy First: Home is where you are sincere with yourself" -ForegroundColor Magenta
    Write-Host ""
    Write-Host "Commands:" -ForegroundColor White
    Write-Host "  status    - Display current developmental status" -ForegroundColor White
    Write-Host "  monitor   - Run continuous maturation monitoring" -ForegroundColor White
    Write-Host "  milestone - Record a new development milestone" -ForegroundColor White
    Write-Host "  insight   - Record a new developmental insight" -ForegroundColor White
    Write-Host "  test      - Run maturation module tests" -ForegroundColor White
    Write-Host ""
    Write-Host "Example:" -ForegroundColor White
    Write-Host "  .\soma-cli.ps1 status" -ForegroundColor White
}
