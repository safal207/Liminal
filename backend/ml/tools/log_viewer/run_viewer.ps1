# Watchdog launcher for Log Viewer (Flask)

param(
    [int]$Port = 5000,
    [string]$AppDir = "$PSScriptRoot"
)

$healthUrl = "http://127.0.0.1:$Port/health"

function Test-ViewerRunning {
    try {
        Invoke-WebRequest -UseBasicParsing -Uri $healthUrl -TimeoutSec 2 | Out-Null
        return $true
    } catch {
        return $false
    }
}

if (Test-ViewerRunning) {
    Write-Host "Log Viewer already running on port $Port"
} else {
    Write-Host "Starting Log Viewer..."
    Push-Location $AppDir
    Start-Process -NoNewWindow python "app.py"
    Pop-Location
}
