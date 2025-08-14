#!/usr/bin/env pwsh
# PowerShell script to reliably start the at-risk server

Write-Host "Starting At-Risk Edges Server..." -ForegroundColor Green

# Change to project directory
$projectDir = Split-Path -Parent $PSScriptRoot
Set-Location $projectDir

# Kill any existing Python processes on port 8080
Write-Host "Checking for existing servers on port 8080..." -ForegroundColor Yellow
$existingProcesses = Get-NetTCPConnection -LocalPort 8080 -ErrorAction SilentlyContinue | ForEach-Object { Get-Process -Id $_.OwningProcess -ErrorAction SilentlyContinue }
if ($existingProcesses) {
    Write-Host "Stopping existing processes..." -ForegroundColor Yellow
    $existingProcesses | Stop-Process -Force -ErrorAction SilentlyContinue
    Start-Sleep -Seconds 2
}

# Start server in new window that stays open
Write-Host "Launching server in new window..." -ForegroundColor Green
$serverCmd = "cd '$projectDir'; python scripts/simple_server.py; Write-Host 'Server stopped. Press any key to close...'; Read-Host"
Start-Process powershell -ArgumentList "-NoExit", "-Command", $serverCmd

# Wait a moment for server to start
Start-Sleep -Seconds 3

# Test if server is responding
try {
    $response = Invoke-WebRequest -Uri "http://127.0.0.1:8080/at-risk" -UseBasicParsing -TimeoutSec 5
    if ($response.StatusCode -eq 200) {
        Write-Host "✅ Server is running successfully!" -ForegroundColor Green
        Write-Host "📱 Open in browser: http://127.0.0.1:8080/at-risk" -ForegroundColor Cyan
        Write-Host "🔧 CLI alternative: python scripts/top_at_risk.py" -ForegroundColor Cyan
        
        # Optionally open browser
        $openBrowser = Read-Host "Open browser automatically? (y/N)"
        if ($openBrowser -eq 'y' -or $openBrowser -eq 'Y') {
            Start-Process "http://127.0.0.1:8080/at-risk"
        }
    }
} catch {
    Write-Host "❌ Server failed to start properly" -ForegroundColor Red
    Write-Host "Try running manually: python scripts/simple_server.py" -ForegroundColor Yellow
}

Write-Host "Done! Server should be running in separate window." -ForegroundColor Green
