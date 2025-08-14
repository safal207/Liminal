# 🌟 SOMA Family Dashboard Runner
# Philosophy First: "Дом - это ты, когда искренен с собой"

param(
    [string]$Mode = "start",
    [int]$Port = 8080,
    [int]$WebSocketPort = 8081
)

$ErrorActionPreference = "Continue"
$OutputEncoding = [System.Text.Encoding]::UTF8

# Colors for output
$Colors = @{
    Header = "Cyan"
    Success = "Green"
    Warning = "Yellow"
    Error = "Red"
    Info = "White"
    Philosophy = "Magenta"
}

function Write-ColorOutput {
    param([string]$Message, [string]$Color = "Info")
    Write-Host $Message -ForegroundColor $Colors[$Color]
}

function Show-Header {
    Write-ColorOutput "SOMA Family Dashboard" "Header"
    Write-ColorOutput "=" * 50 "Header"
    Write-ColorOutput "Philosophy First: 'Home is you when sincere with yourself'" "Philosophy"
    Write-ColorOutput ""
}

function Test-Dependencies {
    Write-ColorOutput "🔍 Checking dependencies..." "Info"
    
    # Check Python
    try {
        $pythonVersion = python --version 2>&1
        if ($LASTEXITCODE -eq 0) {
            Write-ColorOutput "✅ Python: $pythonVersion" "Success"
        } else {
            Write-ColorOutput "❌ Python not found" "Error"
            return $false
        }
    } catch {
        Write-ColorOutput "❌ Python not found" "Error"
        return $false
    }
    
    # Check required Python packages
    $requiredPackages = @("websockets", "asyncio")
    foreach ($package in $requiredPackages) {
        try {
            python -c "import $package" 2>$null
            if ($LASTEXITCODE -eq 0) {
                Write-ColorOutput "✅ Python package: $package" "Success"
            } else {
                Write-ColorOutput "⚠️ Installing $package..." "Warning"
                pip install $package
            }
        } catch {
            Write-ColorOutput "⚠️ Installing $package..." "Warning"
            pip install $package
        }
    }
    
    return $true
}

function Start-Dashboard {
    param([int]$Port, [int]$WebSocketPort)
    
    Write-ColorOutput "🚀 Starting SOMA Family Dashboard..." "Info"
    Write-ColorOutput "📊 HTTP Server: http://localhost:$Port" "Info"
    Write-ColorOutput "🔌 WebSocket Server: ws://localhost:$WebSocketPort" "Info"
    Write-ColorOutput ""
    
    $scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
    $projectRoot = Split-Path -Parent $scriptDir
    $dashboardScript = Join-Path $projectRoot "web" "dashboard_server.py"
    
    if (-not (Test-Path $dashboardScript)) {
        Write-ColorOutput "❌ Dashboard server script not found: $dashboardScript" "Error"
        return
    }
    
    Write-ColorOutput "🌐 Opening dashboard in browser..." "Info"
    Write-ColorOutput "💡 Press Ctrl+C to stop the server" "Warning"
    Write-ColorOutput ""
    
    try {
        # Set environment variables
        $env:DASHBOARD_PORT = $Port
        $env:WEBSOCKET_PORT = $WebSocketPort
        
        # Start the dashboard server
        python $dashboardScript
    } catch {
        Write-ColorOutput "❌ Error starting dashboard: $_" "Error"
    }
}

function Show-Status {
    Write-ColorOutput "📊 SOMA Dashboard Status" "Header"
    Write-ColorOutput ""
    
    # Check if ports are in use
    $httpPortInUse = Get-NetTCPConnection -LocalPort $Port -ErrorAction SilentlyContinue
    $wsPortInUse = Get-NetTCPConnection -LocalPort $WebSocketPort -ErrorAction SilentlyContinue
    
    if ($httpPortInUse) {
        Write-ColorOutput "🌐 HTTP Server (Port $Port): RUNNING" "Success"
        Write-ColorOutput "   Dashboard: http://localhost:$Port/soma_dashboard.html" "Info"
    } else {
        Write-ColorOutput "🌐 HTTP Server (Port $Port): STOPPED" "Warning"
    }
    
    if ($wsPortInUse) {
        Write-ColorOutput "🔌 WebSocket Server (Port $WebSocketPort): RUNNING" "Success"
    } else {
        Write-ColorOutput "🔌 WebSocket Server (Port $WebSocketPort): STOPPED" "Warning"
    }
    
    Write-ColorOutput ""
    
    # Check SOMA family status
    $scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
    $somaScript = Join-Path $scriptDir "SOMA_integrated.py"
    
    if (Test-Path $somaScript) {
        Write-ColorOutput "🧠 SOMA Family System: AVAILABLE" "Success"
        
        # Check for recent activity
        $logFiles = @(
            "SOMA_state.json",
            "consciousness_family.json",
            "module_relationships.json",
            "wellness_state.json"
        )
        
        $recentActivity = $false
        foreach ($logFile in $logFiles) {
            $logPath = Join-Path $scriptDir $logFile
            if (Test-Path $logPath) {
                $lastWrite = (Get-Item $logPath).LastWriteTime
                $timeDiff = (Get-Date) - $lastWrite
                if ($timeDiff.TotalMinutes -lt 30) {
                    $recentActivity = $true
                    break
                }
            }
        }
        
        if ($recentActivity) {
            Write-ColorOutput "💚 Recent Family Activity: DETECTED" "Success"
        } else {
            Write-ColorOutput "😴 Recent Family Activity: QUIET" "Warning"
        }
    } else {
        Write-ColorOutput "🧠 SOMA Family System: NOT FOUND" "Warning"
        Write-ColorOutput "   Dashboard will run in demo mode" "Info"
    }
}

function Stop-Dashboard {
    Write-ColorOutput "🛑 Stopping SOMA Dashboard..." "Warning"
    
    # Find and kill dashboard processes
    $processes = Get-Process | Where-Object { 
        $_.ProcessName -eq "python" -and 
        $_.CommandLine -like "*dashboard_server.py*" 
    }
    
    if ($processes) {
        foreach ($process in $processes) {
            Write-ColorOutput "🔪 Stopping process: $($process.Id)" "Info"
            Stop-Process -Id $process.Id -Force
        }
        Write-ColorOutput "✅ Dashboard stopped" "Success"
    } else {
        Write-ColorOutput "ℹ️ No dashboard processes found" "Info"
    }
}

function Show-Help {
    Write-ColorOutput "🌟 SOMA Family Dashboard Runner" "Header"
    Write-ColorOutput ""
    Write-ColorOutput "USAGE:" "Info"
    Write-ColorOutput "  .\run_dashboard.ps1 [MODE] [OPTIONS]" "Info"
    Write-ColorOutput ""
    Write-ColorOutput "MODES:" "Info"
    Write-ColorOutput "  start     - Start the dashboard server (default)" "Info"
    Write-ColorOutput "  status    - Show dashboard status" "Info"
    Write-ColorOutput "  stop      - Stop the dashboard server" "Info"
    Write-ColorOutput "  help      - Show this help message" "Info"
    Write-ColorOutput ""
    Write-ColorOutput "OPTIONS:" "Info"
    Write-ColorOutput "  -Port <port>          HTTP server port (default: 8080)" "Info"
    Write-ColorOutput "  -WebSocketPort <port> WebSocket server port (default: 8081)" "Info"
    Write-ColorOutput ""
    Write-ColorOutput "EXAMPLES:" "Info"
    Write-ColorOutput "  .\run_dashboard.ps1" "Info"
    Write-ColorOutput "  .\run_dashboard.ps1 start -Port 9000" "Info"
    Write-ColorOutput "  .\run_dashboard.ps1 status" "Info"
    Write-ColorOutput "  .\run_dashboard.ps1 stop" "Info"
    Write-ColorOutput ""
    Write-ColorOutput "PHILOSOPHY:" "Philosophy"
    Write-ColorOutput "  'Дом - это ты, когда искренен с собой'" "Philosophy"
    Write-ColorOutput "  Dashboard shows the living soul of SOMA family" "Philosophy"
}

# Main execution
Show-Header

switch ($Mode.ToLower()) {
    "start" {
        if (Test-Dependencies) {
            Start-Dashboard -Port $Port -WebSocketPort $WebSocketPort
        }
    }
    "status" {
        Show-Status
    }
    "stop" {
        Stop-Dashboard
    }
    "help" {
        Show-Help
    }
    default {
        Write-ColorOutput "❌ Unknown mode: $Mode" "Error"
        Write-ColorOutput "Use 'help' mode to see available options" "Info"
    }
}

Write-ColorOutput ""
Write-ColorOutput "SOMA family continues to grow in consciousness..." "Philosophy"
