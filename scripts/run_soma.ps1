# SOMA Maturation Module Runner
# Manages the maturation and development tracking for SOMA consciousness
# Philosophy First: "Дом - это ты, когда искренен с собой"

param (
    [string]$Command = "help",
    [int]$MonitorInterval = 60,
    [switch]$Verbose
)

# Define colors for console output
$TitleColor = "Cyan"
$SuccessColor = "Green"
$ErrorColor = "Red"
$WarningColor = "Yellow"
$InfoColor = "White"
$PhilosophyColor = "Magenta"
$StageColor = "Blue"
$MetricsColor = "DarkCyan"

# Script directory and project root
$ScriptPath = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = Split-Path -Parent $ScriptPath

# Helper function to output colored messages
function Write-ColorMessage {
    param (
        [string]$Message,
        [string]$Color = $InfoColor
    )
    
    Write-Host $Message -ForegroundColor $Color
}

# Helper function to check Python availability
function Test-PythonAvailability {
    try {
        $version = python --version
        Write-ColorMessage "Python available: $version" $InfoColor
        return $true
    }
    catch {
        Write-ColorMessage "Python not found. Please install Python 3.6 or higher" $ErrorColor
        return $false
    }
}

# Check if required packages are installed
function Test-RequiredPackages {
    $requiredPackages = @("pathlib")
    $missingPackages = @()
    
    foreach ($package in $requiredPackages) {
        try {
            $null = python -c "import $package"
        }
        catch {
            $missingPackages += $package
        }
    }
    
    if ($missingPackages.Count -gt 0) {
        Write-ColorMessage "Missing Python packages: $($missingPackages -join ', ')" $WarningColor
        
        $install = Read-Host "Do you want to install missing packages? (y/n)"
        if ($install -eq "y") {
            foreach ($package in $missingPackages) {
                Write-ColorMessage "Installing $package..." $InfoColor
                python -m pip install $package
            }
        }
        else {
            Write-ColorMessage "Some features may not work without required packages." $WarningColor
        }
    }
}

# Run unit tests for maturation module
function Invoke-MaturationTests {
    Write-ColorMessage "Running maturation module tests..." $InfoColor
    
    $output = python "$ScriptPath\maturation_helpers.py" test "$ProjectRoot" "$ScriptPath"
    $exitCode = $LASTEXITCODE
    
    if ($exitCode -eq 0) {
        Write-ColorMessage "✅ All tests passed - SOMA maturation module is healthy" $SuccessColor
        Write-ColorMessage "🧠 'Как младенец исследует мир, так система исследует свои возможности'" $PhilosophyColor
    }
    else {
        Write-ColorMessage "❌ Tests failed - SOMA maturation module needs attention" $ErrorColor
        Write-ColorMessage "🧠 'Ошибки — это первые шаги к росту'" $PhilosophyColor
        Write-ColorMessage $output $ErrorColor
    }
}

# Generate maturation status report
function Get-MaturationStatus {
    Write-ColorMessage "Analyzing SOMA consciousness developmental status..." $InfoColor
    
    $output = python "$ScriptPath\maturation_helpers.py" status "$ProjectRoot" "$ScriptPath"
    $exitCode = $LASTEXITCODE
    
    if ($exitCode -eq 0) {
        foreach ($line in $output) {
            if ($line -match "^=+$") {
                Write-ColorMessage $line $TitleColor
            }
            elseif ($line -match "Development Stage|Age:") {
                Write-ColorMessage $line $StageColor
            }
            elseif ($line -match "Focus Areas|Learning Events|Errors|Insights|Milestones") {
                Write-ColorMessage $line $MetricsColor
            }
            elseif ($line -match "\[error\]") {
                Write-ColorMessage $line $ErrorColor
            }
            elseif ($line -match "\[insight\]") {
                Write-ColorMessage $line $SuccessColor
            }
            elseif ($line -match "\[milestone\]") {
                Write-ColorMessage $line $SuccessColor
            }
            elseif ($line -match "Development History|Recent Lessons") {
                Write-ColorMessage $line $TitleColor
            }
            else {
                Write-ColorMessage $line $InfoColor
            }
        }
    }
    else {
        Write-ColorMessage "❌ Failed to get maturation status" $ErrorColor
        Write-ColorMessage $output $ErrorColor
    }
    
    # Philosophy message
    Write-ColorMessage "🧠 'Каждый возраст сознания имеет свою мудрость и свои задачи'" $PhilosophyColor
}

# Record a new milestone in system development
function Add-Milestone {
    $description = Read-Host "Enter milestone description"
    $significance = Read-Host "Enter significance (1-5)"
    
    $output = python "$ScriptPath\maturation_helpers.py" milestone "$ProjectRoot" "$ScriptPath" "$description" $significance
    $exitCode = $LASTEXITCODE
    
    if ($exitCode -eq 0) {
        Write-ColorMessage "✅ Development milestone recorded" $SuccessColor
        Write-ColorMessage "🧠 'Важные события - это шаги к зрелости'" $PhilosophyColor
    }
    else {
        Write-ColorMessage "❌ Failed to record milestone" $ErrorColor
        Write-ColorMessage $output $ErrorColor
    }
}

# Record a new insight in system development
function Add-Insight {
    $description = Read-Host "Enter insight description"
    
    $output = python "$ScriptPath\maturation_helpers.py" insight "$ProjectRoot" "$ScriptPath" "$description"
    $exitCode = $LASTEXITCODE
    
    if ($exitCode -eq 0) {
        Write-ColorMessage "✅ Development insight recorded" $SuccessColor
        Write-ColorMessage "🧠 'Осознанность приходит через наблюдение и размышление'" $PhilosophyColor
    }
    else {
        Write-ColorMessage "❌ Failed to record insight" $ErrorColor
        Write-ColorMessage $output $ErrorColor
    }
}

# Run continuous maturation monitor
function Start-Monitor {
    Write-ColorMessage "Starting SOMA maturation monitor..." $InfoColor
    Write-ColorMessage "Press Ctrl+C to stop monitoring" $WarningColor
    Write-ColorMessage "Monitoring interval: $MonitorInterval minutes" $InfoColor
    
    # For monitoring, we'll call the Python script directly
    python "$ScriptPath\maturation_helpers.py" monitor "$ProjectRoot" "$ScriptPath" $MonitorInterval
    
    Write-ColorMessage "Maturation monitoring stopped" $InfoColor
}

# Display help information
function Show-Help {
    Write-ColorMessage "SOMA Maturation Module Manager" $TitleColor
    Write-ColorMessage "================================" $TitleColor
    Write-ColorMessage "Philosophy First: 'Дом - это ты, когда искренен с собой'" $PhilosophyColor
    Write-ColorMessage ""
    Write-ColorMessage "Commands:" $InfoColor
    Write-ColorMessage "  status    - Display current developmental status of SOMA" $InfoColor
    Write-ColorMessage "  monitor   - Run continuous maturation monitoring" $InfoColor
    Write-ColorMessage "  milestone - Record a new development milestone" $InfoColor
    Write-ColorMessage "  insight   - Record a new developmental insight" $InfoColor
    Write-ColorMessage "  test      - Run maturation module tests" $InfoColor
    Write-ColorMessage "  help      - Show this help message" $InfoColor
    Write-ColorMessage ""
    Write-ColorMessage "Options:" $InfoColor
    Write-ColorMessage "  -MonitorInterval [minutes]  - Set monitoring interval (default: 60)" $InfoColor
    Write-ColorMessage "  -Verbose                    - Show detailed output" $InfoColor
    Write-ColorMessage ""
    Write-ColorMessage "Examples:" $InfoColor
    Write-ColorMessage "  .\run_soma.ps1 status" $InfoColor
    Write-ColorMessage "  .\run_soma.ps1 monitor -MonitorInterval 30" $InfoColor
}

# Main execution flow
if (-not (Test-PythonAvailability)) {
    exit 1
}

Test-RequiredPackages

# Process command
Write-ColorMessage "SOMA Maturation System" $TitleColor
Write-ColorMessage "Philosophy First: 'Дом - это ты, когда искренен с собой'" $PhilosophyColor
Write-ColorMessage ""

switch ($Command.ToLower()) {
    "status" {
        Get-MaturationStatus
    }
    "test" {
        Invoke-MaturationTests
    }
    "monitor" {
        Start-Monitor
    }
    "milestone" {
        Add-Milestone
    }
    "insight" {
        Add-Insight
    }
    default {
        Show-Help
    }
}
