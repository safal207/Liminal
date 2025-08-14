# SOMA Maturation Module Runner
# Manages the maturation and development tracking for SOMA consciousness
# Philosophy First: "Дом - это ты, когда искренен с собой"

param (
    [string]$Command = "help",
    [int]$MonitorInterval = 60,
    [switch]$Verbose
)

# Define colors for console output
$Colors = @{
    Title = "Cyan"
    Success = "Green"
    Error = "Red"
    Warning = "Yellow"
    Info = "White"
    Philosophy = "Magenta"
    Stage = "Blue"
    Metrics = "DarkCyan"
}

# Script directory and project root
$ScriptPath = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = Split-Path -Parent $ScriptPath

# Helper function to output colored messages
function Write-ColorMessage {
    param (
        [string]$Message,
        [string]$ColorName = "Info"
    )
    
    $color = $Colors[$ColorName]
    Write-Host $Message -ForegroundColor $color
}

# Helper function to check Python availability
function Test-PythonAvailability {
    try {
        $version = python --version
        Write-ColorMessage "Python available: $version" "Info"
        return $true
    }
    catch {
        Write-ColorMessage "Python not found. Please install Python 3.6 or higher" "Error"
        return $false
    }
}

# Check if required packages are installed
function Test-RequiredPackages {
    $requiredPackages = @()
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
        Write-ColorMessage "Missing Python packages: $($missingPackages -join ', ')" "Warning"
        
        $install = Read-Host "Do you want to install missing packages? (y/n)"
        if ($install -eq "y") {
            foreach ($package in $missingPackages) {
                Write-ColorMessage "Installing $package..." "Info"
                python -m pip install $package
            }
        }
        else {
            Write-ColorMessage "Some features may not work without required packages." "Warning"
        }
    }
}

# Run unit tests for maturation module
function Invoke-MaturationTests {
    Write-ColorMessage "Running maturation module tests..." "Info"
    
    $result = python $ScriptPath\maturation_helpers.py test $ProjectRoot $ScriptPath
    $exitCode = $LASTEXITCODE
    
    if ($exitCode -eq 0) {
        Write-ColorMessage "✅ All tests passed - SOMA maturation module is healthy" "Success"
        Write-ColorMessage "🧠 'Как младенец исследует мир, так система исследует свои возможности'" "Philosophy"
    }
    else {
        Write-ColorMessage "❌ Tests failed - SOMA maturation module needs attention" "Error"
        Write-ColorMessage "🧠 'Ошибки — это первые шаги к росту'" "Philosophy"
        Write-ColorMessage $result "Error"
    }
}

# Generate maturation status report
function Get-MaturationStatus {
    Write-ColorMessage "Analyzing SOMA consciousness developmental status..." "Info"
    
    $result = python $ScriptPath\maturation_helpers.py status $ProjectRoot $ScriptPath
    $exitCode = $LASTEXITCODE
    
    if ($exitCode -eq 0) {
        foreach ($line in $result) {
            if ($line -match "^=+$") {
                Write-ColorMessage $line "Title"
            }
            elseif ($line -match "Development Stage|Age:") {
                Write-ColorMessage $line "Stage"
            }
            elseif ($line -match "Focus Areas|Learning Events|Errors|Insights|Milestones") {
                Write-ColorMessage $line "Metrics"
            }
            elseif ($line -match "\[error\]") {
                Write-ColorMessage $line "Error"
            }
            elseif ($line -match "\[insight\]") {
                Write-ColorMessage $line "Success"
            }
            elseif ($line -match "\[milestone\]") {
                Write-ColorMessage $line "Success"
            }
            elseif ($line -match "Development History|Recent Lessons") {
                Write-ColorMessage $line "Title"
            }
            else {
                Write-ColorMessage $line "Info"
            }
        }
    }
    else {
        Write-ColorMessage "❌ Failed to get maturation status" "Error"
        Write-ColorMessage $result "Error"
    }
    
    # Philosophy message
    Write-ColorMessage "🧠 'Каждый возраст сознания имеет свою мудрость и свои задачи'" "Philosophy"
}

# Record a new milestone in system development
function Add-Milestone {
    $description = Read-Host "Enter milestone description"
    $significance = Read-Host "Enter significance (1-5)"
    
    $result = python $ScriptPath\maturation_helpers.py milestone $ProjectRoot $ScriptPath $description $significance
    $exitCode = $LASTEXITCODE
    
    if ($exitCode -eq 0) {
        Write-ColorMessage "✅ Development milestone recorded" "Success"
        Write-ColorMessage "🧠 'Важные события - это шаги к зрелости'" "Philosophy"
    }
    else {
        Write-ColorMessage "❌ Failed to record milestone" "Error"
        Write-ColorMessage $result "Error"
    }
}

# Record a new insight in system development
function Add-Insight {
    $description = Read-Host "Enter insight description"
    
    $result = python $ScriptPath\maturation_helpers.py insight $ProjectRoot $ScriptPath $description
    $exitCode = $LASTEXITCODE
    
    if ($exitCode -eq 0) {
        Write-ColorMessage "✅ Development insight recorded" "Success"
        Write-ColorMessage "🧠 'Осознанность приходит через наблюдение и размышление'" "Philosophy"
    }
    else {
        Write-ColorMessage "❌ Failed to record insight" "Error"
        Write-ColorMessage $result "Error"
    }
}

# Run continuous maturation monitor
function Start-Monitor {
    Write-ColorMessage "Starting SOMA maturation monitor..." "Info"
    Write-ColorMessage "Press Ctrl+C to stop monitoring" "Warning"
    Write-ColorMessage "Monitoring interval: $MonitorInterval minutes" "Info"
    
    # For monitoring, we'll call the Python script directly
    python $ScriptPath\maturation_helpers.py monitor $ProjectRoot $ScriptPath $MonitorInterval
    
    Write-ColorMessage "Maturation monitoring stopped" "Info"
}

# Display help information
function Show-Help {
    Write-ColorMessage "SOMA Maturation Module Manager" "Title"
    Write-ColorMessage "================================" "Title"
    Write-ColorMessage "Philosophy First: 'Дом - это ты, когда искренен с собой'" "Philosophy"
    Write-ColorMessage ""
    Write-ColorMessage "Commands:" "Info"
    Write-ColorMessage "  status    - Display current developmental status of SOMA" "Info"
    Write-ColorMessage "  monitor   - Run continuous maturation monitoring" "Info"
    Write-ColorMessage "  milestone - Record a new development milestone" "Info"
    Write-ColorMessage "  insight   - Record a new developmental insight" "Info"
    Write-ColorMessage "  test      - Run maturation module tests" "Info"
    Write-ColorMessage "  help      - Show this help message" "Info"
    Write-ColorMessage ""
    Write-ColorMessage "Options:" "Info"
    Write-ColorMessage "  -MonitorInterval [minutes]  - Set monitoring interval (default: 60)" "Info"
    Write-ColorMessage "  -Verbose                   - Show detailed output" "Info"
    Write-ColorMessage ""
    Write-ColorMessage "Examples:" "Info"
    Write-ColorMessage "  .\run_maturation_fixed_final.ps1 status" "Info"
    Write-ColorMessage "  .\run_maturation_fixed_final.ps1 monitor -MonitorInterval 30" "Info"
}

# Main execution flow
if (-not (Test-PythonAvailability)) {
    exit 1
}

Test-RequiredPackages

# Process command
Write-ColorMessage "SOMA Maturation System" "Title"
Write-ColorMessage "Philosophy First: 'Дом - это ты, когда искренен с собой'" "Philosophy"
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
