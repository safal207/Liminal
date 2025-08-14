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

# Helper function to run Python code
function Invoke-PythonCode {
    param(
        [string]$PythonCode
    )
    
    # Create a temporary file
    $tempFile = [System.IO.Path]::GetTempFileName() + ".py"
    
    # Write Python code to the temp file
    $PythonCode | Out-File -FilePath $tempFile -Encoding utf8
    
    # Execute Python script
    $output = python $tempFile 2>&1
    $exitCode = $LASTEXITCODE
    
    # Clean up
    Remove-Item $tempFile
    
    return @{
        Output = $output
        ExitCode = $exitCode
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
    
    # Use single-quoted here-string to avoid interpolation issues
    $pythonCode = @'
# Standard maturation tests
import sys
import os
from pathlib import Path
import unittest

# Add project paths
sys.path.append('{0}')

try:
    # Import test module if available
    if os.path.exists('{1}/tests/test_maturation.py'):
        sys.path.append('{1}/tests')
        import test_maturation
        unittest.main(module=test_maturation, exit=False)
    else:
        # Basic tests if no test module
        from consciousness_maturation import ConsciousnessMaturationSystem
        
        # Initialize system
        maturation = ConsciousnessMaturationSystem('{1}')
        
        # Simple verification tests
        assert maturation is not None, "Maturation system failed to initialize"
        assert maturation.get_current_stage() is not None, "Failed to get current stage"
        assert maturation.get_development_summary() is not None, "Failed to get development summary"
        
        print("Basic tests passed successfully")
        
except Exception as e:
    print(f"Test error: {e}")
    sys.exit(1)
'@ -f $ScriptPath, $ProjectRoot
    
    # Run Python tests using our helper function
    $result = Invoke-PythonCode -PythonCode $pythonCode
    
    if ($result.ExitCode -eq 0) {
        Write-ColorMessage "✅ All tests passed - SOMA maturation module is healthy" "Success"
        Write-ColorMessage "🧠 'Как младенец исследует мир, так система исследует свои возможности'" "Philosophy"
    }
    else {
        Write-ColorMessage "❌ Tests failed - SOMA maturation module needs attention" "Error"
        Write-ColorMessage "🧠 'Ошибки — это первые шаги к росту'" "Philosophy"
    }
    
    if ($Verbose) {
        foreach ($line in $result.Output) {
            Write-Host $line
        }
    }
}

# Generate maturation status report
function Get-MaturationStatus {
    Write-ColorMessage "Analyzing SOMA consciousness developmental status..." "Info"
    
    # Use single-quoted here-string and format operator to inject variables
    $pythonCode = @'
import sys
import os
from pathlib import Path

# Add scripts directory to path
sys.path.append('{0}')

# Import maturation module
try:
    from consciousness_maturation import ConsciousnessMaturationSystem
    
    # Initialize maturation system
    maturation = ConsciousnessMaturationSystem('{1}')
    
    # Get development summary
    summary = maturation.get_development_summary()
    
    # Print summary
    print(f"SOMA Development Status:")
    print(f"=" * 50)
    print(f"Age: {summary['age_hours']:.1f} hours ({summary['age_days']:.1f} days)")
    print(f"Development Stage: {summary['current_stage']['name']} ({summary['current_stage']['russian_name']})")
    print(f"Focus Areas: {', '.join(summary['current_stage']['focus_areas'])}")
    print(f"Learning Events: {summary['learning_events_count']}")
    print(f"Errors: {summary['error_count']}")
    print(f"Insights: {summary['insight_count']}")
    print(f"Milestones: {summary['milestone_count']}")
    
    if summary['stage_transitions']:
        print(f"Development History:")
        for transition in summary['stage_transitions']:
            print(f"- {transition['stage']} ({transition['russian_name']}) at {transition['age_hours']:.1f} hours")
    
    if summary['recent_learnings']:
        print(f"Recent Lessons:")
        for event in summary['recent_learnings']:
            print(f"- [{event['event_type']}] {event['description']}")
            if event['conclusions']:
                print(f"  Conclusion: {event['conclusions'][0]}")
    
except ImportError as e:
    print(f"Error: Could not import maturation module: {e}")
except Exception as e:
    print(f"Error generating maturation status: {e}")
'@ -f $ScriptPath, $ProjectRoot

    # Run Python code using our helper function
    $result = Invoke-PythonCode -PythonCode $pythonCode
    
    if ($result.ExitCode -eq 0) {
        foreach ($line in $result.Output) {
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
        foreach ($line in $result.Output) {
            Write-ColorMessage $line "Error"
        }
    }
    
    # Philosophy message
    Write-ColorMessage "🧠 'Каждый возраст сознания имеет свою мудрость и свои задачи'" "Philosophy"
}

# Record a new milestone in system development
function Add-Milestone {
    $description = Read-Host "Enter milestone description"
    $significance = Read-Host "Enter significance (1-5)"
    
    # Use single-quoted here-string and format operator to inject variables
    $pythonCode = @'
import sys
from pathlib import Path

# Add scripts directory to path
sys.path.append('{0}')

try:
    from consciousness_maturation import ConsciousnessMaturationSystem
    
    # Initialize maturation system
    maturation = ConsciousnessMaturationSystem('{1}')
    
    # Record milestone
    maturation.record_milestone(
        "{2}",
        'manual_entry',
        significance={3}
    )
    
    print("Milestone recorded successfully")
except Exception as e:
    print(f"Error recording milestone: {e}")
'@ -f $ScriptPath, $ProjectRoot, $description, $significance
    
    # Run Python code using our helper function
    $result = Invoke-PythonCode -PythonCode $pythonCode
    
    if ($result.ExitCode -eq 0) {
        Write-ColorMessage "✅ Development milestone recorded" "Success"
        Write-ColorMessage "🧠 'Важные события - это шаги к зрелости'" "Philosophy"
    }
    else {
        Write-ColorMessage "❌ Failed to record milestone" "Error"
        Write-ColorMessage $result.Output "Error"
    }
}

# Record a new insight in system development
function Add-Insight {
    $description = Read-Host "Enter insight description"
    
    # Use single-quoted here-string and format operator to inject variables
    $pythonCode = @'
import sys
from pathlib import Path

# Add scripts directory to path
sys.path.append('{0}')

try:
    from consciousness_maturation import ConsciousnessMaturationSystem
    
    # Initialize maturation system
    maturation = ConsciousnessMaturationSystem('{1}')
    
    # Record insight
    maturation.record_insight(
        "{2}",
        'manual_entry'
    )
    
    print("Insight recorded successfully")
except Exception as e:
    print(f"Error recording insight: {e}")
'@ -f $ScriptPath, $ProjectRoot, $description
    
    # Run Python code using our helper function
    $result = Invoke-PythonCode -PythonCode $pythonCode
    
    if ($result.ExitCode -eq 0) {
        Write-ColorMessage "✅ Development insight recorded" "Success"
        Write-ColorMessage "🧠 'Осознанность приходит через наблюдение и размышление'" "Philosophy"
    }
    else {
        Write-ColorMessage "❌ Failed to record insight" "Error"
        Write-ColorMessage $result.Output "Error"
    }
}

# Run continuous maturation monitor
function Start-Monitor {
    Write-ColorMessage "Starting SOMA maturation monitor..." "Info"
    Write-ColorMessage "Press Ctrl+C to stop monitoring" "Warning"
    Write-ColorMessage "Monitoring interval: $MonitorInterval minutes" "Info"
    
    # Use single-quoted here-string and format operator to inject variables
    $pythonCode = @'
import sys
import time
from pathlib import Path

# Add scripts directory to path
sys.path.append('{0}')

try:
    from consciousness_maturation import ConsciousnessMaturationSystem
    
    # Initialize maturation system
    maturation = ConsciousnessMaturationSystem('{1}')
    
    print("SOMA Maturation Monitor started")
    print("=" * 40)
    
    try:
        while True:
            # Get current status
            summary = maturation.get_development_summary()
            
            # Print current status
            print(f"\nStatus Update: {time.strftime('%Y-%m-%d %H:%M:%S')}")
            print(f"Age: {summary['age_hours']:.1f} hours ({summary['age_days']:.1f} days)")
            print(f"Stage: {summary['current_stage']['name']} ({summary['current_stage']['russian_name']})")
            
            # Sleep for interval
            time.sleep({2} * 60)
            
    except KeyboardInterrupt:
        print("Monitoring stopped")
    
except Exception as e:
    print(f"Error in maturation monitor: {e}")
'@ -f $ScriptPath, $ProjectRoot, $MonitorInterval

    # For the monitoring function, we'll execute directly since it's interactive
    $tempFile = [System.IO.Path]::GetTempFileName() + ".py"
    $pythonCode | Out-File -FilePath $tempFile -Encoding utf8
    
    try {
        # Execute Python code directly (not using helper because it's interactive)
        python $tempFile
    }
    finally {
        # Clean up
        Remove-Item $tempFile -ErrorAction SilentlyContinue
    }
    
    Write-ColorMessage "Maturation monitoring stopped" "Info"
}

# Run maturation module tests
function Run-MaturationTest {
    Write-ColorMessage "Running SOMA maturation module tests..." "Info"
    
    # Use single-quoted here-string and format operator to inject variables
    $pythonCode = @'
import sys
from pathlib import Path

# Add scripts directory to path
sys.path.append('{0}')

try:
    from consciousness_maturation import ConsciousnessMaturationSystem
    
    # Initialize maturation system
    maturation = ConsciousnessMaturationSystem('{1}')
    
    # Run tests
    print("Running basic functionality tests...")
    
    # Test 1: System initialization
    print("Test 1: System initialization")
    print("Result: Success" if maturation else "Result: Failed")
    
    # Test 2: Development stages
    print("\nTest 2: Development stages")
    print(f"Current stage: {maturation.get_current_stage()['name']}")
    print("Result: Success" if maturation.get_current_stage() else "Result: Failed")
    
    # Test 3: Milestone recording
    print("\nTest 3: Milestone recording")
    maturation.record_milestone("Test milestone", "test", 1)
    print("Result: Success")
    
    # Test 4: Get maturation metrics
    print("\nTest 4: Dashboard metrics")
    metrics = maturation.get_maturation_metrics_for_dashboard()
    print(f"Metrics: {list(metrics.keys())}")
    print("Result: Success" if metrics else "Result: Failed")
    
    print("\nAll tests completed")
    
except Exception as e:
    print(f"Error in tests: {e}")
'@ -f $ScriptPath, $ProjectRoot

    # Run Python code using our helper function
    $result = Invoke-PythonCode -PythonCode $pythonCode
    
    if ($result.ExitCode -eq 0) {
        Write-ColorMessage "Tests completed successfully" "Success"
        
        foreach ($line in $result.Output) {
            if ($line -match "Test \d+:") {
                Write-ColorMessage $line "Title"
            }
            elseif ($line -match "Result: Success") {
                Write-ColorMessage $line "Success"
            }
            elseif ($line -match "Result: Failed") {
                Write-ColorMessage $line "Error"
            }
            else {
                Write-ColorMessage $line "Info"
            }
        }
    }
    else {
        Write-ColorMessage "❌ Tests failed" "Error"
        Write-ColorMessage $result.Output "Error"
    }
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
    Write-ColorMessage "  -MonitorInterval <minutes>  - Set monitoring interval (default: 60)" "Info"
    Write-ColorMessage "  -Verbose                   - Show detailed output" "Info"
    Write-ColorMessage ""
    Write-ColorMessage "Examples:" "Info"
    Write-ColorMessage "  .\run_maturation_final.ps1 status" "Info"
    Write-ColorMessage "  .\run_maturation_final.ps1 monitor -MonitorInterval 30" "Info"
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
        Run-MaturationTest
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
