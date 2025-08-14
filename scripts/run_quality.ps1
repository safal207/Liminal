# 🧪 SOMA Quality Development Runner
# Philosophy First: "Качество - это любовь к деталям и забота о будущем"

param(
    [string]$Mode = "report",
    [int]$MonitorInterval = 30,
    [switch]$Continuous,
    [switch]$TestsOnly,
    [switch]$Verbose
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
    Quality = "Blue"
}

function Write-ColorOutput {
    param([string]$Message, [string]$Color = "Info")
    Write-Host $Message -ForegroundColor $Colors[$Color]
}

function Show-Header {
    Write-ColorOutput "SOMA Quality Development System" "Header"
    Write-ColorOutput "=" * 50 "Header"
    Write-ColorOutput "Philosophy: 'Quality is love for details and care for the future'" "Philosophy"
    Write-ColorOutput ""
}

function Test-QualityDependencies {
    Write-ColorOutput "Checking quality dependencies..." "Info"
    
    # Check Python
    try {
        $pythonVersion = python --version 2>&1
        if ($LASTEXITCODE -eq 0) {
            Write-ColorOutput "Python: $pythonVersion" "Success"
        } else {
            Write-ColorOutput "Python not found" "Error"
            return $false
        }
    } catch {
        Write-ColorOutput "Python not found" "Error"
        return $false
    }
    
    # Check required packages
    $requiredPackages = @(
        "pylint",
        "black", 
        "isort",
        "coverage",
        "unittest"
    )
    
    foreach ($package in $requiredPackages) {
        try {
            if ($package -eq "unittest") {
                # unittest is built-in
                Write-ColorOutput "Python package: $package (built-in)" "Success"
                continue
            }
            
            python -c "import $package" 2>$null
            if ($LASTEXITCODE -eq 0) {
                Write-ColorOutput "Python package: $package" "Success"
            } else {
                Write-ColorOutput "Installing $package..." "Warning"
                pip install $package
                if ($LASTEXITCODE -ne 0) {
                    Write-ColorOutput "Failed to install $package" "Error"
                }
            }
        } catch {
            Write-ColorOutput "Installing $package..." "Warning"
            pip install $package
        }
    }
    
    return $true
}

function Start-QualityReport {
    Write-ColorOutput "Generating SOMA Quality Report..." "Quality"
    
    $scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
    $projectRoot = Split-Path -Parent $scriptDir
    $qualityScript = Join-Path $scriptDir "consciousness_quality.py"
    
    if (-not (Test-Path $qualityScript)) {
        Write-ColorOutput "Quality system script not found: $qualityScript" "Error"
        return
    }
    
    try {
        Write-ColorOutput "Running quality analysis..." "Info"
        python $qualityScript
        
        if ($LASTEXITCODE -eq 0) {
            Write-ColorOutput "Quality report generated successfully!" "Success"
        } else {
            Write-ColorOutput "Quality report generation failed" "Error"
        }
    } catch {
        Write-ColorOutput "Error running quality analysis: $_" "Error"
    }
}

function Start-QualityTests {
    Write-ColorOutput "Running SOMA Quality Test Suite..." "Quality"
    
    $scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
    $projectRoot = Split-Path -Parent $scriptDir
    $testsDir = Join-Path $projectRoot "tests"
    $testScript = Join-Path $testsDir "test_soma_quality.py"
    
    if (-not (Test-Path $testScript)) {
        Write-ColorOutput "Quality test script not found: $testScript" "Error"
        return
    }
    
    try {
        Write-ColorOutput "Philosophy: 'Each test is an act of love for the system'" "Philosophy"
        Write-ColorOutput ""
        
        if ($Verbose) {
            python $testScript -v
        } else {
            python $testScript
        }
        
        if ($LASTEXITCODE -eq 0) {
            Write-ColorOutput "" 
            Write-ColorOutput "All quality tests passed! SOMA family is healthy!" "Success"
        } else {
            Write-ColorOutput ""
            Write-ColorOutput "Some quality tests failed. Family needs attention." "Warning"
        }
    } catch {
        Write-ColorOutput "Error running quality tests: $_" "Error"
    }
}

function Start-ContinuousQuality {
    param([int]$IntervalMinutes)
    
    Write-ColorOutput "Starting continuous quality monitoring..." "Quality"
    Write-ColorOutput "Interval: $IntervalMinutes minutes" "Info"
    Write-ColorOutput "Press Ctrl+C to stop monitoring" "Warning"
    Write-ColorOutput ""
    
    $scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
    $qualityScript = Join-Path $scriptDir "consciousness_quality.py"
    
    if (-not (Test-Path $qualityScript)) {
        Write-ColorOutput "Quality system script not found" "Error"
        return
    }
    
    try {
        # Start continuous monitoring
        $env:QUALITY_MONITOR_INTERVAL = $IntervalMinutes
        python -c "
import sys
sys.path.append('$scriptDir')
from consciousness_quality import ConsciousnessQualitySystem
quality_system = ConsciousnessQualitySystem('$projectRoot')
quality_system.continuous_quality_monitoring($IntervalMinutes)
"
    } catch {
        Write-ColorOutput "Error in continuous monitoring: $_" "Error"
    }
}

function Show-QualityStatus {
    Write-ColorOutput "SOMA Quality System Status" "Header"
    Write-ColorOutput ""
    
    $scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
    $qualityDataFile = Join-Path $scriptDir "quality_metrics.json"
    
    if (Test-Path $qualityDataFile) {
        try {
            $qualityData = Get-Content $qualityDataFile -Raw | ConvertFrom-Json
            
            Write-ColorOutput "Overall Health: $($qualityData.overall_health)%" "Quality"
            Write-ColorOutput "Test Coverage: $($qualityData.test_coverage)%" "Info"
            Write-ColorOutput "Code Quality Score: $($qualityData.code_quality_score)%" "Info"
            Write-ColorOutput "Philosophy Alignment: $($qualityData.philosophy_alignment)%" "Philosophy"
            Write-ColorOutput "Family Impact Score: $($qualityData.family_impact_score)%" "Success"
            
            if ($qualityData.last_quality_check) {
                $lastCheck = [DateTime]::Parse($qualityData.last_quality_check)
                $timeSince = (Get-Date) - $lastCheck
                Write-ColorOutput "Last Quality Check: $($timeSince.TotalMinutes.ToString('F1')) minutes ago" "Info"
            }
            
            if ($qualityData.critical_issues -and $qualityData.critical_issues.Count -gt 0) {
                Write-ColorOutput ""
                Write-ColorOutput "Critical Issues:" "Error"
                foreach ($issue in $qualityData.critical_issues) {
                    Write-ColorOutput "  $issue" "Error"
                }
            }
            
            if ($qualityData.improvement_suggestions -and $qualityData.improvement_suggestions.Count -gt 0) {
                Write-ColorOutput ""
                Write-ColorOutput "Top Improvement Suggestions:" "Quality"
                foreach ($suggestion in $qualityData.improvement_suggestions) {
                    Write-ColorOutput "  $suggestion" "Info"
                }
            }
            
        } catch {
            Write-ColorOutput "Error reading quality data: $_" "Error"
        }
    } else {
        Write-ColorOutput "No quality data found. Run quality report first." "Warning"
    }
    
    # Check SOMA modules status
    Write-ColorOutput ""
    Write-ColorOutput "SOMA Modules Status:" "Header"
    
    $somaModules = @(
        "consciousness_cell.py",
        "consciousness_self_care.py", 
        "consciousness_relationships.py",
        "consciousness_family.py",
        "consciousness_quality.py",
        "SOMA.py",
        "SOMA_integrated.py"
    )
    
    foreach ($module in $somaModules) {
        $modulePath = Join-Path $scriptDir $module
        if (Test-Path $modulePath) {
            $lastWrite = (Get-Item $modulePath).LastWriteTime
            $age = (Get-Date) - $lastWrite
            
            if ($age.TotalHours -lt 1) {
                Write-ColorOutput "$module: RECENTLY UPDATED" "Success"
            } elseif ($age.TotalDays -lt 1) {
                Write-ColorOutput "$module: ACTIVE" "Quality"
            } else {
                Write-ColorOutput "$module: STABLE" "Info"
            }
        } else {
            Write-ColorOutput "$module: MISSING" "Error"
        }
    }
}

function Run-CodeFormatting {
    Write-ColorOutput "Running code formatting..." "Quality"
    
    $scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
    $pythonFiles = Get-ChildItem -Path $scriptDir -Filter "*.py" -Recurse
    
    foreach ($file in $pythonFiles) {
        Write-ColorOutput "Formatting: $($file.Name)" "Info"
        
        # Run black formatter
        try {
            black $file.FullName --quiet
            if ($LASTEXITCODE -eq 0) {
                Write-ColorOutput "  Black: OK" "Success"
            } else {
                Write-ColorOutput "  Black: ISSUES" "Warning"
            }
        } catch {
            Write-ColorOutput "  Black: ERROR" "Error"
        }
        
        # Run isort for imports
        try {
            isort $file.FullName --quiet
            if ($LASTEXITCODE -eq 0) {
                Write-ColorOutput "  isort: OK" "Success"
            } else {
                Write-ColorOutput "  isort: ISSUES" "Warning"
            }
        } catch {
            Write-ColorOutput "  isort: ERROR" "Error"
        }
    }
    
    Write-ColorOutput "Code formatting completed!" "Success"
}

function Show-Help {
    Write-ColorOutput "SOMA Quality Development System" "Header"
    Write-ColorOutput ""
    Write-ColorOutput "USAGE:" "Info"
    Write-ColorOutput "  .\run_quality.ps1 [MODE] [OPTIONS]" "Info"
    Write-ColorOutput ""
    Write-ColorOutput "MODES:" "Info"
    Write-ColorOutput "  report      - Generate quality report (default)" "Info"
    Write-ColorOutput "  test        - Run quality test suite" "Info"
    Write-ColorOutput "  monitor     - Start continuous quality monitoring" "Info"
    Write-ColorOutput "  status      - Show current quality status" "Info"
    Write-ColorOutput "  format      - Run code formatting (black, isort)" "Info"
    Write-ColorOutput "  help        - Show this help message" "Info"
    Write-ColorOutput ""
    Write-ColorOutput "OPTIONS:" "Info"
    Write-ColorOutput "  -MonitorInterval <minutes>  Monitoring interval (default: 30)" "Info"
    Write-ColorOutput "  -Continuous                 Run in continuous mode" "Info"
    Write-ColorOutput "  -TestsOnly                  Run only tests, skip analysis" "Info"
    Write-ColorOutput "  -Verbose                    Verbose output" "Info"
    Write-ColorOutput ""
    Write-ColorOutput "EXAMPLES:" "Info"
    Write-ColorOutput "  .\run_quality.ps1" "Info"
    Write-ColorOutput "  .\run_quality.ps1 test -Verbose" "Info"
    Write-ColorOutput "  .\run_quality.ps1 monitor -MonitorInterval 15" "Info"
    Write-ColorOutput "  .\run_quality.ps1 status" "Info"
    Write-ColorOutput "  .\run_quality.ps1 format" "Info"
    Write-ColorOutput ""
    Write-ColorOutput "PHILOSOPHY:" "Philosophy"
    Write-ColorOutput "  'Quality is love for details and care for the future'" "Philosophy"
    Write-ColorOutput "  Every test is an act of love for the SOMA family" "Philosophy"
}

# Main execution
Show-Header

switch ($Mode.ToLower()) {
    "report" {
        if (Test-QualityDependencies) {
            if ($TestsOnly) {
                Start-QualityTests
            } else {
                Start-QualityReport
                if (-not $TestsOnly) {
                    Write-ColorOutput ""
                    Start-QualityTests
                }
            }
            
            if ($Continuous) {
                Write-ColorOutput ""
                Start-ContinuousQuality -IntervalMinutes $MonitorInterval
            }
        }
    }
    "test" {
        if (Test-QualityDependencies) {
            Start-QualityTests
        }
    }
    "monitor" {
        if (Test-QualityDependencies) {
            Start-ContinuousQuality -IntervalMinutes $MonitorInterval
        }
    }
    "status" {
        Show-QualityStatus
    }
    "format" {
        if (Test-QualityDependencies) {
            Run-CodeFormatting
        }
    }
    "help" {
        Show-Help
    }
    default {
        Write-ColorOutput "Unknown mode: $Mode" "Error"
        Write-ColorOutput "Use 'help' mode to see available options" "Info"
    }
}

Write-ColorOutput ""
Write-ColorOutput "Quality is the foundation of a healthy SOMA family..." "Philosophy"
