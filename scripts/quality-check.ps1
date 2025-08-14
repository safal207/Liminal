Param(
  [switch]$Fix
)

$ErrorActionPreference = 'Stop'

# Find the project root by going up one level from the script's directory
$projectRoot = Resolve-Path -Path (Join-Path $PSScriptRoot "..")

# Define the path to the virtual environment's Python executable
$pythonExecutable = Join-Path $projectRoot ".venv\Scripts\python.exe"

# Check if the Python executable exists
if (-not (Test-Path $pythonExecutable)) {
    Write-Error "Python executable not found in virtual environment: $pythonExecutable"
    Write-Error "Please run the setup script to create the virtual environment."
    exit 1
}

if ($Fix) {
  Write-Host "Formatting and imports..."
  & $pythonExecutable -m black .
  & $pythonExecutable -m isort --profile black .
} else {
  Write-Host "Checking format and imports (no changes)..."
  & $pythonExecutable -m black --check .
  & $pythonExecutable -m isort --check-only --profile black .
}

Write-Host "Lint: flake8"
& $pythonExecutable -m flake8

Write-Host "Type check: mypy"
& $pythonExecutable -m mypy src tests

Write-Host "Done. Quality pulse measured."
