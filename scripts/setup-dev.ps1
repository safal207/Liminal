Param(
  [switch]$NoInstall
)

$ErrorActionPreference = 'Stop'

Write-Host "Setting up developer environment..."

if (-not $NoInstall) {
  Write-Host "Upgrading pip..."
  python -m pip install --upgrade pip

  if (Test-Path -Path "requirements.txt") {
    Write-Host "Installing requirements.txt..."
    pip install -r requirements.txt
  }
  if (Test-Path -Path "requirements-dev.txt") {
    Write-Host "Installing requirements-dev.txt..."
    pip install -r requirements-dev.txt
  }
}

function Show-Version($name, $cmd) {
  try {
    $ver = Invoke-Expression $cmd
    Write-Host ("{0}: {1}" -f $name, $ver)
  } catch {
    Write-Host ("{0}: not found" -f $name)
  }
}

Write-Host "Tool versions (quality pulse):"
Show-Version "python" "python --version"
Show-Version "pip" "python -m pip --version"
Show-Version "black" "python -m black --version"
Show-Version "isort" "python -m isort --version-number"
Show-Version "flake8" "python -m flake8 --version"
Show-Version "mypy" "python -m mypy --version"
Show-Version "pytest" "python -m pytest --version"

Write-Host "Done. You can now run:"
Write-Host "  scripts\\quality-check.ps1    # lint + types"
Write-Host "  python -m pytest -q          # unit tests"
