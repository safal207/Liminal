$ErrorActionPreference = 'Stop'

Write-Host "Installing pre-commit hooks..."
python -m pip install --upgrade pip | Out-Null
if (Test-Path -Path "requirements-dev.txt") {
  pip install -r requirements-dev.txt | Out-Null
}

pre-commit install
# Optional: update hook versions in local environment
# pre-commit autoupdate

Write-Host "Done. Hooks are active. Each commit will run black/isort/flake8/mypy automatically."
