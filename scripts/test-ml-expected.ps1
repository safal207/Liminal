param(
    [switch]$All
)

$ErrorActionPreference = 'Stop'

# Перейти в корень репозитория (скрипт лежит в /scripts)
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RepoRoot = Split-Path -Parent $ScriptDir
Set-Location $RepoRoot

function Write-Info($msg){ Write-Host $msg -ForegroundColor Cyan }

if ($All) {
  Write-Info "[ML TEST] Running full non-integration suite..."
  python -m pytest -v -m "not integration"
} else {
  Write-Info "[ML TEST] Running multilingual expected_lang tests..."
  python -m pytest -q backend/tests/test_multilingual_expected_lang.py
}
