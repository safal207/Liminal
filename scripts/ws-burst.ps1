# ASCII-only PowerShell wrapper for Artillery WS burst test
param(
  [Parameter(Mandatory=$true)][string]$Token,
  [string]$Target = "ws://127.0.0.1:8000",
  [string]$Path = "/ws/timeline",
  [int]$Duration = 10,
  [int]$Arrival = 5,
  [int]$Messages = 20,
  [double]$Think = 0.01,
  [switch]$ShowYaml
)

$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest

Write-Host "--- WS burst wrapper ---" -ForegroundColor Cyan

# 0) Check Artillery availability
if (-not (Get-Command artillery -ErrorAction SilentlyContinue)) {
  Write-Host "[error] Artillery not found. Install with:" -ForegroundColor Red
  Write-Host "       npm i -g artillery@2" -ForegroundColor Yellow
  exit 1
}

# 1) Read template
$templatePath = Join-Path $PSScriptRoot "artillery\ws-burst.yml"
if (-not (Test-Path $templatePath)) {
  throw "Template not found: $templatePath"
}
$yaml = Get-Content -Path $templatePath -Raw

# 2) Replace placeholders
$yaml = $yaml.Replace("__TARGET__", $Target)
$yaml = $yaml.Replace("__PATH__", $Path)
$yaml = $yaml.Replace("__TOKEN__", $Token)
$yaml = $yaml.Replace("__DURATION__", "$Duration")
$yaml = $yaml.Replace("__ARRIVAL__", "$Arrival")
$yaml = $yaml.Replace("__MESSAGES__", "$Messages")
$yaml = $yaml.Replace("__THINK__", "$Think")

# 3) Write temp scenario
$tmp = [IO.Path]::Combine([IO.Path]::GetTempPath(), "ws-burst-" + [Guid]::NewGuid().ToString() + ".yml")
Set-Content -Path $tmp -Value $yaml -Encoding ASCII

if ($ShowYaml) {
  Write-Host "--- scenario preview ---" -ForegroundColor Gray
  Get-Content $tmp | ForEach-Object { Write-Host $_ }
}

# 4) Run Artillery
Write-Host "[run] artillery run $tmp" -ForegroundColor Yellow
$proc = Start-Process -FilePath "artillery" -ArgumentList @("run", $tmp) -NoNewWindow -PassThru -Wait

if ($proc.ExitCode -ne 0) {
  Write-Host ("[fail] artillery exited with code {0}" -f $proc.ExitCode) -ForegroundColor Red
  exit $proc.ExitCode
}

Write-Host "[ok] burst completed" -ForegroundColor Green
