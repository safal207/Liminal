param(
  [switch]$ForcePublicIndex = $false,
  [int]$TimeoutSec = 180
)

$ErrorActionPreference = 'Stop'

$logPath = Join-Path -Path (Get-Location) -ChildPath "install_playwright_log.txt"
Start-Transcript -Path $logPath -Append | Out-Null

function Write-Section($title){
  Write-Host "`n==== $title ====\n" -ForegroundColor Cyan
}

function Show-Env(){
  Write-Section "Environment"
  Write-Host "Date: $(Get-Date -Format o)"
  Write-Host "PWD: $(Get-Location)"
  Write-Host "Python: $(python -V 2>$null)"
  Write-Host "Pip: $(python -m pip -V 2>$null)"
  Write-Host "Which python: $(Get-Command python | Select-Object -Expand Source)"
  Write-Host "VENV: $env:VIRTUAL_ENV"
}

function Show-PipConfig(){
  Write-Section "pip config"
  try { python -m pip config list } catch { Write-Warning "pip config list failed: $_" }
  try { python -m pip config debug } catch { Write-Warning "pip config debug failed: $_" }
}

function Show-Proxy(){
  Write-Section "Proxy env"
  $vars = 'HTTP_PROXY','HTTPS_PROXY','PIP_INDEX_URL','PIP_PROXY','NO_PROXY'
  foreach($v in $vars){ Write-Host ("{0}={1}" -f $v, (Get-Item -Path env:$v -ErrorAction SilentlyContinue).Value) }
}

function Check-Network(){
  Write-Section "Network checks"
  $hosts = @('pypi.org','files.pythonhosted.org','cdnjs.cloudflare.com')
  foreach($h in $hosts){
    try {
      $r = Test-NetConnection -ComputerName $h -Port 443 -InformationLevel Quiet -WarningAction SilentlyContinue
      $status = if ($r) { 'OK' } else { 'BLOCKED' }
      Write-Host ("TLS 443 to {0}: {1}" -f $h, $status)
    } catch { Write-Warning "Test-NetConnection $h failed: $_" }
  }
}

function Ensure-Toolchain(){
  Write-Section "Upgrade pip/setuptools/wheel"
  python -m pip install --upgrade pip setuptools wheel --timeout $TimeoutSec --retries 5
}

function Install-Playwright(){
  Write-Section "Install playwright packages"
  $base = @('playwright','pytest-playwright')
  try {
    if($ForcePublicIndex){
      python -m pip install --index-url https://pypi.org/simple --trusted-host pypi.org --trusted-host files.pythonhosted.org --timeout $TimeoutSec --retries 5 @base
    } else {
      python -m pip install --timeout $TimeoutSec --retries 5 @base
    }
  } catch {
    Write-Warning "Direct install failed: $_"
    Write-Host "Retrying with public PyPI and trusted hosts..." -ForegroundColor Yellow
    python -m pip install --index-url https://pypi.org/simple --trusted-host pypi.org --trusted-host files.pythonhosted.org --timeout $TimeoutSec --retries 5 @base
  }
}

function Install-Browsers(){
  Write-Section "Install Playwright browsers"
  python -m playwright install chromium firefox
}

function Verify(){
  Write-Section "Verify"
  $code = @"
import sys
try:
    import playwright
    from playwright.sync_api import sync_playwright
except Exception as e:
    print("IMPORT_FAIL:", e)
    sys.exit(2)
print("IMPORT_OK")
"@
  $temp = [System.IO.Path]::Combine([System.IO.Path]::GetTempPath(), "verify_playwright_" + [System.Guid]::NewGuid().ToString() + ".py")
  Set-Content -Path $temp -Value $code -Encoding UTF8
  try {
    python $temp
  } finally {
    Remove-Item $temp -ErrorAction SilentlyContinue
  }
}

try {
  Show-Env
  Show-PipConfig
  Show-Proxy
  Check-Network
  Ensure-Toolchain
  Install-Playwright
  Verify
  Install-Browsers
  Write-Host "\nSUCCESS: Playwright installed and browsers provisioned." -ForegroundColor Green
  Write-Host "Log: $logPath"
} catch {
  Write-Error "FAILED: $($_.Exception.Message)"
  Write-Host "See log: $logPath" -ForegroundColor Yellow
  exit 1
} finally {
  Stop-Transcript | Out-Null
}
