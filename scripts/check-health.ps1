<#
.SYNOPSIS
  Быстрая проверка эндпоинтов /health и /ready.

.DESCRIPTION
  Пингует API по адресам /health и /ready, печатает человеко‑читаемое резюме
  и JSON‑ответы. Опционально может завершаться с ошибкой, если ready:false.

.PARAMETER BaseUrl
  Базовый URL API (по умолчанию http://127.0.0.1:8000)

.PARAMETER Retries
  Количество попыток при временных сбоях (по умолчанию 10)

.PARAMETER DelaySec
  Пауза между попытками в секундах (по умолчанию 2)

.PARAMETER FailOnNotReady
  Если указан, завершает скрипт с кодом 2, если готовность не достигнута.

.EXAMPLE
  ./scripts/check-health.ps1

.EXAMPLE
  ./scripts/check-health.ps1 -BaseUrl http://127.0.0.1:8080 -Retries 20 -DelaySec 3 -FailOnNotReady
#>
param(
  [string]$BaseUrl = "http://127.0.0.1:8000",
  [int]$Retries = 10,
  [int]$DelaySec = 2,
  [switch]$FailOnNotReady
)

$ErrorActionPreference = 'Stop'

function Invoke-Json {
  param(
    [Parameter(Mandatory=$true)][string]$Url
  )
  try {
    $resp = Invoke-RestMethod -Uri $Url -TimeoutSec 5
    return @{ ok = $true; data = $resp }
  } catch {
    return @{ ok = $false; error = $_.Exception.Message }
  }
}

Write-Host ("Checking API at {0} (health/ready)" -f $BaseUrl) -ForegroundColor Cyan

$healthUrl = ("{0}/health" -f $BaseUrl)
$readyUrl  = ("{0}/ready"  -f $BaseUrl)

$attempt = 0
$health = $null
$ready = $null

while ($attempt -lt $Retries) {
  $attempt++
  $health = Invoke-Json -Url $healthUrl
  $ready  = Invoke-Json -Url $readyUrl

  if ($health.ok -and $ready.ok) {
    break
  }
  Start-Sleep -Seconds $DelaySec
}

Write-Host "\n— /health —" -ForegroundColor Yellow
if ($health.ok) {
  $health.data | ConvertTo-Json -Depth 10
} else {
  Write-Host ("failed: {0}" -f $health.error) -ForegroundColor Red
}

Write-Host "\n— /ready —" -ForegroundColor Yellow
if ($ready.ok) {
  $ready.data | ConvertTo-Json -Depth 10
} else {
  Write-Host ("failed: {0}" -f $ready.error) -ForegroundColor Red
}

$readyFlag = $false
if ($ready.ok -and $null -ne $ready.data.ready) {
  $readyFlag = [bool]$ready.data.ready
}

Write-Host "\nSummary:" -ForegroundColor Green
Write-Host ("health: {0}" -f ($(if($health.ok){'ok'}else{'fail'})))
Write-Host ("ready:  {0}" -f ($(if($ready.ok){$readyFlag}else{'fail'})))

if ($FailOnNotReady -and (-not $readyFlag)) {
  Write-Error "API is not ready (ready:false)"
  exit 2
}

exit 0
