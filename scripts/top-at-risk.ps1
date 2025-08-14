param(
    [switch]$Server,
    [switch]$Open,
    [switch]$Cli,
    [int]$Limit = 5,
    [Nullable[Double]]$Threshold,
    [int]$Port = 8000,
    [string]$GraphQLEndpoint
)

$ErrorActionPreference = 'Stop'

function Show-Help {
    Write-Host "Top At-Risk helper" -ForegroundColor Cyan
    Write-Host "";
    Write-Host "Usage:" -ForegroundColor Yellow
    Write-Host "  .\scripts\top-at-risk.ps1 -Server -Open -Limit 5 -Port 8000"
    Write-Host "  .\scripts\top-at-risk.ps1 -Cli -Limit 5 [-Threshold 0.35] [-GraphQLEndpoint http://127.0.0.1:8000/graphql]"
    Write-Host "";
    Write-Host "Switches:" -ForegroundColor Yellow
    Write-Host "  -Server     Запустить мини-сервер (uvicorn liminal.at_risk_server:app)"
    Write-Host "  -Open       Открыть страницу /at-risk в браузере"
    Write-Host "  -Cli        Выполнить CLI (scripts/top_at_risk.py)"
    Write-Host "  -Limit      Сколько пар показать (по умолчанию 5)"
    Write-Host "  -Threshold  Порог здоровья для советов (например, 0.35)"
    Write-Host "  -Port       Порт сервера (по умолчанию 8000)"
    Write-Host "  -GraphQLEndpoint  URL GraphQL (для CLI HTTP-режима)"
}

if (-not $PSBoundParameters.Keys.Count) {
    Show-Help
    return
}

$repoRoot = Resolve-Path (Join-Path $PSScriptRoot '..') | Select-Object -ExpandProperty Path
$srcPath = Join-Path $repoRoot 'src'

function Start-AtRiskServer {
    param([int]$Port)
    Write-Host "▶️  Запуск сервера на порту $Port..." -ForegroundColor Green
    $cmd = "$env:PYTHONPATH='$srcPath'; python -m uvicorn liminal.at_risk_server:app --reload --port $Port"
    Start-Process -FilePath "powershell" -ArgumentList "-NoProfile","-NoExit","-Command", $cmd -WorkingDirectory $repoRoot | Out-Null
}

function Wait-Server {
    param([int]$Port,[int]$TimeoutSec = 15)
    $url = "http://127.0.0.1:$Port/at-risk"
    $deadline = (Get-Date).AddSeconds($TimeoutSec)
    while ((Get-Date) -lt $deadline) {
        try {
            # Using -UseBasicParsing for older PS compatibility
            Invoke-WebRequest -Uri $url -UseBasicParsing -Method GET -TimeoutSec 3 | Out-Null
            return $true
        } catch {
            Start-Sleep -Milliseconds 500
        }
    }
    return $false
}

function Open-AtRiskPage {
    param([int]$Port,[int]$Limit)
    $url = "http://127.0.0.1:$Port/at-risk?limit=$Limit"
    Write-Host "🌐 Открываю: $url" -ForegroundColor Cyan
    Start-Process $url | Out-Null
}

function Run-TopCli {
    param([int]$Limit,[Nullable[Double]]$Threshold,[string]$GraphQLEndpoint,[int]$Port)
    Push-Location $repoRoot
    try {
        if (-not $GraphQLEndpoint) { $GraphQLEndpoint = "http://127.0.0.1:$Port/graphql" }
        $env:LIMINAL_GRAPHQL_URL = $GraphQLEndpoint
        $args = @('scripts/top_at_risk.py','-l',"$Limit")
        if ($Threshold -ne $null) { $args += @('-t', ("{0}" -f $Threshold)) }
        Write-Host "🧪 CLI: python $($args -join ' ') (LIMINAL_GRAPHQL_URL=$GraphQLEndpoint)" -ForegroundColor Yellow
        python @args
    }
    finally {
        Pop-Location
    }
}

if ($Server) { Start-AtRiskServer -Port $Port }
if ($Server -or $Open -or $Cli) {
    if (-not (Wait-Server -Port $Port -TimeoutSec 20)) {
        Write-Warning "Сервер не ответил за отведённое время. Проверь логи окна сервера."
    }
}
if ($Open)   { Open-AtRiskPage -Port $Port -Limit $Limit }
if ($Cli)    { Run-TopCli -Limit $Limit -Threshold $Threshold -GraphQLEndpoint $GraphQLEndpoint -Port $Port }
