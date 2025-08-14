#!/usr/bin/env pwsh
# =============================================================================
# Resonance Liminal - Turbo Build Script
# Ускоренная сборка Docker с оптимизацией кешей и параллельными процессами
# =============================================================================

param(
    [switch]$Clean = $false,  # Полная пересборка с очисткой кешей
    [switch]$FastStart = $false,  # Сразу запустить после сборки
    [string]$Mode = "demo"  # demo или production
)

$ErrorActionPreference = "Stop"
$startTime = Get-Date

# ASCII-баннер
Write-Host "=================================" -ForegroundColor Cyan
Write-Host "    RESONANCE LIMINAL BUILDER    " -ForegroundColor Cyan
Write-Host "=================================" -ForegroundColor Cyan
Write-Host ""

# Проверка Docker
try {
    docker --version | Out-Null
    Write-Host "[OK] Docker найден" -ForegroundColor Green
} catch {
    Write-Host "[ERROR] Docker не установлен!" -ForegroundColor Red
    exit 1
}

# Путь к Docker Compose и Dockerfile
$composeFile = "../docker-compose.fast.yml"
$dockerfilePath = "../backend/Dockerfile.optimized"

# Опции для Docker BuildKit (ускорение сборки)
$env:DOCKER_BUILDKIT = 1
$env:COMPOSE_DOCKER_CLI_BUILD = 1

# Очистка кешей если запрошено
if ($Clean) {
    Write-Host "[INFO] Очистка Docker кешей перед сборкой..." -ForegroundColor Yellow
    docker system prune -f --filter "until=24h"
    Write-Host "[INFO] Кеши очищены" -ForegroundColor Green
}

# Проверка и оптимизация настроек сборки
Write-Host "[INFO] Проверка оптимальных настроек Docker..." -ForegroundColor Cyan
$configPath = "$env:USERPROFILE/.docker/config.json"

if (Test-Path $configPath) {
    $config = Get-Content $configPath -Raw | ConvertFrom-Json
    
    # Проверка включен ли BuildKit
    if (-not ($config.PSObject.Properties.Name -contains "experimental") -or $config.experimental -ne "enabled") {
        Write-Host "[WARN] BuildKit не включен в Docker config. Рекомендуется включить для ускорения сборки." -ForegroundColor Yellow
    } else {
        Write-Host "[OK] BuildKit включен" -ForegroundColor Green
    }
} else {
    Write-Host "[INFO] Docker config не найден. Для максимальной производительности рекомендуется создать $configPath с содержимым: { 'experimental': 'enabled' }" -ForegroundColor Yellow
}

# Запуск Docker Compose с оптимизированными настройками
Write-Host "[BUILD] Запуск ускоренной сборки с BuildKit..." -ForegroundColor Cyan

try {
    # Установка переменных среды для оптимизации
    $env:DOCKER_BUILDKIT = 1
    $env:COMPOSE_DOCKER_CLI_BUILD = 1
    $env:BUILDKIT_PROGRESS = "plain"
    
    # Сборка с оптимальными параметрами
    docker-compose -f docker-compose.fast.yml build --parallel --progress=plain
    
    if ($LASTEXITCODE -ne 0) {
        throw "Ошибка при сборке Docker"
    }
    
    # Расчет времени сборки
    $endTime = Get-Date
    $buildTime = ($endTime - $startTime).TotalSeconds
    Write-Host "[SUCCESS] Сборка завершена за $buildTime секунд" -ForegroundColor Green
    
    # Запуск если затребован
    if ($FastStart) {
        Write-Host "[STARTING] Запуск системы..." -ForegroundColor Cyan
        & "$PSScriptRoot\fast-start.ps1" -Mode $Mode
    } else {
        Write-Host ""
        Write-Host "Для запуска системы выполните:" -ForegroundColor White
        Write-Host "  ./scripts/fast-start.ps1 -Mode $Mode" -ForegroundColor White
        Write-Host ""
    }
} catch {
    Write-Host "[ERROR] Ошибка при сборке: $_" -ForegroundColor Red
    exit 1
}

# Для проектов LIMINAL: добавление философской цитаты
Write-Host ""
Write-Host "Home is not a place. Home is you when you are sincere with yourself." -ForegroundColor Magenta
Write-Host "</> Философия + Код = LIMINAL" -ForegroundColor Magenta
