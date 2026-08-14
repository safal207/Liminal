#!/usr/bin/env pwsh
# =============================================================================
# Resonance Liminal - Оптимизированный скрипт запуска с кешированием
# =============================================================================

param(
    [string]$Mode = "demo",  # demo или production
    [bool]$RunTests = $false,
    [bool]$OpenBrowser = $true
)

$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue" # Ускоряет PowerShell в Docker

# ASCII Art баннер в стиле Liminal Resonance
Write-Host "=================================" -ForegroundColor Cyan
Write-Host "                                 " -ForegroundColor Cyan
Write-Host "    Advanced AI Orchestration    " -ForegroundColor Cyan
Write-Host "=================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Mode: $Mode" -ForegroundColor White
Write-Host "Run Tests: $RunTests" -ForegroundColor White
Write-Host "Open Browser: $OpenBrowser" -ForegroundColor White
Write-Host ""

# Проверка наличия Docker
try {
    docker --version | Out-Null
    Write-Host "✅ Docker установлен" -ForegroundColor Green
} catch {
    Write-Host "❌ Docker не установлен. Пожалуйста, установите Docker и повторите попытку." -ForegroundColor Red
    exit 1
}

# Проверка наличия docker-compose
try {
    docker-compose --version | Out-Null
    Write-Host "✅ Docker Compose установлен" -ForegroundColor Green
} catch {
    Write-Host "❌ Docker Compose не установлен. Пожалуйста, установите Docker Compose и повторите попытку." -ForegroundColor Red
    exit 1
}

# Проверка и создание .env файла если отсутствует
if (-not (Test-Path ".env")) {
    Write-Host "⚠️ Файл .env не найден, создаю демо-версию..." -ForegroundColor Yellow
    
    if (Test-Path ".env.example") {
        Copy-Item ".env.example" ".env"
        Write-Host "✅ Создан .env из примера" -ForegroundColor Green
    } else {
        @"
# Демо окружение для Resonance Liminal
OPENAI_API_KEY=
ANTHROPIC_API_KEY=
MULTI_LLM_MODE=demo
XAI_CACHE_SIZE=1000
XAI_ENABLE_SHAP=true
XAI_ENABLE_LIME=true
"@ | Out-File -FilePath ".env" -Encoding utf8
        Write-Host "✅ Создан минимальный .env файл" -ForegroundColor Green
    }
}

# Запуск в демо-режиме или production
if ($Mode -eq "demo") {
    Write-Host "🚀 Запуск системы в демо-режиме..." -ForegroundColor Cyan
    $env:MULTI_LLM_MODE = "demo"
} else {
    Write-Host "🚀 Запуск системы в production-режиме..." -ForegroundColor Cyan
    
    # Проверка API ключей
    if (-not $env:OPENAI_API_KEY -or $env:OPENAI_API_KEY -eq "demo_key") {
        Write-Host "⚠️ OPENAI_API_KEY не установлен или демо. Проверьте .env файл." -ForegroundColor Yellow
    }
    
    if (-not $env:ANTHROPIC_API_KEY -or $env:ANTHROPIC_API_KEY -eq "demo_key") {
        Write-Host "⚠️ ANTHROPIC_API_KEY не установлен или демо. Проверьте .env файл." -ForegroundColor Yellow
    }
}

# Запуск с оптимизированным Docker Compose
try {
    Write-Host "🔄 Запуск контейнеров с кешированием зависимостей..." -ForegroundColor Cyan
    docker-compose -f docker-compose.fast.yml up -d
    
    if ($LASTEXITCODE -ne 0) {
        throw "Ошибка при запуске Docker Compose"
    }
    
    Write-Host "✅ Контейнеры запущены успешно" -ForegroundColor Green
} catch {
    Write-Host "❌ Ошибка при запуске контейнеров: $_" -ForegroundColor Red
    exit 1
}

# Ожидание готовности сервисов
Write-Host "⏳ Ожидание готовности сервисов..." -ForegroundColor Cyan
$maxRetries = 30
$retryCount = 0
$allServicesReady = $false

while (-not $allServicesReady -and $retryCount -lt $maxRetries) {
    try {
        $healthResponse = Invoke-RestMethod -Uri "http://localhost:8000/health" -TimeoutSec 2
        
        if ($healthResponse.status -eq "healthy") {
            $allServicesReady = $true
            Write-Host "✅ Все сервисы готовы" -ForegroundColor Green
            break
        }
    } catch {
        # Продолжаем ожидание
    }
    
    $retryCount++
    Write-Host "⏳ Ожидание сервисов: попытка $retryCount из $maxRetries" -ForegroundColor Yellow
    Start-Sleep -Seconds 2
}

if (-not $allServicesReady) {
    Write-Host "⚠️ Превышено время ожидания готовности сервисов, но попробуем продолжить..." -ForegroundColor Yellow
}

# Запуск тестов если требуется
if ($RunTests) {
    Write-Host "🧪 Запуск тестов..." -ForegroundColor Cyan
    
    try {
        Write-Host "🔍 Запуск минимальных тестов..." -ForegroundColor White
        & "$PSScriptRoot\test-minimal.ps1"
        
        Write-Host "✅ Минимальные тесты пройдены" -ForegroundColor Green
    } catch {
        Write-Host "❌ Ошибка при запуске тестов: $_" -ForegroundColor Red
    }
}

# Открытие браузера если требуется
if ($OpenBrowser) {
    Write-Host "🌐 Открытие браузера..." -ForegroundColor Cyan
    Start-Process "http://localhost:8000/docs"
}

# Финальное сообщение
Write-Host ""
Write-Host "=================================" -ForegroundColor Cyan
Write-Host "    Multi-LLM System Operational " -ForegroundColor Green
Write-Host "=================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "📚 API Documentation: http://localhost:8000/docs" -ForegroundColor White
Write-Host "🏥 Health Check: http://localhost:8000/health" -ForegroundColor White
Write-Host "📊 ML Status: http://localhost:8000/ml/status" -ForegroundColor White
Write-Host ""
Write-Host "💡 Запустите тесты:" -ForegroundColor Cyan
Write-Host "   Minimal Test:     powershell -File scripts/test-minimal.ps1" -ForegroundColor White
Write-Host "   API Endpoints:    python scripts/test-api-endpoints.py" -ForegroundColor White
Write-Host "   Detailed Test:    python scripts/detailed-test.py" -ForegroundColor White
Write-Host "   Load Test:        python scripts/run-load-tests.py" -ForegroundColor White
Write-Host ""
Write-Host "🛑 Для остановки системы:" -ForegroundColor Cyan
Write-Host "   docker-compose -f docker-compose.fast.yml down" -ForegroundColor White
Write-Host ""

# Использование философских концепций из книжных материалов LIMINAL
Write-Host "🏠 'Дом - это не место. Дом - это ты, когда ты искренен с собой.'" -ForegroundColor Magenta
Write-Host "   Резонанс начинается с присутствия здесь и сейчас." -ForegroundColor Magenta
