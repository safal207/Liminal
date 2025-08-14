# TURBO ML BUILD SCRIPT для Resonance Liminal
# Оптимизированный скрипт для быстрой сборки и запуска ML-инфраструктуры

# Параметры скрипта
param(
    [switch]$CleanCache,    # Очистить кеши Docker перед сборкой
    [switch]$StartAfterBuild, # Запустить систему после сборки
    [switch]$TestAfterStart, # Запустить тесты после запуска
    [switch]$Production      # Использовать productionв настройки
)

# Устанавливаем значения по умолчанию для параметров
$StartAfterBuild = $StartAfterBuild.IsPresent -or (-not $PSBoundParameters.ContainsKey('StartAfterBuild'))
$CleanCache = $CleanCache.IsPresent
$TestAfterStart = $TestAfterStart.IsPresent
$Production = $Production.IsPresent

# Цвета для вывода
$Green = [System.ConsoleColor]::Green
$Cyan = [System.ConsoleColor]::Cyan
$Yellow = [System.ConsoleColor]::Yellow
$Red = [System.ConsoleColor]::Red

# Функция для вывода статуса
function Write-Status($message, $color = $Cyan) {
    Write-Host $message -ForegroundColor $color
}

# Проверяем наличие Docker
try {
    $dockerVersion = docker --version
    Write-Status "✅ Docker обнаружен: $dockerVersion" $Green
} catch {
    Write-Status "❌ Docker не установлен! Пожалуйста, установите Docker Desktop." $Red
    exit 1
}

# Проверяем наличие docker-compose
try {
    $composeVersion = docker-compose --version
    Write-Status "✅ Docker Compose обнаружен: $composeVersion" $Green
} catch {
    Write-Status "❌ Docker Compose не установлен!" $Red
    exit 1
}

# Основная директория проекта
$projectDir = Split-Path -Parent (Split-Path -Parent $MyInvocation.MyCommand.Path)
Set-Location $projectDir
Write-Status "📂 Директория проекта: $projectDir"

# Настраиваем Docker BuildKit для ускорения сборки
$env:DOCKER_BUILDKIT = 1
$env:COMPOSE_DOCKER_CLI_BUILD = 1
Write-Status "🚀 BuildKit активирован для ускорения сборки" $Green

# Проверяем наличие .env файла
if (-not (Test-Path ".env")) {
    Write-Status "📄 Создаем .env файл с настройками по умолчанию..."
    @"
OPENAI_API_KEY=demo_key
ANTHROPIC_API_KEY=demo_key
XAI_CACHE_SIZE=1000
XAI_ENABLE_SHAP=true
XAI_ENABLE_LIME=true
MULTI_LLM_FALLBACK_ENABLED=true
MULTI_LLM_CONSENSUS_THRESHOLD=0.7
MULTI_LLM_MODE=demo
"@ | Out-File -FilePath ".env" -Encoding utf8
}

# Очистка кеша Docker при необходимости
if ($CleanCache) {
    Write-Status "🧹 Очистка кешей Docker..." $Yellow
    docker builder prune -f
    docker system prune -f
}

# Выбор docker-compose файла в зависимости от режима
$composeFile = if ($Production) { "docker-compose.ml.yml" } else { "docker-compose.ml.yml" }
Write-Status "📋 Используется файл конфигурации: $composeFile" $Cyan

# Сборка образов
Write-Status "🔧 Начинаем сборку Docker образов (оптимизированная)..." $Cyan
$buildCommand = "docker-compose -f $composeFile build --parallel --progress plain"
Write-Status "> $buildCommand" $Yellow
Invoke-Expression $buildCommand

# Проверяем статус сборки
if ($LASTEXITCODE -ne 0) {
    Write-Status "❌ Ошибка при сборке Docker образов!" $Red
    exit 1
}

Write-Status "✅ Сборка Docker образов успешно завершена!" $Green

# Запускаем систему после сборки, если указан флаг
if ($StartAfterBuild) {
    Write-Status "🚀 Запуск системы..." $Cyan
    $startCommand = "docker-compose -f $composeFile up -d"
    Write-Status "> $startCommand" $Yellow
    Invoke-Expression $startCommand
    
    # Проверяем статус запуска
    if ($LASTEXITCODE -ne 0) {
        Write-Status "❌ Ошибка при запуске системы!" $Red
        exit 1
    }
    
    Write-Status "⏳ Ожидаем запуска всех сервисов..." $Yellow
    Start-Sleep -Seconds 10
    
    # Проверяем статус сервисов
    $services = docker-compose -f $composeFile ps --services
    foreach ($service in $services) {
        $status = docker-compose -f $composeFile ps $service | Select-String "Up"
        if ($status) {
            Write-Status "✅ Сервис $service запущен" $Green
        } else {
            Write-Status "⚠️ Сервис $service может не быть готов" $Yellow
        }
    }
    
    # Выводим URL доступа к сервисам
    Write-Status "`n🔗 Доступ к сервисам:" $Cyan
    Write-Status "  • Backend API:  http://localhost:8000" $Green
    Write-Status "  • Prometheus:   http://localhost:9090" $Green
    Write-Status "  • Grafana:      http://localhost:3000" $Green
    Write-Status "  • MinIO:        http://localhost:9001" $Green
    Write-Status "  • Jupyter:      http://localhost:8888" $Green
    
    # Запускаем тесты, если указан флаг
    if ($TestAfterStart) {
        Write-Status "`n🧪 Запуск тестов..." $Cyan
        $testCommand = ".\scripts\run-load-tests.ps1"
        Write-Status "> $testCommand" $Yellow
        Invoke-Expression $testCommand
    }
}

Write-Status "`n🎉 Процесс сборки и запуска завершен!" $Green
Write-Status "   Для остановки системы выполните: docker-compose -f $composeFile down" $Cyan
