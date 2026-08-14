# ML-INFRASTRUCTURE START SCRIPT для Resonance Liminal
# Быстрый запуск ML-инфраструктуры с оптимизированными настройками

param(
    [switch]$Demo,          # Запуск в демо-режиме с упрощенными настройками
    [switch]$RunTests,      # Запустить тесты после запуска
    [switch]$OpenDashboard  # Открыть Grafana дашборд после запуска
)

# Устанавливаем значения по умолчанию
$Demo = $Demo.IsPresent
$RunTests = $RunTests.IsPresent
$OpenDashboard = $OpenDashboard.IsPresent

# Цвета для вывода
$Green = [System.ConsoleColor]::Green
$Cyan = [System.ConsoleColor]::Cyan
$Yellow = [System.ConsoleColor]::Yellow
$Red = [System.ConsoleColor]::Red

# Функция для вывода статуса
function Write-Status($message, $color = $Cyan) {
    Write-Host $message -ForegroundColor $color
}

# Переходим в корневую директорию проекта
$projectDir = Split-Path -Parent (Split-Path -Parent $MyInvocation.MyCommand.Path)
Set-Location $projectDir
Write-Status "📂 Директория проекта: $projectDir"

# Проверяем наличие Docker
try {
    $dockerVersion = docker --version
    Write-Status "✅ Docker обнаружен: $dockerVersion" $Green
} catch {
    Write-Status "❌ Docker не установлен! Пожалуйста, установите Docker Desktop." $Red
    exit 1
}

# Выбираем docker-compose файл
$composeFile = "docker-compose.ml.yml"
Write-Status "📋 Используется конфигурация: $composeFile" $Cyan

# Проверяем и создаем .env файл, если нужно
if (-not (Test-Path ".env")) {
    Write-Status "📄 Создаем .env файл..."
    
    if ($Demo) {
        Write-Status "🧪 Настройка ДЕМО режима с тестовыми ключами API" $Yellow
        $apiKeyConfig = @"
OPENAI_API_KEY=$openAiKey
ANTHROPIC_API_KEY=$anthropicKey
XAI_CACHE_SIZE=1000
XAI_ENABLE_SHAP=true
XAI_ENABLE_LIME=true
MULTI_LLM_FALLBACK_ENABLED=true
MULTI_LLM_CONSENSUS_THRESHOLD=0.7
MULTI_LLM_MODE=demo
"@
    } else {
        Write-Status "🔐 Настройка PRODUCTION режима" $Yellow
        $openAiKey = Read-Host "Введите ваш OpenAI API ключ (или оставьте пустым для demo_key)"
        if ([string]::IsNullOrWhiteSpace($openAiKey)) { $openAiKey = "demo_key" }
        
        $anthropicKey = Read-Host "Введите ваш Anthropic API ключ (или оставьте пустым для demo_key)"
        if ([string]::IsNullOrWhiteSpace($anthropicKey)) { $anthropicKey = "demo_key" }
        
        $apiKeyConfig = @"
OPENAI_API_KEY=$openAiKey
ANTHROPIC_API_KEY=$anthropicKey
XAI_CACHE_SIZE=2000
XAI_ENABLE_SHAP=true
XAI_ENABLE_LIME=true
MULTI_LLM_FALLBACK_ENABLED=true
MULTI_LLM_CONSENSUS_THRESHOLD=0.8
MULTI_LLM_MODE=production
"@
    }
    
    $apiKeyConfig | Out-File -FilePath ".env" -Encoding utf8
    Write-Status "✅ Файл .env создан успешно!" $Green
}

# Запускаем Docker контейнеры
Write-Status "🚀 Запуск ML-инфраструктуры..." $Cyan
docker-compose -f $composeFile up -d

# Проверяем статус запуска
if ($LASTEXITCODE -ne 0) {
    Write-Status "❌ Ошибка при запуске контейнеров!" $Red
    exit 1
}

Write-Status "⏳ Ожидаем запуска всех сервисов..." $Yellow
Start-Sleep -Seconds 15

# Проверяем доступность основных сервисов
$backend_status = docker-compose -f $composeFile ps backend | Select-String "Up"
if ($backend_status) {
    Write-Status "✅ Backend сервис запущен успешно" $Green
} else {
    Write-Status "⚠️ Backend сервис может не быть готов, проверяем логи..." $Yellow
    docker-compose -f $composeFile logs --tail=20 backend
}

# Проверяем Kenning ML сервис
$kenning_status = docker-compose -f $composeFile ps kenning | Select-String "Up"
if ($kenning_status) {
    Write-Status "✅ ML сервис запущен успешно" $Green
} else {
    Write-Status "⚠️ ML сервис может не быть готов, проверяем логи..." $Yellow
    docker-compose -f $composeFile logs --tail=20 kenning
}

# Выводим информацию о доступных сервисах
Write-Status "`n🔗 Доступ к ML-инфраструктуре:" $Cyan
Write-Status "  • Backend API:  http://localhost:8000" $Green
Write-Status "  • ML API:       http://localhost:8000/ml_docs" $Green
Write-Status "  • Prometheus:   http://localhost:9090" $Green
Write-Status "  • Grafana:      http://localhost:3000 (логин/пароль: admin/admin)" $Green
Write-Status "  • Jupyter:      http://localhost:8888" $Green
Write-Status "  • MinIO:        http://localhost:9001 (логин/пароль: minioadmin/minioadmin)" $Green

# Запуск тестов если указан флаг
if ($RunTests) {
    Write-Status "`n🧪 Запуск тестов нагрузки и интеграции..." $Cyan
    & "$projectDir\scripts\run-load-tests.ps1"
}

# Открываем Grafana дашборд, если указан флаг
if ($OpenDashboard) {
    Write-Status "`n📊 Открываем Grafana дашборд..." $Cyan
    Start-Process "http://localhost:3000"
}

Write-Status "`n🎉 ML-инфраструктура успешно запущена!" $Green
Write-Status "   Используйте API по адресу http://localhost:8000" $Cyan
Write-Status "   Для остановки выполните: docker-compose -f $composeFile down" $Cyan
