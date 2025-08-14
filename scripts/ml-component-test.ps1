# ML-COMPONENT TEST SCRIPT
# Скрипт для поэтапного тестирования компонентов ML-инфраструктуры

param(
    [switch]$CleanEnv,          # Очистить окружение перед запуском
    [switch]$SkipRedis,         # Пропустить тестирование Redis
    [switch]$SkipPrometheus,    # Пропустить тестирование Prometheus
    [switch]$SkipBackend,       # Пропустить тестирование Backend
    [switch]$SkipKenning        # Пропустить тестирование Kenning ML
)

# Цвета для вывода
$Green = [System.ConsoleColor]::Green
$Cyan = [System.ConsoleColor]::Cyan
$Yellow = [System.ConsoleColor]::Yellow
$Red = [System.ConsoleColor]::Red

# Функция для вывода статуса
function Write-Status($message, $color = $Cyan) {
    Write-Host $message -ForegroundColor $color
}

# Функция для запуска контейнера и проверки его состояния
function Test-Container {
    param(
        [string]$Name,
        [string]$Command,
        [string]$HealthCheck,
        [int]$WaitTime = 10
    )

    Write-Status "🔍 Тестирование $Name" $Cyan
    Write-Status "> $Command" $Yellow
    
    try {
        Invoke-Expression $Command | Out-Null
        Start-Sleep -Seconds $WaitTime
        
        $status = docker ps --filter "name=$Name" --format "{{.Status}}"
        
        if ($status -like "*healthy*" -or $status -like "*Up*") {
            Write-Status "✅ Контейнер $Name успешно запущен" $Green
            return $true
        } else {
            Write-Status "⚠️ Контейнер $Name запущен, но может иметь проблемы: $status" $Yellow
            
            Write-Status "📝 Логи контейнера $Name:" $Yellow
            docker logs --tail 20 $Name
            
            return $false
        }
    }
    catch {
        Write-Status "❌ Ошибка при запуске $Name: $_" $Red
        return $false
    }
}

# Проверяем наличие Docker
try {
    $dockerVersion = docker --version
    Write-Status "✅ Docker обнаружен: $dockerVersion" $Green
} catch {
    Write-Status "❌ Docker не установлен! Пожалуйста, установите Docker Desktop." $Red
    exit 1
}

# Очищаем среду, если указано
if ($CleanEnv) {
    Write-Status "🧹 Очистка среды тестирования..." $Yellow
    docker rm -f ml-redis ml-prometheus ml-backend ml-kenning 2>$null
}

$testResults = @()

# 1. Тестирование Redis
if (-not $SkipRedis) {
    Write-Status "`n📊 ТЕСТИРОВАНИЕ REDIS" $Green
    $redisSuccess = Test-Container -Name "ml-redis" `
                               -Command "docker run -d --name ml-redis -p 6379:6379 redis:latest" `
                               -HealthCheck "redis-cli ping" `
                               -WaitTime 5
    
    if ($redisSuccess) {
        Write-Status "🔍 Проверка Redis CLI" $Cyan
        docker exec ml-redis redis-cli ping
    }
    
    $testResults += @{
        Component = "Redis"
        Status = if ($redisSuccess) { "✅ УСПЕШНО" } else { "❌ ОШИБКА" }
    }
}

# 2. Тестирование Prometheus
if (-not $SkipPrometheus) {
    Write-Status "`n📊 ТЕСТИРОВАНИЕ PROMETHEUS" $Green
    
    # Создаем временный конфиг, если нужно
    if (-not (Test-Path -Path "c:/Users/safal/OneDrive/Documente/GitHub/resonance-liminal/prometheus/prometheus.yml")) {
        Write-Status "📝 Создаем временный конфиг для Prometheus..." $Yellow
        $prometheusConfig = @"
global:
  scrape_interval: 15s

scrape_configs:
  - job_name: 'prometheus'
    static_configs:
      - targets: ['localhost:9090']
  
  - job_name: 'backend'
    static_configs:
      - targets: ['ml-backend:8000']
"@
        New-Item -Path "c:/Users/safal/OneDrive/Documente/GitHub/resonance-liminal/prometheus" -ItemType Directory -Force | Out-Null
        $prometheusConfig | Out-File -FilePath "c:/Users/safal/OneDrive/Documente/GitHub/resonance-liminal/prometheus/prometheus.yml" -Encoding utf8
    }
    
    $prometheusSuccess = Test-Container -Name "ml-prometheus" `
                                    -Command "docker run -d --name ml-prometheus -p 9090:9090 -v c:/Users/safal/OneDrive/Documente/GitHub/resonance-liminal/prometheus/prometheus.yml:/etc/prometheus/prometheus.yml prom/prometheus:latest" `
                                    -HealthCheck "curl http://localhost:9090/-/healthy" `
                                    -WaitTime 10
                                    
    if ($prometheusSuccess) {
        Write-Status "🔍 Prometheus доступен по адресу: http://localhost:9090" $Cyan
    }
    
    $testResults += @{
        Component = "Prometheus"
        Status = if ($prometheusSuccess) { "✅ УСПЕШНО" } else { "❌ ОШИБКА" }
    }
}

# 3. Тестирование Backend с ML-компонентами
if (-not $SkipBackend) {
    Write-Status "`n📊 ТЕСТИРОВАНИЕ BACKEND С ML-ПОДДЕРЖКОЙ" $Green
    
    $backendSuccess = Test-Container -Name "ml-backend" `
                                 -Command "docker run -d --name ml-backend -p 8000:8000 --link ml-redis:redis --link ml-prometheus:prometheus -e REDIS_HOST=ml-redis -e PROMETHEUS_URL=http://ml-prometheus:9090 -e TEST_MODE=true -e ML_ENABLED=true -e OPENAI_API_KEY=demo_key -e ANTHROPIC_API_KEY=demo_key resonance-liminal-backend:latest" `
                                 -HealthCheck "curl http://localhost:8000/health" `
                                 -WaitTime 15
    
    if ($backendSuccess) {
        Write-Status "🔍 Проверка доступности API" $Cyan
        try {
            $response = Invoke-WebRequest -Uri "http://localhost:8000/docs" -UseBasicParsing
            if ($response.StatusCode -eq 200) {
                Write-Status "✅ API документация доступна" $Green
            }
        } catch {
            Write-Status "⚠️ API документация недоступна: $_" $Yellow
        }
        
        Write-Status "📝 Логи Backend:" $Yellow
        docker logs --tail 20 ml-backend
    }
    
    $testResults += @{
        Component = "Backend ML"
        Status = if ($backendSuccess) { "✅ УСПЕШНО" } else { "❌ ОШИБКА" }
    }
}

# 4. Тестирование Kenning ML-сервиса
if (-not $SkipKenning) {
    Write-Status "`n📊 ТЕСТИРОВАНИЕ KENNING ML-СЕРВИСА" $Green
    
    # Проверяем наличие образа
    $kenningImageExists = docker images kenning/kenning:latest -q
    
    if (-not $kenningImageExists) {
        Write-Status "⚠️ Образ kenning/kenning:latest не найден. Попытка использования резервного образа..." $Yellow
        $kenningImageExists = docker images kenning/kenning-serving:latest -q
        
        if (-not $kenningImageExists) {
            Write-Status "❌ Ни один из образов Kenning не найден. Пропускаем тестирование Kenning." $Red
            $kenningSuccess = $false
        }
        else {
            $kenningImage = "kenning/kenning-serving:latest"
        }
    }
    else {
        $kenningImage = "kenning/kenning:latest"
    }
    
    if ($kenningImageExists) {
        $kenningSuccess = Test-Container -Name "ml-kenning" `
                                     -Command "docker run -d --name ml-kenning -p 8001:8000 --link ml-redis:redis --link ml-prometheus:prometheus -e PROMETHEUS_URL=http://ml-prometheus:9090 -e REDIS_HOST=ml-redis -v c:/Users/safal/OneDrive/Documente/GitHub/resonance-liminal/models:/app/models $kenningImage" `
                                     -HealthCheck "curl http://localhost:8001/health" `
                                     -WaitTime 20
        
        if ($kenningSuccess) {
            Write-Status "🔍 Проверка доступности Kenning API" $Cyan
            try {
                $response = Invoke-WebRequest -Uri "http://localhost:8001/docs" -UseBasicParsing
                if ($response.StatusCode -eq 200) {
                    Write-Status "✅ Kenning API документация доступна" $Green
                }
            } catch {
                Write-Status "⚠️ Kenning API документация недоступна: $_" $Yellow
            }
            
            Write-Status "📝 Логи Kenning:" $Yellow
            docker logs --tail 20 ml-kenning
        }
    }
    
    $testResults += @{
        Component = "Kenning ML"
        Status = if ($kenningSuccess) { "✅ УСПЕШНО" } else { "❌ ОШИБКА" }
    }
}

# Вывод результатов
Write-Status "`n📊 РЕЗУЛЬТАТЫ ТЕСТИРОВАНИЯ КОМПОНЕНТОВ ML-ИНФРАСТРУКТУРЫ" $Green
$testResults | ForEach-Object {
    $statusColor = if ($_.Status -like "*УСПЕШНО*") { $Green } else { $Red }
    Write-Status ("  • {0,-15}: {1}" -f $_.Component, $_.Status) $statusColor
}

# Вывод итоговой информации
$successCount = ($testResults | Where-Object { $_.Status -like "*УСПЕШНО*" }).Count
$totalCount = $testResults.Count

Write-Status "`n🎯 ИТОГ: $successCount из $totalCount компонентов работают корректно" $(if ($successCount -eq $totalCount) { $Green } else { $Yellow })

if ($successCount -eq $totalCount) {
    Write-Status "`n🎉 ML-инфраструктура готова к работе!" $Green
    Write-Status "   Доступные сервисы:" $Cyan
    Write-Status "   • Backend API:    http://localhost:8000/docs" $Green
    Write-Status "   • Prometheus:     http://localhost:9090" $Green
    if (-not $SkipKenning) {
        Write-Status "   • Kenning ML API: http://localhost:8001/docs" $Green
    }
} else {
    Write-Status "`n⚠️ Некоторые компоненты ML-инфраструктуры не работают корректно." $Yellow
    Write-Status "   Проверьте логи и конфигурацию для устранения проблем." $Yellow
}

Write-Status "`n💡 Для остановки всех тестовых контейнеров выполните:" $Cyan
Write-Status "   docker rm -f ml-redis ml-prometheus ml-backend ml-kenning" $Yellow
