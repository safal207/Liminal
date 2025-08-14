# ML Health Check Script
# Проверка работоспособности ML-инфраструктуры Resonance Liminal
# Поддерживает работу с русской раскладкой в Windows 11

# Настройка кодировки для корректного отображения кириллицы
[Console]::OutputEncoding = [System.Text.Encoding]::UTF8
$PSDefaultParameterValues['*:Encoding'] = 'utf8'

# Функция для проверки доступности сервиса
function Test-ServiceHealth {
    param(
        [string]$Name,
        [string]$Url,
        [string]$ExpectedStatus = "200"
    )
    
    Write-Host "Проверка сервиса $Name по адресу $Url..." -ForegroundColor Cyan
    
    try {
        $response = Invoke-WebRequest -Uri $Url -TimeoutSec 5 -ErrorAction Stop
        $status = $response.StatusCode
        
        if ($status -eq $ExpectedStatus) {
            Write-Host "✅ Сервис $Name доступен (статус: $status)" -ForegroundColor Green
            return $true
        } else {
            Write-Host "❌ Сервис $Name вернул неожиданный статус: $status" -ForegroundColor Red
            return $false
        }
    } catch {
        Write-Host "❌ Сервис $Name недоступен: $_" -ForegroundColor Red
        return $false
    }
}

# Функция для проверки Prometheus метрик
function Test-PrometheusMetric {
    param(
        [string]$MetricName,
        [string]$PrometheusUrl = "http://localhost:9090/api/v1/query"
    )
    
    Write-Host "Проверка метрики $MetricName в Prometheus..." -ForegroundColor Cyan
    
    try {
        $query = "$PrometheusUrl?query=$MetricName"
        $response = Invoke-RestMethod -Uri $query -TimeoutSec 5 -ErrorAction Stop
        
        if ($response.status -eq "success" -and $response.data.result.Count -gt 0) {
            Write-Host "✅ Метрика $MetricName доступна в Prometheus" -ForegroundColor Green
            return $true
        } else {
            Write-Host "⚠️ Метрика $MetricName не найдена в Prometheus" -ForegroundColor Yellow
            return $false
        }
    } catch {
        Write-Host "❌ Ошибка при проверке метрики $MetricName $_" -ForegroundColor Red
        return $false
    }
}

# Функция для проверки Redis ключей
function Test-RedisKeys {
    param(
        [string]$Pattern,
        [string]$RedisHost = "localhost",
        [int]$RedisPort = 6379
    )
    
    Write-Host "Проверка ключей '$Pattern' в Redis..." -ForegroundColor Cyan
    
    try {
        # Используем redis-cli для проверки ключей
        $result = docker exec redis redis-cli -h $RedisHost -p $RedisPort --scan --pattern $Pattern
        
        if ($result) {
            Write-Host "✅ Ключи по шаблону '$Pattern' найдены в Redis" -ForegroundColor Green
            return $true
        } else {
            Write-Host "⚠️ Ключи по шаблону '$Pattern' не найдены в Redis" -ForegroundColor Yellow
            return $false
        }
    } catch {
        Write-Host "❌ Ошибка при проверке Redis: $_" -ForegroundColor Red
        return $false
    }
}

# Функция для проверки моделей в MinIO
function Test-MinIOModels {
    param(
        [string]$Bucket = "models",
        [string]$MinioHost = "localhost",
        [int]$MinioPort = 9000
    )
    
    Write-Host "Проверка моделей в MinIO..." -ForegroundColor Cyan
    
    try {
        # Используем mc (MinIO Client) через Docker
        $env:MINIO_ACCESS_KEY = "minioadmin"
        $env:MINIO_SECRET_KEY = "minioadmin"
        
        # Проверяем, настроен ли уже MinIO клиент
        docker exec minio mc config host ls minio 2>&1 | Out-Null
        if ($LASTEXITCODE -ne 0) {
            docker exec minio mc config host add minio http://$MinioPort $env:MINIO_ACCESS_KEY $env:MINIO_SECRET_KEY
        }
        
        # Проверяем наличие бакета и файлов
        $result = docker exec minio mc ls minio/$Bucket
        
        if ($result) {
            Write-Host "✅ Модели найдены в MinIO:" -ForegroundColor Green
            Write-Host $result -ForegroundColor Gray
            return $true
        } else {
            Write-Host "⚠️ Модели не найдены в MinIO" -ForegroundColor Yellow
            return $false
        }
    } catch {
        Write-Host "❌ Ошибка при проверке MinIO: $_" -ForegroundColor Red
        return $false
    }
}

# Функция для проверки логов контейнера
function Test-ContainerLogs {
    param(
        [string]$ContainerName,
        [string]$ErrorPattern = "error|exception|critical"
    )
    
    Write-Host "Проверка логов контейнера $ContainerName..." -ForegroundColor Cyan
    
    try {
        # Получаем последние 50 строк логов
        $logs = docker logs --tail 50 $ContainerName 2>&1
        
        # Ищем ошибки в логах
        $errors = $logs | Select-String -Pattern $ErrorPattern -CaseSensitive:$false
        
        if ($errors) {
            Write-Host "⚠️ В логах контейнера $ContainerName найдены ошибки:" -ForegroundColor Yellow
            $errors | ForEach-Object { Write-Host "   $_" -ForegroundColor Yellow }
            return $false
        } else {
            Write-Host "✅ В логах контейнера $ContainerName не найдено ошибок" -ForegroundColor Green
            return $true
        }
    } catch {
        Write-Host "❌ Ошибка при проверке логов контейнера $ContainerName $_" -ForegroundColor Red
        return $false
    }
}

# Функция для проверки прогноза аномалий
function Test-AnomalyPrediction {
    param(
        [string]$PredictorUrl = "http://localhost:8002/predict"
    )
    
    Write-Host "Тестирование API предсказания аномалий..." -ForegroundColor Cyan
    
    try {
        $response = Invoke-RestMethod -Uri $PredictorUrl -TimeoutSec 5 -ErrorAction Stop
        
        if ($response -and $null -ne $response.prediction) {
            Write-Host "✅ API предсказания работает. Текущий score: $($response.anomaly_score)" -ForegroundColor Green
            
            # Проверяем объяснение, если доступно
            if ($response.explanation) {
                Write-Host "   Объяснение доступно через XAI" -ForegroundColor Green
            }
            
            return $true
        } else {
            Write-Host "⚠️ API предсказания вернуло некорректный формат ответа" -ForegroundColor Yellow
            return $false
        }
    } catch {
        Write-Host "❌ Ошибка при тестировании API предсказания: $_" -ForegroundColor Red
        return $false
    }
}

# Основная логика скрипта
function Start-MLHealthCheck {
    $totalChecks = 0
    $passedChecks = 0
    
    Write-Host "=== Проверка здоровья ML-инфраструктуры Resonance Liminal ===" -ForegroundColor Magenta
    Write-Host "Дата и время проверки: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')" -ForegroundColor Magenta
    Write-Host "--------------------------------------------------------" -ForegroundColor Magenta
    
    # Проверка основных сервисов
    $services = @(
        @{Name="Backend"; Url="http://localhost:8000/health"; ExpectedStatus="200"},
        @{Name="Kenning"; Url="http://localhost:5000/health"; ExpectedStatus="200"},
        @{Name="ML Predictor"; Url="http://localhost:8002/health"; ExpectedStatus="200"},
        @{Name="Prometheus"; Url="http://localhost:9090/-/healthy"; ExpectedStatus="200"},
        @{Name="Grafana"; Url="http://localhost:3000/api/health"; ExpectedStatus="200"}
    )
    
    foreach ($service in $services) {
        $totalChecks++
        if (Test-ServiceHealth -Name $service.Name -Url $service.Url -ExpectedStatus $service.ExpectedStatus) {
            $passedChecks++
        }
    }
    
    # Проверка ML-метрик
    $metrics = @(
        "ml_anomaly_score",
        "ml_burstiness_score",
        "ml_ip_entropy",
        "ml_requests_per_user_rate",
        "ml_model_drift"
    )
    
    foreach ($metric in $metrics) {
        $totalChecks++
        if (Test-PrometheusMetric -MetricName $metric) {
            $passedChecks++
        }
    }
    
    # Проверка Redis ключей
    $redisPatterns = @(
        "ml:features:*",
        "ml:predictions:*",
        "ml:models:*"
    )
    
    foreach ($pattern in $redisPatterns) {
        $totalChecks++
        if (Test-RedisKeys -Pattern $pattern) {
            $passedChecks++
        }
    }
    
    # Проверка контейнеров
    $containers = @(
        "ml-feature-extractor",
        "ml-predictor",
        "kenning"
    )
    
    foreach ($container in $containers) {
        $totalChecks++
        if (Test-ContainerLogs -ContainerName $container) {
            $passedChecks++
        }
    }
    
    # Проверка MinIO
    $totalChecks++
    if (Test-MinIOModels) {
        $passedChecks++
    }
    
    # Проверка предсказаний
    $totalChecks++
    if (Test-AnomalyPrediction) {
        $passedChecks++
    }
    
    # Итоговый отчет
    Write-Host "--------------------------------------------------------" -ForegroundColor Magenta
    $healthPercentage = [math]::Round(($passedChecks / $totalChecks) * 100, 2)
    
    Write-Host "Итоговое здоровье ML-инфраструктуры: $healthPercentage%" -ForegroundColor Magenta
    Write-Host "Прошло проверок: $passedChecks из $totalChecks" -ForegroundColor Magenta
    
    if ($healthPercentage -ge 90) {
        Write-Host "Статус: ОТЛИЧНО" -ForegroundColor Green
    } elseif ($healthPercentage -ge 70) {
        Write-Host "Статус: ХОРОШО" -ForegroundColor Yellow
    } else {
        Write-Host "Статус: ТРЕБУЕТ ВНИМАНИЯ" -ForegroundColor Red
    }
    
    Write-Host "--------------------------------------------------------" -ForegroundColor Magenta
    
    return @{
        Date = Get-Date -Format 'yyyy-MM-dd HH:mm:ss'
        HealthPercentage = $healthPercentage
        PassedChecks = $passedChecks
        TotalChecks = $totalChecks
    }
}

# Запуск проверки здоровья
Start-MLHealthCheck
