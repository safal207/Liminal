# PowerShell script для запуска Neo4j-версии at-risk сервера
# Запускает автономный HTTP/WebSocket сервер для at-risk эмоциональных связей
# с Neo4j вместо in-memory хранения

# Установка переменных окружения для подключения к Neo4j
$env:LIMINAL_USE_NEO4J = "1"
$env:LIMINAL_NEO4J_URI = "bolt://localhost:7687"
$env:LIMINAL_NEO4J_USER = "neo4j"
$env:LIMINAL_NEO4J_PASSWORD = $env:NEO4J_PASSWORD
if ([string]::IsNullOrWhiteSpace($env:LIMINAL_NEO4J_PASSWORD)) {
    Write-Error "Set NEO4J_PASSWORD before starting the Neo4j server."
    exit 1
}

Write-Host "🌐 Запуск Neo4j-версии REST API сервера для at-risk связей" -ForegroundColor Cyan

# Определяем тип БД в зависимости от переменной окружения
if ($env:LIMINAL_USE_NEO4J -eq "1") {
    Write-Host "💾 Используется база данных: Neo4j ($env:LIMINAL_NEO4J_URI)" -ForegroundColor Green
} else {
    Write-Host "💾 Используется база данных: In-Memory (локальная)" -ForegroundColor Yellow
}

# Проверка доступности Neo4j, если используется
if ($env:LIMINAL_USE_NEO4J -eq "1") {
    Write-Host "🔍 Проверка подключения к Neo4j..." -ForegroundColor Cyan
    
    try {
        # Создаем временный скрипт для проверки подключения
        $tempFile = [System.IO.Path]::GetTempFileName() + ".py"
        @"
from neo4j import GraphDatabase
import sys

try:
    driver = GraphDatabase.driver(
        "$env:LIMINAL_NEO4J_URI",
        auth=("$env:LIMINAL_NEO4J_USER", "$env:LIMINAL_NEO4J_PASSWORD")
    )
    with driver.session() as session:
        result = session.run("RETURN 1 as test")
        record = result.single()
        if record and record['test'] == 1:
            print("Connection successful")
        else:
            print("Connection failed: unexpected result")
    driver.close()
    sys.exit(0)
except Exception as e:
    print(f"Connection failed: {e}")
    sys.exit(1)
"@ | Set-Content $tempFile

        # Запускаем проверку
        $result = python $tempFile
        $exitCode = $LASTEXITCODE
        
        # Удаляем временный файл
        Remove-Item $tempFile -ErrorAction SilentlyContinue
        
        if ($exitCode -eq 0) {
            Write-Host "✅ Подключение к Neo4j успешно установлено" -ForegroundColor Green
        } else {
            Write-Host "❌ Ошибка подключения к Neo4j: $result" -ForegroundColor Red
            Write-Host "⚠️ Переключение на In-Memory хранилище" -ForegroundColor Yellow
            $env:LIMINAL_USE_NEO4J = "0"
        }
    } catch {
        Write-Host "❌ Ошибка при проверке подключения к Neo4j: $_" -ForegroundColor Red
        Write-Host "⚠️ Переключение на In-Memory хранилище" -ForegroundColor Yellow
        $env:LIMINAL_USE_NEO4J = "0"
    }
}

# Запуск Uvicorn сервера
try {
    Write-Host "🚀 Запуск сервера на http://127.0.0.1:8080/at-risk..." -ForegroundColor Cyan
    python -m uvicorn liminal.at_risk_server_neo4j:app --port 8080 --host 127.0.0.1
} catch {
    Write-Host "❌ Ошибка запуска сервера: $_" -ForegroundColor Red
}

Write-Host "👋 Сервер остановлен" -ForegroundColor Cyan
