# Нежный тест Variant A для WebSocket Rate Limiting
# Цель: проверить Redis, запустить backend, получить JWT, сделать burst-тест, снять метрики

$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest

Write-Host "--- Нежный запуск Variant A (автоматизация) ---" -ForegroundColor Cyan

# 0) Версия Python
Write-Host "[0] Проверка Python..." -ForegroundColor Yellow
python --version

# 1) Проверка зависимостей Python
Write-Host "[1] Проверка зависимостей..." -ForegroundColor Yellow
$env:PYTHONWARNINGS = "ignore"
$global:LASTEXITCODE = 0
python -c "import uvicorn, fastapi, websockets, requests, redis" 2>$null
if ($LASTEXITCODE -ne 0) {
  Write-Host "[info] Установка зависимостей backend (один раз)..." -ForegroundColor Yellow
  python -m pip install --upgrade pip
  if (Test-Path "backend\requirements.txt") {
    python -m pip install -r "backend\requirements.txt"
  } else {
    python -m pip install uvicorn fastapi websockets requests redis
  }
}

# 2) Запуск Redis через Docker (без разрушения данных)
Write-Host "[2] Запуск Redis..." -ForegroundColor Yellow
try {
  docker compose up -d redis | Out-Host
} catch {
  Write-Host "[warn] Docker недоступен; предполагается, что Redis уже запущен на 6379" -ForegroundColor Yellow
}

# 3) Ожидание доступности порта Redis 6379
Write-Host "[3] Ожидание Redis на порту 6379..." -ForegroundColor Yellow
$redisReady = $false
for ($i=0; $i -lt 30; $i++) {
  try {
    $tnc = Test-NetConnection -ComputerName 127.0.0.1 -Port 6379 -WarningAction SilentlyContinue
    if ($tnc.TcpTestSucceeded) { $redisReady = $true; break }
  } catch {}
  Start-Sleep -Milliseconds 500
}
Write-Host ("[redis-ready] = {0}" -f $redisReady)

# 4) Экспорт переменных Redis из .env
Write-Host "[4] Настройка переменных окружения Redis..." -ForegroundColor Yellow
$pw = $null
if (Test-Path ".env") {
  $match = Select-String -Path ".env" -Pattern '^REDIS_PASSWORD=' | Select-Object -First 1
  if ($match) { $pw = ($match.Line -split '=',2)[1].Trim() }
}
$env:REDIS_HOST = "localhost"
$env:REDIS_PORT = "6379"
if ($pw) { $env:REDIS_PASSWORD = $pw }

# 5) Запуск uvicorn (api:app) локально
Write-Host "[5] Запуск сервера backend (uvicorn api:app)..." -ForegroundColor Yellow
$uv = Start-Process -FilePath "python" -ArgumentList @("-m","uvicorn","api:app","--host","127.0.0.1","--port","8000") -WorkingDirectory "backend" -PassThru -WindowStyle Hidden
Write-Host ("[started] uvicorn PID={0}" -f $uv.Id)

# 6) Ожидание готовности сервера (/health/ready)
Write-Host "[6] Ожидание готовности сервера..." -ForegroundColor Yellow
$ready = $false
for ($i=0; $i -lt 60; $i++) {
  try {
    $r = Invoke-WebRequest -Uri "http://127.0.0.1:8000/health/ready" -UseBasicParsing -TimeoutSec 1
    if ($r.StatusCode -eq 200) { $ready = $true; break }
  } catch {}
  Start-Sleep -Milliseconds 500
}
Write-Host ("[backend-ready] = {0}" -f $ready)
if (-not $ready) {
  try { if (-not $uv.HasExited) { $uv.Kill() | Out-Null } } catch {}
  throw "Backend не готов; прерывание"
}

# 7) Получение JWT токена
Write-Host "[7] Получение JWT токена..." -ForegroundColor Yellow
$tokenResp = Invoke-RestMethod -Method Post -Uri "http://127.0.0.1:8000/token" -Body (@{ username = 'testuser'; password = 'testpass' } | ConvertTo-Json) -ContentType 'application/json'
$tok = $tokenResp.access_token
if (-not $tok) {
  try { if (-not $uv.HasExited) { $uv.Kill() | Out-Null } } catch {}
  throw "Токен не получен"
}
Write-Host "[token] получен" -ForegroundColor Green

# 8) Мягкий burst-тест WebSocket
Write-Host "[8] Запуск burst-теста WebSocket (30 сообщений, 10мс)..." -ForegroundColor Yellow
$env:WS_URL = "ws://127.0.0.1:8000/ws/timeline"
$env:WS_TOKEN = $tok
$env:BURST = "30"
$env:DELAY = "0.01"
python -u ".\scripts\ws_rate_limit_quick_test.py"

# 9) Снятие выдержки из метрик
Write-Host "[9] Снятие метрик Prometheus..." -ForegroundColor Yellow
$metrics = Invoke-WebRequest -Uri "http://127.0.0.1:8000/metrics" -UseBasicParsing -TimeoutSec 3
$filtered = $metrics.Content -split "`n" | Where-Object { $_ -match '(websocket|rate|429)' }
Write-Host "[metrics excerpt]" -ForegroundColor Cyan
$filtered | Select-Object -First 60 | ForEach-Object { Write-Host $_ }

# 10) Остановка backend
Write-Host "[10] Остановка сервера..." -ForegroundColor Yellow
try { if (-not $uv.HasExited) { $uv.Kill() | Out-Null } } catch {}
Write-Host "[stopped] uvicorn" -ForegroundColor Gray

Write-Host "--- Готово. Пульс устойчив. Сердце бьётся ровно. ---" -ForegroundColor Magenta
