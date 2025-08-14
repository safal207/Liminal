# Tender Variant A test for WebSocket Rate Limiting
# Goal: verify Redis, start backend, get JWT, run burst-test, capture metrics

$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest

Write-Host "--- Tender launch of Variant A (automation) ---" -ForegroundColor Cyan

# 0) Python version
Write-Host "[0] Checking Python..." -ForegroundColor Yellow
python --version

# 1) Checking Python dependencies
Write-Host "[1] Checking dependencies..." -ForegroundColor Yellow
$env:PYTHONWARNINGS = "ignore"
$global:LASTEXITCODE = 0
python -c "import uvicorn, fastapi, websockets, requests, redis" 2>$null
if ($LASTEXITCODE -ne 0) {
  Write-Host "[info] Installing backend dependencies (one-time)..." -ForegroundColor Yellow
  python -m pip install --upgrade pip
  if (Test-Path "backend\requirements.txt") {
    python -m pip install -r "backend\requirements.txt"
  } else {
    python -m pip install uvicorn fastapi websockets requests redis
  }
}

# 2) Start Redis via Docker (non-destructive)
Write-Host "[2] Starting Redis..." -ForegroundColor Yellow
try {
  docker compose up -d redis | Out-Host
} catch {
  Write-Host "[warn] Docker unavailable; assuming Redis is already running on 6379" -ForegroundColor Yellow
}

# 3) Wait for Redis port 6379 availability
Write-Host "[3] Waiting for Redis on port 6379..." -ForegroundColor Yellow
$redisReady = $false
for ($i=0; $i -lt 30; $i++) {
  try {
    $tnc = Test-NetConnection -ComputerName 127.0.0.1 -Port 6379 -WarningAction SilentlyContinue
    if ($tnc.TcpTestSucceeded) { $redisReady = $true; break }
  } catch {}
  Start-Sleep -Milliseconds 500
}
Write-Host ("[redis-ready] = {0}" -f $redisReady)

# 4) Export Redis variables from .env
Write-Host "[4] Setting up Redis environment variables..." -ForegroundColor Yellow
$pw = $null
if (Test-Path ".env") {
  $match = Select-String -Path ".env" -Pattern '^REDIS_PASSWORD=' | Select-Object -First 1
  if ($match) { $pw = ($match.Line -split '=',2)[1].Trim() }
}
$env:REDIS_HOST = "localhost"
$env:REDIS_PORT = "6379"
if ($pw) { $env:REDIS_PASSWORD = $pw }

# 5) Start uvicorn (api:app) locally
Write-Host "[5] Starting backend server (uvicorn api:app)..." -ForegroundColor Yellow
$uv = Start-Process -FilePath "python" -ArgumentList @("-m","uvicorn","api:app","--host","127.0.0.1","--port","8000") -WorkingDirectory "backend" -PassThru -WindowStyle Hidden
Write-Host ("[started] uvicorn PID={0}" -f $uv.Id)

# 6) Wait for server readiness (/health/ready)
Write-Host "[6] Waiting for server readiness..." -ForegroundColor Yellow
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
  throw "Backend not ready; aborting"
}

# 7) Obtain JWT token
Write-Host "[7] Obtaining JWT token..." -ForegroundColor Yellow
$tokenResp = Invoke-RestMethod -Method Post -Uri "http://127.0.0.1:8000/token" -Body (@{ username = 'testuser'; password = 'testpass' } | ConvertTo-Json) -ContentType 'application/json'
$tok = $tokenResp.access_token
if (-not $tok) {
  try { if (-not $uv.HasExited) { $uv.Kill() | Out-Null } } catch {}
  throw "Token not received"
}
Write-Host "[token] received" -ForegroundColor Green

# 8) Gentle WebSocket burst-test
Write-Host "[8] Running WebSocket burst-test (30 messages, 10ms)..." -ForegroundColor Yellow
$env:WS_URL = "ws://127.0.0.1:8000/ws/timeline"
$env:WS_TOKEN = $tok
$env:BURST = "30"
$env:DELAY = "0.01"
python -u ".\scripts\ws_rate_limit_quick_test.py"

# 9) Capture metrics excerpt
Write-Host "[9] Capturing Prometheus metrics..." -ForegroundColor Yellow
$metrics = Invoke-WebRequest -Uri "http://127.0.0.1:8000/metrics" -UseBasicParsing -TimeoutSec 3
$filtered = $metrics.Content -split "`n" | Where-Object { $_ -match '(websocket|rate|429)' }
Write-Host "[metrics excerpt]" -ForegroundColor Cyan
$filtered | Select-Object -First 60 | ForEach-Object { Write-Host $_ }

# 10) Stop backend
Write-Host "[10] Stopping server..." -ForegroundColor Yellow
try { if (-not $uv.HasExited) { $uv.Kill() | Out-Null } } catch {}
Write-Host "[stopped] uvicorn" -ForegroundColor Gray

Write-Host "--- Done. Pulse steady. Heart beats evenly. ---" -ForegroundColor Magenta
