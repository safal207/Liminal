param([string]$Action = "help")
$ErrorActionPreference = 'Stop'

# Paths
$WIN_ROOT = (Resolve-Path "$PSScriptRoot\..\").Path.TrimEnd('\')
$WSL_ROOT = "/mnt/" + $WIN_ROOT.Substring(0,1).ToLower() + ($WIN_ROOT.Substring(2) -replace "\\","/")
$WSL_PY   = "$WSL_ROOT/.venv/bin/python"
$WSL_PIP  = "$WSL_ROOT/.venv/bin/pip"
$WSL_UV   = "$WSL_ROOT/.venv/bin/uvicorn"

function W([string]$cmd){ wsl.exe bash -lc $cmd }

switch ($Action) {
  'wsl:proxy:clean' {
    W "unset http_proxy https_proxy HTTP_PROXY HTTPS_PROXY; export NO_PROXY=localhost,127.0.0.1,::1; echo 'WSL proxy cleaned'"
  }
  'deps:ensure' {
    W "export PIP_CACHE_DIR=/tmp/pip-cache; \"$WSL_PIP\" install -U pip -i https://pypi.org/simple --default-timeout 25 --cache-dir /tmp/pip-cache -vvv && \"$WSL_PIP\" install -i https://pypi.org/simple --default-timeout 25 --cache-dir /tmp/pip-cache -vvv structlog loguru uvicorn pytest httpx prometheus_client"
  }
  'server:start' {
    W "export PYTHONPATH='$WSL_ROOT'; $WSL_UV backend.main:app --host 0.0.0.0 --port 8000 --log-level debug"
  }
  'server:stop' {
    W "pkill -f uvicorn || true; echo 'uvicorn stopped'"
  }
  'smoke:health' {
    W "export TEST_MODE=1 LIMINAL_DISABLE_HOME_STATE=1 PYTHONPATH='$WSL_ROOT'; $WSL_PY -m pytest -q backend/tests/test_health_endpoints.py backend/tests/test_ready_degraded.py"
  }
  'smoke:auth' {
    W "export TEST_MODE=1 LIMINAL_DISABLE_HOME_STATE=1 PYTHONPATH='$WSL_ROOT'; $WSL_PY -m pytest -q backend/tests/test_auth*.py || true"
  }
  'smoke:all' {
    W "export TEST_MODE=1 LIMINAL_DISABLE_HOME_STATE=1 PYTHONPATH='$WSL_ROOT'; $WSL_PY -m pytest -q backend/tests/test_health_endpoints.py backend/tests/test_ready_degraded.py backend/tests/test_auth*.py || true"
  }
  Default {
    Write-Host "Usage: .\\scripts\\liminal-dev.ps1 <action>" -ForegroundColor Cyan
    Write-Host "Actions: wsl:proxy:clean | deps:ensure | server:start | server:stop | smoke:health | smoke:auth | smoke:all"
  }
}
