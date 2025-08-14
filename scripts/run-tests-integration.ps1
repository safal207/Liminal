Param(
  [string]$ApiUrl = "http://localhost:8080",
  [string]$WsUrl = "ws://localhost:8080/ws",
  [switch]$VerboseOutput
)

$ErrorActionPreference = 'Stop'

$env:WS_API_URL = $ApiUrl
$env:WS_URL = $WsUrl

$pytestArgs = @('-m','integration')
if ($VerboseOutput) { $pytestArgs = @('-vv','-rA') + $pytestArgs } else { $pytestArgs = @('-q') + $pytestArgs }

Write-Host "Running integration smoke tests against API=$ApiUrl WS=$WsUrl ..."
python -m pytest @pytestArgs
