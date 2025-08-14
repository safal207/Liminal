Param(
  [switch]$VerboseOutput
)

$ErrorActionPreference = 'Stop'

$pytestArgs = @()
if ($VerboseOutput) { $pytestArgs += @('-vv','-rA') } else { $pytestArgs += @('-q') }

Write-Host 'Running unit tests (excluding integration)...'
python -m pytest @pytestArgs -m "not integration"
