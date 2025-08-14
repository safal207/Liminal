<#
    build_check.ps1 - Smart diagnostic script

    Runs Go vet and build, captures errors into build_errors.log, and appends an entry with basic insights to build_insights.md.
    Designed for quick iterative development so you can calmly review issues instead of panicking.
#>

param(
    [string]$ProjectPath = "go_ws_relay"
)

# Ensure we run in the repo root directory
$RepoRoot = Split-Path $PSScriptRoot -Parent
Push-Location $RepoRoot

$ErrorFile   = Join-Path $PSScriptRoot "build_errors.log"
$InsightFile = Join-Path $PSScriptRoot "build_insights.md"

# Clear previous error log
if (Test-Path $ErrorFile) {
    Remove-Item $ErrorFile -Force
}

Write-Host "[build_check] Running 'go vet'..." -ForegroundColor Cyan
Push-Location $ProjectPath
& go vet ./... 2>&1 | Tee-Object -FilePath $ErrorFile -Append
$vetExit = $LASTEXITCODE

Write-Host "[build_check] Building project..." -ForegroundColor Cyan
& go build . 2>&1 | Tee-Object -FilePath $ErrorFile -Append
$buildExit = $LASTEXITCODE
Pop-Location

$hasErrors = ($vetExit -ne 0 -or $buildExit -ne 0)

if ($hasErrors) {
    Write-Host "[build_check] ⚠️  Issues detected. See build_errors.log" -ForegroundColor Yellow
    # Append insights
    $timestamp = (Get-Date).ToString("yyyy-MM-dd HH:mm:ss")
    Add-Content $InsightFile "`n## $timestamp"
    Add-Content $InsightFile "### Observed errors"
    Get-Content $ErrorFile | Add-Content $InsightFile
    Add-Content $InsightFile "### Preliminary insights"
    Add-Content $InsightFile "- Check for syntax errors, unmatched braces or comments."
    Add-Content $InsightFile "- Verify all imports are used or removed."
    Add-Content $InsightFile "- Ensure duplicate code blocks are fully deleted."
    
    # Запускаем анализ сознания паттернов
    Write-Host "[build_check] Analyzing consciousness patterns..." -ForegroundColor Magenta
    & "$PSScriptRoot\consciousness_simple.ps1" -InsightsPath $InsightFile
} else {
    Write-Host "[build_check] ✅ Build clean." -ForegroundColor Green
    
    # Даже при чистой сборке анализируем для отслеживания прогресса
    Write-Host "[build_check] Updating success consciousness..." -ForegroundColor Cyan
    & "$PSScriptRoot\consciousness_simple.ps1" -InsightsPath $InsightFile
}

Pop-Location
