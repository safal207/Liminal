# consciousness_simple.ps1 - Simple consciousness insights system
param(
    [string]$InsightsPath = "build_insights.md"
)

Push-Location $PSScriptRoot

$ConsciousnessFile = "consciousness_patterns.md"
$timestamp = (Get-Date).ToString("yyyy-MM-dd HH:mm:ss")

if (Test-Path $InsightsPath) {
    Write-Host "[consciousness] Analyzing patterns..." -ForegroundColor Magenta
    
    $content = Get-Content $InsightsPath -Raw -ErrorAction SilentlyContinue
    if ($content) {
        # Count patterns
        $duplicateCount = ([regex]::Matches($content, "redeclared|duplicate")).Count
        $unusedImportCount = ([regex]::Matches($content, "imported and not used")).Count
        $undefinedCount = ([regex]::Matches($content, "undefined:")).Count
        $modulePathCount = ([regex]::Matches($content, "go.mod file not found")).Count
        $workingDirCount = ([regex]::Matches($content, "directory prefix")).Count
        $successCount = ([regex]::Matches($content, "Build clean|SUCCESS")).Count
        
        # Calculate cleanliness score
        $totalErrors = $duplicateCount + $unusedImportCount + $undefinedCount + $modulePathCount + $workingDirCount
        $cleanlinessScore = if ($totalErrors -eq 0) { 100 } else { [Math]::Max(0, 100 - ($totalErrors * 10)) }
        
        # Generate insights
        $insights = @()
        $insights += "## $timestamp - Code Consciousness Analysis"
        $insights += "### Cleanliness Score: $cleanlinessScore/100"
        
        if ($cleanlinessScore -ge 90) {
            $insights += "**State: CLEAN CONSCIOUSNESS** ✨"
        } elseif ($cleanlinessScore -ge 70) {
            $insights += "**State: TRANSITIONAL** 🌊"
        } else {
            $insights += "**State: NEEDS HEALING** 🔄"
        }
        
        $insights += ""
        $insights += "### Pattern Analysis:"
        
        if ($duplicateCount -gt 0) {
            $insights += "- Duplicate code patterns: $duplicateCount occurrences"
            if ($duplicateCount -gt 2) { $insights += "  ⚠️ CHRONIC PATTERN - refactor needed" }
        }
        
        if ($unusedImportCount -gt 0) {
            $insights += "- Unused imports: $unusedImportCount occurrences"
            if ($unusedImportCount -gt 3) { $insights += "  💡 RECOMMENDATION - setup auto-cleanup" }
        }
        
        if ($undefinedCount -gt 0) {
            $insights += "- Undefined symbols: $undefinedCount occurrences"
        }
        
        if ($modulePathCount -gt 0) {
            $insights += "- Module path errors: $modulePathCount occurrences"
        }
        
        if ($workingDirCount -gt 0) {
            $insights += "- Working directory errors: $workingDirCount occurrences"
        }
        
        if ($successCount -gt 0) {
            $insights += "- Success builds: $successCount occurrences ✅"
        }
        
        $insights += ""
        $insights += "### Philosophy First Wisdom:"
        
        if ($duplicateCount -gt 2) {
            $insights += "- 🏠 Home principle: Code should be authentic - no duplication"
        }
        
        if ($cleanlinessScore -lt 50) {
            $insights += "- 🔄 Self-healing principle: Through love for code we heal architecture"
        }
        
        $insights += ""
        $insights += "---"
        $insights += ""
        
        # Write to consciousness file
        $insights | Out-File -FilePath $ConsciousnessFile -Append -Encoding UTF8
        
        Write-Host "[consciousness] Cleanliness: $cleanlinessScore/100" -ForegroundColor $(if ($cleanlinessScore -ge 90) { "Green" } elseif ($cleanlinessScore -ge 70) { "Yellow" } else { "Red" })
        Write-Host "[consciousness] Patterns recorded in $ConsciousnessFile" -ForegroundColor Cyan
        
        # Show top patterns
        if ($duplicateCount -gt 0) { Write-Host "  - Duplicate code: ${duplicateCount}x" -ForegroundColor Gray }
        if ($unusedImportCount -gt 0) { Write-Host "  - Unused imports: ${unusedImportCount}x" -ForegroundColor Gray }
        if ($undefinedCount -gt 0) { Write-Host "  - Undefined symbols: ${undefinedCount}x" -ForegroundColor Gray }
    }
} else {
    Write-Host "[consciousness] Insights file not found: $InsightsPath" -ForegroundColor Red
}

Pop-Location
