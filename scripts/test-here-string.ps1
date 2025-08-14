# Test script for PowerShell here-strings
Write-Host "Testing here-strings in PowerShell"

# This is the correct way to define a here-string in PowerShell
$pythonCode = @'
import sys
import os
from pathlib import Path

print("Hello from Python")
'@

# Write the Python code to a temporary file
$tempFile = [System.IO.Path]::GetTempFileName() + ".py"
$pythonCode | Out-File -FilePath $tempFile -Encoding utf8

# Run the Python script and capture the output
Write-Host "Running Python code..."
$output = python $tempFile 2>&1
$exitCode = $LASTEXITCODE

# Clean up
Remove-Item $tempFile

# Show results
Write-Host "Python exit code: $exitCode"
Write-Host "Python output: $output"
