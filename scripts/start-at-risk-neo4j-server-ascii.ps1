# PowerShell script for starting Neo4j-based at-risk server
# Launches standalone HTTP/WebSocket server for at-risk emotional connections
# with Neo4j instead of in-memory storage

# Force UTF-8 for stable console and subprocess I/O (Windows PowerShell)
chcp 65001 > $null
[Console]::OutputEncoding = [System.Text.Encoding]::UTF8
[Console]::InputEncoding  = [System.Text.Encoding]::UTF8
$OutputEncoding = [System.Text.UTF8Encoding]::new()
$env:PYTHONIOENCODING = 'utf-8'
$env:PYTHONUTF8 = '1'

# Setting environment variables for Neo4j connection
$env:LIMINAL_USE_NEO4J = "1"
$env:LIMINAL_NEO4J_URI = "bolt://localhost:7687"
$env:LIMINAL_NEO4J_USER = "neo4j"
$env:LIMINAL_NEO4J_PASSWORD = "NewStrongPass123!"

Write-Host "Starting Neo4j-based REST API server for at-risk connections" -ForegroundColor Cyan

# Determine DB type based on environment variable
if ($env:LIMINAL_USE_NEO4J -eq "1") {
    Write-Host "Database: Neo4j ($env:LIMINAL_NEO4J_URI)" -ForegroundColor Green
} else {
    Write-Host "Database: In-Memory (local)" -ForegroundColor Yellow
}

# Check Neo4j availability if used
if ($env:LIMINAL_USE_NEO4J -eq "1") {
    Write-Host "Checking Neo4j connection..." -ForegroundColor Cyan
    
    try {
        # Create temporary script for connection check
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

        # Run check
        $result = python $tempFile
        $exitCode = $LASTEXITCODE
        
        # Remove temporary file
        Remove-Item $tempFile -ErrorAction SilentlyContinue
        
        if ($exitCode -eq 0) {
            Write-Host "Neo4j connection successful" -ForegroundColor Green
        } else {
            Write-Host "Neo4j connection error (no auto-fallback):" -ForegroundColor Red
            Write-Host $result -ForegroundColor DarkYellow
            Write-Host "Server will start; UI will show Neo4j init error banner. Use Toggle Neo4j in UI to retry." -ForegroundColor Yellow
            # Do NOT flip LIMINAL_USE_NEO4J here; allow backend to surface error explicitly
        }
    } catch {
        Write-Host "Error checking Neo4j connection (no auto-fallback): $_" -ForegroundColor Red
        Write-Host "Server will start; UI will show Neo4j init error banner." -ForegroundColor Yellow
        # Do NOT flip LIMINAL_USE_NEO4J here
    }
}

# Start Uvicorn server
try {
    Write-Host "Starting server on http://127.0.0.1:8080/at-risk..." -ForegroundColor Cyan
    
    # Set proper Python path to find the liminal module (src layout)
    $srcPath = (Resolve-Path "$PSScriptRoot\..\src").Path
    $paths = ($env:PYTHONPATH -split ';') | Where-Object { $_ -and $_.Trim() -ne '' }
    if (-not ($paths -contains $srcPath)) {
        $env:PYTHONPATH = "$srcPath;" + ($env:PYTHONPATH)
    }
    
    # Run the server via uvicorn CLI with auto-reload for development
    python -m uvicorn liminal.at_risk_server_neo4j:app --host 127.0.0.1 --port 8080 --reload
} catch {
    Write-Host "Server start error: $_" -ForegroundColor Red
}

Write-Host "Server stopped" -ForegroundColor Cyan
