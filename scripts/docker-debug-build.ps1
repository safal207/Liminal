# DOCKER DEBUG BUILD SCRIPT for Resonance Liminal
# Script for debugging Docker build issues with detailed progress output

# Colors for output
$Green = [System.ConsoleColor]::Green
$Cyan = [System.ConsoleColor]::Cyan
$Yellow = [System.ConsoleColor]::Yellow
$Red = [System.ConsoleColor]::Red

# Function for status output
function Write-Status($message, $color = $Cyan) {
    Write-Host $message -ForegroundColor $color
}

# Enable BuildKit for better performance
$env:DOCKER_BUILDKIT = 1

# Project directory
$projectDir = Split-Path -Parent (Split-Path -Parent $MyInvocation.MyCommand.Path)
Set-Location $projectDir
Write-Status "Project directory: $projectDir"

# Show Docker info
Write-Status "Docker info:" $Green
docker info
Write-Status "Docker disk usage:" $Green
docker system df

# Check backend context size
Write-Status "Checking backend directory size..." $Yellow
$backendSize = (Get-ChildItem -Path ./backend -Recurse | Measure-Object -Property Length -Sum).Sum / 1MB
Write-Status "Backend directory size: $([math]::Round($backendSize, 2)) MB" $Cyan

# Check requirements.txt
Write-Status "Checking requirements.txt..." $Yellow
if (Test-Path "./backend/requirements.txt") {
    $reqCount = (Get-Content "./backend/requirements.txt" | Measure-Object -Line).Lines
    Write-Status "requirements.txt has $reqCount packages" $Cyan
    
    # Check for heavy ML packages
    $mlPackages = @("tensorflow", "torch", "numpy", "scipy", "pandas", "scikit-learn", "transformers", "keras")
    foreach ($pkg in $mlPackages) {
        if (Get-Content "./backend/requirements.txt" | Select-String -Pattern $pkg) {
            Write-Status "Found heavy ML package: $pkg" $Yellow
        }
    }
} else {
    Write-Status "requirements.txt not found!" $Red
}

# Create .dockerignore if not exists
if (-not (Test-Path "./backend/.dockerignore")) {
    Write-Status "Creating optimized .dockerignore file..." $Yellow
    @"
__pycache__
*.pyc
*.pyo
*.pyd
.Python
.pytest_cache
.coverage
htmlcov
.tox
.nox
.hypothesis
.mypy_cache
.dmypy.json
dmypy.json
*.so
.Python
env/
venv/
ENV/
.venv/
.git
.gitignore
.idea
*.egg-info/
dist/
build/
"@ | Out-File -FilePath "./backend/.dockerignore" -Encoding utf8
    Write-Status "Created .dockerignore to reduce context size" $Green
}

# Run build with debug flags
Write-Status "Starting DEBUG build with detailed logging..." $Green
Write-Status "This will show progress of each step including pip install" $Yellow
Write-Status "Building optimized image..." $Cyan

# Execute build with progress and verbose flags
docker build --progress=plain --no-cache -t resonance-liminal-backend:debug -f backend/Dockerfile.basic backend

if ($LASTEXITCODE -eq 0) {
    Write-Status "Docker build SUCCESSFUL!" $Green
    
    # Show built image info
    docker images resonance-liminal-backend:debug
    
    Write-Status "`nTo run the image:" $Green
    Write-Status "docker run -p 8000:8000 resonance-liminal-backend:debug" $Cyan
} else {
    Write-Status "Docker build FAILED with error code $LASTEXITCODE" $Red
    Write-Status "`nPossible solutions:" $Yellow
    Write-Status "1. Check network connectivity to Docker Hub and PyPI" $Cyan
    Write-Status "2. Try using --network=host flag if behind proxy" $Cyan
    Write-Status "3. Inspect the requirements.txt for problematic packages" $Cyan
    Write-Status "4. Consider using Dockerfile.cached for better performance" $Cyan
}
