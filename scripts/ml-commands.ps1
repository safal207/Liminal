# ML Infrastructure Test Commands
# Run these commands one by one to test components

# Clean environment
Write-Host "Cleaning previous containers..." -ForegroundColor Cyan
docker rm -f ml-redis ml-prometheus ml-backend ml-kenning 2>$null

# 1. Start Redis
Write-Host "Starting Redis..." -ForegroundColor Green
docker run -d --name ml-redis -p 6379:6379 redis:latest

# Wait a bit
Start-Sleep -Seconds 5

# Check Redis
Write-Host "Checking Redis..." -ForegroundColor Cyan
docker ps --filter "name=ml-redis" --format "{{.Status}}"
docker exec ml-redis redis-cli ping

# 2. Setup Prometheus
Write-Host "Creating Prometheus config..." -ForegroundColor Green
$promConfig = @"
global:
  scrape_interval: 15s

scrape_configs:
  - job_name: 'prometheus'
    static_configs:
      - targets: ['localhost:9090']
  
  - job_name: 'backend'
    static_configs:
      - targets: ['ml-backend:8000']
"@

# Create dir and config
New-Item -Path "c:/Users/safal/OneDrive/Documente/GitHub/resonance-liminal/prometheus" -ItemType Directory -Force | Out-Null
$promConfig | Out-File -FilePath "c:/Users/safal/OneDrive/Documente/GitHub/resonance-liminal/prometheus/prometheus.yml" -Encoding utf8

# Start Prometheus
Write-Host "Starting Prometheus..." -ForegroundColor Green
docker run -d --name ml-prometheus -p 9090:9090 -v "c:/Users/safal/OneDrive/Documente/GitHub/resonance-liminal/prometheus/prometheus.yml:/etc/prometheus/prometheus.yml" prom/prometheus:latest

# Wait a bit
Start-Sleep -Seconds 10

# Check Prometheus
Write-Host "Checking Prometheus..." -ForegroundColor Cyan
docker ps --filter "name=ml-prometheus" --format "{{.Status}}"

# 3. Start Backend (uncomment when ready to test)
Write-Host "Starting Backend with ML support..." -ForegroundColor Green
# Note: Replace resonance-liminal-backend:latest with your actual image
# docker run -d --name ml-backend -p 8000:8000 --link ml-redis:redis --link ml-prometheus:prometheus -e REDIS_HOST=ml-redis -e PROMETHEUS_URL=http://ml-prometheus:9090 -e TEST_MODE=true -e ML_ENABLED=true -e OPENAI_API_KEY=demo_key -e ANTHROPIC_API_KEY=demo_key resonance-liminal-backend:latest

# Output available services
Write-Host "`nAvailable Services:" -ForegroundColor Cyan
Write-Host "• Redis:            localhost:6379" -ForegroundColor Green
Write-Host "• Prometheus:       http://localhost:9090" -ForegroundColor Green
# Write-Host "• Backend API:      http://localhost:8000/docs" -ForegroundColor Green

# Cleanup instructions
Write-Host "`nTo stop and remove all containers:" -ForegroundColor Yellow
Write-Host "docker rm -f ml-redis ml-prometheus ml-backend ml-kenning" -ForegroundColor Yellow
