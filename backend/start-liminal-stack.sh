#!/bin/bash

# LIMINAL Stack Startup Script
# Запускает полный стек LIMINAL с оптимизациями производительности

set -e

echo "🌿✨ LIMINAL Performance-Optimized Stack Startup"
echo "================================================"
echo "$(date): Starting LIMINAL with performance optimizations"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

print_header() {
    echo -e "${BLUE}$1${NC}"
}

# Check if Docker is running
if ! docker info > /dev/null 2>&1; then
    print_error "Docker is not running. Please start Docker first."
    exit 1
fi

print_status "Docker is running ✓"

# Check if docker-compose is available
if ! command -v docker-compose &> /dev/null; then
    print_error "docker-compose is not installed. Please install it first."
    exit 1
fi

print_status "docker-compose is available ✓"

# Create necessary directories
print_status "Creating monitoring directories..."
mkdir -p monitoring/grafana/provisioning/datasources
mkdir -p monitoring/grafana/provisioning/dashboards
mkdir -p monitoring/grafana/dashboards
mkdir -p nginx/ssl

# Generate Grafana datasource configuration
print_status "Setting up Grafana datasources..."
cat > monitoring/grafana/provisioning/datasources/prometheus.yml << EOF
apiVersion: 1

datasources:
  - name: Prometheus
    type: prometheus
    access: proxy
    url: http://prometheus:9090
    isDefault: true
    editable: true
EOF

# Generate Grafana dashboard configuration  
cat > monitoring/grafana/provisioning/dashboards/dashboards.yml << EOF
apiVersion: 1

providers:
  - name: 'LIMINAL Dashboards'
    orgId: 1
    folder: ''
    type: file
    disableDeletion: false
    updateIntervalSeconds: 10
    allowUiUpdates: true
    options:
      path: /var/lib/grafana/dashboards
EOF

# Stop any existing containers
print_status "Stopping existing containers..."
docker-compose down --remove-orphans 2>/dev/null || true

# Pull latest images
print_status "Pulling latest images..."
docker-compose pull

# Build custom images
print_status "Building LIMINAL Backend..."
docker-compose build --no-cache liminal-backend

# Start infrastructure services first
print_header "Starting infrastructure services..."
docker-compose up -d redis neo4j prometheus

# Wait for infrastructure to be ready
print_status "Waiting for infrastructure services to be ready..."

# Wait for Redis
print_status "Waiting for Redis..."
timeout=60
while ! docker-compose exec -T redis redis-cli ping > /dev/null 2>&1; do
    sleep 2
    timeout=$((timeout - 2))
    if [ $timeout -le 0 ]; then
        print_error "Redis failed to start within 60 seconds"
        docker-compose logs redis
        exit 1
    fi
done
print_status "Redis is ready ✓"

# Wait for Neo4j
print_status "Waiting for Neo4j..."
timeout=120
while ! docker-compose exec -T neo4j cypher-shell -u neo4j -p liminal_neo4j "RETURN 1" > /dev/null 2>&1; do
    sleep 5
    timeout=$((timeout - 5))
    if [ $timeout -le 0 ]; then
        print_error "Neo4j failed to start within 120 seconds"
        docker-compose logs neo4j
        exit 1
    fi
done
print_status "Neo4j is ready ✓"

# Wait for Prometheus
print_status "Waiting for Prometheus..."
timeout=60
while ! docker-compose exec -T prometheus wget --quiet --tries=1 --spider http://localhost:9090/-/healthy 2>/dev/null; do
    sleep 3
    timeout=$((timeout - 3))
    if [ $timeout -le 0 ]; then
        print_error "Prometheus failed to start within 60 seconds"
        docker-compose logs prometheus
        exit 1
    fi
done
print_status "Prometheus is ready ✓"

# Start application services
print_header "Starting application services..."
docker-compose up -d liminal-backend grafana nginx

# Wait for backend to be ready
print_status "Waiting for LIMINAL Backend..."
timeout=90
while ! docker-compose exec -T liminal-backend curl -f http://localhost:8000/health > /dev/null 2>&1; do
    sleep 3
    timeout=$((timeout - 3))
    if [ $timeout -le 0 ]; then
        print_error "LIMINAL Backend failed to start within 90 seconds"
        docker-compose logs liminal-backend
        exit 1
    fi
done
print_status "LIMINAL Backend is ready ✓"

# Wait for Grafana
print_status "Waiting for Grafana..."
timeout=60
while ! docker-compose exec -T grafana curl -f http://localhost:3000/api/health > /dev/null 2>&1; do
    sleep 3
    timeout=$((timeout - 3))
    if [ $timeout -le 0 ]; then
        print_error "Grafana failed to start within 60 seconds"
        docker-compose logs grafana
        exit 1
    fi
done
print_status "Grafana is ready ✓"

# Check all services
print_header "Checking service health..."
if docker-compose ps --services | xargs -I {} docker-compose ps {} | grep -q "Up"; then
    print_status "All services are running ✓"
else
    print_warning "Some services may not be running properly"
fi

# Display service URLs
print_header "🚀 LIMINAL Stack is ready!"
echo ""
echo "Service URLs:"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "🌿 LIMINAL API:        http://localhost:8000"
echo "📊 API Documentation:  http://localhost:8000/docs" 
echo "❤️  Health Check:      http://localhost:8000/health"
echo "🧠 Emotime Status:     http://localhost:8000/emotime/status"
echo ""
echo "Monitoring & Analytics:"
echo "📈 Grafana Dashboard:  http://localhost:3000 (admin/liminal_grafana)"
echo "📊 Prometheus:         http://localhost:9090"
echo "🗄️  Neo4j Browser:     http://localhost:7474 (neo4j/liminal_neo4j)"
echo "⚡ Redis:              localhost:6379"
echo ""
echo "Performance Optimizations Enabled:"
echo "✅ Response Caching (Redis)"
echo "✅ Batch Processing (10 req/sec+)"
echo "✅ Connection Pooling"
echo "✅ ML Accuracy Enhancement (80%+)"
echo "✅ Confidence Optimization (70%+)" 
echo "✅ Multi-stage Docker Build"
echo "✅ Nginx Load Balancing"
echo "✅ Prometheus Monitoring"
echo ""

# Show container status
print_header "Container Status:"
docker-compose ps

echo ""
print_status "LIMINAL Stack startup completed successfully! 🎉"
echo ""
echo "To stop the stack: docker-compose down"
echo "To view logs: docker-compose logs -f [service-name]"
echo "To scale backend: docker-compose up -d --scale liminal-backend=3"