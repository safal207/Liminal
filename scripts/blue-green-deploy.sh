#!/bin/bash

# LIMINAL Production Deployment Script
# Implements blue-green deployment strategy for zero-downtime deployments

set -euo pipefail

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
DEPLOYMENT_CONFIG="${PROJECT_ROOT}/config/deployment.env"
COMPOSE_FILE="${PROJECT_ROOT}/docker-compose.yml"
BACKUP_DIR="/opt/liminal/backups"
LOG_FILE="/var/log/liminal-deployment.log"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging function
log() {
    echo -e "${2:-$NC}[$(date +'%Y-%m-%d %H:%M:%S')] $1${NC}" | tee -a "$LOG_FILE"
}

error() {
    log "ERROR: $1" "$RED"
    exit 1
}

warning() {
    log "WARNING: $1" "$YELLOW"
}

info() {
    log "INFO: $1" "$BLUE"
}

success() {
    log "SUCCESS: $1" "$GREEN"
}

# Load configuration
if [[ -f "$DEPLOYMENT_CONFIG" ]]; then
    # shellcheck source=/dev/null
    source "$DEPLOYMENT_CONFIG"
else
    warning "Deployment config not found, using defaults"
fi

# Default values
ENVIRONMENT="${ENVIRONMENT:-production}"
BACKUP_ENABLED="${BACKUP_ENABLED:-true}"
HEALTH_CHECK_TIMEOUT="${HEALTH_CHECK_TIMEOUT:-300}"
ROLLBACK_ON_FAILURE="${ROLLBACK_ON_FAILURE:-true}"
NOTIFICATION_WEBHOOK="${NOTIFICATION_WEBHOOK:-}"

# Pre-deployment checks
pre_deployment_checks() {
    info "Running pre-deployment checks..."
    
    # Check if running as root
    if [[ $EUID -eq 0 ]]; then
        error "This script should not be run as root"
    fi
    
    # Check Docker and Docker Compose
    if ! command -v docker &> /dev/null; then
        error "Docker is not installed"
    fi
    
    if ! command -v docker-compose &> /dev/null; then
        error "Docker Compose is not installed"
    fi
    
    # Check if Docker daemon is running
    if ! docker info &> /dev/null; then
        error "Docker daemon is not running"
    fi
    
    # Check available disk space (minimum 10GB)
    available_space=$(df / | awk 'NR==2 {print $4}')
    if [[ $available_space -lt 10485760 ]]; then
        error "Insufficient disk space. At least 10GB required"
    fi
    
    # Check if required environment variables are set
    required_vars=("POSTGRES_PASSWORD" "NEO4J_PASSWORD" "JWT_SECRET_KEY")
    for var in "${required_vars[@]}"; do
        if [[ -z "${!var:-}" ]]; then
            error "Required environment variable $var is not set"
        fi
    done
    
    success "Pre-deployment checks passed"
}

# Backup databases
backup_databases() {
    if [[ "$BACKUP_ENABLED" != "true" ]]; then
        info "Backup disabled, skipping..."
        return 0
    fi
    
    info "Creating database backups..."
    
    local backup_timestamp=$(date +%Y%m%d_%H%M%S)
    local backup_path="${BACKUP_DIR}/${backup_timestamp}"
    
    mkdir -p "$backup_path"
    
    # Backup PostgreSQL
    info "Backing up PostgreSQL database..."
    docker-compose exec -T postgres pg_dump -U liminal liminal > "${backup_path}/postgres_backup.sql" || {
        error "Failed to backup PostgreSQL database"
    }
    
    # Backup Neo4j
    info "Backing up Neo4j database..."
    docker-compose exec -T neo4j neo4j-admin database dump --to-path=/tmp neo4j || {
        warning "Neo4j backup may have failed, continuing..."
    }
    docker cp liminal-neo4j:/tmp/neo4j.dump "${backup_path}/neo4j_backup.dump" 2>/dev/null || {
        warning "Could not copy Neo4j backup"
    }
    
    # Backup Redis
    info "Backing up Redis database..."
    docker-compose exec -T redis redis-cli BGSAVE || {
        warning "Redis backup may have failed, continuing..."
    }
    docker cp liminal-redis:/data/dump.rdb "${backup_path}/redis_backup.rdb" 2>/dev/null || {
        warning "Could not copy Redis backup"
    }
    
    # Compress backups
    tar -czf "${backup_path}.tar.gz" -C "$BACKUP_DIR" "$backup_timestamp"
    rm -rf "$backup_path"
    
    # Keep only last 5 backups
    cd "$BACKUP_DIR" && ls -t *.tar.gz | tail -n +6 | xargs -r rm --
    
    success "Database backups completed"
}

# Health check function
health_check() {
    local service_url="$1"
    local timeout="$2"
    local elapsed=0
    
    info "Performing health check on $service_url"
    
    while [[ $elapsed -lt $timeout ]]; do
        if curl -f -s "$service_url/health" > /dev/null; then
            success "Health check passed for $service_url"
            return 0
        fi
        
        sleep 5
        elapsed=$((elapsed + 5))
        echo -n "."
    done
    
    echo
    error "Health check failed for $service_url after ${timeout}s"
}

# Blue-green deployment
blue_green_deploy() {
    info "Starting blue-green deployment..."
    
    # Determine current and new environments
    if docker-compose ps | grep -q "liminal-backend-blue"; then
        CURRENT_ENV="blue"
        NEW_ENV="green"
    else
        CURRENT_ENV="green"
        NEW_ENV="blue"
    fi
    
    info "Current environment: $CURRENT_ENV, deploying to: $NEW_ENV"
    
    # Pull latest images
    info "Pulling latest Docker images..."
    docker-compose pull liminal-backend
    
    # Start new environment
    info "Starting $NEW_ENV environment..."
    
    # Create new compose file for blue-green
    cat > docker-compose.${NEW_ENV}.yml << EOF
version: '3.8'
services:
  liminal-backend-${NEW_ENV}:
    image: \${LIMINAL_BACKEND_IMAGE:-ghcr.io/your-username/resonance-liminal/liminal-backend:latest}
    container_name: liminal-backend-${NEW_ENV}
    restart: unless-stopped
    ports:
      - "800${NEW_ENV == "blue" && echo "1" || echo "2"}:8000"
    environment:
      - ENV=production
      - DATABASE_URL=postgresql://liminal:\${POSTGRES_PASSWORD}@postgres:5432/liminal
      - NEO4J_URI=bolt://neo4j:7687
      - NEO4J_USER=neo4j
      - NEO4J_PASSWORD=\${NEO4J_PASSWORD}
      - REDIS_URL=redis://redis:6379
      - JWT_SECRET_KEY=\${JWT_SECRET_KEY}
    volumes:
      - liminal_logs:/app/logs
      - liminal_data:/app/data
    depends_on:
      - postgres
      - neo4j
      - redis
    networks:
      - liminal-network
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:8000/health"]
      interval: 30s
      timeout: 10s
      retries: 3

networks:
  liminal-network:
    external: true

volumes:
  liminal_logs:
    external: true
  liminal_data:
    external: true
EOF
    
    # Start new environment
    docker-compose -f "docker-compose.${NEW_ENV}.yml" up -d
    
    # Wait for new environment to be ready
    local new_port=$([[ "$NEW_ENV" == "blue" ]] && echo "8001" || echo "8002")
    health_check "http://localhost:${new_port}" "$HEALTH_CHECK_TIMEOUT"
    
    # Switch traffic (update load balancer)
    info "Switching traffic to $NEW_ENV environment..."
    update_load_balancer "$NEW_ENV" "$new_port"
    
    # Wait a bit for traffic to settle
    sleep 30
    
    # Verify new deployment
    health_check "http://localhost" "$HEALTH_CHECK_TIMEOUT"
    
    # Stop old environment
    if [[ -n "$CURRENT_ENV" ]]; then
        info "Stopping $CURRENT_ENV environment..."
        docker-compose -f "docker-compose.${CURRENT_ENV}.yml" down
        rm -f "docker-compose.${CURRENT_ENV}.yml"
    fi
    
    success "Blue-green deployment completed successfully"
}

# Update load balancer configuration
update_load_balancer() {
    local environment="$1"
    local port="$2"
    
    info "Updating load balancer for environment: $environment"
    
    # Update Nginx configuration
    cat > /etc/nginx/sites-available/liminal << EOF
upstream liminal_backend {
    server localhost:${port} max_fails=3 fail_timeout=30s;
}

server {
    listen 80;
    listen 443 ssl http2;
    server_name api.liminal.consciousness;
    
    ssl_certificate /etc/ssl/certs/liminal.crt;
    ssl_certificate_key /etc/ssl/private/liminal.key;
    
    location / {
        proxy_pass http://liminal_backend;
        proxy_set_header Host \$host;
        proxy_set_header X-Real-IP \$remote_addr;
        proxy_set_header X-Forwarded-For \$proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto \$scheme;
        
        # WebSocket support
        proxy_http_version 1.1;
        proxy_set_header Upgrade \$http_upgrade;
        proxy_set_header Connection "upgrade";
        
        # Timeout settings
        proxy_connect_timeout 60s;
        proxy_send_timeout 60s;
        proxy_read_timeout 60s;
    }
    
    location /health {
        proxy_pass http://liminal_backend/health;
        access_log off;
    }
}
EOF
    
    # Test Nginx configuration
    nginx -t || error "Invalid Nginx configuration"
    
    # Reload Nginx
    systemctl reload nginx || error "Failed to reload Nginx"
    
    success "Load balancer updated successfully"
}

# Rollback function
rollback() {
    error "Deployment failed, initiating rollback..."
    
    info "Restoring from latest backup..."
    
    # Find latest backup
    local latest_backup=$(ls -t "${BACKUP_DIR}"/*.tar.gz | head -n1)
    
    if [[ -n "$latest_backup" ]]; then
        info "Restoring from backup: $latest_backup"
        
        # Extract backup
        local backup_dir="${BACKUP_DIR}/restore_$(date +%s)"
        mkdir -p "$backup_dir"
        tar -xzf "$latest_backup" -C "$backup_dir"
        
        # Restore PostgreSQL
        if [[ -f "${backup_dir}/*/postgres_backup.sql" ]]; then
            docker-compose exec -T postgres psql -U liminal -d liminal < "${backup_dir}"/*/postgres_backup.sql
        fi
        
        # Restore other databases as needed...
        
        # Clean up
        rm -rf "$backup_dir"
        
        success "Rollback completed"
    else
        error "No backups found for rollback"
    fi
}

# Send notification
send_notification() {
    local status="$1"
    local message="$2"
    
    if [[ -n "$NOTIFICATION_WEBHOOK" ]]; then
        local color=$([[ "$status" == "success" ]] && echo "good" || echo "danger")
        local emoji=$([[ "$status" == "success" ]] && echo "✅" || echo "❌")
        
        curl -X POST -H 'Content-type: application/json' \
            --data "{\"text\":\"$emoji LIMINAL Deployment $status: $message\"}" \
            "$NOTIFICATION_WEBHOOK" || warning "Failed to send notification"
    fi
}

# Main deployment function
deploy() {
    local start_time=$(date +%s)
    
    info "Starting LIMINAL production deployment..."
    
    # Trap for cleanup on failure
    trap 'rollback' ERR
    
    # Run deployment steps
    pre_deployment_checks
    backup_databases
    blue_green_deploy
    
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))
    
    success "Deployment completed successfully in ${duration}s"
    send_notification "success" "Deployment completed in ${duration}s"
}

# Rollback to previous version
rollback_deployment() {
    info "Starting deployment rollback..."
    rollback
}

# Show deployment status
status() {
    info "LIMINAL Deployment Status:"
    echo
    
    # Check running containers
    docker-compose ps
    echo
    
    # Check health endpoints
    if curl -f -s http://localhost/health > /dev/null; then
        success "Application is healthy"
    else
        error "Application health check failed"
    fi
    
    # Show recent logs
    info "Recent application logs:"
    docker-compose logs --tail=20 liminal-backend
}

# Clean up old Docker images and volumes
cleanup() {
    info "Cleaning up old Docker resources..."
    
    # Remove unused images
    docker image prune -f
    
    # Remove unused volumes (be careful with this in production)
    docker volume prune -f
    
    # Remove old backups (keep last 10)
    cd "$BACKUP_DIR" && ls -t *.tar.gz | tail -n +11 | xargs -r rm --
    
    success "Cleanup completed"
}

# Show help
show_help() {
    cat << EOF
LIMINAL Production Deployment Script

Usage: $0 [COMMAND]

Commands:
    deploy      - Deploy new version using blue-green strategy
    rollback    - Rollback to previous version
    status      - Show deployment status
    cleanup     - Clean up old Docker resources
    help        - Show this help message

Examples:
    $0 deploy
    $0 status
    $0 rollback

Configuration:
    Set environment variables in: $DEPLOYMENT_CONFIG
    
    Required variables:
        POSTGRES_PASSWORD
        NEO4J_PASSWORD
        JWT_SECRET_KEY
    
    Optional variables:
        ENVIRONMENT (default: production)
        BACKUP_ENABLED (default: true)
        HEALTH_CHECK_TIMEOUT (default: 300)
        ROLLBACK_ON_FAILURE (default: true)
        NOTIFICATION_WEBHOOK
EOF
}

# Main script logic
case "${1:-}" in
    deploy)
        deploy
        ;;
    rollback)
        rollback_deployment
        ;;
    status)
        status
        ;;
    cleanup)
        cleanup
        ;;
    help|--help|-h)
        show_help
        ;;
    *)
        echo "Unknown command: ${1:-}"
        echo "Use '$0 help' for usage information"
        exit 1
        ;;
esac