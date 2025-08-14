# Load Testing Framework for LIMINAL API

This directory contains a comprehensive load testing framework for the LIMINAL API, built with Locust and designed following industry best practices from Silicon Valley tech companies.

## Features

- 🚀 Multiple test profiles (smoke, load, stress, soak)
- 📊 Real-time metrics and monitoring
- 📈 Interactive and headless testing modes
- 🔍 Detailed test reports and visualizations
- 🔄 WebSocket and HTTP/HTTPS support
- 📦 Containerized testing environment

## Prerequisites

- Python 3.8+
- Docker (optional, for containerized testing)
- LIMINAL API server running (default: http://localhost:8000)

## Quick Start

### 1. Install Dependencies

```bash
# Create and activate virtual environment
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install dependencies
pip install -r requirements.txt
```

### 2. Run a Smoke Test

```bash
python run_load_test.py smoke --users 10
```

### 3. Run a Full Load Test

```bash
python run_load_test.py load --users 100
```

## Test Profiles

| Profile | Users | Spawn Rate | Duration | Purpose |
|---------|-------|------------|----------|----------|
| `smoke` | 5-10  | 1/s        | 1m       | Quick verification |
| `load`  | 100   | 10/s       | 5m       | Normal load |
| `stress`| 1000  | 50/s       | 10m      | System limits |
| `soak`  | 50    | 5/s        | 1h       | Long-running test |

## Advanced Usage

### Custom Test

```bash
python run_load_test.py custom -u 500 -r 20 -d 10m
```

### With Web UI

```bash
python run_load_test.py custom -u 50 -r 5 -d 5m --ui
```
Then open http://localhost:8089 in your browser.

### Using Docker

```bash
# Build the image
docker build -t liminal-load-test .

# Run a test
docker run -it --rm --network="host" liminal-load-test smoke --users 10
```

## Test Scenarios

### WebSocket Operations
- Connection establishment and heartbeat
- Real-time message broadcasting
- Connection stability and reconnection

### Memory Operations
- Creating new memories
- Retrieving memory lists
- Memory update notifications
- Concurrent access patterns

## Results and Analysis

Test results are saved in the `reports/` directory with the following structure:

```
reports/
  ├── smoke_12345678/
  │   ├── report.html       # HTML report
  │   ├── results_stats.csv # Request statistics
  │   └── results_failures.csv # Failure details
  └── load_12345678/
      └── ...
```

## Monitoring

### Prometheus Metrics

Enable Prometheus metrics with:
```bash
ENABLE_PROMETHEUS=true python run_load_test.py load
```

Metrics will be available at http://localhost:9090

### Key Metrics
- Request rate (RPS)
- Response times (p50, p90, p99)
- Error rates
- Active users
- WebSocket connections

## Best Practices

1. **Start Small**: Always begin with a smoke test
2. **Monitor Resources**: Keep an eye on server metrics
3. **Compare Results**: Track performance over time
4. **Test Realistic Scenarios**: Mimic production traffic patterns
5. **Isolate Tests**: Test one component at a time

## Troubleshooting

- **Connection Issues**: Verify the target host is reachable
- **High Failure Rate**: Check server logs and resource usage
- **Performance Bottlenecks**: Use the profiler to identify slow operations

## License

This project is part of the LIMINAL platform and follows the same license terms.
