# Performance Testing Guide

This document describes how to run and analyze performance tests for the LIMINAL API.

## Prerequisites

- Python 3.8+
- Required packages: `pip install -r requirements.txt`
- Prometheus (for metrics collection, optional)

## Test Configuration

Edit `performance_config.yaml` to configure test scenarios, thresholds, and metrics.

## Running Tests

### Single Test Scenario

```bash
python run_performance_analysis.py --test-type load --users 100 --duration 5m
```

### Test Suite

Run all scenarios:
```bash
python run_test_suite.py
```

Run specific scenarios:
```bash
python run_test_suite.py smoke load
```

## Test Results

- Reports are saved in `performance_reports/`
- Each test run creates a timestamped directory
- View `index.html` in the report directory for results

## Analyzing Results

1. **HTML Report**: Open `index.html` in a web browser
2. **Metrics**: View Prometheus metrics at `http://localhost:9090`
3. **Logs**: Check `performance_suite.log` for detailed logs

## Test Types

- **smoke**: Quick verification (10 users, 1 minute)
- **load**: Normal operation (100 users, 5 minutes)
- **stress**: System limits (500 users, 10 minutes)
- **soak**: Long-running stability (50 users, 1 hour)

## Custom Scenarios

Edit `performance_config.yaml` to add custom test scenarios:

```yaml
test_scenarios:
  my_test:
    users: 200
    spawn_rate: 20
    duration: "10m"
    description: "Custom test scenario"
```

## Best Practices

1. Start with smoke tests before running full suite
2. Run tests in a staging environment first
3. Monitor system resources during tests
4. Compare results across test runs
5. Set appropriate thresholds in the config file

## Troubleshooting

- **Prometheus connection issues**: Ensure Prometheus is running and accessible
- **Test failures**: Check logs for detailed error messages
- **Performance issues**: Review system metrics and adjust test parameters

## License

MIT
