# Autonomous Workflow Playbook

The LIMINAL platform now exposes key pipelines as declarative [Prefect 2](https://docs.prefect.io/latest/) flows.  This keeps
agent orchestration observable and makes it easy to integrate with external orchestrators.

## Dependency Injection & Runtime Setup

* Core in-memory components (`RealityWebInMemory`, `SystemBreath`, workflow audit logger) are supplied through
  `liminal.di.LiminalContainer`.  The container can be overridden in tests via `liminal.di.set_container`.
* Backend services (Neo4j, ML, memory timeline, WebSocket manager) are resolved through `backend.app.container.AppContainer`.
  Accessors in `backend.app.dependencies` now resolve providers instead of touching module-level singletons.

When the Prefect flows run they automatically request a `WorkflowAuditLogger` from the container so any custom wiring or
structlog configuration is respected.

## Running Workflows

### Top at-risk relationships

```bash
# Show a formatted table
python scripts/top_at_risk.py --limit 10

# Emit raw JSON
python scripts/top_at_risk.py --limit 20 --json
```

Inside Python the same flow can be invoked directly:

```python
from liminal.workflows import top_at_risk_flow

edges = top_at_risk_flow(limit=8, threshold=0.35)
for edge in edges:
    print(edge)
```

### Consciousness Cell agent

```bash
# Generate a report and persist to scripts/consciousness_insights_agent.md
python scripts/consciousness_cell.py

# Analyse a different checkout and keep the report in a custom location
python scripts/consciousness_cell.py /path/to/project --output-path /tmp/agent_report.md
```

Programmatic usage:

```python
from liminal.workflows import consciousness_cell_flow

result = consciousness_cell_flow(project_root="/workspace/Liminal", persist_report=False)
print(result["report"])
```

The returned dictionary includes `report_path`, the raw `report` text, and structured insight lists for past, present, and
future observations.

## Observability & Audit Trail

Workflow execution is captured by `liminal.observability.workflow.WorkflowAuditLogger` (structlog-based).  Every flow logs:

* `workflow.start` with parameters and run id.
* Intermediate `workflow.event` entries (e.g. GraphQL fallback triggers, report generation, insight totals).
* `workflow.complete` with duration and a summarised payload (preview of reports, counts).
* `workflow.failure` including exception details when errors propagate.

Configure structlog handlers (console, JSON, FluentBit, etc.) once and the audit logger will automatically use them.

## Launching Autonomous Agents

1. Ensure dependencies are installed (`prefect`, `dependency-injector`, `structlog`).
2. Wire optional overrides into `liminal.di.set_container` (e.g. custom reality web implementation or alternate audit sinks).
3. Trigger the relevant Prefect flow (via CLI or Python API).
4. Monitor audit output or tail log files to trace behaviour.

Example: orchestrate Consciousness Cell from another agent task:

```python
from liminal.di import get_container
from liminal.workflows import consciousness_cell_flow

container = get_container()
audit = container.workflow_audit_logger()
run = consciousness_cell_flow(project_root="/srv/liminal")
audit.log_event("agent_orchestrator", "manual", "subflow_complete", report=run["report_path"])
```

This pattern enables higher-level planners to launch autonomous analytical sweeps while keeping a verifiable audit trail.
