"""Declarative Prefect flows for LIMINAL workflows."""
from __future__ import annotations

import json
import os
import time
import uuid
from dataclasses import asdict
from pathlib import Path
from typing import Any, Dict, List, Mapping

from prefect import flow, get_run_logger, task

from ..di import get_container
from .consciousness_agent import ConsciousnessCell


def _coerce_threshold(threshold: float | None) -> float | None:
    if threshold is None:
        return None
    return float(threshold)


@task(name="fetch-top-at-risk")
def fetch_top_at_risk(limit: int, threshold: float | None = None) -> List[Dict[str, Any]]:
    logger = get_run_logger()
    thresh = _coerce_threshold(threshold)
    if thresh is not None:
        os.environ["LIMINAL_HEALTH_THRESHOLD"] = str(thresh)

    query = "query($n:Int!){ topAtRiskEdges(limit:$n){ sourceId targetId score advice rationale }}"
    variables = {"n": int(limit)}

    try:
        from ..graphql_schema import schema as graphql_schema  # type: ignore

        result = graphql_schema.execute_sync(query, variable_values=variables)
        if result.errors:
            raise RuntimeError(result.errors)
        assert result.data is not None
        rows = list(result.data["topAtRiskEdges"])  # type: ignore[index]
        logger.info("Fetched top-at-risk edges via in-process GraphQL", count=len(rows))
        return rows
    except Exception as exc:  # pragma: no cover - network fallback
        logger.warning("GraphQL execution failed, falling back to HTTP", exc_info=True)
        audit = get_container().workflow_audit_logger()
        audit.log_event(
            workflow="top_at_risk",
            run_id="fallback",
            message="graphql_unavailable",
            error=str(exc),
        )
        import urllib.error
        import urllib.request

        rest_base = os.getenv("LIMINAL_REST_URL", "http://127.0.0.1:8000")
        rest_url = f"{rest_base}/api/top-at-risk?limit={int(limit)}"
        try:
            with urllib.request.urlopen(rest_url, timeout=10) as resp:
                payload = json.loads(resp.read().decode("utf-8"))
        except urllib.error.URLError as url_exc:  # pragma: no cover
            raise RuntimeError(f"REST fallback failed: {url_exc}") from url_exc
        rows = list(payload.get("topAtRiskEdges", []))
        logger.info(
            "Fetched top-at-risk edges via REST fallback",
            count=len(rows),
            endpoint=rest_url,
        )
        return rows


@task(name="instantiate-consc-cell")
def build_consciousness_cell(project_root: str) -> ConsciousnessCell:
    logger = get_run_logger()
    cell = ConsciousnessCell(project_root)
    logger.info("Constructed ConsciousnessCell", project_root=project_root)
    return cell


@task(name="collect-consc-insights")
def collect_consciousness_insights(
    cell: ConsciousnessCell,
) -> Dict[str, List[Dict[str, Any]]]:
    logger = get_run_logger()
    past = cell.analyze_past()
    present = cell.analyze_present()
    future = cell.predict_future()
    logger.info(
        "Collected insights",
        past=len(past),
        present=len(present),
        future=len(future),
    )
    return {
        "past": [asdict(i) for i in past],
        "present": [asdict(i) for i in present],
        "future": [asdict(i) for i in future],
    }


@task(name="generate-consc-report")
def generate_consciousness_report(cell: ConsciousnessCell) -> str:
    report = cell.generate_comprehensive_report()
    logger = get_run_logger()
    logger.info("Generated comprehensive report", length=len(report))
    return report


@task(name="persist-consc-report")
def persist_consciousness_report(
    report: str,
    output_path: Path,
    enabled: bool = True,
) -> str:
    logger = get_run_logger()
    if not enabled:
        logger.info("Skipping report persistence", path=str(output_path))
        return str(output_path)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(report, encoding="utf-8")
    logger.info("Report written", path=str(output_path), size=len(report))
    return str(output_path)


@flow(name="top-at-risk-workflow")
def top_at_risk_flow(limit: int = 5, threshold: float | None = None) -> List[Dict[str, Any]]:
    container = get_container()
    audit = container.workflow_audit_logger()
    run_id = str(uuid.uuid4())
    params: Mapping[str, Any] = {"limit": limit, "threshold": threshold}
    audit.log_start("top_at_risk", run_id, params)
    start = time.perf_counter()

    try:
        rows = fetch_top_at_risk(limit, threshold)
        audit.log_event(
            "top_at_risk",
            run_id,
            "edges_resolved",
            edge_count=len(rows),
        )
        audit.log_completion(
            "top_at_risk",
            run_id,
            rows,
            duration_seconds=time.perf_counter() - start,
        )
        return rows
    except Exception as exc:
        audit.log_failure("top_at_risk", run_id, exc, parameters=dict(params))
        raise


@flow(name="consciousness-cell-workflow")
def consciousness_cell_flow(
    project_root: str,
    persist_report: bool = True,
    output_path: str | None = None,
) -> Dict[str, Any]:
    container = get_container()
    audit = container.workflow_audit_logger()
    run_id = str(uuid.uuid4())
    params: Mapping[str, Any] = {
        "project_root": project_root,
        "persist_report": persist_report,
        "output_path": output_path,
    }
    audit.log_start("consciousness_cell", run_id, params)
    start = time.perf_counter()

    try:
        cell = build_consciousness_cell(project_root)
        insights = collect_consciousness_insights(cell)
        report = generate_consciousness_report(cell)
        output = Path(output_path) if output_path else Path(project_root) / "scripts" / "consciousness_insights_agent.md"
        persisted_path = persist_consciousness_report(report, output, enabled=persist_report)
        summary = {
            "report_path": persisted_path,
            "report": report,
            "insights": insights,
        }
        log_summary = {
            "report_path": persisted_path,
            "insight_totals": {k: len(v) for k, v in insights.items()},
            "report_preview": report[:280],
        }
        audit.log_event(
            "consciousness_cell",
            run_id,
            "report_generated",
            report_path=persisted_path,
            insight_totals={k: len(v) for k, v in insights.items()},
        )
        audit.log_completion(
            "consciousness_cell",
            run_id,
            log_summary,
            duration_seconds=time.perf_counter() - start,
        )
        return summary
    except Exception as exc:
        audit.log_failure("consciousness_cell", run_id, exc, parameters=dict(params))
        raise


__all__ = ["top_at_risk_flow", "consciousness_cell_flow"]
