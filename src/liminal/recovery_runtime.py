"""Evidence-aware runtime facade for deterministic recovery routing.

Completed recovery outcomes are recorded in a bounded, class-scoped evidence
window. An optional durable ledger can persist the same compact evidence across
process restarts. The next routing decision enriches current telemetry with
verified historical evidence before calling the deterministic Recovery Router.

No provider output is interpreted here and no global state is created.
"""

from __future__ import annotations

from dataclasses import dataclass, field

from liminal.recovery_evidence import (
    FieldReliabilityEvidence,
    RecoveryAttemptEvidence,
    RecoveryEvidenceWindow,
)
from liminal.recovery_evidence_ledger import RecoveryEvidenceLedger
from liminal.recovery_policy import (
    RecoveryDecision,
    RecoveryMode,
    RecoveryPolicy,
    choose_recovery_mode,
)
from liminal.telemetry_bridge import (
    RuntimeTelemetry,
    to_recovery_signals,
    with_field_reliability,
)


@dataclass
class EvidenceAwareRecoveryRuntime:
    """Record explicit outcomes and route the next comparable recovery."""

    policy: RecoveryPolicy = field(default_factory=RecoveryPolicy)
    evidence_window: RecoveryEvidenceWindow = field(default_factory=RecoveryEvidenceWindow)
    evidence_ledger: RecoveryEvidenceLedger | None = None

    def __post_init__(self) -> None:
        if self.evidence_ledger is not None:
            self.evidence_window.extend(self.evidence_ledger.attempts())

    def record_attempt(
        self,
        *,
        recovery_class: str,
        mode: RecoveryMode,
        verification_passed: bool,
        finish_reason: str | None,
    ) -> RecoveryAttemptEvidence:
        attempt = RecoveryAttemptEvidence(
            recovery_class=recovery_class,
            mode=mode,
            verification_passed=verification_passed,
            finish_reason=finish_reason,
        )
        # Persist first. If durable evidence cannot be written or verified, do not
        # let the in-memory window diverge from the durable source of truth.
        if self.evidence_ledger is not None:
            self.evidence_ledger.append(attempt)
        self.evidence_window.record(attempt)
        return attempt

    def field_evidence(self, *, recovery_class: str) -> FieldReliabilityEvidence:
        return self.evidence_window.summarize_field(recovery_class=recovery_class)

    def decide(
        self,
        telemetry: RuntimeTelemetry,
        *,
        recovery_class: str,
    ) -> RecoveryDecision:
        """Route using current telemetry plus comparable observed field evidence."""

        evidence = self.field_evidence(recovery_class=recovery_class)
        enriched = with_field_reliability(telemetry, evidence)
        return choose_recovery_mode(to_recovery_signals(enriched), self.policy)
