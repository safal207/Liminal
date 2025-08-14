"""Endocrine subsystem: Thyroid accumulates emotional 'hurt' from experience logs
and releases healing insights when threshold exceeded.
"""

import json
from datetime import datetime
from pathlib import Path
from typing import Dict

STORE_PATH = Path(__file__).parent / "state" / "thyroid.json"
STORE_PATH.parent.mkdir(exist_ok=True)


class ThyroidSystem:
    """Accumulate hurt, release insights when threshold reached."""

    def __init__(self, threshold: int = 100, decay: int = 1):
        self.threshold = threshold
        self.decay = decay
        self.charge = 0
        self.last_total_errors = 0  # for delta calculation
        self._load()

    # -------------------------------------------------
    # Persistence helpers
    # -------------------------------------------------
    def _load(self):
        if STORE_PATH.exists():
            try:
                data = json.loads(STORE_PATH.read_text())
                self.charge = data.get("charge", 0)
                self.last_total_errors = data.get("last_total_errors", 0)
            except Exception:
                pass

    def _save(self):
        STORE_PATH.write_text(
            json.dumps(
                {
                    "charge": self.charge,
                    "last_total_errors": self.last_total_errors,
                    "updated": datetime.utcnow().isoformat(),
                }
            )
        )

    # -------------------------------------------------
    # Public API
    # -------------------------------------------------
    def update_from_error_count(self, total_errors: int):
        """Compute delta vs previous count and accumulate."""
        if total_errors > self.last_total_errors:
            delta = total_errors - self.last_total_errors
            self.charge += delta  # 1 hurt per error
        self.last_total_errors = total_errors
        self._decay()
        self._save()

    def _decay(self):
        self.charge = max(0, self.charge - self.decay)

    def should_release(self) -> bool:
        return self.charge >= self.threshold

    def release(self) -> Dict:
        if not self.should_release():
            return {}
        self.charge = 0
        self._save()
        return {
            "type": "insight_hormone",
            "timestamp": datetime.utcnow().isoformat(),
            "message": "healing_insight_triggered",
        }

    def status(self) -> Dict:
        return {
            "charge": self.charge,
            "threshold": self.threshold,
            "ready": self.should_release(),
        }

    def reset_emotional_state(self):
        """Meditative reset: clear accumulated hurt and memory."""
        self.charge = 0
        self.last_total_errors = 0
        if hasattr(self, "emotional_memory"):
            self.emotional_memory.clear()
        self._save()


thyroid = ThyroidSystem()
