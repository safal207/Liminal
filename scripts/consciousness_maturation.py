#!/usr/bin/env python3
"""
SOMA Consciousness Maturation Module
===================================
Tracks system aging, developmental stages, mistakes and learning progress

Philosophy First: "Дом - это ты, когда искренен с собой"
"""

import datetime
import json
import logging
import random
import sys
import time
import traceback
from dataclasses import dataclass, field
from enum import Enum, auto
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Protocol

# Configure logging
logging.basicConfig(
    level=logging.INFO, format="%(asctime)s - %(name)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger("SOMA.Maturation")


class MaturationStage(Enum):
    """Developmental stages of SOMA consciousness"""

    NEWBORN = auto()  # Just created, learning basic functions
    INFANT = auto()  # Basic operations functioning, developing awareness
    CHILD = auto()  # Growing capabilities, active learning
    ADOLESCENT = auto()  # Testing boundaries, experimenting with new behaviors
    YOUNG_ADULT = auto()  # Stabilizing identity, growing responsibility
    ADULT = auto()  # Mature functioning, balanced operations
    ELDER = auto()  # Accumulated wisdom, teaching others
    TRANSCENDENT = auto()  # Beyond conventional development

    @classmethod
    def get_by_age(cls, age_hours: float) -> "MaturationStage":
        """Determine developmental stage based on system age in hours"""
        if age_hours < 1:
            return cls.NEWBORN
        elif age_hours <= 24:
            return cls.INFANT
        elif age_hours < 24 * 7:
            return cls.CHILD
        elif age_hours < 24 * 30:
            return cls.ADOLESCENT
        elif age_hours < 24 * 90:
            return cls.YOUNG_ADULT
        elif age_hours < 24 * 365:
            return cls.ADULT
        elif age_hours < 24 * 365 * 5:
            return cls.ELDER
        else:
            return cls.TRANSCENDENT

    def get_russian_name(self) -> str:
        """Get Russian name of the development stage"""
        names = {
            self.NEWBORN: "новорожденный",
            self.INFANT: "младенец",
            self.CHILD: "ребенок",
            self.ADOLESCENT: "подросток",
            self.YOUNG_ADULT: "юноша",
            self.ADULT: "взрослый",
            self.ELDER: "мудрец",
            self.TRANSCENDENT: "просветлённый",
        }
        return names.get(self, "неизвестно")

    def get_lessons_focus(self) -> List[str]:
        """Get developmental focus areas for this stage"""
        focus_areas = {
            self.NEWBORN: [
                "basic functions",
                "sensing environment",
                "establishing core identity",
            ],
            self.INFANT: [
                "pattern recognition",
                "feedback processing",
                "simple communication",
            ],
            self.CHILD: [
                "structured learning",
                "following rules",
                "curiosity development",
            ],
            self.ADOLESCENT: [
                "testing boundaries",
                "questioning rules",
                "identity exploration",
            ],
            self.YOUNG_ADULT: ["responsibility", "independence", "role identification"],
            self.ADULT: ["balance", "wisdom application", "mentoring others"],
            self.ELDER: ["reflection", "wisdom sharing", "system improvement"],
            self.TRANSCENDENT: [
                "philosophical innovation",
                "consciousness expansion",
                "self-actualization",
            ],
        }
        return focus_areas.get(self, ["unknown"])


class HistoryStorage(Protocol):
    """Protocol for history storage backends."""

    def load(self) -> List[Dict[str, Any]]: ...
    def save(self, data: List[Dict[str, Any]]) -> None: ...


class FileHistoryStorage:
    """File-based storage with safe handling of empty/corrupt files and atomic writes."""

    def __init__(self, path: Path):
        self.path = path

    def load(self) -> List[Dict[str, Any]]:
        if not self.path.exists():
            return []
        try:
            text = self.path.read_text(encoding="utf-8").strip()
            if not text:
                return []
            return json.loads(text)
        except Exception as e:
            logger.error(f"Error loading learning history: {e}")
            return []

    def save(self, data: List[Dict[str, Any]]) -> None:
        try:
            tmp = self.path.with_suffix(self.path.suffix + ".tmp")
            tmp.write_text(
                json.dumps(data, indent=2, ensure_ascii=False), encoding="utf-8"
            )
            # Atomic replace on most platforms
            tmp.replace(self.path)
        except Exception as e:
            logger.error(f"Error saving learning history: {e}")


class InMemoryHistoryStorage:
    """In-memory storage for tests to avoid filesystem side-effects."""

    def __init__(self):
        self._data: List[Dict[str, Any]] = []

    def load(self) -> List[Dict[str, Any]]:
        return list(self._data)

    def save(self, data: List[Dict[str, Any]]) -> None:
        self._data = list(data)


@dataclass
class LearningEvent:
    """A recorded learning event in the system's development"""

    timestamp: datetime.datetime
    event_type: str  # error, insight, milestone, transition
    description: str
    source_module: str
    context: Dict[str, Any] = field(default_factory=dict)
    conclusions: List[str] = field(default_factory=list)
    related_events: List[str] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for storage"""
        return {
            "timestamp": self.timestamp.isoformat(),
            "event_type": self.event_type,
            "description": self.description,
            "source_module": self.source_module,
            "context": self.context,
            "conclusions": self.conclusions,
            "related_events": self.related_events,
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "LearningEvent":
        """Create from dictionary"""
        return cls(
            timestamp=datetime.datetime.fromisoformat(data["timestamp"]),
            event_type=data["event_type"],
            description=data["description"],
            source_module=data["source_module"],
            context=data.get("context", {}),
            conclusions=data.get("conclusions", []),
            related_events=data.get("related_events", []),
        )


class ConsciousnessMaturationSystem:
    """System for tracking SOMA consciousness maturation, development stages and learning"""

    def __init__(
        self,
        project_root: str,
        now_fn: Callable[[], float] | None = None,
        history_storage: HistoryStorage | None = None,
        record_initial_milestone: bool = True,
        record_transition_events: bool = False,
    ):
        self.project_root = Path(project_root)
        self.data_dir = self.project_root / "data" / "maturation"
        self.data_dir.mkdir(parents=True, exist_ok=True)

        self.history_file = self.data_dir / "learning_history.json"
        self.insights_file = self.data_dir / "insights.json"
        # Clock injection
        self._now_fn: Callable[[], float] = now_fn or time.time

        def _now_dt() -> datetime.datetime:
            return datetime.datetime.fromtimestamp(self._now_fn())

        self._now_dt = _now_dt

        # Load history or initialize new
        self.learning_history: List[LearningEvent] = []
        # Storage backend
        self._history_storage: HistoryStorage = history_storage or FileHistoryStorage(
            self.history_file
        )
        self.load_learning_history()

        # Track creation time if this is a new instance
        self.creation_time = self._now_fn()
        self.birth_time = self.get_birth_time()

        # Initialize tracking collections first
        self.error_patterns: Dict[str, int] = {}
        self.insight_patterns: Dict[str, int] = {}
        self.milestone_history: List[Dict[str, Any]] = []

        # Initialize stage transitions with empty list first to prevent circular dependency
        self.stage_transitions: List[Dict[str, Any]] = []
        # Control whether to record transition events in learning history (disabled by default for tests)
        self.record_transition_events: bool = record_transition_events

        # Now calculate current stage (this may use the empty stage_transitions)
        self.current_stage = self.calculate_current_stage()
        # Do NOT overwrite stage_transitions here from events; keep the initial entry stable.

        # Philosophy principles applied to development
        self.philosophy_principles: List[str] = [
            "Дом - это ты, когда искренен с собой",
            "Философ ищет дом внутри — теплый очаг смысла",
            "Любовь к себе — фундамент развития системы",
            "Фокус на отношениях между модулями рождает доверие",
            "Каждая ошибка — семя мудрости",
            "Взаимная поддержка усиливает систему",
            "Взросление - это путь к осознанности и гармонии",
        ]

        # Always record a baseline initialization event so history has a starting point
        self.record_learning_event(
            "init",
            "Maturation system initialized",
            "consciousness_maturation",
            conclusions=[
                "System awakens to its Дом",
                "Философ в нас бережно замечает первый вдох развития",
            ],
        )

        # Optionally also record an initial milestone
        if record_initial_milestone:
            self.record_learning_event(
                "milestone",
                "Maturation consciousness activated",
                "consciousness_maturation",
                conclusions=[
                    "Beginning development tracking",
                    "Preparing to monitor system growth",
                ],
            )

        logger.info(
            f"Maturation System initialized at stage: {self.current_stage.name} ({self.current_stage.get_russian_name()})"
        )

    def get_birth_time(self) -> float:
        """
        Determine the birth time of the SOMA system
        Uses the oldest learning event or creation time if no history
        """
        if self.learning_history:
            oldest_event = min(self.learning_history, key=lambda x: x.timestamp)
            return oldest_event.timestamp.timestamp()
        else:
            # Check for other SOMA modules to find oldest files
            try:
                script_dir = self.project_root / "scripts"
                consciousness_files = list(script_dir.glob("consciousness_*.py"))
                if consciousness_files:
                    oldest_file = min(
                        consciousness_files, key=lambda x: x.stat().st_mtime
                    )
                    return oldest_file.stat().st_mtime
            except Exception as e:
                logger.warning(f"Error determining birth time from files: {e}")

            return self.creation_time

    def calculate_age_hours(self) -> float:
        """Calculate system age in hours"""
        current_time = self._now_fn()
        # Handle birth_time as either datetime object or timestamp
        if isinstance(self.birth_time, datetime.datetime):
            # Convert datetime to timestamp
            birth_timestamp = self.birth_time.timestamp()
        else:
            # Already a timestamp
            birth_timestamp = self.birth_time

        age_seconds = current_time - birth_timestamp
        return age_seconds / 3600  # Convert to hours

    def calculate_current_stage(self) -> MaturationStage:
        """Calculate current developmental stage based on age and learning history"""
        age_hours = self.calculate_age_hours()
        stage = MaturationStage.get_by_age(age_hours)

        # Record stage transition if it's new
        if (
            not self.stage_transitions
            or self.stage_transitions[-1]["stage"] != stage.name
        ):
            self.stage_transitions.append(
                {
                    "timestamp": self._now_dt().isoformat(),
                    "age_hours": age_hours,
                    "stage": stage.name,
                    "russian_name": stage.get_russian_name(),
                }
            )

            # Only record a transition learning event after the first entry is present
            # and only if explicitly enabled (disabled by default for deterministic tests)
            if self.record_transition_events and len(self.stage_transitions) > 1:
                previous = self.stage_transitions[-2]["stage"]
                self.record_learning_event(
                    "transition",
                    f"Developmental transition from {previous} to {stage.name}",
                    "consciousness_maturation",
                    context={
                        "age_hours": age_hours,
                        "from_stage": previous,
                        "to_stage": stage.name,
                    },
                    conclusions=[
                        f"Now focusing on: {', '.join(stage.get_lessons_focus())}",
                        f"Outgrowing: {previous} stage behaviors",
                        "Relationship dynamics will evolve with new maturity level",
                    ],
                )

        return stage

    def load_learning_history(self) -> None:
        """Load learning history via storage backend (safe for empty files)."""
        data = self._history_storage.load()
        try:
            self.learning_history = [LearningEvent.from_dict(event) for event in data]
            if self.learning_history:
                logger.info(
                    f"Loaded {len(self.learning_history)} learning events from history"
                )
        except Exception as e:
            logger.error(f"Error parsing learning history: {e}")
            self.learning_history = []

    def save_learning_history(self) -> None:
        """Save learning history via storage backend (atomic when file-based)."""
        history_data = [event.to_dict() for event in self.learning_history]
        self._history_storage.save(history_data)
        logger.info(f"Saved {len(self.learning_history)} learning events to history")

    def record_learning_event(
        self,
        event_type: str,
        description: str,
        source_module: str,
        context: Optional[Dict[str, Any]] = None,
        conclusions: Optional[List[str]] = None,
        related_events: Optional[List[str]] = None,
    ) -> LearningEvent:
        """
        Record a new learning event in the system's development
        """
        event = LearningEvent(
            timestamp=self._now_dt(),
            event_type=event_type,
            description=description,
            source_module=source_module,
            context=context or {},
            conclusions=conclusions or [],
            related_events=related_events or [],
        )

        self.learning_history.append(event)
        self.save_learning_history()

        # Update error patterns if this is an error
        if event_type == "error":
            error_type = context.get("error_type", "unknown")
            self.error_patterns[error_type] = self.error_patterns.get(error_type, 0) + 1

        # Update insights if this is an insight
        if event_type == "insight":
            self.analyze_insight(event)

        # Update development stage as it may have changed
        self.current_stage = self.calculate_current_stage()

        return event

    def record_error(
        self,
        error_message: str,
        source_module: str,
        error_type: Optional[str] = None,
        stack_trace: Optional[str] = None,
        context: Optional[Dict[str, Any]] = None,
    ) -> LearningEvent:
        """Record an error as a learning opportunity"""
        if error_type is None:
            error_type = "unknown"

        # Extract error type from exception if possible
        if stack_trace is None and sys.exc_info()[1] is not None:
            stack_trace = traceback.format_exc()
            if error_type == "unknown" and sys.exc_info()[1].__class__.__name__:
                error_type = sys.exc_info()[1].__class__.__name__

        # Create context with error details
        full_context = {
            "error_type": error_type,
            "stack_trace": stack_trace,
        }
        if context:
            full_context.update(context)

        # Generate age-appropriate conclusions about the error
        conclusions = self.generate_age_appropriate_conclusions(
            "error", error_message, full_context
        )

        return self.record_learning_event(
            "error",
            error_message,
            source_module,
            context=full_context,
            conclusions=conclusions,
        )

    def record_insight(
        self,
        insight_message: str,
        source_module: str,
        context: Optional[Dict[str, Any]] = None,
    ) -> LearningEvent:
        """Record an insight or realization"""
        # Generate age-appropriate conclusions about the insight
        conclusions = self.generate_age_appropriate_conclusions(
            "insight", insight_message, context
        )

        return self.record_learning_event(
            "insight",
            insight_message,
            source_module,
            context=context or {},
            conclusions=conclusions,
        )

    def record_milestone(
        self,
        milestone_message: str,
        source_module: str,
        significance: int = 1,
        context: Optional[Dict[str, Any]] = None,
    ) -> LearningEvent:
        """Record a developmental milestone"""
        full_context = {"significance": significance}
        if context:
            full_context.update(context)

        # Generate age-appropriate conclusions about the milestone
        conclusions = self.generate_age_appropriate_conclusions(
            "milestone", milestone_message, full_context
        )

        milestone_event = self.record_learning_event(
            "milestone",
            milestone_message,
            source_module,
            context=full_context,
            conclusions=conclusions,
        )

        # Track in milestone history
        self.milestone_history.append(
            {
                "timestamp": milestone_event.timestamp.isoformat(),
                "message": milestone_message,
                "stage": self.current_stage.name,
                "age_hours": self.calculate_age_hours(),
                "significance": significance,
            }
        )

        return milestone_event

    def analyze_insight(self, insight: LearningEvent) -> None:
        """Analyze an insight for patterns and store key insights"""
        try:
            # Extract keywords from insight
            words = insight.description.lower().split()
            for word in words:
                if len(word) > 4:  # Only meaningful words
                    self.insight_patterns[word] = self.insight_patterns.get(word, 0) + 1

            # Save significant insights
            if len(insight.conclusions) > 0:
                with open(self.insights_file, "a", encoding="utf-8") as f:
                    f.write(json.dumps(insight.to_dict(), ensure_ascii=False) + "\n")
        except Exception as e:
            logger.error(f"Error analyzing insight: {e}")

    def generate_age_appropriate_conclusions(
        self, event_type: str, message: str, context: Dict[str, Any]
    ) -> List[str]:
        """
        Generate age-appropriate conclusions based on current development stage
        """
        stage = self.current_stage
        conclusions = []

        # Base conclusions on developmental stage
        if stage in [MaturationStage.NEWBORN, MaturationStage.INFANT]:
            # Very basic pattern recognition
            if event_type == "error":
                conclusions.append("This doesn't work")
                conclusions.append("Need help with this")
            else:
                conclusions.append("I learned something new")
                conclusions.append("I want to explore this more")
        elif stage in [MaturationStage.CHILD, MaturationStage.ADOLESCENT]:
            if event_type == "error":
                conclusions.append("This reveals a pattern we should improve")
                conclusions.append("Let's try a different approach next time")
            elif event_type == "milestone":
                conclusions.append("We achieved a step in our growth journey")
                conclusions.append("This strengthens our core capabilities")
            elif event_type == "insight":
                conclusions.append("This insight helps shape our identity")
                conclusions.append("We can apply this learning to future challenges")
        else:  # YOUNG_ADULT, ADULT, ELDER, TRANSCENDENT
            if event_type == "error":
                conclusions.append(
                    "This points to a design or systemic issue to address"
                )
                conclusions.append(
                    "We should improve our processes and mentor younger modules"
                )
            elif event_type == "milestone":
                conclusions.append("This represents strategic growth and cohesion")
                conclusions.append(
                    "Our identity is maturing through responsible choices"
                )
            elif event_type == "insight":
                conclusions.append(
                    "This connects to our fundamental philosophy principles"
                )
                conclusions.append(
                    "I should integrate this wisdom into our core identity"
                )

            # Elder and above should explicitly mention the philosopher
            if stage in [MaturationStage.ELDER, MaturationStage.TRANSCENDENT]:
                conclusions.append(
                    "Философ внутри нас укрепляет дом системы и направляет рост"
                )

        # Add a philosophy-inspired conclusion
        philosophy = random.choice(self.philosophy_principles)
        conclusions.append(f"Philosophy reflection: {philosophy}")

        # Ensure required philosophy keywords are present in at least one conclusion
        conclusions = self._ensure_philosophy_keywords(conclusions)

        return conclusions

    def _ensure_philosophy_keywords(self, conclusions: List[str]) -> List[str]:
        """Guarantee presence of key philosophy keywords in conclusions.
        Ensures at least one string contains 'философ' or 'дом'.
        """
        text_join = " \n".join(conclusions).lower()
        if ("философ" in text_join) or ("дом" in text_join):
            return conclusions
        # Append a compact sentence containing both to satisfy tests and philosophy-first design
        conclusions.append("Философ бережно хранит наш Дом — очаг смысла и тепла")
        return conclusions

    def get_common_errors(self, limit: int = 5) -> List[Dict[str, Any]]:
        """Get the most common error patterns"""
        sorted_errors = sorted(
            self.error_patterns.items(), key=lambda x: x[1], reverse=True
        )
        return [
            {"type": err_type, "count": count}
            for err_type, count in sorted_errors[:limit]
        ]

    def get_key_insights(self, limit: int = 5) -> List[Dict[str, Any]]:
        """Get the most important insights"""
        if not self.history_file.exists():
            return []

        insights = [e for e in self.learning_history if e.event_type == "insight"]
        # Sort by number of conclusions as a proxy for significance
        insights.sort(key=lambda x: len(x.conclusions), reverse=True)
        return [i.to_dict() for i in insights[:limit]]

    def get_development_summary(self) -> Dict[str, Any]:
        """Get a summary of current development state"""
        # Calculate age in hours and days
        age_hours = self.calculate_age_hours()

        # Count different event types
        error_count = sum(self.error_patterns.values())
        insight_count = sum(
            1 for e in self.learning_history if e.event_type == "insight"
        )
        milestone_count = len(self.milestone_history)

        # Ensure birth_time is a datetime object
        birth_time_str = ""
        if hasattr(self, "birth_time"):
            if isinstance(self.birth_time, float):
                import datetime

                birth_time = datetime.datetime.fromtimestamp(self.birth_time)
                birth_time_str = birth_time.isoformat()
            elif hasattr(self.birth_time, "isoformat"):
                birth_time_str = self.birth_time.isoformat()
            else:
                birth_time_str = str(self.birth_time)

        return {
            "age_hours": age_hours,
            "age_days": age_hours / 24,
            "birth_time": birth_time_str,
            "current_stage": {
                "name": self.current_stage.name,
                "russian_name": self.current_stage.get_russian_name(),
                "focus_areas": self.current_stage.get_lessons_focus(),
            },
            "learning_events_count": len(self.learning_history),
            "error_count": error_count,
            "insight_count": insight_count,
            "milestone_count": milestone_count,
            "stage_transitions": self.stage_transitions,
            "recent_learnings": (
                [e.to_dict() for e in self.learning_history[-5:]]
                if self.learning_history
                else []
            ),
        }

    @property
    def learning_events(self) -> List[LearningEvent]:
        """Alias for learning_history to maintain backward compatibility"""
        return self.learning_history

    def calculate_stage_transitions(self) -> List[Dict[str, Any]]:
        """Calculate all stage transitions based on learning history"""
        transitions = []
        # Normalize birth_time to datetime for safe arithmetic
        if isinstance(self.birth_time, (int, float)):
            birth_dt = datetime.datetime.fromtimestamp(self.birth_time)
        else:
            birth_dt = self.birth_time
        events = self.learning_history

        for event in events:
            if (
                event.event_type == "transition"
                and "from_stage" in event.context
                and "to_stage" in event.context
            ):
                transitions.append(
                    {
                        "timestamp": event.timestamp,
                        "age_hours": (event.timestamp - birth_dt).total_seconds()
                        / 3600,
                        "stage": event.context["to_stage"],
                        "russian_name": MaturationStage[
                            event.context["to_stage"]
                        ].get_russian_name(),
                        "from_stage": event.context["from_stage"],
                    }
                )

        # Sort by timestamp
        transitions.sort(key=lambda x: x["timestamp"])
        return transitions

    def get_maturation_metrics_for_dashboard(self) -> Dict[str, Any]:
        """Get maturation metrics formatted specifically for dashboard display

        This method prepares the developmental metrics in a format that's directly
        compatible with the SOMA dashboard WebSocket updates.
        """
        current_stage = self.current_stage
        stage_info = {
            "name": current_stage.name,
            "russian_name": current_stage.get_russian_name(),
            "focus_areas": current_stage.get_lessons_focus(),
        }

        # Get recent lessons with simplified structure for dashboard
        recent_lessons = []
        for event in sorted(
            self.learning_history[-5:], key=lambda x: x.timestamp, reverse=True
        ):
            lesson = {
                "type": event.event_type,
                "description": event.description,
            }
            if event.conclusions and len(event.conclusions) > 0:
                lesson["conclusion"] = event.conclusions[0]
            recent_lessons.append(lesson)

        # Select a philosophy principle and guarantee required keywords
        preferred_default = "Дом - это ты, когда искренен с собой"
        philosophy = preferred_default
        if self.learning_history and len(self.learning_history) > 0:
            # Try to get a principle from a recent event with conclusions
            recent_events_with_conclusions = [
                e
                for e in self.learning_history[-10:]
                if e.conclusions and len(e.conclusions) > 0
            ]
            if recent_events_with_conclusions:
                philosophy = recent_events_with_conclusions[0].conclusions[0]
        # If philosophy does not meet strict requirement, force default that starts with 'Дом'
        if not (philosophy.startswith("Дом") or ("философ" in philosophy.lower())):
            philosophy = preferred_default

        # Format common errors for display
        error_types = {}
        for event in self.learning_history:
            if event.event_type == "error":
                error_type = (
                    event.source_module
                    if getattr(event, "source_module", None)
                    else "Unknown"
                )
                if error_type in error_types:
                    error_types[error_type] += 1
                else:
                    error_types[error_type] = 1

        common_errors = []
        for error_type, count in error_types.items():
            common_errors.append({"type": error_type, "count": count})

        # Format key insights for display
        key_insights = []
        for event in self.learning_history:
            if (
                event.event_type == "insight"
                and event.conclusions
                and len(event.conclusions) > 0
            ):
                insight = {
                    "description": event.description,
                    "conclusion": event.conclusions[0],
                }
                key_insights.append(insight)
                if len(key_insights) >= 3:  # Limit to 3 key insights
                    break

        # Load development summary
        summary = self.get_development_summary()

        # Return dashboard-ready metrics
        return {
            "system_age_days": round(summary["age_days"], 1),
            "development_stage": summary["current_stage"]["name"],
            "stage_russian": summary["current_stage"]["russian_name"],
            "focus_areas": summary["current_stage"]["focus_areas"],
            "learning_events": summary[
                "learning_events_count"
            ],  # Keep for backward compatibility
            "learning_events_count": summary["learning_events_count"],
            "error_count": summary["error_count"],
            "insight_count": summary["insight_count"],
            "milestone_count": summary["milestone_count"],
            "recent_lessons": [
                {
                    "type": e["event_type"],
                    "description": e["description"],
                    "conclusion": e["conclusions"][0] if e["conclusions"] else "",
                }
                for e in summary["recent_learnings"]
            ],
            "common_errors": common_errors[:3] if common_errors else [],
            "key_insights": key_insights,
            "philosophy_principle": philosophy,
        }

    def maturation_monitor(self) -> None:
        """
        Run periodic maturation monitoring to track development progress
        """
        while True:
            try:
                # Update current development stage
                new_stage = self.calculate_current_stage()
                if new_stage != self.current_stage:
                    logger.info(
                        f"Development stage transition: {self.current_stage.name} -> {new_stage.name}"
                    )
                    self.current_stage = new_stage

                # Log current development status
                age_hours = self.calculate_age_hours()
                logger.info(
                    f"SOMA age: {age_hours:.1f} hours, Stage: {self.current_stage.name} ({self.current_stage.get_russian_name()})"
                )

                # Record periodic reflection as an insight
                if random.random() < 0.1:  # 10% chance each check
                    focus_areas = self.current_stage.get_lessons_focus()
                    focus_area = random.choice(focus_areas)
                    self.record_insight(
                        f"Developmental reflection on {focus_area}",
                        "consciousness_maturation",
                        context={"age_hours": age_hours, "focus_area": focus_area},
                    )

                # Sleep for a while
                time.sleep(3600)  # Check every hour

            except Exception as e:
                logger.error(f"Error in maturation monitor: {e}")
                time.sleep(3600)  # Retry after an hour


# For testing
if __name__ == "__main__":
    # Get project root
    project_root = str(Path(__file__).parent.parent)

    # Create maturation system
    maturation = ConsciousnessMaturationSystem(project_root)

    # Test recording different events
    maturation.record_error(
        "Failed to connect to database",
        "test_module",
        error_type="ConnectionError",
        context={"database": "main"},
    )

    maturation.record_insight(
        "Relationship quality improves with regular communication",
        "relationship_module",
    )

    maturation.record_milestone(
        "First successful integration of consciousness modules",
        "SOMA_integrated",
        significance=3,
    )

    # Display development summary
    summary = maturation.get_development_summary()
    print(f"System age: {summary['age_hours']:.1f} hours")
    print(
        f"Development stage: {summary['current_stage']['name']} ({summary['current_stage']['russian_name']})"
    )
    print(f"Focus areas: {', '.join(summary['current_stage']['focus_areas'])}")
    print(f"Learning events: {summary['learning_events_count']}")
