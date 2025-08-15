from __future__ import annotations

import datetime as _dt
import re
from dataclasses import dataclass, field
from typing import Callable, Dict, List, Optional, Protocol, Tuple


class REINCEInterface(Protocol):
    """Protocol for REINCE core capabilities.

    Philosophy-first contract:
    - record_emotional_event captures text + gentle meta and returns stored event
    - get_resonance_map returns a stable, deterministic keyword map (no network, no heavy deps)
    - recommend_intervention proposes a small, kind action based on simple triggers
    """

    def record_emotional_event(
        self, text: str, meta: Optional[Dict[str, str]] = None
    ) -> "ResonanceEvent": ...

    def get_resonance_map(self, top_n: int = 10) -> Dict[str, int]: ...

    def recommend_intervention(self, text: str) -> str: ...

    def list_recent(self, limit: int = 5) -> List["ResonanceEvent"]: ...
    def sentiments(self, text: str) -> Dict[str, float]: ...
    def sentiments_recent(self, limit: int = 50) -> Dict[str, float]: ...


@dataclass(frozen=True)
class ResonanceEvent:
    timestamp: _dt.datetime
    text: str
    meta: Dict[str, str] = field(default_factory=dict)


class InMemoryREINCE(REINCEInterface):
    """Deterministic, offline REINCE implementation for local use and tests.

    - No external dependencies
    - Stable tokenizer
    - Clock injection for tests
    """

    def __init__(self, now_fn: Optional[Callable[[], float]] = None) -> None:
        self._events: List[ResonanceEvent] = []
        self._now_fn: Callable[[], float] = now_fn or (
            lambda: _dt.datetime.now().timestamp()
        )
        # Precompile a unicode-friendly tokenizer (Python re has no \p{L}):
        # split on any non-letter. We cover Latin + Cyrillic (including Ё/ё).
        self._tokenizer = re.compile(r"[^A-Za-zА-Яа-яЁё]+")
        # Minimal sentiment lexicon (Russian stems → tags)
        self._sent_lex: List[Tuple[str, str]] = [
            ("радост", "joy"),
            ("спокой", "calm"),
            ("тихо", "calm"),
            ("нежн", "tenderness"),
            ("мягк", "tenderness"),
            ("любов", "love"),
            ("благодар", "love"),
            ("страх", "fear"),
            ("тревог", "fear"),
            ("злос", "anger"),
            ("гнев", "anger"),
            ("грусть", "sadness"),
            ("печал", "sadness"),
            ("тоска", "sadness"),
        ]

    def _now_dt(self) -> _dt.datetime:
        return _dt.datetime.fromtimestamp(self._now_fn())

    def record_emotional_event(
        self, text: str, meta: Optional[Dict[str, str]] = None
    ) -> ResonanceEvent:
        event = ResonanceEvent(
            timestamp=self._now_dt(), text=text, meta=dict(meta or {})
        )
        self._events.append(event)
        return event

    def _tokens(self) -> List[str]:
        words: List[str] = []
        for e in self._events:
            # Lowercase and split using tokenizer; filter empty and very short tokens
            for w in self._tokenizer.split(e.text.lower()):
                if len(w) >= 2:
                    words.append(w)
        return words

    def get_resonance_map(self, top_n: int = 10) -> Dict[str, int]:
        counts: Dict[str, int] = {}
        for w in self._tokens():
            counts[w] = counts.get(w, 0) + 1
        # Stabilize ordering by (count desc, token asc) and take top_n
        items: List[Tuple[str, int]] = sorted(
            counts.items(), key=lambda kv: (-kv[1], kv[0])
        )[:top_n]
        return {k: v for k, v in items}

    def recommend_intervention(self, text: str) -> str:
        t = text.lower()
        # Simple, deterministic trigger rules
        if any(k in t for k in ("плохо", "плохой", "вина", "страх")):
            return "Мягкий рефрейм: заметь чувство, назови его и обними. Затем 4-7-8 дыхание."
        if any(k in t for k in ("напряж", "стресс", "устал")):
            return "Дыхание 4-7-8 и пауза на стакан воды. Вспомни: Дом внутри тебя."
        if any(k in t for k in ("радость", "благодар", "любов")):
            return "Усили радость: 3 благодарности и короткая прогулка."
        # Default compassionate nudge
        return "Остановись на минуту, подыши мягко. Спроси себя: чего мне хочется с любовью к себе?"

    def list_recent(self, limit: int = 5) -> List[ResonanceEvent]:
        return list(self._events[-limit:])

    def sentiments(self, text: str) -> Dict[str, float]:
        """Map text to basic affect tags using a tiny lexicon; normalized to [0,1]."""
        t = text.lower()
        counts: Dict[str, int] = {}
        for stem, tag in self._sent_lex:
            if stem in t:
                counts[tag] = counts.get(tag, 0) + 1
        if not counts:
            return {}
        m = max(counts.values())
        return {k: v / float(m) for k, v in counts.items()}

    def sentiments_recent(self, limit: int = 50) -> Dict[str, float]:
        agg: Dict[str, int] = {}
        for e in self.list_recent(limit=limit):
            s = self.sentiments(e.text)
            for k, v in s.items():
                # convert back to integer-like counts by summing scaled scores
                agg[k] = agg.get(k, 0) + (1 if v > 0 else 0)
        if not agg:
            return {}
        m = max(agg.values())
        return {k: v / float(m) for k, v in agg.items()}


__all__ = [
    "REINCEInterface",
    "ResonanceEvent",
    "InMemoryREINCE",
]
