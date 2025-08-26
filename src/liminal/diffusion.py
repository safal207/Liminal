from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, List, Optional, Protocol, Tuple


@dataclass(frozen=True)
class ModuleState:
    name: str
    traits: Dict[str, float]
    notes: List[str]


@dataclass(frozen=True)
class BlendResult:
    state: ModuleState
    rationale: List[str]


class DiffusionInterface(Protocol):
    """Philosophy-first diffusion interface for gentle merging.

    - blend: softly merges multiple module states into a coherent composite
    - dissolve_tension: resolves two states with conflicts into harmony
    - similarity: reports cosine-like similarity over shared traits deterministically
    """

    def blend(
        self,
        states: List[ModuleState],
        weights: Optional[List[float]] = None,
        name: str = "diffused",
    ) -> BlendResult: ...

    def dissolve_tension(
        self, a: ModuleState, b: ModuleState, name: str = "harmonized"
    ) -> BlendResult: ...

    def similarity(self, a: ModuleState, b: ModuleState) -> float: ...


class InMemoryDiffusion(DiffusionInterface):
    """Deterministic, offline diffusion. No external deps.

    Rules:
    - Weighted average over numeric traits (missing trait treated as 0.0)
    - Clamp results into [0.0, 1.0]
    - Notes are deduplicated and stably sorted for determinism
    - Rationale lists top contributing traits with weights
    """

    def _normalize_weights(self, n: int, weights: Optional[List[float]]) -> List[float]:
        if not weights:
            return [1.0 / n] * n
        if len(weights) != n:
            raise ValueError("weights length must match states length")
        s = sum(weights)
        if s == 0:
            return [1.0 / n] * n
        return [w / s for w in weights]

    def _all_traits(self, states: List[ModuleState]) -> List[str]:
        keys = set()
        for s in states:
            keys.update(s.traits.keys())
        return sorted(keys)

    def _clamp(self, x: float) -> float:
        if x < 0.0:
            return 0.0
        if x > 1.0:
            return 1.0
        return x

    def _merge_notes(self, states: List[ModuleState]) -> List[str]:
        seen = set()
        merged: List[str] = []
        for s in states:
            for note in s.notes:
                if note not in seen:
                    seen.add(note)
                    merged.append(note)
        return sorted(merged)

    def blend(
        self,
        states: List[ModuleState],
        weights: Optional[List[float]] = None,
        name: str = "diffused",
    ) -> BlendResult:
        if not states:
            raise ValueError("states must be non-empty")
        ws = self._normalize_weights(len(states), weights)
        traits: Dict[str, float] = {}
        keys = self._all_traits(states)
        for k in keys:
            acc = 0.0
            for s, w in zip(states, ws):
                acc += w * s.traits.get(k, 0.0)
            traits[k] = self._clamp(acc)
        notes = self._merge_notes(states)
        # Rationale: top 3 traits by value
        top = sorted(traits.items(), key=lambda kv: (-kv[1], kv[0]))[:3]
        rationale = [f"Trait '{k}' integrated at {v:.2f}" for k, v in top]
        return BlendResult(
            state=ModuleState(name=name, traits=traits, notes=notes),
            rationale=rationale,
        )

    def dissolve_tension(
        self, a: ModuleState, b: ModuleState, name: str = "harmonized"
    ) -> BlendResult:
        # Equal weights by default for two states
        return self.blend([a, b], weights=[0.5, 0.5], name=name)

    def similarity(self, a: ModuleState, b: ModuleState) -> float:
        # Deterministic cosine-like similarity over shared keys
        if not a.traits and not b.traits:
            return 1.0  # By convention, two empty modules are identical

        keys = set(a.traits.keys()) & set(b.traits.keys())
        if not keys:
            return 0.0
        dot = sum(a.traits.get(k, 0.0) * b.traits.get(k, 0.0) for k in keys)
        na = sum(a.traits.get(k, 0.0) ** 2 for k in keys) ** 0.5
        nb = sum(b.traits.get(k, 0.0) ** 2 for k in keys) ** 0.5
        if na == 0.0 or nb == 0.0:
            return 0.0
        return dot / (na * nb)


__all__ = [
    "ModuleState",
    "BlendResult",
    "DiffusionInterface",
    "InMemoryDiffusion",
]
