from __future__ import annotations

import time
import uuid
from dataclasses import dataclass, field
from typing import Callable, Dict, List, Optional, Tuple

from .adapters import build_state_from_reince
from .diffusion import InMemoryDiffusion, ModuleState
from .reince import InMemoryREINCE


@dataclass(frozen=True)
class Node:
    id: str
    kind: str  # e.g., "module_state", "event", "resonance_map"
    traits: Dict[str, float] = field(default_factory=dict)
    notes: List[str] = field(default_factory=list)


@dataclass(frozen=True)
class Edge:
    source_id: str
    target_id: str
    kind: str  # e.g., "influences", "similar_to", "merged_into"
    weight: float = 0.0  # [0..1]
    notes: List[str] = field(default_factory=list)
    rationale: List[str] = field(default_factory=list)


class RealityWebInMemory:
    """Minimal, deterministic in-memory web of nodes and edges.

    Philosophy: мягкое связывание смыслов, прозрачные причины (rationale), без внешних зависимостей.
    """

    def __init__(self) -> None:
        self._nodes: Dict[str, Node] = {}
        self._edges: List[Edge] = []
        self._diff = InMemoryDiffusion()

    # ---- Node/Edge management ----
    def add_node(
        self,
        kind: str,
        traits: Optional[Dict[str, float]] = None,
        notes: Optional[List[str]] = None,
        id: Optional[str] = None,
    ) -> Node:
        nid = id or str(uuid.uuid4())
        n = Node(id=nid, kind=kind, traits=dict(traits or {}), notes=list(notes or []))
        self._nodes[nid] = n
        return n

    def add_edge(
        self,
        source_id: str,
        target_id: str,
        kind: str,
        weight: float = 0.0,
        notes: Optional[List[str]] = None,
        rationale: Optional[List[str]] = None,
    ) -> Edge:
        e = Edge(
            source_id=source_id,
            target_id=target_id,
            kind=kind,
            weight=max(0.0, min(1.0, weight)),
            notes=list(notes or []),
            rationale=list(rationale or []),
        )
        self._edges.append(e)
        return e

    def nodes(self) -> List[Node]:
        return list(self._nodes.values())

    def edges(self) -> List[Edge]:
        return list(self._edges)

    # ---- Adapters ----
    def node_from_module_state(self, ms: ModuleState, id: Optional[str] = None) -> Node:
        return self.add_node(
            kind="module_state", traits=ms.traits, notes=ms.notes, id=id
        )

    def node_from_reince(
        self,
        reince: InMemoryREINCE,
        name: str,
        top_n: int = 10,
        id: Optional[str] = None,
    ) -> Node:
        ms = build_state_from_reince(
            reince, name=name, top_n=top_n, notes=["from_reince"]
        )
        return self.node_from_module_state(ms, id=id)

    # ---- Relationships ----
    def link_similarity(self, a: Node, b: Node, kind: str = "similar_to") -> Edge:
        sim = self._diff.similarity(
            ModuleState("_a", traits=a.traits, notes=[]),
            ModuleState("_b", traits=b.traits, notes=[]),
        )
        return self.add_edge(
            a.id, b.id, kind=kind, weight=sim, rationale=[f"cosine_like={sim:.3f}"]
        )

    def link_merge(
        self, a: Node, b: Node, name: str = "merged", kind: str = "merged_into"
    ) -> Tuple[Node, Edge]:
        # Blend the states and create a new node representing the merge
        br = self._diff.blend(
            [
                ModuleState("a", traits=a.traits, notes=a.notes),
                ModuleState("b", traits=b.traits, notes=b.notes),
            ],
            name=name,
        )
        merged = self.node_from_module_state(br.state)
        e = self.add_edge(
            a.id, merged.id, kind=kind, weight=1.0, rationale=br.rationale
        )
        self.add_edge(b.id, merged.id, kind=kind, weight=1.0, rationale=br.rationale)
        return merged, e

    def link_parent(self, parent: Node, child: Node) -> Edge:
        """Create explicit parent→child relationship.

        Edge kind: "parent"; weight 1.0; rationale explains caring bond.
        """
        return self.add_edge(
            parent.id,
            child.id,
            kind="parent",
            weight=1.0,
            rationale=["family_bond=parent_child"],
        )


# ---- Vital signs: system pulsation/breathing ----


@dataclass
class VitalSigns:
    phase: str  # "inhale" | "exhale"
    bpm: float
    last_ts: float
    cycles: int = 0


class SystemBreath:
    """Deterministic, gentle modulation of the web to simulate pulse/breath.

    Philosophy: вдох — укрупнение мягких качеств (love/calm/tenderness);
    выдох — растворение напряжения (fear/anger) без агрессии, с клиппингом [0,1].
    """

    SOFT_KEYS_POS = (
        "любовь",
        "спокойствие",
        "нежность",
        "мягкость",
        "calm",
        "love",
        "tenderness",
    )
    SOFT_KEYS_NEG = ("страх", "гнев", "злость", "тревога", "anger", "fear")

    def __init__(
        self,
        now_fn: Optional[Callable[[], float]] = None,
        epsilon: float = 0.05,
        bpm: float = 6.0,
    ) -> None:
        self._now_fn = now_fn or (lambda: time.time())
        self._eps = float(epsilon)
        self._vitals = VitalSigns(
            phase="inhale", bpm=float(bpm), last_ts=self._now_fn(), cycles=0
        )

    @property
    def vitals(self) -> VitalSigns:
        return self._vitals

    def _period(self) -> float:
        # full inhale or exhale duration in seconds (half-cycle)
        return 60.0 / max(0.1, self._vitals.bpm)

    def step(self, web: RealityWebInMemory, steps: int = 1) -> VitalSigns:
        for _ in range(max(1, int(steps))):
            self._apply_phase(web)
            # toggle phase after each step, update timestamps/cycles
            self._toggle()
        return self._vitals

    def _toggle(self) -> None:
        self._vitals.phase = "exhale" if self._vitals.phase == "inhale" else "inhale"
        self._vitals.last_ts = self._now_fn()
        if self._vitals.phase == "inhale":
            # completed a full cycle when returning to inhale
            self._vitals.cycles += 1

    def _apply_phase(self, web: RealityWebInMemory) -> None:
        # gentle modulation
        if self._vitals.phase == "inhale":
            self._modulate(web, keys=self.SOFT_KEYS_POS, delta=+self._eps)
        else:
            self._modulate(web, keys=self.SOFT_KEYS_NEG, delta=-self._eps)

    @staticmethod
    def _clip01(x: float) -> float:
        return 0.0 if x < 0.0 else 1.0 if x > 1.0 else x

    def _modulate(
        self, web: RealityWebInMemory, keys: Tuple[str, ...], delta: float
    ) -> None:
        for nid, n in list(web._nodes.items()):  # internal safe access
            if not n.traits:
                continue
            changed = False
            traits = dict(n.traits)
            for k in keys:
                if k in traits:
                    traits[k] = self._clip01(traits[k] + delta)
                    changed = True
            if changed:
                # replace node with updated traits (frozen dataclass → recreate)
                web._nodes[nid] = Node(
                    id=n.id, kind=n.kind, traits=traits, notes=n.notes
                )


__all__ = [
    "Node",
    "Edge",
    "RealityWebInMemory",
    "VitalSigns",
    "SystemBreath",
]
