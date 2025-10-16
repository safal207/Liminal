from __future__ import annotations

from typing import Dict, List, Optional

import strawberry
from strawberry.scalars import JSON

from .diffusion import InMemoryDiffusion, ModuleState
from .reality_web import Edge as RWEdge
from .reality_web import Node as RWNode
from .di import get_container
from .reality_web import SystemBreath
from .reince import InMemoryREINCE, ResonanceEvent

# ===== Helpers to convert internal models to GraphQL types =====


@strawberry.type
class ResonanceMapEntry:
    key: str
    count: int


@strawberry.type
class TraitEntry:
    key: str
    value: float


@strawberry.type
class ResonanceEventType:
    timestamp: str
    text: str
    meta: JSON


@strawberry.type
class ModuleStateType:
    name: str
    traits: List[TraitEntry]
    notes: List[str]


@strawberry.type
class BlendResultType:
    state: ModuleStateType
    rationale: List[str]


# ===== Reality Web Types =====


@strawberry.type
class NodeType:
    id: str
    kind: str
    traits: List[TraitEntry]
    notes: List[str]


@strawberry.type
class EdgeType:
    source_id: str
    target_id: str
    kind: str
    weight: float
    notes: List[str]
    rationale: List[str]


@strawberry.type
class VitalSignsType:
    phase: str
    bpm: float
    last_ts: float
    cycles: int


@strawberry.type
class BreakdownEntryType:
    key: str
    weight: float
    contribution: float


@strawberry.type
class RelationshipHealthType:
    score: float
    rationale: List[str]
    breakdown: List[BreakdownEntryType]


@strawberry.type
class AtRiskEdgeType:
    source_id: str
    target_id: str
    score: float
    advice: List[str]
    rationale: List[str]


@strawberry.input
class TraitEntryInput:
    key: str
    value: float


@strawberry.input
class ModuleStateInput:
    name: str
    traits: List[TraitEntryInput]
    notes: Optional[List[str]] = None


def _to_module_state(msi: ModuleStateInput) -> ModuleState:
    return ModuleState(
        name=msi.name,
        traits={te.key: float(te.value) for te in msi.traits},
        notes=list(msi.notes or []),
    )


def _to_trait_entries(traits: Dict[str, float]) -> List[TraitEntry]:
    return [TraitEntry(key=k, value=float(v)) for k, v in sorted(traits.items())]


def _to_module_state_type(ms: ModuleState) -> ModuleStateType:
    return ModuleStateType(
        name=ms.name, traits=_to_trait_entries(ms.traits), notes=ms.notes
    )


def _to_event_type(ev: ResonanceEvent) -> ResonanceEventType:
    return ResonanceEventType(
        timestamp=ev.timestamp.isoformat(), text=ev.text, meta=ev.meta
    )


def _to_node_type(n: RWNode) -> NodeType:
    return NodeType(
        id=n.id, kind=n.kind, traits=_to_trait_entries(n.traits), notes=n.notes
    )


def _to_edge_type(e: RWEdge) -> EdgeType:
    return EdgeType(
        source_id=e.source_id,
        target_id=e.target_id,
        kind=e.kind,
        weight=float(e.weight),
        notes=e.notes,
        rationale=e.rationale,
    )


# ===== Root Schema =====


@strawberry.type
class Query:
    @strawberry.field
    def resonance_map(
        self, seed: Optional[List[str]] = None, top_n: int = 10
    ) -> List[ResonanceMapEntry]:
        r = InMemoryREINCE()
        for t in seed or []:
            r.record_emotional_event(t)
        m = r.get_resonance_map(top_n=top_n)
        return [ResonanceMapEntry(key=k, count=v) for k, v in m.items()]

    @strawberry.field
    def sentiments(self, text: str) -> List[TraitEntry]:
        r = InMemoryREINCE()
        s = r.sentiments(text)
        return _to_trait_entries(s)

    @strawberry.field
    def blend(
        self,
        states: List[ModuleStateInput],
        weights: Optional[List[float]] = None,
        name: str = "diffused",
    ) -> BlendResultType:
        d = InMemoryDiffusion()
        ms_list = [_to_module_state(s) for s in states]
        res = d.blend(ms_list, weights=weights, name=name)
        return BlendResultType(
            state=_to_module_state_type(res.state), rationale=res.rationale
        )

    # Reality Web
    @strawberry.field
    def reality_web_nodes(self) -> List[NodeType]:
        return [_to_node_type(n) for n in WEB.nodes()]

    @strawberry.field
    def reality_web_edges(self) -> List[EdgeType]:
        return [_to_edge_type(e) for e in WEB.edges()]

    @strawberry.field
    def top_at_risk_edges(self, limit: int = 5) -> List[AtRiskEdgeType]:
        # Compute pairwise relationship health for all node pairs and return lowest scores
        nodes = list(WEB.nodes())
        pairs: List[AtRiskEdgeType] = []

        # Soft keys and weights aligned with compute_relationship_health
        POS_KEYS = getattr(
            SystemBreath,
            "SOFT_KEYS_POS",
            (
                "любовь",
                "спокойствие",
                "нежность",
                "мягкость",
                "calm",
                "love",
                "tenderness",
            ),
        )
        NEG_KEYS = getattr(
            SystemBreath,
            "SOFT_KEYS_NEG",
            ("страх", "гнев", "злость", "тревога", "anger", "fear"),
        )
        POS_W = 0.10
        NEG_W = 0.10
        PARENT_BONUS = 0.05

        try:
            threshold = float(os.getenv("LIMINAL_HEALTH_THRESHOLD", "0.4"))
        except ValueError:
            threshold = 0.4

        d = InMemoryDiffusion()

        def has_parent(a: RWNode, b: RWNode) -> bool:
            return any(
                (e.source_id == a.id and e.target_id == b.id and e.kind == "parent")
                or (e.source_id == b.id and e.target_id == a.id and e.kind == "parent")
                for e in WEB.edges()
            )

        for i in range(len(nodes)):
            for j in range(i + 1, len(nodes)):
                a, b = nodes[i], nodes[j]
                base = float(
                    d.similarity(
                        ModuleState("a", traits=a.traits, notes=[]),
                        ModuleState("b", traits=b.traits, notes=[]),
                    )
                )

                def avg(keys: tuple[str, ...]) -> float:
                    vals: List[float] = []
                    for k in keys:
                        if k in a.traits or k in b.traits:
                            va = float(a.traits.get(k, 0.0))
                            vb = float(b.traits.get(k, 0.0))
                            vals.append((va + vb) / 2.0)
                    return sum(vals) / len(vals) if vals else 0.0

                pos_avg = avg(tuple(POS_KEYS))
                neg_avg = avg(tuple(NEG_KEYS))
                bonus = POS_W * pos_avg
                penalty = NEG_W * neg_avg
                parent = has_parent(a, b)
                raw_score = base + bonus - penalty + (PARENT_BONUS if parent else 0.0)
                score = (
                    0.0 if raw_score < 0.0 else 1.0 if raw_score > 1.0 else raw_score
                )

                rationale = [
                    f"base_similarity={base:.3f}",
                    f"pos_avg={pos_avg:.3f}*{POS_W:.2f} -> +{bonus:.3f}",
                    f"neg_avg={neg_avg:.3f}*{NEG_W:.2f} -> -{penalty:.3f}",
                    (
                        ("parent_bonus=+%.3f" % PARENT_BONUS)
                        if parent
                        else "parent_bonus=+0.000"
                    ),
                    f"score_clipped={score:.3f}",
                ]

                advice = []  # lightweight hints
                if score < threshold:
                    advice.append("breathStep")
                    if not parent:
                        advice.append("consider_linkParent")
                    advice.append("consider_merge")

                pairs.append(
                    AtRiskEdgeType(
                        source_id=a.id,
                        target_id=b.id,
                        score=score,
                        advice=advice,
                        rationale=rationale,
                    )
                )

        pairs.sort(key=lambda x: x.score)
        return pairs[: max(0, int(limit))]


@strawberry.type
class Mutation:
    @strawberry.field
    def record_event(
        self, text: str, meta: Optional[JSON] = None
    ) -> ResonanceEventType:
        r = InMemoryREINCE()
        ev = r.record_emotional_event(
            text, meta=meta if isinstance(meta, dict) else None
        )
        return _to_event_type(ev)

    # Reality Web mutations
    @strawberry.field
    def add_node(
        self,
        kind: str,
        traits: List[TraitEntryInput],
        notes: Optional[List[str]] = None,
        id: Optional[str] = None,
    ) -> NodeType:
        node = WEB.add_node(
            kind=kind,
            traits={t.key: float(t.value) for t in traits},
            notes=notes or [],
            id=id,
        )
        return _to_node_type(node)

    @strawberry.field
    def link_similarity(self, source_id: str, target_id: str) -> EdgeType:
        a = next(n for n in WEB.nodes() if n.id == source_id)
        b = next(n for n in WEB.nodes() if n.id == target_id)
        e = WEB.link_similarity(a, b)
        return _to_edge_type(e)

    @strawberry.field
    def link_merge(
        self, source_id: str, target_id: str, name: str = "merged"
    ) -> NodeType:
        a = next(n for n in WEB.nodes() if n.id == source_id)
        b = next(n for n in WEB.nodes() if n.id == target_id)
        merged, _ = WEB.link_merge(a, b, name=name)
        return _to_node_type(merged)

    @strawberry.field
    def breath_step(self, steps: int = 1) -> VitalSignsType:
        vit = BREATH.step(WEB, steps=steps)
        return VitalSignsType(
            phase=vit.phase, bpm=vit.bpm, last_ts=vit.last_ts, cycles=vit.cycles
        )

    @strawberry.field
    def link_parent(self, parent_id: str, child_id: str) -> EdgeType:
        parent = next(n for n in WEB.nodes() if n.id == parent_id)
        child = next(n for n in WEB.nodes() if n.id == child_id)
        e = WEB.link_parent(parent, child)
        return _to_edge_type(e)

    @strawberry.field
    def compute_relationship_health(
        self, source_id: str, target_id: str
    ) -> RelationshipHealthType:
        # locate nodes
        a = next(n for n in WEB.nodes() if n.id == source_id)
        b = next(n for n in WEB.nodes() if n.id == target_id)

        # base similarity using diffusion (cosine-like in [0,1])
        d = InMemoryDiffusion()
        base = float(
            d.similarity(
                ModuleState("a", traits=a.traits, notes=[]),
                ModuleState("b", traits=b.traits, notes=[]),
            )
        )

        # soft trait adjustments
        POS_KEYS = getattr(
            SystemBreath,
            "SOFT_KEYS_POS",
            (
                "любовь",
                "спокойствие",
                "нежность",
                "мягкость",
                "calm",
                "love",
                "tenderness",
            ),
        )
        NEG_KEYS = getattr(
            SystemBreath,
            "SOFT_KEYS_NEG",
            ("страх", "гнев", "злость", "тревога", "anger", "fear"),
        )

        def avg(keys: tuple[str, ...]) -> float:
            vals: List[float] = []
            for k in keys:
                if k in a.traits or k in b.traits:
                    va = float(a.traits.get(k, 0.0))
                    vb = float(b.traits.get(k, 0.0))
                    vals.append((va + vb) / 2.0)
            return sum(vals) / len(vals) if vals else 0.0

        pos_avg = avg(tuple(POS_KEYS))
        neg_avg = avg(tuple(NEG_KEYS))

        # weights: gentle influence
        POS_W = 0.10
        NEG_W = 0.10

        bonus = POS_W * pos_avg
        penalty = NEG_W * neg_avg

        # family bond bonus if an explicit parent edge exists between nodes
        has_parent = any(
            (e.source_id == a.id and e.target_id == b.id and e.kind == "parent")
            or (e.source_id == b.id and e.target_id == a.id and e.kind == "parent")
            for e in WEB.edges()
        )
        PARENT_BONUS = 0.05

        raw_score = base + bonus - penalty + (PARENT_BONUS if has_parent else 0.0)
        score = 0.0 if raw_score < 0.0 else 1.0 if raw_score > 1.0 else raw_score

        rationale = [
            f"base_similarity={base:.3f}",
            f"pos_avg={pos_avg:.3f}*{POS_W:.2f} -> +{bonus:.3f}",
            f"neg_avg={neg_avg:.3f}*{NEG_W:.2f} -> -{penalty:.3f}",
            (
                ("parent_bonus=+%.3f" % PARENT_BONUS)
                if has_parent
                else "parent_bonus=+0.000"
            ),
            f"score_clipped={score:.3f}",
        ]

        breakdown = [
            BreakdownEntryType(key="base_similarity", weight=1.0, contribution=base),
            BreakdownEntryType(key="positive_traits", weight=POS_W, contribution=bonus),
            BreakdownEntryType(
                key="negative_traits", weight=NEG_W, contribution=-penalty
            ),
        ]

        # --- Self-healing insight on low health ---
        try:
            threshold = float(os.getenv("LIMINAL_HEALTH_THRESHOLD", "0.4"))
        except ValueError:
            threshold = 0.4
        if score < threshold:
            advice = ["breathStep", "consider_linkParent", "consider_merge"]
            record = {
                "ts": datetime.utcnow().isoformat() + "Z",
                "type": "relationship_low_health",
                "source_id": a.id,
                "target_id": b.id,
                "score": score,
                "threshold": threshold,
                "rationale": rationale,
                "advice": advice,
            }
            path = os.getenv(
                "LIMINAL_INSIGHTS_PATH", str(Path("data") / "insights.jsonl")
            )
            p = Path(path)
            try:
                p.parent.mkdir(parents=True, exist_ok=True)
                with p.open("a", encoding="utf-8") as f:
                    f.write(json.dumps(record, ensure_ascii=False) + "\n")
            except Exception:
                # Non-fatal: insights writing should never break the mutation
                pass

        return RelationshipHealthType(
            score=score, rationale=rationale, breakdown=breakdown
        )


# Module-level singletons sourced from the dependency container
_container = get_container()
WEB = _container.reality_web()
BREATH = _container.system_breath()

schema = strawberry.Schema(query=Query, mutation=Mutation)

# Optional ASGI app for mounting by servers (FastAPI/any ASGI)
try:
    from strawberry.asgi import GraphQL

    graphql_app = GraphQL(schema)
except Exception:  # pragma: no cover - allow import without ASGI extras installed
    graphql_app = None  # type: ignore

import json
import os
from datetime import datetime
from pathlib import Path
