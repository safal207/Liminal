from __future__ import annotations

from .diffusion import ModuleState
from .reince import InMemoryREINCE


def normalize_counts_to_traits(counts: dict[str, int]) -> dict[str, float]:
    """Normalize keyword counts to [0,1] traits by max count.

    Deterministic, scale-invariant mapping suitable for Diffusion traits.
    Empty input returns empty traits.
    """
    if not counts:
        return {}
    max_c = max(counts.values())
    if max_c <= 0:
        return dict.fromkeys(counts, 0.0)
    return {k: min(1.0, counts[k] / float(max_c)) for k in counts}


def resonance_map_to_module_state(
    name: str, counts: dict[str, int], notes: list[str] | None = None
) -> ModuleState:
    traits = normalize_counts_to_traits(counts)
    return ModuleState(name=name, traits=traits, notes=list(notes or []))


def build_state_from_reince(
    reince: InMemoryREINCE, name: str, top_n: int = 10, notes: list[str] | None = None
) -> ModuleState:
    m = reince.get_resonance_map(top_n=top_n)
    return resonance_map_to_module_state(name=name, counts=m, notes=notes)
