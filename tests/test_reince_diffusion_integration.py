from liminal.adapters import build_state_from_reince
from liminal.diffusion import InMemoryDiffusion
from liminal.reince import InMemoryREINCE


def test_reince_resonance_to_diffusion_state_and_blend_offline():
    # Build two resonance maps from different event sets
    r1 = InMemoryREINCE(now_fn=lambda: 1735732800.0)  # 2025-01-01T12:00:00Z
    r1.record_emotional_event("я чувствую страх и напряжение, но помню про дом")
    r1.record_emotional_event("дом и мягкость дают спокойствие")

    r2 = InMemoryREINCE(now_fn=lambda: 1735732800.0)
    r2.record_emotional_event("любовь и благодарность усиливают радость")
    r2.record_emotional_event("страх уходит, остаётся любовь")

    s1 = build_state_from_reince(r1, name="state_alpha", top_n=10, notes=["alpha"])
    s2 = build_state_from_reince(r2, name="state_beta", top_n=10, notes=["beta"])

    d = InMemoryDiffusion()
    blended = d.blend([s1, s2], weights=[0.6, 0.4], name="merged")

    # Basic invariants
    assert blended.state.name == "merged"
    assert "alpha" in blended.state.notes
    assert "beta" in blended.state.notes

    # Traits must include shared keywords like "страх" if present in either map
    # and be within [0,1]
    for _k, v in blended.state.traits.items():
        assert 0.0 <= v <= 1.0

    # If both states mention "страх", expect non-zero
    if "страх" in s1.traits or "страх" in s2.traits:
        assert blended.state.traits.get("страх", 0.0) > 0.0

    # Rationale present and references top traits
    assert len(blended.rationale) >= 1
