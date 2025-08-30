from liminal.reality_web import RealityWebInMemory, SystemBreath


def test_system_breath_inhale_exhale_modulation_and_clip():
    rw = RealityWebInMemory()
    n = rw.add_node("module_state", traits={"любовь": 0.95, "страх": 0.05, "прочее": 0.5})

    # Deterministic time
    ts = 1000.0
    breath = SystemBreath(now_fn=lambda: ts, epsilon=0.05, bpm=6.0)

    # Start phase = inhale: should increase love-like traits, capped at 1.0
    vit1 = breath.step(rw, steps=1)
    assert vit1.phase in ("inhale", "exhale")
    n1 = [x for x in rw.nodes() if x.id == n.id][0]
    assert n1.traits["любовь"] >= 0.95  # grew or clipped
    assert 0.0 <= n1.traits["любовь"] <= 1.0

    # Next phase = exhale: reduce fear-like traits, floored at 0.0
    breath.step(rw, steps=1)
    n2 = [x for x in rw.nodes() if x.id == n.id][0]
    assert n2.traits["страх"] <= 0.05
    assert 0.0 <= n2.traits["страх"] <= 1.0

    # Non-target traits stay untouched
    assert n2.traits["прочее"] == 0.5
