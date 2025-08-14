from liminal.diffusion import InMemoryDiffusion, ModuleState


def test_blend_weighted_average_and_notes_dedup_sorted():
    d = InMemoryDiffusion()
    a = ModuleState(
        name="A", traits={"calm": 0.8, "focus": 0.2}, notes=["soft", "focus"]
    )
    b = ModuleState(name="B", traits={"calm": 0.2, "love": 0.9}, notes=["love", "soft"])

    res = d.blend([a, b], weights=[0.75, 0.25], name="mix")

    # calm = 0.75*0.8 + 0.25*0.2 = 0.6 + 0.05 = 0.65
    assert abs(res.state.traits["calm"] - 0.65) < 1e-9
    # focus = 0.75*0.2 + 0.25*0.0 = 0.15
    assert abs(res.state.traits.get("focus", 0.0) - 0.15) < 1e-9
    # love = 0.75*0 + 0.25*0.9 = 0.225
    assert abs(res.state.traits.get("love", 0.0) - 0.225) < 1e-9

    # notes dedup and sorted
    assert res.state.notes == sorted(["soft", "focus", "love"])
    assert res.state.name == "mix"
    assert len(res.rationale) <= 3


def test_dissolve_tension_equals_equal_weights_blend():
    d = InMemoryDiffusion()
    a = ModuleState(name="A", traits={"calm": 1.0}, notes=["note1"])
    b = ModuleState(name="B", traits={"calm": 0.0}, notes=["note2"])

    res1 = d.dissolve_tension(a, b, name="harm")
    res2 = d.blend([a, b], weights=[0.5, 0.5], name="harm")

    assert res1.state.traits == res2.state.traits
    assert res1.state.notes == res2.state.notes
    assert res1.state.name == "harm"


def test_similarity_cosine_like_and_bounds():
    d = InMemoryDiffusion()
    a = ModuleState(name="A", traits={"x": 1.0, "y": 0.0}, notes=[])
    b = ModuleState(name="B", traits={"x": 1.0, "y": 0.0}, notes=[])
    c = ModuleState(name="C", traits={"x": 0.0, "y": 1.0}, notes=[])

    assert abs(d.similarity(a, b) - 1.0) < 1e-12
    assert d.similarity(a, c) == 0.0

    # empty traits -> similarity 1.0 by convention
    e = ModuleState(name="E", traits={}, notes=[])
    f = ModuleState(name="F", traits={}, notes=[])
    assert d.similarity(e, f) == 1.0
