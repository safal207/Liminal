from liminal.reality_web import RealityWebInMemory


def test_reality_web_nodes_edges_and_similarity_and_merge():
    rw = RealityWebInMemory()
    a = rw.add_node(
        "module_state", traits={"дом": 0.9, "спокойствие": 0.8}, notes=["alpha"]
    )
    b = rw.add_node(
        "module_state", traits={"любовь": 0.7, "страх": 0.2}, notes=["beta"]
    )

    # similarity link
    e = rw.link_similarity(a, b)
    assert 0.0 <= e.weight <= 1.0

    # merge
    merged, e2 = rw.link_merge(a, b, name="merged_state")
    assert merged.kind == "module_state"
    assert merged.traits
    assert any(x for x in merged.notes)
    # Edge from a to merged
    assert e2.source_id == a.id and e2.target_id == merged.id
    # There should also be an edge from b to merged
    assert any(ed.source_id == b.id and ed.target_id == merged.id for ed in rw.edges())
