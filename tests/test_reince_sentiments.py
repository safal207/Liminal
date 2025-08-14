from liminal.reince import InMemoryREINCE


def test_sentiments_basic_lexicon_mapping_and_normalization():
    r = InMemoryREINCE(now_fn=lambda: 1735732800.0)
    s = r.sentiments("я чувствую радость и нежность, тихо и спокойно")
    # Expect tags present
    assert s.get("joy", 0) > 0
    assert s.get("tenderness", 0) > 0
    assert s.get("calm", 0) > 0
    # Normalized to [0,1]
    assert all(0.0 <= v <= 1.0 for v in s.values())


def test_sentiments_recent_aggregates_over_events():
    r = InMemoryREINCE(now_fn=lambda: 1735732800.0)
    r.record_emotional_event("любовь и благодарность")
    r.record_emotional_event("страх и тревога")
    agg = r.sentiments_recent(limit=10)
    assert agg.get("love", 0.0) > 0.0
    assert agg.get("fear", 0.0) > 0.0
    assert all(0.0 <= v <= 1.0 for v in agg.values())
