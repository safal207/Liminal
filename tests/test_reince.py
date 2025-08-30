import datetime as dt

from liminal.reince import InMemoryREINCE


def test_empathic_event_recording_is_deterministic():
    fixed_dt = dt.datetime(2025, 1, 1, 12, 0, 0)
    fixed_ts = fixed_dt.timestamp()
    rein = InMemoryREINCE(now_fn=lambda: fixed_ts)

    ev = rein.record_emotional_event(
        "Сегодня было тяжело, много стресса, но я всё равно выбираю идти вперёд",
        meta={"source": "journal"},
    )

    assert ev.timestamp == fixed_dt
    assert ev.meta == {"source": "journal"}
    recent = rein.list_recent(1)
    assert len(recent) == 1
    assert recent[0] == ev


def test_resonance_map_is_stable_and_counts_tokens():
    fixed_dt = dt.datetime(2025, 1, 1, 12, 0, 0)
    rein = InMemoryREINCE(now_fn=lambda: fixed_dt.timestamp())

    rein.record_emotional_event("я чувствую страх и напряжение, но помню про дом внутри")
    rein.record_emotional_event("дом — это место любви и благодарности")
    rein.record_emotional_event("страх уходит, остаётся мягкость и любовь")

    m = rein.get_resonance_map(top_n=5)
    # We expect stable counting; specific words should appear with correct frequencies
    assert m["страх"] == 2
    assert m["дом"] >= 1
    # ensure ordering stability by count desc then token asc
    items = list(m.items())
    assert items == sorted(items, key=lambda kv: (-kv[1], kv[0]))
