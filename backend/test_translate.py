try:
    from backend.pythia_graph.pythia_graph import translate_concept
except ModuleNotFoundError:  # pragma: no cover - legacy fallback
    from pythia_graph.pythia_graph import translate_concept


def test_translation():
    word = "доверие"
    translated = translate_concept(word)
    print(f"Перевод для '{word}' → '{translated}'")


if __name__ == "__main__":
    test_translation()
