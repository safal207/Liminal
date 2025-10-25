.PHONY: test-ml test

# Быстрый тест мультиязычного tie-break (expected_lang)
test-ml:
	python -m pytest -q backend/tests/test_multilingual_expected_lang.py

# Весь набор без integration-маркера
test:
	python -m pytest -v -m "not integration"
