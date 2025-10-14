# flake8-shim

This lightweight shim offers a drop-in replacement for the `flake8` command
when external package repositories are unreachable. It performs a couple of
fast, deterministic checks:

* verifies that every discovered Python file parses successfully via `ast`
* flags lines with trailing whitespace

The implementation is intentionally simple so that it can run in restricted
CI environments while still catching the most common issues that the real
flake8 would surface.
