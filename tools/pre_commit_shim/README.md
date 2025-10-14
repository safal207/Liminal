# pre-commit-shim

A tiny offline-friendly shim that stands in for `pre-commit` when PyPI access
is unavailable. The shim writes a log entry indicating that hooks were skipped
and exits successfully so CI pipelines can continue running other checks.
