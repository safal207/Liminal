# RFC: Memory Timeline ↔ Neo4j Event Protocol

## Summary

This document describes the minimal event protocol that synchronises the
in-memory **Memory Timeline** with the Neo4j knowledge graph. The protocol
defines the events emitted by the timeline, their payload, and how the
application-level bridge translates them into Neo4j operations.

## Motivation

Historically the FastAPI routes interacted with Neo4j by importing a concrete
writer implementation through direct `sys.path` manipulation. This made the
codebase brittle and prevented the timeline from broadcasting changes to the
graph. The new protocol:

* formalises how timeline mutations are emitted as events;
* allows multiple listeners (Neo4j, observability, analytics) without tight
  coupling;
* enables dependency injection of different Neo4j gateways (real driver vs
  in-memory mock) for production and tests.

## Event Model

Events are emitted by `MemoryTimeline` immediately after a mutation completes.
Each event conforms to the following shape:

```jsonc
{
  "type": "memory.fragment.created",
  "version": "1.0",
  "source": "memory.timeline",
  "occurred_at": "2025-06-18T15:32:11.482931",
  "payload": {
    "id": "mem_42",
    "timestamp": "2025-06-18T15:32:11.480129+00:00",
    "content": "Discover hidden dune patterns",
    "type": "observation",
    "metadata": {
      "growth_stage": "seedling",
      "tags": ["dune", "insight"]
    }
  }
}
```

Key fields:

* `type` — categorises the event. The current minimal protocol defines a single
  event: `memory.fragment.created`.
* `version` — semantic version of the payload contract. Consumers should reject
  unsupported versions.
* `source` — logical originator. Useful for tracing in distributed systems.
* `occurred_at` — ISO-8601 timestamp generated at emission time.
* `payload` — domain specific data for the event.

### Payload Contract

The payload captures the timeline entry and is intentionally close to the
Neo4j `MemoryFragment` node schema. The `metadata` object is a free-form map
that can include additional context (tags, author, growth hints). The bridge
derives the required `growth_stage` property from `metadata.growth_stage`,
defaulting to `"observed"` when absent.

## Bridge Responsibilities

`TimelineGraphBridge` subscribes to the timeline events and translates them into
`Neo4jGateway` operations. The minimal responsibilities are:

1. Receive the event asynchronously.
2. Validate the event `type` and `version` (implicit in the current bridge).
3. Construct the Neo4j node payload:

   ```python
   {
       "id": payload["id"],
       "content": payload["content"],
       "type": payload["type"],
       "growth_stage": payload.get("metadata", {}).get("growth_stage", "observed"),
       "timestamp": payload["timestamp"],
       "metadata": payload.get("metadata", {})
   }
   ```

4. Invoke `Neo4jGateway.create_memory_fragment_node` with the translated data.

Because the gateway is expressed as a Python `Protocol`, any implementation that
provides the required methods (official Neo4j driver, mocks, fakes) can be
plugged in via FastAPI dependency overrides.

## Extensibility

The protocol is intentionally minimal but ready for extension:

* Additional timeline events (e.g. `memory.fragment.updated`,
  `memory.wave.linked`) can be added by defining new `type` values and extending
  the bridge.
* Consumers can register as additional listeners without modifying the timeline
  or the Neo4j bridge.
* The version field enables non-breaking payload evolution by supporting
  multiple translators side-by-side.

## Testing Strategy

Integration tests (`backend/tests/test_memory_timeline_graph_integration.py`)
exercise the protocol by:

* overriding the dependency container with `MockNeo4jGateway`;
* creating a memory via the timeline HTTP endpoint and asserting that the mock
  gateway stored the translated fragment;
* verifying that direct fragment creation routes also use the injected gateway.

These tests ensure both the protocol and dependency injection work end-to-end
without requiring a running Neo4j instance.
