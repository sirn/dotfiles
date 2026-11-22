---
name: sirn-coding-style
type: reference
description: Use when designing, writing, refactoring, or reviewing application code, libraries, shell scripts, build files, or tests in sirn's style, or when code drifts toward monolithic managers, hidden globals, deeply nested branches, stringly-typed errors, or brittle mock-heavy tests.
---

# Coding Style

Pair this skill with `sirn-writing-style` for docstrings and comments, and `john-ousterhout-software-design` for module boundaries. Prefer standard library primitives, explicit dependencies, flat control flow, and declarative structure over framework magic, clever abstractions, or deep inheritance hierarchies.

## Architecture & Explicitness

- **Prefer the standard library and deep, simple modules**: Reach for standard library tools before adding external dependencies or thin wrapper libraries. Avoid extracting 1-line pass-through helpers just to shorten functions; prefer one cohesive interface that hides internal complexity.
- **Keep entrypoints thin**: HTTP handlers, CLI commands, and framework hooks should only parse input, validate, delegate to domain logic, and format output. Keep persistence, domain rules, caching, and orchestration inside focused domain modules rather than fat controllers or models.
- **Group by shared invariants, split across boundaries**: Keep operations that share a data representation, schema, or cache-invalidation invariant together in one deep module (avoid splitting mechanically into shallow CRUD wrappers that leak shared internals). Split modules when responsibilities genuinely differ in abstraction, lifecycle, or callers (for example, transport adapters vs. domain services, or rate limiting vs. content filtering).
- **Inject dependencies at boundaries, not through call chains**: Pass collaborators (database handles, caches, clocks, environment maps, sibling services) once at module or service construction (or inside a cohesive runtime context) rather than reading hidden globals or threading pass-through arguments down unrelated intermediate functions.
- **Prefer declarative tables over branch ladders**: Express mappings for configuration keys, provider dispatch, routes, and error translation as declarative tables or registries rather than repetitive `if`/`else` or `switch` chains.
- **Model data and absence explicitly**: Return named structs, records, or typed objects instead of ad-hoc maps or positional tuples. Distinguish "unset" from a valid empty or null value using the language's idiomatic mechanism (sentinel value, nested `Option`, or discriminated union).

## Control Flow, Errors & Reliability

- **Keep control flow flat with guard clauses**: Validate preconditions, missing records, and invalid states upfront with early returns or errors so the main happy path stays unindented at the bottom.
- **Eliminate or mask internal errors; type boundary errors**: First design edge cases out of existence (for example, idempotent deletes, or returning `false` when an optional filter has no providers configured) and mask recoverable low-level errors inside the module. For genuinely caller-actionable failures, define distinct typed domain errors carrying the context needed at the boundary (machine-readable kind, status, or details) and translate them in one centralized handler.
- **Fail fast on misconfiguration, degrade explicitly on optional I/O**: Reject missing required config or collaborators immediately at startup or construction. For optional external calls, set an explicit timeout, catch only the expected failure modes, return a clear fallback, and avoid caching failed or missing results.
- **Keep scripts and builds predictable**: Write portable shell scripts in POSIX `sh` where practical with `set -eu` and quoted or escaped arguments. Keep build definitions (`Makefile`, task runners, Nix) incremental, overridable, and grouped by clear lifecycle targets.

## Naming & Code Hygiene

- **Make names symmetrical and self-describing**: Name lookups after both what they return and what they key on (`userFromId`, `board_from_slug`), prefix boolean predicates clearly (`is`, `has`, `can`, `should`, `was`), and follow the host language's idiomatic casing and visibility conventions.
- **Keep layout consistent**: Rely on the project's formatter and linter, and keep imports grouped cleanly (standard library, third-party, local) and sorted within each group.

## Testing

- **Keep tests self-contained**: Construct minimal test state explicitly near each test using small local helpers rather than opaque, shared global fixtures.
- **Prefer lightweight fakes over heavy mocks**: Use tiny in-memory doubles or fakes implementing only the required contract instead of mocking frameworks and brittle call-order assertions.
- **Isolate transitions and boundaries**: Write separate, clearly named tests for each state transition, error path, and boundary input, asserting observable public outputs and errors rather than private internals.

## Examples

### Declarative dispatch + flat guard clauses + masked optional I/O

```go
var formatters = map[string]Formatter{
	"json": formatJSON,
	"text": formatText,
}

func (s *ExportService) Export(format string, id string) (ExportResult, error) {
	fn, ok := formatters[format]
	if !ok {
		return ExportResult{}, ErrUnsupportedFormat{Format: format}
	}
	record, err := s.store.RecordFromID(id)
	if err != nil {
		return ExportResult{}, err
	}
	// Optional enrichment: timeout degrades cleanly instead of failing export.
	meta, ok := s.fetchMetaWithTimeout(id)
	return ExportResult{Body: fn(record, meta), Enriched: ok}, nil
}
```

### Explicit absence modeling (distinguishing unset from explicitly empty)

```ts
type ProxySetting =
  | { kind: "unset" }
  | { kind: "disabled" }
  | { kind: "url"; url: string };

function proxyFromEnv(
  env: Record<string, string | null | undefined>,
): ProxySetting {
  if (!("PROXY_URL" in env) || env.PROXY_URL === undefined)
    return { kind: "unset" };
  if (env.PROXY_URL === null || env.PROXY_URL === "")
    return { kind: "disabled" };
  return { kind: "url", url: env.PROXY_URL };
}
```

### Self-contained test with a lightweight inline fake

```python
class _FakeStore:
    def __init__(self, statuses):
        self._statuses = statuses
        self.saved = []

    def channel_from_slug(self, slug):
        return self._statuses[slug]

    def save(self, item):
        self.saved.append(item)


def test_create_rejects_archived_channel():
    store = _FakeStore({"general": "archived"})
    service = SubmissionService(store=store)
    with pytest.raises(ChannelArchivedError):
        service.create("general", "hello")
    assert store.saved == []
```
