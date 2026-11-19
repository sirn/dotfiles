---
name: code-plan
description: Create actionable plans before modifying code — feature/fix implementation, targeted refactoring (plan-only), API contracts, or database schema/migration design. For small in-place cleanup, use `code-cleanup` instead.
---

Generate a concise, actionable plan based on codebase inspection and official documentation. Do not apply code changes unless explicitly asked.

## Process

### Step 1 - Identify Scope & Context

- For working-copy changes, run `jj diff -s` (and `jj diff -- <path>` to narrow scope).
- Inspect affected files, tests, call sites, dependency edges, and existing conventions directly.
- Clarify expected behavior, constraints, non-goals, and verification criteria.

### Step 2 - Apply Domain Design References

Consult local design references when they apply:

- **Modules, interfaces, or abstractions**: Read `john-ousterhout-software-design` (deep modules, information hiding, pull complexity downward, "better together or better apart", define errors out of existence).
- **Refactoring**: Read `martin-fowler-code-smells` (match smells to primary fixes; treat smells as heuristics and skip what tooling already enforces). Prefer deleting or inlining code over adding layers, identify characterization tests needed first, and break into small behavior-preserving steps with verification after each.
- **Business logic, APIs, or schemas**: Read `martin-fowler-eaa-patterns` (domain-logic and data-source patterns; Remote Facade + DTO across process/network boundaries only; O/R mapping patterns for hierarchies, associations, and N+1 avoidance).

### Step 3 - Research & Risk Check

- Verify external library, protocol, or database/ORM constraints against official docs (note version requirements and pitfalls).
- Evaluate simplicity first: consider no-code, deletion, or minimal viable scope before adding abstractions, fields, or endpoints.
- For production-bound changes, flag material risks: security/authz, data loss, migration/backfill hazards, breaking contract changes, and rollback safety.

### Step 4 - Report

Present only the sections relevant to the task:

1. **Context & Constraints** — relevant files, conventions, integration points, and external doc constraints.
2. **Simplicity & Tradeoffs** — simpler alternatives considered, design choices, and assumptions.
3. **Design** — module boundaries, API shapes, or schema/migration details as needed.
4. **Risks & Mitigations** — security, data integrity, compatibility, or rollback hazards.
5. **Step-by-Step Plan** — numbered concrete steps with target files, verification commands, and stop/rollback points.
