---
name: code-cleanup
description: Find and fix surface hygiene (typos, naming, comment hygiene, editing artifacts, debug statements) and redundancies, non-idiomatic code, dead code, and unnecessary complexity. Use when asked to clean up, simplify, fix typos or comment hygiene, remove dead code, or address code-quality findings directly.
---

Apply small, behavior-preserving fixes — surface hygiene and complexity/redundancy — until further changes would have diminishing returns.

## Operating Principles

- Assume cleanup is requested; focus on safe, behavior-preserving improvements within scope.
- Prefer applying validated fixes over merely generating review findings.
- If analysis is requested without edits, report findings and proposed fixes instead of modifying files.
- Keep changesets easy to review: make small, targeted changes with clear verification.
- For larger structural refactors requiring planning (extraction/rename/movement across modules), use `code-plan-refactor`; this skill applies only small in-place fixes.
- For runtime/production errors and symptoms, use `code-debug`; this skill addresses code quality, not diagnosing failures.

## Modes

- **Focused** (default): Clean the requested files, diff, or subsystem.
- **Diff**: Clean only current working-copy changes.
- **Opportunistic**: Clean nearby low-risk issues within the scope.
- **Conservative**: Remove only obvious dead or redundant code and simple non-idioms.

## Process

### Step 1 - Identify Scope

- If code changes are involved: run `jj diff -s` first to see changed files, then use `jj diff -- path` to narrow down to specific files/directories.
- Focus on user-specified files or paths.
- Determine the cleanup mode from the request, defaulting to **Focused**.

### Step 2 - Map Cleanup Opportunities

Spawn the `scout` subagent:

```
Map cleanup opportunities in the following scope:
{scope}

- Read the `code-smells` reference and flag relevant Fowler smells (Duplicated Code, Lazy Elements, Speculative Generality, Middle Man, Long Function, etc.); treat smells as heuristics and skip what the project linter/formatter already enforces.

### Redundancies

- Redundant defensive checks that restate the obvious (where surrounding code or types already guarantee the condition)

  Example:
  - `if arr is not None and len(arr) > 0:` when the caller or context already ensures `arr` is a non-empty list

- Redundant intermediate variables that just rename and are used once, adding no clarity

  Example:
  - `is_valid = check(x); if is_valid:` -> `if check(x):`

- Duplicate logic that could share a single source

  Example:
  - Three near-identical blocks differing only by a constant

### Non-idiomatic code

- Code that ignores the language/framework's established idioms in favor of a verbose manual equivalent

  Example:
  - Manual index-based loop where a comprehension or `for...in` loop is idiomatic

- Reinventing standard-library functionality

  Example:
  - Reimplemented `unique()` list utility instead of using the language's built-in set/deduplication primitive

### Dead code

- Unreachable code after early returns or raises

  Example:
  - Statements placed after an unconditional `return` or `raise` in the same block

- Unused imports, variables, functions, or fields

  Example:
  - An import kept only by a reference that was since removed

- Commented-out code left behind after a change

### Simplification opportunities

- Overly complex conditionals that collapse to a simpler expression

  Example:
  - `if x == True:` -> `if x:`
  - Nested `if` chains that combine cleanly

- Unnecessary abstraction layers for single use cases

  Example:
  - A generic wrapper interface or class with only one caller and one implementation

### Unnecessary complexity

- Premature generalization not justified by current requirements

  Example:
  - Configurable hooks, strategies, or parameters with only one strategy or value ever passed

- Deep nesting that flattens with early returns or guards

  Example:
  - A four-level nested `if` structure that reads more cleanly as a sequence of guard clauses

### Reimplemented functionality

- Reimplemented logic that duplicates a well-known, trustworthy library or stdlib utility

  Example:
  - Reimplemented date parsing instead of using the language's date/time library
  - Custom retry/backoff loop instead of a maintained retry library
  - Reimplemented argument parser instead of the language's standard CLI library

- Reinventing functionality that a popular, trusted dependency in the ecosystem already provides

  Example:
  - Custom HTTP client wrapper reimplementing timeout/retry/redirect handling already offered by a mainstream client
  - Reimplemented JSON schema validation when a maintained validator exists

  Exception:
  - The user explicitly asked for a reimplemented or dependency-free implementation
  - No existing library fits the constraints (license, size, platform) and a note explains why

### Hygiene and artifacts

- Spelling typos, grammatical errors, and naming or formatting convention violations.
- Whitespace/newline noise, missing final newline, and merge artifacts.
- Ad-hoc debug statements such as `print()` or `console.log()`.
- Out-of-scope changes and inconsistent coding conventions.

### Comment hygiene

- Core principle: comments must capture *why not* — the non-obvious rationale, rejected alternative, or constraint. Any comment describing what the code does or how it works is a violation.
- Decorated inline comments (e.g. `// --- Title ---` -> `// Title`; `# ==== Title ====` -> `# Title`). Exception: multi-line section borders.
- Commented-out code and orphaned TODOs; transitional/legacy comments; comments referencing deleted constructs; comments describing removed behavior.
- Comments restating a following list's size/count; obvious section headers; step-by-step narration; block-end markers; "Note:/Important:" prefixes that add no value; type-signature restatements; built-in/stdlib explanations; "This function does X" repeating the name; over-detailed docstrings on trivial functions; filler words (simply/just/basically); conversational references ("As discussed above"); change-history narration ("replaced x with y").
- Exceptions: section headers that improve readability; extremely non-obvious code.

Report file paths, line numbers, and evidence for each.
```

Once the subagent completes:

- Consult the `code-check` skill to identify project test and lint commands.
- Run targeted tests or checks on the current diff to establish a safety baseline.

### Step 3 - Synthesize Findings

- Filter to safe, behavior-preserving improvements, discarding speculative items that lack clear evidence.
- Align with existing project patterns rather than external preferences.

### Step 4 - Apply Fixes

Spawn the `worker` subagent:

```
Apply these cleanup fixes in the following files:
{files}

{prioritized findings list}

Apply one logical cleanup per step.
Preserve public behavior, API signatures, and test expectations.
```

### Step 5 - Stop at Diminishing Returns

Halt cleanup when:

- Remaining issues are speculative or lack clear evidence.
- Fixes require risky, cross-cutting refactors.
- Changes require product or API decisions beyond the request.
- Edits produce churn without clear maintainability value.

### Step 6 - Verify

- Re-run baseline safety checks.
- Apply the project formatter if applicable.

### Step 7 - Report

Report the following details to the user:

- **Scope**: Files and mode targeted.
- **Cleanup Applied**: Each modification with a before/after summary.
- **Verification**: Test and check results confirming behavior was preserved.
- **Deferred/Remaining Items**: Identified issues left unfixed, with rationale.

## Guardrails

- Avoid broad rewrites or behavioral/API changes unless explicitly requested.
- Do not fix unrelated issues outside the defined scope.
- Do not delete code unless usage analysis and build evidence support removal, or the user explicitly asked.
- Always preserve public behavior and test outcomes.
- Do not invoke other skills directly; only reading `code-check` for command detection is permitted.