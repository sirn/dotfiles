---
name: code-cleanup
description: Find and fix redundancies, non-idiomatic code, dead code, and unnecessary complexity. Use when asked to clean up, simplify, refactor for clarity, remove dead code, or address code-quality findings directly.
---

Clean up code by applying small, behavior-preserving fixes until additional changes would have diminishing returns.

## Operating Principles

- Assume cleanup is requested; focus on safe, behavior-preserving improvements within scope.
- Prefer applying validated fixes over merely generating review findings.
- If analysis is requested without edits, report findings and proposed fixes instead of modifying files.
- Keep changesets easy to review: make small, targeted changes with clear verification.

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

For all the following:
- redundancies
- non-idiomatic code
- dead code
- simplification opportunities
- unnecessary complexity

Report file paths, line numbers, and evidence for each.
```

Once the subagent completes:

- Consult the `code-test` skill to identify project test and lint commands.
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
- Do not invoke other skills directly; only reading `code-test` for command detection is permitted.
