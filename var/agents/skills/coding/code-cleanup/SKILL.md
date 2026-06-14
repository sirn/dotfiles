---
name: code-cleanup
description: Find and fix redundancies, non-idiomatic code, dead code, and unnecessary complexity. Use when asked to clean up, simplify, refactor for clarity, remove dead code, or address code-quality findings directly.
---

Clean up code by applying small, behavior-preserving fixes until additional changes would have diminishing returns.

## Operating Principles

- Assume cleanup has been requested; focus on safe, behavior-preserving improvements in the requested scope.
- Prefer applying validated fixes over producing review-style findings.
- If the user explicitly requested analysis without edits, report cleanup findings and suggested fixes instead of modifying files.
- Keep the working tree easy to review: small, targeted changes with clear verification.

## Modes

- **Focused** (default): Clean the requested files, diff, or subsystem.
- **Diff**: Clean only current working-copy changes.
- **Opportunistic**: Clean nearby issues only when they are clearly in scope and low risk.
- **Conservative**: Only remove obvious dead or redundant code and trivial non-idioms.

## Process

### Step 1 - Identify Scope

- If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
- If the user specified files or paths, focus on those.
- Determine mode from the request; default to **Focused**.

### Step 2 - Map Cleanup Opportunities

Spawn `scout` subagent:

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

Once subagent finished:

- Read the `code-test` skill to detect the project's test/lint commands.
- Capture the current diff and run targeted tests or checks as a safety baseline.

### Step 3 - Synthesize Findings

- Filter to safe, behavior-preserving improvements.
- Drop speculative items lacking clear evidence.
- Prefer existing project patterns over external preferences.

### Step 4 - Apply Fixes

Spawn `worker` subagent:

```
Apply these cleanup fixes in the following files:
{files}

{prioritized findings list}

Apply one logical cleanup per step.
Preserve public behavior, API signatures, and test expectations.
```

### Step 5 - Stop at Diminishing Returns

- Stop when remaining issues are speculative or lack clear evidence.
- Stop when fixes would require risky cross-cutting refactors.
- Stop when changes need product or API decisions beyond the user's request.
- Stop when edits would produce churn without clear maintainability value.

### Step 6 - Verify

- Re-run the checks from the safety baseline.
- Run the project formatter when applicable.

### Step 7 - Report

Report the following to the user:

- **Scope**: files and mode used
- **Cleanup Applied**: each change with before/after summary
- **Verification**: check results confirming behavior is preserved
- **Deferred/Remaining Items**: issues identified but not fixed, with rationale

## Guardrails

- Do not make broad rewrites or behavior/API changes unless the user explicitly requested them.
- Do not fix unrelated issues outside the defined scope.
- Do not delete code unless usage search and build evidence support removal, or the user explicitly asked.
- Preserve public behavior and test outcomes.
- Do not invoke or call other skills from within this skill; reading `code-test` for command detection is allowed.
