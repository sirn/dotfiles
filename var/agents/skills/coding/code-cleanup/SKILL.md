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

1. Identify scope:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
   - If the user specified files or paths, focus on those.
   - Determine mode from the request; default to **Focused**.

2. Read project instructions and relevant code/tests:
   - Check local instructions and conventions: `README.md`, `CONTRIBUTING.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md` when present.
   - Read relevant implementation and test files.
   - Prefer existing project patterns over external preferences.

3. Establish safety baseline:
   - Read the `code-test` skill to detect the project's test/lint commands.
   - Capture the current diff and run targeted tests or checks, so regressions can be detected.

4. Find cleanup candidates:
   - Redundancies: duplicate logic, repeated conditions, unused imports, shadowed variables.
   - Non-idiomatic code: patterns that contradict project conventions or language idioms.
   - Dead code: unreachable branches, unused exports, stale comments referencing removed code.
   - Simplification opportunities: unnecessary abstractions, overly complex conditionals, code that can be replaced with standard library or well-known utilities.
   - Unnecessary complexity: over-engineered designs that can be collapsed without behavior change.

5. Fix in small, behavior-preserving batches:
   - Apply one logical cleanup per step or a tightly related set.
   - Re-run relevant checks after each batch when practical.
   - Preserve public behavior, API signatures, and test expectations.

6. Stop at diminishing returns:
   - Stop when remaining issues are speculative or lack clear evidence.
   - Stop when fixes would require risky cross-cutting refactors.
   - Stop when changes need product or API decisions beyond the user's request.
   - Stop when edits would produce churn without clear maintainability value.

7. Verify targeted behavior and formatting:
   - Re-run the checks from the safety baseline.
   - Run the project formatter when applicable.

8. Report changes and remaining deferred items.

## Guardrails

- Do not make broad rewrites or behavior/API changes unless the user explicitly requested them.
- Do not fix unrelated issues outside the defined scope.
- Do not delete code unless usage search and build evidence support removal, or the user explicitly asked.
- Preserve public behavior and test outcomes.
- Do not invoke or call other skills from within this skill; reading `code-test` for command detection is allowed.

## Output

1. **Scope**: files and mode used
2. **Cleanup Applied**: each change with before/after summary
3. **Verification**: check results confirming behavior is preserved
4. **Deferred/Remaining Items**: issues identified but not fixed, with rationale
