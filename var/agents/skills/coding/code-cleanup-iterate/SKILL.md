---
name: code-cleanup-iterate
description: Iteratively clean up code until no new cleanup opportunities remain. Use when asked to polish, simplify, or clean up until nothing else can be improved.
---

Run a convergent cleanup loop: scout identifies cleanup opportunities, worker applies fixes, repeat until clean.

## When to Use

- When the user asks to "clean up until there's nothing left" or "keep polishing until clean"
- When you want iterative cleanup with dedicated fix application (vs `code-cleanup` which is single-pass)
- When the code needs several rounds of simplification and each round may reveal further opportunities

## When Not to Use

- Use `code-cleanup` for a single pass (faster, good enough for most cases)
- Use `code-review-iterate` when you want independent multi-lens reviewer assessment (catches more, higher overhead)
- Use `code-review` when you only need a report, no fixes

## Operating Principles

- Assume iterative cleanup has been requested; focus on safe, behavior-preserving improvements.
- Prefer applying validated fixes over producing review-style findings.
- Keep the working tree easy to review: small, targeted changes with clear verification.
- Each iteration should reveal fewer new opportunities than the prior one.

## Process

1. **Identify scope**:
   - If code changes are involved: `jj diff -s` to see changed files; `jj diff -- path` to restrict scope.
   - If the user specified files or paths, focus on those.

2. **Establish safety baseline**:
   - Read the `code-test` skill to detect the project's test/lint commands.
   - Capture the current diff and run targeted tests or checks, so regressions can be detected.

3. **Iteration loop** (max 5 iterations by default):

   a. **Find cleanup candidates** — spawn `scout`:
      - `scout`: "Identify cleanup opportunities in {files}: redundancies, non-idiomatic code, dead code, simplification opportunities, and unnecessary complexity. Report each with file path, line, and what should change."

   b. **Filter findings**:
      - Deduplicate: drop issues already addressed or explicitly deferred in a prior iteration.
      - If no new cleanup opportunities remain → exit loop.

   c. **Fix** — delegate to `worker`:
      - `worker`: "Apply these cleanup fixes in {files}: {opportunities list}. Use small, behavior-preserving changes. Preserve public behavior, API signatures, and test expectations."

   d. **Verify**:
      - Re-run the checks from the safety baseline.
      - If regressions appear, delegate to `worker` to fix regressions before continuing.

4. **Final verification**:
   - Re-run the full safety baseline checks.
   - Run the project formatter when applicable.

5. **Report**:
   - Total iterations run.
   - Summary of changes per iteration.
   - Final check results.
   - Any remaining items deferred due to diminishing returns.

## Convergence Rules

- **Stop when**: no new cleanup opportunities (all remaining are speculative or cross-cutting).
- **Stop when**: an iteration produces no changes.
- **Stop when**: max iterations reached (default 5, user can override).
- **Stop at diminishing returns**: remaining issues are speculative, need product decisions, or would require risky cross-cutting refactors.

## Guardrails

- Do not make broad rewrites or behavior/API changes unless explicitly requested.
- Do not fix unrelated issues outside the defined scope.
- Do not delete code unless usage search and build evidence support removal, or the user explicitly asked.
- Preserve public behavior and test outcomes.
- Do not invoke or call other skills from within this skill; reading `code-test` for command detection is allowed.

## Output

1. **Scope**: files used
2. **Iterations**: for each iteration — opportunities found, fixes applied, deferred
3. **Final Verification**: check results confirming behavior is preserved
4. **Convergence Status**: why the loop stopped (clean / max iterations / diminishing returns)
5. **Deferred/Remaining Items**: issues identified but not fixed, with rationale
