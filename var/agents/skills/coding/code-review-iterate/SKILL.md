---
name: code-review-iterate
description: Iteratively review and fix code until no new actionable findings remain. Use when asked to review-and-fix until clean, harden code before committing, or iterate on quality.
---

Run a convergent review-fix loop: reviewer subagents find issues, worker fixes them, repeat until clean.

## When to Use

- When the user asks to "review and fix until clean" or "iterate until reviewer is happy"
- When you want both independent assessment and resolution (vs `code-review` which only reports)
- When changes need quality hardening before committing

## When Not to Use

- Use `code-review` when you only need a report, no fixes
- Use `code-cleanup-iterate` for quick self-polish without independent multi-lens review
- Use `code-cleanup` for a single pass of direct fixes

## Process

1. **Identify scope**:
   - If code changes are involved: `jj diff -s` to see changed files; `jj diff -- path` to restrict scope.
   - If the user specified files or paths, focus on those.
   - Capture a safety baseline: note the current diff state.

2. **Iteration loop** (max 5 iterations by default):

   a. **Review** — spawn `reviewer` subagents in parallel:
      - `reviewer`: "Review {files} with a correctness/quality lens: bugs, logic errors, edge cases, error handling, resource leaks, concurrency, and performance traps."
      - `reviewer`: "Review {files} with a security lens: OWASP risks, injection flaws, auth/authz, cryptography, sensitive data exposure, dependency risks, and secure defaults."
      - `reviewer`: "Review {files} with a convention/simplicity lens: naming, organization, documentation, project consistency, over-engineering, unnecessary abstractions, dead code, and avoidable indirection."

   b. **Filter findings** — synthesize reviewer output:
      - Deduplicate: drop issues already addressed or explicitly deferred in a prior iteration.
      - Classify: critical/high (must fix), medium (should fix), low/speculative (defer).
      - If no new actionable findings remain → exit loop.

   c. **Fix** — delegate to `worker`:
      - `worker`: "Fix these issues in {files}: {prioritized findings list}. Apply minimal targeted changes. Do not refactor beyond what's needed to resolve each finding."

   d. **Verify** — run project checks:
      - Read `code-test` skill to detect test/lint/format commands.
      - Run the most specific applicable commands with timeouts.
      - If regressions appear, delegate to `worker` to fix regressions before continuing.

3. **Final verification**:
   - Run full project checks (test + lint + format).
   - Ensure the diff from the safety baseline is coherent and no unintended changes slipped in.

4. **Report**:
   - Total iterations run.
   - Summary of findings per iteration (what was found, what was fixed, what was deferred).
   - Final check results.
   - Any remaining items requiring manual attention.

## Convergence Rules

- **Stop when**: no new actionable findings (all remaining are deferred/speculative).
- **Stop when**: an iteration produces no changes (nothing new to fix).
- **Stop when**: max iterations reached (default 5, user can override).
- **Stop when**: findings grow instead of shrink — stop and report, something is wrong.

## Guardrails

- Preserve public behavior, API signatures, and test outcomes.
- Each iteration must produce fewer or equal new findings vs. the prior one.
- Do not make broad rewrites; worker must apply minimal targeted fixes per finding.
- Do not fix issues outside the defined scope.
- Do not invoke or call other skills from within this skill; reading `code-test` for command detection is allowed.

## Output

1. **Scope**: files and scope used
2. **Iterations**: for each iteration — findings, fixes applied, deferred
3. **Final Verification**: check results
4. **Convergence Status**: why the loop stopped (clean / max iterations / stuck)
5. **Remaining Items**: deferred findings requiring manual attention or product decisions
