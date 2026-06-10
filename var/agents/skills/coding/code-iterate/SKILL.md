---
name: code-iterate
description: Iteratively find and fix issues until no new actionable findings remain.
---

Run a convergent fix loop: find issues, fix them, repeat until clean.

## Mode Selection

- **Default** (no skill specified): perform review and cleanup iteration using code-review skill, and code-cleanup skill.
- **Specific**: if a skill or an instruction is provided, follow that skill or instruction.

## Process

1. **Identify scope**:
   - If code changes are involved: `jj diff -s` to see changed files; `jj diff -- path` to restrict scope.
   - If the user specified files or paths, focus on those.
   - Capture a safety baseline: note the current diff state or create a new commit on top (`jj new`).
   - Read the `code-test` skill to detect the project's test/lint commands; run a baseline check.

2. **Iteration loop** (max 5 iterations by default):

   a. **Find issues** — use applicable skills and spawn subagents:
       - Default: spawn subagents according to code-review skill, and code-cleanup skill.
       - Specific: spawn subagents that closely matches user's request.

   b. **Filter findings** — synthesize agent output:
       - Deduplicate: drop issues already addressed or explicitly deferred in a prior iteration.
       - Classify: critical/high (must fix), medium (should fix), low/speculative (defer).
       - If no new actionable findings remain → exit loop.

   c. **Fix** — delegate to `worker`:
       - `worker`: "Fix these issues in {files}: {findings list}. Apply minimal targeted, behavior-preserving changes. Do not refactor beyond what's needed to resolve each finding."

   d. **Verify** — run project checks:
       - Run the most specific applicable commands with timeouts.
       - If regressions appear, delegate to `worker` to fix regressions before continuing.

   e. **Commit** — commit iteration pass:
       - Make a commit with "Iteration <n>: <desc>"; these commits are ephemeral and will be reviewed and squashed by the user.

3. **Production audit** (for production-bound changes, after the loop converges):
   - `auditor`: "Audit {files} for production readiness: correctness, security, data loss, migration hazards, and rollback safety."

4. **Final verification**:
   - Run full project checks (test + lint + format).
   - Ensure the diff from the safety baseline is coherent and no unintended changes slipped in.

5. **Report**:
   - Total iterations run.
   - Summary of findings per iteration (what was found, what was fixed, what was deferred).
   - Auditor findings, if run.
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

1. **Scope**: files and mode used
2. **Iterations**: for each iteration — findings, fixes applied, deferred
3. **Final Verification**: check results
4. **Convergence Status**: why the loop stopped (clean / max iterations / stuck)
5. **Remaining Items**: deferred findings requiring manual attention or product decisions
