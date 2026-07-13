---
name: code-iterate
description: Iteratively find and fix issues until no new actionable findings remain.
---

Run a convergent fix loop: find issues, fix them, repeat until clean.

## Mode Selection

- **Default** (no skill specified): Perform review and cleanup iterations using the `code-review` and `code-cleanup` skills.
- **Specific**: If a skill or instruction is specified, follow that target instead.

## Process

### Step 1 - Identify Scope

- **Code Changes**: Use `jj diff -s` to inspect changed files, or `jj diff -- path` to restrict the scope.
- **Targeted Files**: Focus on any paths specified by the user.
- **Safety Baseline**: Run `jj new` to create a fresh, empty commit on top of the current state.
- **Baseline Verification**: Read the `code-check` skill to detect the project's test/lint commands and run a baseline check.

### Step 2 - Iteration Loop

#### 1. Find issues

If a specific skill is provided, load it and spawn its corresponding subagents. Otherwise:

- Load the `code-review` and `code-cleanup` skills, then spawn their subagents.

#### 2. Filter findings

Synthesize agent outputs by applying these rules:

- **Deduplicate**: Drop issues already addressed or explicitly deferred in a prior iteration.
- **Classify**: Categorize findings as critical/high (must fix), medium (should fix), or low/speculative (defer).
- **Exit Condition**: If no new actionable findings remain, exit the loop.

#### 3. Fix

Spawn the `worker` subagent with this prompt:

```
Fix these issues in {files}: {findings list}. Apply minimal targeted, behavior-preserving changes. Do not refactor beyond what's needed to resolve each finding.
```

#### 4. Verify

Run project checks:

- Execute the most specific applicable test/lint commands with timeouts.
- If regressions are introduced, delegate to the `worker` subagent to resolve them before continuing.

#### 5. Commit

Commit the iteration pass:

- Create an ephemeral commit named `"Iteration <n>: <desc>"`. These commits will be reviewed and squashed by the user later.

### Step 3 - Production Audit

If production readiness is requested, spawn the `auditor` subagent with this prompt:

```
Audit {files} for production readiness: correctness, security, data loss, migration hazards, and rollback safety.
```

### Step 4 - Verification

- Run comprehensive project checks (test, lint, and format).
- Ensure the diff against the safety baseline is coherent and no unintended changes slipped in.

### Step 5 - Report

Present a summary report to the user:

- **Scope**: Target files and mode of operation.
- **Iterations**: Total iterations run, with a brief summary of findings (found, fixed, deferred) for each pass.
- **Final Verification**: Results of the final checks.
- **Convergence Status**: Reason the loop stopped (e.g., clean run, maximum iterations reached, or progress stalled).
- **Auditor Findings**: Results of the production audit (if requested).
- **Remaining Items**: Deferred findings requiring manual review or product decisions.

## Convergence Rules

Stop the iteration loop when:

- **No actionable findings**: All remaining issues are deferred or speculative.
- **No changes**: An iteration pass makes no modifications to the code.
- **Limit reached**: The maximum iteration count is reached (default is 5, unless overridden by the user).
- **Divergence**: The list of findings grows rather than shrinks, indicating a potential issue with the fixes.

## Guardrails

- **Preserve Behavior**: Do not alter public behavior, API signatures, or test outcomes.
- **Progressive Improvement**: Each iteration must yield fewer or equal new findings compared to the previous pass.
- **Minimal Rewrites**: Apply strictly targeted fixes; do not perform broad refactoring.
- **Strict Scope**: Do not address issues outside the defined target scope.
- **Isolation**: Do not chain-invoke other skills as black boxes; read `code-review` and `code-cleanup` and replicate their subagent prompts. Only reading `code-check` for command detection is permitted.
