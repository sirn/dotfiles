---
name: code-iterate
description: Iteratively find and fix issues until no new actionable findings remain.
---

Run a convergent fix loop: find issues, fix them, repeat until clean.

## Mode Selection

- **Default** (no skill specified): perform review and cleanup iteration using code-review skill, and code-cleanup skill.
- **Specific**: if a skill or an instruction is provided, follow that skill or instruction.

## Process

### Step 1 - Identify Scope

- If code changes are involved: `jj diff -s` to see changed files; `jj diff -- path` to restrict scope.
- If the user specified files or paths, focus on those.
- Capture a safety baseline: note the current diff state or create a new commit on top (`jj new`).
- Read the `code-test` skill to detect the project's test/lint commands; run a baseline check.

### Step 2 - Iteration Loop

#### 1. Find issues

If user specify a skill, load that skill and spawn the approrpiate subagents, otherwise:

- Load `code-review` skill.
- Load `code-cleanup` skill.
- Spawn subagents according to these two skills.

#### 2. Filter findings

Synthesize agent output:

- Deduplicate: drop issues already addressed or explicitly deferred in a prior iteration.
- Classify: critical/high (must fix), medium (should fix), low/speculative (defer).
- If no new actionable findings remain → exit loop.

#### 3. Fix

Spawn `worker` subagent:

```
Fix these issues in {files}: {findings list}. Apply minimal targeted, behavior-preserving changes. Do not refactor beyond what's needed to resolve each finding.
```

#### 4. Verify

Run project checks:

- Run the most specific applicable commands with timeouts.
- If regressions appear, delegate to `worker` to fix regressions before continuing.

#### 5. Commit

Commit iteration pass:

- Make a commit with "Iteration <n>: <desc>"; these commits are ephemeral and will be reviewed and squashed by the user.

### Step 3 - Production Audit

If production readiness is requested, spawn `auditor` subagent:

```
Audit {files} for production readiness: correctness, security, data loss, migration hazards, and rollback safety.
```

### Step 4 - Verification

- Run full project checks (test + lint + format).
- Ensure the diff from the safety baseline is coherent and no unintended changes slipped in.

### Step 5 - Report

Report the following to the user:

- **Scope**: files and mode used
- **Iterations**: for each iteration — total iterations run, summary of findings per iteration (what was found, what was fixed, what was deferred)
- **Final Verification**: final check results
- **Convergence Status**: why the loop stopped (clean / max iterations / stuck)
- **Auditor Findings**: if the production audit was run
- **Remaining Items**: deferred findings requiring manual attention or product decisions

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
