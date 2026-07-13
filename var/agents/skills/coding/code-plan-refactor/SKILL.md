---
name: code-plan-refactor
description: Create safe targeted refactoring plans. Use when asked to plan simplification, cleanup, extraction, renaming, deletion, or module movement without applying changes. For small in-place fixes, use `code-cleanup` instead.
---

Generate a safe refactoring plan only; do not apply code changes.

## Process

### Step 1 - Identify Context

- If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
- Focus on any user-specified files, paths, abstractions, or pain points.
- Clarify the desired outcome and non-goals.

### Step 2 - Research and Scout

Research the codebase and design the refactoring plan using specialized subagents.

Spawn the `scout` subagent:

```
Analyze affected code areas, call sites, tests, abstractions, dependency edges, and local conventions for {task}.
```

Spawn the `planner` subagent:

```
Design a minimal behavior-preserving refactoring plan for {task}, including ordering, stop points, alternatives, and tradeoffs.
```

Spawn the `reviewer` subagent:

```
Review the proposed refactor for correctness, behavior preservation, simplicity, and project-convention risks.
```

### Step 3 - Architecture Review

For cross-module refactors, spawn the `architect` subagent before the `planner` subagent:

```
Analyze module boundaries, ownership, dependency direction, and structural invariants for {task}. Recommend the minimal architecture that preserves behavior.
```

### Step 4 - Adjudicate Decisions

For high-impact or conflicting design decisions, spawn the `oracle` subagent to adjudicate:

```
Adjudicate the conflicting refactoring recommendations for {task}. Choose the safest minimal path and state assumptions, tradeoffs, and confidence.
```

### Step 5 - Audit

For production-bound refactors, spawn the `auditor` subagent after synthesizing the plan:

```
Audit the refactoring plan for {task} for production risks: data loss, migration hazards, rollback safety, and contract compatibility.
```

### Step 6 - Validate Findings

Read relevant code yourself and validate the agent's findings.

### Step 7 - Design Refactor

- Identify safe transformations (e.g., extraction, rename, simplification, deletion, inlining, module movement).
- Simplify design: prefer deleting or inlining code over adding new layers, ensuring every abstraction earns its complexity.
- When deciding whether to split or combine modules, read the `software-design` reference and apply its "better together OR better apart" and deep-modules / information-hiding principles; do not split into shallow modules or leak information across boundaries.
- When choosing refactorings, read the `code-smells` reference and match each smell to its primary fix; treat smells as heuristics, not hard violations, and skip what tooling already enforces.
- Break the work into small, behavior-preserving steps, including verification after each step.
- Identify where characterization tests are required before refactoring starts.
- Do not apply any code changes.

### Step 8 - Report

Provide a structured report with:

1. **Context Analysis**
   - Relevant code structure and patterns
   - Call sites and dependency edges
   - Behavior-preservation constraints

2. **Risk Analysis**
   - Risky transformations and mitigation strategies
   - Tests or checks needed before changes

3. **Simplicity Constraint**
   - Deletion, inlining, or no-code alternatives
   - Over-engineering risks and minimal viable scope

4. **Refactoring Plan**
   - Numbered, concrete steps with target files
   - Verification procedures for each step
   - Rollback or stop points

Prioritize actionable, specific guidance over abstract advice.
