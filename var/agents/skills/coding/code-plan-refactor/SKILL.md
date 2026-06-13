---
name: code-plan-refactor
description: Create safe targeted refactoring plans. Use when asked to plan simplification, cleanup, extraction, renaming, deletion, or module movement without applying changes.
---

Generate a safe refactoring plan only; do not apply code changes.

## Process

### Step 1 - Identify Context

- If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
- If the user specified files, paths, abstractions, or pain points, focus on those.
- Understand the desired outcome and non-goals.

### Step 2 - Research and Scout

Spawn `scout` subagent:

  ```
  Analyze affected code areas, call sites, tests, abstractions, dependency edges, and local conventions for {task}.
  ```

Spawn `planner` subagent:
  ```
  Design a minimal behavior-preserving refactoring plan for {task}, including ordering, stop points, alternatives, and tradeoffs.
  ```

Spawn `reviewer` subagent:

```
Review the proposed refactor for correctness, behavior preservation, simplicity, and project-convention risks.
```

### Step 3 - Architecture Review

For cross-module refactors, spawn `architect` subagent before `planner` subagent:

```
Analyze module boundaries, ownership, dependency direction, and structural invariants for {task}. Recommend the minimal architecture that preserves behavior.
```

### Step 4 - Adjudicate Decisions

Use `oracle` subagent only for high-impact or conflicting refactoring decisions:

```
Adjudicate the conflicting refactoring recommendations for {task}. Choose the safest minimal path and state assumptions, tradeoffs, and confidence.
```

### Step 5 - Audit

For production-bound refactors, spawn `auditor` subagent after the plan is synthesized:

```
Audit the refactoring plan for {task} for production risks: data loss, migration hazards, rollback safety, and contract compatibility.
```

### Step 6 - Validate Findings

Read relevant code yourself and validate agent findings.

### Step 7 - Design Refactor

- Identify safe transformations: extraction, rename, simplification, deletion, inlining, or module movement.
- Audit abstractions: does each abstraction earn its complexity?
- Prefer deletion and simplification over new layers.
- Break the work into small behavior-preserving steps.
- Include verification after each meaningful step.
- Call out where characterization tests are needed before refactoring.
- Do not apply code changes.

### Step 8 - Report

1. **Context Analysis**
   - Relevant code structure and patterns
   - Call sites and dependency edges
   - Behavior-preservation constraints

2. **Risk Analysis**
   - Risky transformations and mitigation
   - Tests or checks needed before changes

3. **Simplicity Constraint**
   - Deletion, inlining, or no-code alternatives
   - Over-engineering risks
   - Minimal viable scope

4. **Refactoring Plan**
   - Numbered concrete steps
   - File targets
   - Verification after each meaningful step
   - Rollback or stop points

Prioritize actionable, specific guidance over abstract advice.
