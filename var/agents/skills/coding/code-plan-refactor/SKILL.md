---
name: code-plan-refactor
description: Create safe targeted refactoring plans. Use when asked to plan simplification, cleanup, extraction, renaming, deletion, or module movement without applying changes.
---

Generate a safe refactoring plan only; do not apply code changes.

## Process

1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
   - If the user specified files, paths, abstractions, or pain points, focus on those.
   - Understand the desired outcome and non-goals.

2. Analyze the codebase:
   - Read relevant code areas, neighboring tests, and call sites.
   - Identify safe transformations: extraction, rename, simplification, deletion, inlining, or module movement.
   - Audit abstractions: does each abstraction earn its complexity?
   - Identify behavior-preservation constraints and risky dependency edges.

3. Design the refactor:
   - Prefer deletion and simplification over new layers.
   - Break the work into small behavior-preserving steps.
   - Include verification after each meaningful step.
   - Call out where characterization tests are needed before refactoring.
   - Do not apply code changes.

## Output

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
