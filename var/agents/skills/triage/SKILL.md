---
name: triage
description: Quickly summarize changes and identify review priorities. Use when user asks to triage or assess a large diff.
---

Triage the changes to identify review priorities.

## Process
1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories
2. Skim relevant diffs as needed
3. Identify risk hotspots and review order

## Output
1. **Areas touched**: Key files or modules changed
2. **Risk hotspots**: Where bugs or regressions are most likely
3. **Suggested review order**: What to review first and why
