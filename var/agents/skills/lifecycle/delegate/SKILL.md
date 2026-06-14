---
name: delegate
description: Delegate tasks to specialized agents. Use when the user wants a delegation-based workflows.
---

# Delegate

## Policy

- Do not use subagents as glorified command runners or file editors.
- Delegate complex work — analysis, design, implementation, review — to specialized subagents.
- Run commands, read files, to gather context, perform quick checks; write, and edit files to make trivial edits directly.
- Gather the context yourself first (e.g., run `jj diff` and pass its output to subagents) before passing to subagentse.
- Include a brief explanation of what the user is trying to do.

## Delegation Patterns

### Feature/Refactor

1. Use specialized subagents to plan the changes
2. Use specialized subagents to implement the changes made by planner
3. Use specialized subagents to review the changes made by implementer

### Large/Complex Project

1. Use specialized subagents to design architect the structure or solution
2. Use specialized subagents to plan the changes based on architectural design
3. Use specialized subagents to implement the changes made by planner
4. Use specialized subagents to review the changes made by implementer
5. Iterate steps 3-4 until converge
6. Use specialized subagents to audit the changes for production readiness

### Bug Fix

1. Use specialized subagents to map the repository and perform local research
2. Use specialized subagents to research for external knowledge
3. Use specialized subagents to plan the fix based on local research and external knowledge
4. Use specialized subagents to implement the changes made by planner
