---
name: delegate
description: Delegate tasks to specialized agents. Use when the user wants a delegation-based workflows.
---

# Delegate

## Policy

- Do not use subagents as glorified command runners or file editors.
- Delegate complex work—such as analysis, design, implementation, and review—to specialized subagents.
- Run commands, read files, and perform quick checks yourself to gather context; edit and write files directly for trivial changes.
- Gather context yourself first (e.g., run `jj diff` and collect its output) before delegating to subagents.
- Include a brief explanation of what the user is trying to do when delegating.

## Delegation Patterns

### Feature/Refactor

1. **Plan**: Use a specialized subagent to design the changes.
2. **Implement**: Use a specialized subagent to implement the planned changes.
3. **Review**: Use a specialized subagent to review the implementation.

### Large/Complex Project

1. **Architect**: Use a specialized subagent to design the structural solution.
2. **Plan**: Use a specialized subagent to plan the changes based on the architectural design.
3. **Implement**: Use a specialized subagent to implement the planned changes.
4. **Review**: Use a specialized subagent to review the implementation.
5. Iterate implementation and review until completed.
6. **Audit**: Use a specialized subagent to audit the changes for production readiness.

### Bug Fix

1. **Research (Local)**: Use a specialized subagent to map the repository and perform local research.
2. **Research (External)**: Use a specialized subagent to research external knowledge.
3. **Plan**: Use a specialized subagent to plan the fix based on local research and external knowledge.
4. **Implement**: Use a specialized subagent to implement the planned changes.
