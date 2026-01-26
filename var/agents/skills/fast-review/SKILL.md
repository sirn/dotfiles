---
name: fast-review
description: Quick review for bugs and complexity. Use when user asks for a fast or lightweight review.
---

Run a fast review using two focused reviewer agents.

## Process
1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories
   - If $ARGUMENTS provided, focus on those specific files/paths

2. Spawn both reviewer agents in parallel using the Task tool:
   - Use `quality-reviewer` agent: Focus on bugs, logic, and error handling
   - Use `simplicity-reviewer` agent: Focus on over-engineering and unnecessary complexity

3. Consolidate findings into a short report

## Agent Invocation
- `quality-reviewer`: "Review {files} for bugs, logic errors, and error handling issues"
- `simplicity-reviewer`: "Review {files} for over-engineering and unnecessary complexity"

## Output
1. **Critical Issues**
2. **Quality Issues**
3. **Simplification Opportunities**
4. **Quick Wins**
