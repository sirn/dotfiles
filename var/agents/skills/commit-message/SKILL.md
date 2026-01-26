---
name: commit-message
description: Analyzes changes and suggests commit messages following repository conventions. Use when user asks about commits, commit messages, or wants to commit changes.
---

Analyze changes and suggest commit messages following repository conventions.

## Important
**IMPORTANT**: Always use `jj` (Jujutsu) commands. Only fall back to `git` if jj is not available.
Refer to the `jujutsu-reference` skill for command syntax if needed.

## Process
1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories
   - If $ARGUMENTS provided, focus on those specific files/paths

2. Analyze commit message patterns:
   - Run: `jj log -r ::@ -n 20 --no-graph -T 'description ++ "\n---\n"'`
   - Note the style: conventional commits, imperative mood, length, etc.

3. Analyze the changes:
   - Use `jj diff` for full diff view if needed
   - Are changes logically related or distinct?
   - Do they touch different subsystems/features?
   - Are there mixed concerns (refactor + feature, fix + cleanup)?

4. Suggest appropriate commit message(s)

## Output

Provide:
1. **Suggested commit message** following the repo's existing style
2. **Should split?** Yes/No with reasoning
3. If split recommended:
   - How to split (which files/hunks in each commit)
   - Suggested message for each commit
   - Commands to execute the split (use `jj` commands)
